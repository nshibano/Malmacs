namespace Malmacs

open System
open System.Windows.Forms
open System.Drawing
open System.IO
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Diagnostics

open FsMiniMAL
open FsMiniMAL.Types
open FsMiniMAL.Value
open FileHelper
    
type EditorState =
    | Idle
    | LeftDown // MouseMove will be accepted in this state only. (If it accept MouseMove in Idle state, selection range will be lost when miximizing the window by double click the bar.)
    | Compositioning

and EditorHighlightingState =
    | EHSdone // highlighting thread is not running
    | EHShighlighting // highlighting thread is running
    | EHSneedRestart // highlighting thread is running, but document has been modified after start, so that restarting highlighting thread is required after the highlighting thread does sei().

and Message =
    | MhighlightingRequired of Editor
    | MkeyPress of Editor * int

and MalThread =
    | MTchunk of string
    | MTmessage of Message * Interpreter
    | MThighlighting of Editor * Interpreter

and Repl() as this =
    inherit Form()

    let mutable state = Idle
    let mutable hasCaret = false

    let vScrollWidth = 20
    let leftMargin = 5
    let padding = 1

    let menu = new MenuStripWithClickThrough()
    let menu_file = new ToolStripMenuItem("File")
    let menu_file_new = new ToolStripMenuItem("New")
    let menu_file_open = new ToolStripMenuItem("Open")

    let textArea = new OpaqueIMEControl()
    let vscroll = new VScrollBar()

    let mal = FsMiniMAL.Top.createInterpreter()

    do
        for ty in [| typeof<Editor>; typeof<Regex>; typeof<Match>; typeof<Group>; typeof<Capture>; typeof<Color> |] do
            mal.RegisterAbstractType(ty.Name.ToLowerInvariant(), ty)

        mal.RegisterFsharpTypes([|
            ("colorInfo", typeof<ColorInfo>)
            ("message", typeof<Message>)
            ("malToken", typeof<FsMiniMAL.Parser.token>)
            ("json", typeof<MalJson.json>) |])
        mal.Do("var malproc : message -> unit = ignore")
    
    let malproc = mal.GetVar("malproc")
    
    let cols = 80

    let mutable logDoc =
        Doc.create
            { FontName = "MS Gothic"
              FontSize = 20
              PageWidth = max (20 / 2 * cols) textArea.ClientRectangle.Width
              TabWidth = 4 * 20
              Padding = padding
              YOffset = 1 }

    let mutable commandDoc = Doc.create logDoc.LayoutInfo

    let mutable topRowIndex = 0

    let history = List<string>()
    let mutable historyPos = 0 // [0 : history.Count]

    let editors = List<Editor>()
    let mutable selectedEditor : Editor option = None

    let logAreaRectangle() =
        let clientRect = textArea.ClientRectangle
        Rectangle(clientRect.X, clientRect.Y, clientRect.Width, clientRect.Height - logDoc.LayoutInfo.LineHeight)

    let commandAreaRectangle() =
        let clientRect = textArea.ClientRectangle
        Rectangle(clientRect.X, clientRect.Bottom - logDoc.LayoutInfo.LineHeight, clientRect.Width, logDoc.LayoutInfo.LineHeight)
    
    let chunkQueue = List<string>()
    let messageQueue = List<Message>()
    let runningQueue = List<MalThread>()

    let upd() =

        this.Text <- sprintf "Repl (%d)" runningQueue.Count
        // textArea
        textArea.Bounds <- Rectangle(0, menu.Height, this.ClientSize.Width - vScrollWidth, this.ClientSize.Height - menu.Height)

        // scroll bar
        vscroll.Bounds <- Rectangle(this.ClientSize.Width - vScrollWidth, menu.Height, vScrollWidth, this.ClientSize.Height - menu.Height)
        vscroll.SmallChange <- 1
        let screenRowCount = textArea.Height / logDoc.LayoutInfo.LineHeight
        vscroll.LargeChange <- max (screenRowCount - 1) 0
        vscroll.Minimum <- 0
        vscroll.Maximum <- max (logDoc.RowCount - 2) 0
        topRowIndex <- min topRowIndex vscroll.MaximumValueThatCanBeReachedThroughUserInteraction
        vscroll.Value <- topRowIndex

        // caret
        let p0 = commandAreaRectangle().Location
        let p1 = Doc.getCaretPoint commandDoc
        let p2 = Point(p0.X + leftMargin + p1.X, p0.Y + p1.Y)
        let w = 2
        if textArea.Focused then
            Win32.CreateCaret(textArea.Handle, IntPtr.Zero, w, logDoc.LayoutInfo.FontSize) |> ignore
            Win32.SetCaretPos(p2.X - w/2, logDoc.LayoutInfo.Padding + p2.Y) |> ignore
            Win32.ShowCaret(textArea.Handle) |> ignore
            hasCaret <- true
        elif hasCaret then
            Win32.DestroyCaret() |> ignore
            hasCaret <- false
        textArea.Invalidate()
    
    let paint (ev : PaintEventArgs) =
        let clientRect = textArea.ClientRectangle
        if clientRect.Width > 0 && clientRect.Height > 0 then
            use g0 = ev.Graphics
            use buffer = BufferedGraphicsManager.Current.Allocate(g0, clientRect)
            let g = buffer.Graphics
            Doc.draw
                g
                Color.White
                logDoc
                (logAreaRectangle())
                0
                leftMargin
                0
                false
                false
                topRowIndex

            Doc.draw
                g
                (Color.FromArgb(0xFFF0F0F0))
                commandDoc
                (commandAreaRectangle())
                0
                leftMargin
                0
                false
                false
                0
            buffer.Render()
            buffer.Dispose()
    
    let resetVScroll() =
        topRowIndex <- max (logDoc.RowCount - logAreaRectangle().Height / logDoc.LayoutInfo.LineHeight) 0
    
    let logInput (s : string) =
        logDoc <- Doc.replace logDoc s
        resetVScroll()
    
    let commandInput (s : string) =
        commandDoc <- Doc.replace commandDoc s
        
    let key_press (ev : KeyPressEventArgs) =
        if (not ev.Handled) && '\x20' < ev.KeyChar then // enter, space and tab are excluded here
            if state <> Compositioning then
                commandInput (String(ev.KeyChar, 1))
                upd()
            ev.Handled <- true

    let beep() = System.Media.SystemSounds.Beep.Play()

    let startHighlighting (e : Editor) =
        e.HighlightingState <- EHShighlighting
        let interp = Interpreter(mal.Runtime, Typechk.tyenv_clone mal.Tyenv, alloc.Create(), [||])
        interp.MessageHook <- mal.MessageHook
        interp.Store <- false
        interp.StartApply([| malproc.Value; mal.ValueOfObj(MhighlightingRequired e) |])
        runningQueue.Add(MThighlighting (e, interp))
    
    let requestHighlighting (e : Editor) =
        match e.HighlightingState with
        | EHSdone -> startHighlighting e
        | EHShighlighting -> e.HighlightingState <- EHSneedRestart
        | EHSneedRestart -> ()

    /// returns true when need more time slice
    let tick() =
        if (not inNestedMessageLoop) then
            
            // if there is queued chunk, and there is no running chunk thread, start new chunk thread.
            if chunkQueue.Count > 0 && Seq.forall (fun mt -> match mt with MTchunk _ -> false | _ -> true) runningQueue then
                let chunk = chunkQueue.[0]
                chunkQueue.RemoveAt(0)
                mal.Store <- false
                mal.Start(chunk)
                runningQueue.Add(MTchunk chunk)
            
            // if there is queued message, and there is no running message thread, start new message thread.
            if messageQueue.Count > 0 && Seq.forall (fun mt -> match mt with MTmessage _ -> false | _ -> true) runningQueue then
                let message = messageQueue.[0]
                messageQueue.RemoveAt(0)
                let interp = Interpreter(mal.Runtime, Typechk.tyenv_clone mal.Tyenv, alloc.Create(), [||])
                interp.Store <- false
                interp.MessageHook <- mal.MessageHook
                interp.StartApply([| malproc.Value; mal.ValueOfObj(message) |])
                runningQueue.Add(MTmessage (message, interp))
            
            // if the thread on the queue top is a highlighting thread, and it needs to be restarted, and its intterupt flag is set, restart it. Repeat that until there is no more.
            while (runningQueue.Count > 0 &&
                   let mt = runningQueue.[0]
                   match mt with
                   | MThighlighting (e, i) ->
                       e.HighlightingState = EHSneedRestart &&
                       (i.Store :?> bool) &&
                       (runningQueue.RemoveAt(0)
                        startHighlighting e
                        true)
                   | _ -> false) do ()

            // give time slice to running thread.
            if runningQueue.Count > 0 then
                let mt = runningQueue.[0]
                let interp =
                    match mt with
                    | MTchunk _ -> mal
                    | MTmessage (_, interp) -> interp
                    | MThighlighting (_, interp) -> interp

                interp.Run(1)

                if (match interp.State with State.Running | State.Paused -> false | _ -> true) then
                    // the thread is finished
                    runningQueue.RemoveAt(0)
                    match mt with
                    | MThighlighting (e, _) ->
                        match e.HighlightingState with
                        | EHSneedRestart -> startHighlighting(e)
                        | EHShighlighting -> e.HighlightingState <- EHSdone
                        | EHSdone -> dontcare()
                    | _ -> ()
                elif interp.Store :?> bool then
                    runningQueue.Add(runningQueue.[0])
                    runningQueue.RemoveAt(0)
                
                upd()

            runningQueue.Count > 0
               
        else false

    let run (src : string) =
        chunkQueue.Add(src)

    let key_down (ev : KeyEventArgs) =
        let key = ev.KeyData
        match key with
        | Keys.Enter ->
            let src = Doc.getAllString commandDoc
            commandDoc <- Doc.create commandDoc.LayoutInfo
            if history.Contains(src) then
                history.Remove(src) |> ignore
            history.Add(src)
            historyPos <- history.Count
            logInput (src + "\r\n")
            run src
            ev.Handled <- true
        | Keys.Space
        | CombinedKeys.Shift_Space ->
            commandInput " "
            upd()
            ev.Handled <- true
        | Keys.Tab ->
            ()
            ev.Handled <- true
        | Keys.Back
        | Keys.Delete ->
            match Doc.backDelete (key = Keys.Delete) commandDoc with
            | Some newDoc -> commandDoc <- newDoc
            | None -> beep()
            upd()
            ev.Handled <- true
        | Keys.Left
        | Keys.Right ->
            match Doc.leftRight (key = Keys.Right) commandDoc with
            | Some newDoc -> commandDoc <- newDoc
            | None -> beep()
            upd()
            ev.Handled <- true
        | Keys.Up ->
            if history.Count <> 0 && 0 <= historyPos - 1 then
                historyPos <- historyPos - 1
                commandDoc <- Doc.replace (Doc.create commandDoc.LayoutInfo) history.[historyPos]
            else beep()
            upd()
            ev.Handled <- true
        | Keys.Down ->
            if history.Count <> 0 && historyPos + 1 < history.Count then
                historyPos <- historyPos + 1
                commandDoc <- Doc.replace (Doc.create commandDoc.LayoutInfo) history.[historyPos]
            else beep()
            upd()
            ev.Handled <- true
        | _ -> ()
    
    // let getVp (p : Point) = Point(p.X - leftMargin, topRowIndex * logDoc.LayoutInfo.LineHeight + p.Y)
    let point_sub (a : Point) (b : Point) = Point(a.X - b.X, a.Y - b.Y)

    let mouse_down (ev : MouseEventArgs) =
        if ev.Button.HasFlag(MouseButtons.Left) && commandAreaRectangle().Contains(ev.Location) then
            let p = point_sub ev.Location (commandAreaRectangle().Location)
            let pos = Doc.getCharPosFromPoint commandDoc p
            commandDoc <- Doc.setPos commandDoc pos
        upd()
    
    let new_editor() =
        let w = new Editor(this, None)
        editors.Add(w)
        w.Show()

    let open_file_with_new_editor() =
        match FileHelper.tryOpen None with
        | FileHelper.TryOpenResult.TORcancelled -> ()
        | FileHelper.TryOpenResult.TORfailed exn ->
            inNestedMessageLoop <- true
            MessageBox.Show(exn.Message) |> ignore
            inNestedMessageLoop <- false
        | FileHelper.TryOpenResult.TORsuccess handle ->
            let w = new Editor(this, Some handle)
            editors.Add(w)
            w.Show()

    let open_path_with_new_editor path =
        match FileHelper.tryOpenPath path with
        | TOPRfailed exn ->
            inNestedMessageLoop <- true
            MessageBox.Show(exn.Message) |> ignore
            inNestedMessageLoop <- false
        | TOPRsuccess handle ->
            let w = new Editor(this, Some handle)
            editors.Add(w)
            w.Show()
        
    let messageHook msg =
        match msg with
        | FsMiniMAL.Message.EvaluationComplete (tyenv, _, ty) when FsMiniMAL.Unify.same_type tyenv ty FsMiniMAL.Types.ty_unit -> ()
        //| FsMiniMAL.Message.UncaughtException _ when mal.OperationCancelled -> ()
        | _ -> logInput (FsMiniMAL.Printer.print_message FsMiniMAL.Printer.lang.Ja 80 msg)

    do
        this.Icon <- new Icon(System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("Malmacs.PlayIcon.ico"))
        this.Text <- "Repl"
        menu_file.DropDownItems.Add(menu_file_new) |> ignore
        menu_file.DropDownItems.Add(menu_file_open) |> ignore
        menu.Items.Add(menu_file) |> ignore

        textArea.Paint.Add(paint)
        textArea.KeyPress.Add(key_press)
        textArea.KeyDown.Add(key_down)
        textArea.MouseDown.Add(mouse_down)
        textArea.GotFocus.Add(fun ev -> upd())
        textArea.LostFocus.Add(fun ev ->
            upd())
        textArea.ImeStartComposition.Add(fun ev ->
            state <- Compositioning)
        textArea.ImeEndComposition.Add(fun ev ->
            state <- Idle)
        textArea.ImeResultStr.Add(fun ev ->
            commandInput ev.ResultStr
            upd())
        
        menu.Dock <- DockStyle.Top
        menu.Parent <- this
        menu.GotFocus.Add(fun ev -> textArea.Focus() |> ignore)

        vscroll.Parent <- this
        textArea.Parent <- this
        
        this.ClientSize <- Size(800, 450)
        this.SizeChanged.Add(fun ev -> upd())

        menu_file_new.Click.Add(fun ev -> new_editor())
        menu_file_open.Click.Add(fun ev -> open_file_with_new_editor())

        vscroll.Scroll.Add(fun ev ->
            topRowIndex <- ev.NewValue
            upd())

        mal.Fun("printString", (fun mm s -> logInput s))
        mal.Set("printUValue", Vfunc (1, (fun mm argv ->
            let s = 
                if box argv.[0] = null
                then "<null>\r\n"
                else Printf.sprintf "%A" argv.[0]
            logInput s
            Value.unit)), Types.arrow Types.ty_a Types.ty_unit)
        mal.Fun("sei", (fun mm () ->
            let i = this.RunningInterp
            i.Store <- true
            i.State <- State.Paused))
        mal.Fun("cli", (fun mm () ->
            let i = this.RunningInterp
            i.Store <- false
            i.State <- State.Paused))

        mal.MessageHook <- messageHook

        upd()
    
    member this.Mal = mal
    member this.RunningInterp : Interpreter =
        match runningQueue.[0] with
        | MTchunk _ -> mal
        | MTmessage (_, i) -> i
        | MThighlighting (_, i) -> i
    member this.Editors = editors
    member this.SelectedEditor with get() = selectedEditor and set x = selectedEditor <- x
    member this.NewEditor() = new_editor()
    member this.OpenFileWithNewEditor() = open_file_with_new_editor()
    member this.OpenPathWithNewEditor path = open_path_with_new_editor path
    member this.Run(src : string) = run src
    member this.Tick() = tick()
    member this.RequestHighlighting e = requestHighlighting e

    override this.OnFormClosing(ev) =
        if ev.CloseReason <> CloseReason.ApplicationExitCall then
            this.Hide()
            ev.Cancel <- true
        base.OnFormClosing(ev)
    
and Editor(repl : Repl, textFileHandle : FileHelper.TextFileHandle option) as this =
    inherit Form()

    let statusHeight = 20
    let mutable linenoWidth = 40
    let leftMargin = 5
    let rightMargin = 5
    let mutable drawNewlineSymbol = false
    let mutable drawTabSymbol = false
    let mutable drawEofSymbol = false
    let mutable drawPageBoundary = true

    let textArea = new OpaqueIMEControl()
    let statusArea = new OpaqueIMEControl()
    let menu = new MenuStripWithClickThrough()
    let vScroll = new VScrollBar()
    let hScroll = new HScrollBar()
    let contextMenu = new ContextMenuStrip()

    let cols = 200

    let mutable state = EditorState.Idle
    let mutable highlightingState = EditorHighlightingState.EHSdone
    let mutable hasCaret = false
    let undoTree : UndoTree<Doc> = UndoTree.Create(Doc.createFromString (DocLayoutInfo.Default (20 / 2 * cols) 20) (match textFileHandle with Some handle -> handle.LatestText | None -> ""))
    let mutable lastlySavedRevision = 0
    let mutable topRowIndex = 0
    let mutable xOffset = 0
    let mutable wheel_accu = 0
    let mutable caretXPos = 0
    let mutable textFileHandle = textFileHandle
    let mutable latestCommitDate = DateTime.MinValue
    let mutable infoText : string option = None

    let resetCaretXPos() =
        caretXPos <- (Doc.getCaretPoint undoTree.Get).X

    let setPos (charPos : int) =
        undoTree.Amend(Doc.setPos undoTree.Get charPos)
        resetCaretXPos()

    let setSelection (sel : Selection) =
        undoTree.Amend({ undoTree.Get with Selection = sel })
        resetCaretXPos()
    
    let clearColor() =
        undoTree.Amend(Doc.clearColor undoTree.Get)
        
    let undo() =
        if undoTree.CanUndo then
            latestCommitDate <- DateTime.MinValue
            undoTree.Undo()
            clearColor()
            resetCaretXPos()
            repl.RequestHighlighting(this)

    let redo() =
        if undoTree.CanRedo then
            latestCommitDate <- DateTime.MinValue
            undoTree.Redo()
            clearColor()
            resetCaretXPos()
            repl.RequestHighlighting(this)

    let maximumAmendPeriod = TimeSpan.FromSeconds(1.0)

    let commit (newDoc : Doc) (atomic : bool) =
        if atomic || undoTree.Current.HasBeenSavedOnce || undoTree.Current.Next.Count > 0 || DateTime.Now - latestCommitDate > maximumAmendPeriod then
            latestCommitDate <- if atomic then DateTime.MinValue else DateTime.Now
            undoTree.Commit(newDoc)
        else
            undoTree.Amend(newDoc)
        resetCaretXPos()
        repl.RequestHighlighting(this)

    let beep() = System.Media.SystemSounds.Beep.Play()
    
    let upd (scrollToCaret : bool) =
        let clientRectangle = this.ClientRectangle
        
        let doc = undoTree.Get
        let lineHeight = doc.LayoutInfo.LineHeight
        linenoWidth <- 4 * (Doc.measure doc.LayoutInfo "0")
        
        textArea.Bounds <- Rectangle(0, menu.Height, clientRectangle.Width - vScroll.Width, clientRectangle.Height - menu.Height - hScroll.Height - statusHeight)
        statusArea.Bounds <- Rectangle(0, clientRectangle.Bottom - statusHeight, clientRectangle.Width, statusHeight)
        menu.Bounds <- Rectangle(0, 0, clientRectangle.Width, menu.Height)

        vScroll.Bounds <- Rectangle(clientRectangle.Right - vScroll.Width, menu.Height, vScroll.Width, clientRectangle.Height - menu.Height - hScroll.Height - statusHeight)
        vScroll.Minimum <- 0
        vScroll.Maximum <- max 0 (doc.RowCount + textArea.Height / lineHeight - 2)
        vScroll.SmallChange <- 1
        vScroll.LargeChange <- max (textArea.Height / lineHeight) 1
        topRowIndex <- max 0 (min topRowIndex vScroll.MaximumValueThatCanBeReachedThroughUserInteraction)
        if scrollToCaret then
            let caretRowIndex = Doc.getRowIndexFromCharPos doc doc.Selection.CaretPos
            topRowIndex <- max (caretRowIndex - (max 1 textArea.Height / lineHeight) + 1) (min topRowIndex caretRowIndex)
        vScroll.Value <- topRowIndex

        hScroll.Bounds <- Rectangle(0, clientRectangle.Bottom - statusHeight - hScroll.Height, clientRectangle.Width - vScroll.Width, hScroll.Height)
        hScroll.Minimum <- 0
        hScroll.Maximum <- max 0 (doc.RowTree.RootMeasure.MaximumWidth + rightMargin)
        hScroll.SmallChange <- 1
        hScroll.LargeChange <- max 0 (textArea.Width - linenoWidth - leftMargin)
        xOffset <- max 0 (min xOffset hScroll.MaximumValueThatCanBeReachedThroughUserInteraction)
        if scrollToCaret then
            let caretXOffset = (Doc.getCaretPoint doc).X
            xOffset <- max (caretXOffset - (textArea.Width - linenoWidth - leftMargin - rightMargin)) (min xOffset caretXOffset)
        hScroll.Value <- xOffset

        let p0 = Doc.getCaretPoint doc
        let p1 = Point(linenoWidth + leftMargin + p0.X, p0.Y - lineHeight * topRowIndex)
        // caret
        if state <> EditorState.Compositioning && textArea.Focused then
            let w = 2
            Win32.CreateCaret(textArea.Handle, IntPtr.Zero, w, doc.LayoutInfo.FontSize) |> ignore
            Win32.SetCaretPos(p1.X - w/2 - xOffset, doc.LayoutInfo.Padding + p1.Y) |> ignore
            Win32.ShowCaret(textArea.Handle) |> ignore
            hasCaret <- true
        elif hasCaret then
            Win32.DestroyCaret() |> ignore
            hasCaret <- false
        
        match state with
        | EditorState.Compositioning ->
            let ime = Win32.ImmGetContext(textArea.Handle)
            let mutable compForm = Win32.COMPOSITIONFORM(dwStyle = Win32.CFS_POINT, ptCurrentPos = Win32.POINT(x = p1.X, y = p1.Y + undoTree.Get.LayoutInfo.Padding + undoTree.Get.LayoutInfo.YOffset), rcArea = Win32.RECT())
            Win32.ImmSetCompositionWindow(ime, &compForm) |> ignore
            use font = new Font("MS Gothic", float32 undoTree.Get.LayoutInfo.FontSize, GraphicsUnit.Pixel)
            let logfont = Win32.LOGFONT()
            font.ToLogFont(logfont)
            Win32.ImmSetCompositionFont(ime, logfont) |> ignore
            Win32.ImmReleaseContext(textArea.Handle, ime) |> ignore
        | _ -> ()

        // redraw all
        textArea.Invalidate()
        statusArea.Invalidate()
        
        let menu_file_save = (menu.Items.[0] :?> ToolStripMenuItem).DropDownItems.[2] :?> ToolStripMenuItem
        menu_file_save.Enabled <- textFileHandle.IsSome
        
        this.Text <-
            match textFileHandle with
            | Some handle -> sprintf "%s (%s) - Malmacs" (Path.GetFileName(handle.OriginalPath)) (Path.GetDirectoryName(handle.OriginalPath))
            | None -> "Untitled - Malmacs"
    
    let input_upd (atomic : bool) (s : string) =
        let newDoc = Doc.replace undoTree.Get s
        commit newDoc atomic
        upd true

    let cut_upd() =
        let s = Doc.getSelectedString undoTree.Get
        if s.Length > 0 then
            Clipboard.SetText(s)
            input_upd true ""

    let copy() =
        let s = Doc.getSelectedString undoTree.Get
        if s.Length > 0 then
            Clipboard.SetText(s)

    let paste_upd() =
        input_upd true (Clipboard.GetText())

    let selectAll() =
        setSelection { AnchorPos = 0; CaretPos = undoTree.Get.CharCount }
    
    let selectAll_upd() =
        selectAll()
        upd false

    let getVp (p : Point) = Point(p.X - linenoWidth - leftMargin + xOffset, undoTree.Get.LayoutInfo.LineHeight * topRowIndex + p.Y)
    let getLineEnding() = match textFileHandle with None -> CRLF | Some handle -> handle.LineEnding
    let getEncoding() = match textFileHandle with None -> UTF8 | Some handle -> handle.TextEncoding

    let keyDown (ev : KeyEventArgs) =
        let key = ev.KeyData
        match key with
        | Keys.Enter ->
            let s = match getLineEnding() with CRLF -> "\r\n" | LF -> "\n" | CR -> "\r"
            input_upd true s
            ev.Handled <- true
        | Keys.Space
        | CombinedKeys.Shift_Space
        | Keys.Tab ->
            let s =
                if key.HasFlag(Keys.Space) then " "
                else "\t"
            input_upd false s
            ev.Handled <- true
        | Keys.Back
        | Keys.Delete ->
            let atomic = undoTree.Get.Selection.Length <> 0
            match Doc.backDelete (key = Keys.Delete) undoTree.Get with
            | Some newDoc ->
                commit newDoc atomic
            | None -> beep()
            upd true
            ev.Handled <- true
        | Keys.Left
        | Keys.Right ->
            match Doc.leftRight (key = Keys.Right) undoTree.Get with
            | Some newDoc -> undoTree.Amend newDoc
            | None -> beep()
            upd true
            ev.Handled <- true
        | CombinedKeys.Shift_Left
        | CombinedKeys.Shift_Right ->
            match Doc.shiftLeftRight (key = CombinedKeys.Shift_Right) undoTree.Get with
            | Some newDoc -> undoTree.Amend(newDoc)
            | None -> beep()
            upd true
            ev.Handled <- true
        | Keys.Up
        | Keys.Down ->
            let vp = Doc.getCaretPoint undoTree.Get
            let y =
                match key with
                | Keys.Up ->
                    if vp.Y = 0 then None
                    else Some (vp.Y - 1)
                | Keys.Down ->
                    let y = vp.Y + undoTree.Get.LayoutInfo.LineHeight
                    if y >= undoTree.Get.LayoutInfo.LineHeight * undoTree.Get.RowCount then
                        None
                    else
                        Some y
                | _ -> dontcare()
            match y with
            | None -> beep()
            | Some y ->
                let vp = Point(caretXPos, y)
                let pos = Doc.getCharPosFromPoint undoTree.Get vp
                setPos pos
            upd true
            ev.Handled <- true
        | CombinedKeys.Control_X ->
            cut_upd()
            ev.Handled <- true
        | CombinedKeys.Control_C ->
            copy()
            ev.Handled <- true
        | CombinedKeys.Control_V ->
            paste_upd()
            ev.Handled <- true
        | CombinedKeys.Control_A ->
            selectAll_upd()
            ev.Handled <- true
        | CombinedKeys.Control_Z ->
            undo()
            upd true
        | CombinedKeys.Control_Y
        | CombinedKeys.Control_Shift_Z ->
            redo()
            upd true
        | Keys.F5 ->
            GC.Collect()
            upd false
            ev.Handled <- true
        | Keys.F9 ->
            repl.Show()
            ev.Handled <- true
        | _ -> ()

    let keyPress (ev : KeyPressEventArgs) =
        if (not ev.Handled) && '\x20' < ev.KeyChar then // enter, space, tab はここで除外される
            match state with
            | Compositioning -> ()
            | _ -> input_upd false (String(ev.KeyChar, 1))
            ev.Handled <- true

    let textAreaPaint (ev : PaintEventArgs) =
        let clientRectangle = textArea.ClientRectangle
        if clientRectangle.Width > 0 && clientRectangle.Height > 0 then
            let g0 = ev.Graphics
            use buffer = BufferedGraphicsManager.Current.Allocate(g0, clientRectangle)
            let g = buffer.Graphics
            let doc = undoTree.Get
            Doc.draw
                g
                Color.White
                doc
                clientRectangle
                linenoWidth
                leftMargin
                xOffset
                drawNewlineSymbol
                drawTabSymbol
                (Doc.getRowIndexFromCharPos doc (Doc.getCharPosFromPoint doc (Point(0, doc.LayoutInfo.LineHeight * topRowIndex))))
            
            // EOF mark
            if drawEofSymbol then
                let p = Doc.getPointFromCharPos doc doc.CharCount
                let p = Point(p.X + linenoWidth + leftMargin, - doc.LayoutInfo.LineHeight * topRowIndex + p.Y + doc.LayoutInfo.Padding + doc.LayoutInfo.YOffset)
                use eofFont = new Font("MS Gothic", float32 doc.LayoutInfo.FontSize, GraphicsUnit.Pixel)
                g.DrawString("[EOF]", eofFont, Brushes.LightGray, float32 p.X, float32 p.Y, StringFormat.GenericTypographic)
            
            // right boundary of page
            if drawPageBoundary then
                let x = linenoWidth + leftMargin + doc.LayoutInfo.FontSize / 2 * cols
                g.DrawLine(Pens.LightGray, x, clientRectangle.Top, x, clientRectangle.Bottom)

            buffer.Render()

    let statusAreaPaint (ev : PaintEventArgs) =
        let g0 = ev.Graphics
        use buf = BufferedGraphicsManager.Current.Allocate(g0, statusArea.ClientRectangle)
        let g = buf.Graphics
        let doc = undoTree.Get
        use statusFont = new Font("MS Gothic", 20.0f, GraphicsUnit.Pixel)
        
        let bgColor =
            if repl.SelectedEditor.IsSome && repl.SelectedEditor.Value = this
            then Color.FromArgb(0xFF505060)
            else Color.FromArgb(0xFFC0C0C0)
        
        g.Clear(bgColor)

        if infoText = None then
            let posDesc =
                if doc.Selection.Length = 0 then
                    sprintf "%d" doc.Selection.CaretPos
                else
                    sprintf "%d-%d" doc.Selection.AnchorPos doc.Selection.CaretPos

            let symbolDesc =
                if doc.Selection.CaretPos = doc.CharCount then "EOF"
                else
                    let symbol = Doc.getSymbolFromCharPos doc doc.Selection.CaretPos
                    let name =
                        match symbol with
                        | " " -> "SP"
                        | "\r" -> "CR"
                        | "\n" -> "LF"
                        | "\r\n" -> "CRLF"
                        | "\t" -> "TAB"
                        | "\uFEFF" -> "BOM"
                        | _ -> symbol
                    let code =
                        match symbol with
                        | "\r\n" -> "(U+0D U+0A)"
                        | _ ->
                            let codepoint = Char.ConvertToUtf32(symbol, 0)
                            if codepoint <= 0xFF then
                                sprintf "(U+%02x)" codepoint
                            elif codepoint <= 0xFFFF then
                                sprintf "(U+%04x)" codepoint
                            else
                                sprintf "(U+%06x)" codepoint
                    name + " " + code
       
        
            let leftLine =
                sprintf "r%d%s a%d b%d"
                    undoTree.Current.Revision
                    (if lastlySavedRevision <> undoTree.Current.Revision then "*" else "")
                    undoTree.Current.RevisionsAhead
                    undoTree.Current.Next.Count
            g.DrawString(leftLine, statusFont, Brushes.White, 0.f, 1.f, StringFormat.GenericTypographic)

            let centerLine = sprintf "%s %s" posDesc symbolDesc
            use sfCenter = new StringFormat(StringFormat.GenericTypographic)
            sfCenter.Alignment <- StringAlignment.Center
            g.DrawString(centerLine, statusFont, Brushes.White, float32 ((statusArea.Left + statusArea.Right) / 2), 1.f, sfCenter)

            let rightLine = sprintf "%s %s" (getEncoding().ToString()) (getLineEnding().ToString())
            use sfRight = new StringFormat(StringFormat.GenericTypographic)
            sfRight.Alignment <- StringAlignment.Far
            g.DrawString(rightLine, statusFont, Brushes.White, float32 statusArea.Right, 1.f, sfRight)
        else
            g.DrawString(infoText.Value, statusFont, Brushes.White, 0.f, 1.f, StringFormat.GenericTypographic)

        buf.Render()

    let mouseDown (ev : MouseEventArgs) =
        textArea.Focus() |> ignore
        if state = EditorState.Idle then
            if ev.Button.HasFlag(MouseButtons.Left) then
                let vp = getVp ev.Location
                let doc = undoTree.Get
                let pos = Doc.getCharPosFromPoint doc vp
                state <- LeftDown
                setPos pos
                caretXPos <- vp.X
                upd false
            else ()
        elif state = Compositioning then
            let vp = getVp ev.Location
            let doc = undoTree.Get
            let pos = Doc.getCharPosFromPoint doc vp
            setPos pos
            caretXPos <- vp.X
            upd false

    let mouseUp (ev : MouseEventArgs) =
        if ev.Button.HasFlag(MouseButtons.Left) && state = LeftDown then
            state <- EditorState.Idle
            upd false
        elif ev.Button.HasFlag(MouseButtons.Right) then
            contextMenu.Show(Cursor.Position)
    
    let mouseDoubleClick (ev : MouseEventArgs) =
        if ev.Button.HasFlag(MouseButtons.Left) then
            let vp = getVp ev.Location
            let sel = Doc.getWordSelection undoTree.Get vp
            setSelection sel
            upd false
 
    let mouseMove (ev : MouseEventArgs) =
        if state = Idle then
            let vp = getVp ev.Location
            let doc = undoTree.Get
            let idx = Doc.getCharIndexFromPoint doc vp
            let ci =
                match idx with
                | Some idx -> Doc.getColorInfo doc idx
                | None -> Doc.ColorInfo_Default
            if ci.ciText <> infoText then
                infoText <- ci.ciText
                upd false

        elif state = LeftDown && ev.Button.HasFlag(MouseButtons.Left) then
            let vp = getVp ev.Location
            let doc = undoTree.Get
            let pos = Doc.getCharPosFromPoint doc vp
            setSelection { doc.Selection with CaretPos = pos }
            caretXPos <- vp.X
            upd false

    let mouseWheel (ev : MouseEventArgs) =
        if Control.ModifierKeys.HasFlag(Keys.Control) then
            let i = Array.BinarySearch(fontSizeSeries, undoTree.Get.LayoutInfo.FontSize)
            let newSize =
                if ev.Delta > 0 then
                    fontSizeSeries.[min (i + 1) (fontSizeSeries.Length - 1)]
                else
                    fontSizeSeries.[max (i - 1) 0]

            let newLayoutInfo = DocLayoutInfo.Default (newSize / 2 * cols) newSize
            undoTree.Amend(Doc.changeLayout newLayoutInfo undoTree.Get)
            resetCaretXPos()
            repl.RequestHighlighting(this)
            upd false
        else
            wheel_accu <- wheel_accu + ev.Delta
            let scroll =
                if wheel_accu > 0 then
                    wheel_accu / 120
                else
                    -((-wheel_accu) / 120)
            wheel_accu <- wheel_accu - 120 * scroll
            topRowIndex <- topRowIndex - 3 * scroll
            upd false
        
    let ensure() =
        if (match textFileHandle with Some h -> h.LatestText | None -> "") <> Doc.getAllString undoTree.Get then
            let result = applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show("Save?", "Malmacs",  MessageBoxButtons.YesNoCancel, MessageBoxIcon.None)) ()
            match result with
            | DialogResult.Cancel -> false
            | DialogResult.No -> true
            | DialogResult.Yes ->
                let text = Doc.getAllString undoTree.Get
                match textFileHandle with
                | Some handle ->
                    match FileHelper.trySave handle text with
                    | TrySaveResult.TSRsuccess -> true
                    | TrySaveResult.TSRfialed exn ->
                        applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
                        false
                | None ->
                    let text = Doc.getAllString undoTree.Get
                    match FileHelper.trySaveAs None text with
                    | TrySaveAsResult.TSARsuccess handle ->
                        textFileHandle <- Some handle
                        true
                    | TrySaveAsResult.TSARcancelled -> false
                    | TrySaveAsResult.TSARfailed exn ->
                        applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
                        false
            | _ -> dontcare()
        else true

    let new_upd() =
        if ensure() then
            undoTree.Clear(Doc.create undoTree.Get.LayoutInfo)
            caretXPos <- 0
            textFileHandle <- None
        upd false
    
    let openPath (path : string) =
        if ensure() then

            Option.iter (fun (h : TextFileHandle) -> h.FileStream.Close()) textFileHandle
            textFileHandle <- None

            match FileHelper.tryOpenPath path with
            | TOPRsuccess f ->
                undoTree.Clear(Doc.createFromString undoTree.Get.LayoutInfo f.LatestText)
                textFileHandle <- Some f
                topRowIndex <- 0
                lastlySavedRevision <- 0
                resetCaretXPos()
                repl.RequestHighlighting(this)
                upd false
            | TOPRfailed exn ->
                applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
        
    let open_upd() =
        if ensure() then
            match FileHelper.tryOpen textFileHandle with
            | TryOpenResult.TORcancelled -> ()
            | TryOpenResult.TORfailed exn ->
                applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
                match textFileHandle with
                | Some h ->
                    h.FileStream.Close()
                    textFileHandle <- None
                | None -> ()
            | TryOpenResult.TORsuccess f ->
                undoTree.Clear(Doc.createFromString undoTree.Get.LayoutInfo f.LatestText)
                textFileHandle <- Some f
                topRowIndex <- 0
                lastlySavedRevision <- 0
                resetCaretXPos()
                repl.RequestHighlighting(this)
        upd false
    
    let saveas_upd() =
        let text = Doc.getAllString undoTree.Get
        match FileHelper.trySaveAs textFileHandle text with
        | TrySaveAsResult.TSARsuccess handle ->
            undoTree.Current.HasBeenSavedOnce <- true
            textFileHandle <- Some handle
            lastlySavedRevision <- undoTree.Current.Revision
        | TrySaveAsResult.TSARcancelled -> ()
        | TrySaveAsResult.TSARfailed exn ->
            textFileHandle <- None
            applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
        upd false
    
    let save_upd() =
        let text = Doc.getAllString undoTree.Get
        match FileHelper.trySave textFileHandle.Value text with
        | FileHelper.TrySaveResult.TSRsuccess ->
            undoTree.Current.HasBeenSavedOnce <- true
            lastlySavedRevision <- undoTree.Current.Revision
        | FileHelper.TrySaveResult.TSRfialed exn ->
            applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
        upd false

    do  
        this.Icon <- new Icon(System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("Malmacs.TextFileIcon.ico"))
        vScroll.Parent <- this
        vScroll.Scroll.Add(fun ev ->
            topRowIndex <- ev.NewValue
            upd false)
        
        hScroll.Parent <- this
        hScroll.Scroll.Add(fun ev ->
            xOffset <- ev.NewValue
            upd false)

        textArea.KeyDown.Add(keyDown)
        textArea.KeyPress.Add(keyPress)
        textArea.Paint.Add(textAreaPaint)
        textArea.GotFocus.Add(fun ev ->
            repl.SelectedEditor <- Some this
            upd false)
        textArea.LostFocus.Add(fun ev ->
            upd false)
        this.Resize.Add(fun ev ->
            resetCaretXPos()
            repl.RequestHighlighting(this)
            upd false)
        textArea.MouseDown.Add(mouseDown)
        textArea.MouseUp.Add(mouseUp)
        textArea.MouseDoubleClick.Add(mouseDoubleClick)
        textArea.MouseMove.Add(mouseMove)
        textArea.MouseWheel.Add(mouseWheel)
        textArea.ImeStartComposition.Add(fun ev ->
            state <- EditorState.Compositioning
            upd false)
        textArea.ImeEndComposition.Add(fun ev ->
            state <- EditorState.Idle
            upd false)
        textArea.ImeResultStr.Add(fun ev ->
            input_upd false ev.ResultStr)
        
        statusArea.Paint.Add(statusAreaPaint)

        menu.Items.Add(
            new ToolStripMenuItem("File", null,
                new ToolStripMenuItem("New", null, fun o ev -> new_upd()),
                new ToolStripMenuItem("Open", null, fun o ev -> open_upd()),
                new ToolStripMenuItem("Save", null, (fun o ev -> save_upd())),
                new ToolStripMenuItem("Save As", null, fun o ev -> saveas_upd()))) |> ignore
        menu.Items.Add(
            new ToolStripMenuItem("Window", null,
                new ToolStripMenuItem("Open New Editor", null, fun o ev -> repl.NewEditor()),
                new ToolStripMenuItem("Show Repl", null, fun o ev -> repl.Show()))) |> ignore

        contextMenu.Items.Add("Cut", null, fun o e -> cut_upd()) |> ignore
        contextMenu.Items.Add("Copy", null, fun o e -> copy()) |> ignore
        contextMenu.Items.Add("Paste", null, fun o e -> paste_upd()) |> ignore
        contextMenu.Items.Add(new ToolStripSeparator()) |> ignore
        contextMenu.Items.Add("Run All", null, fun o e -> repl.Run(this.EditorText)) |> ignore
        contextMenu.Items.Add("Run Selection", null, fun o e -> repl.Run(Doc.getSelectedString undoTree.Get)) |> ignore

        this.ClientSize <- Size(800, 450)
        textArea.Parent <- this

        statusArea.Parent <- this

        menu.Parent <- this
        menu.GotFocus.Add(fun ev -> textArea.Focus() |> ignore)

        this.AllowDrop <- true
        this.DragEnter.Add(fun ev ->
            if ev.Data.GetDataPresent(DataFormats.FileDrop) then
                ev.Effect <- DragDropEffects.Copy
            else
                ev.Effect <- DragDropEffects.None)
        this.DragDrop.Add(fun ev ->
            let paths = ev.Data.GetData(DataFormats.FileDrop, false) :?> string array
            openPath paths.[0])

        upd false
        resetCaretXPos()
        repl.RequestHighlighting(this)
        
    member this.HighlightingState  with get() = highlightingState and set x = highlightingState <- x
    member this.TextArea : OpaqueIMEControl = textArea
    member this.Doc
        with get() = undoTree.Get
        and set x =
            undoTree.Amend(x)
    
    member this.EditorText
        with get() = Doc.getAllString undoTree.Get
        and set s =
            selectAll()
            input_upd true s
    
    member this.TextFileHandle
        with get() = textFileHandle
        and set x = textFileHandle <- x

    override this.OnFormClosing(ev) =
        let ensured = ensure()
        if not ensured then
            ev.Cancel <- true
        base.OnFormClosing(ev)
    
    override this.OnFormClosed(ev) =
        match this.TextFileHandle with
        | Some handle ->
            if not (isNull handle.FileStream) then
                handle.FileStream.Dispose()
        | None -> ()
        repl.Editors.Remove(this) |> ignore
        base.OnFormClosed(ev)