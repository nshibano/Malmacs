namespace Malmacs

#nowarn "9"

open System
open System.Drawing
open System.Collections.Generic
open System.Text
open System.Diagnostics

type ColorInfo =
    { ciForeColor : Color option
      ciBackColor : Color option
      ciUnderlineColor : Color option
      ciText : string option }

// Terminology
//
// Symbol : A 'character' item edited in text editor. It consists of 1 or 2 System.Char. 2-character symbols are either of "\r\n" or surrogate pair.
//          Press Left/right key will skip one symbol, and press backspace/delete key will remove one symbol.
//
// Row : Sequence of symbols displayed horizontally on the editor, or one may call it "visual line". A line contain 1 or more rows.
//
// Line : Sequence of symbols in the text which ends with newline symbols ("\r\n", "\r" or "\n"), or ends when there is no more symbols in the file.
//        The last newline symbol of the line is part of the content of the line.
//        If a text ends with newline symbol, the final line of the text is empty line (a line that contains zero symbols). 
//        Empty text contains one empty line.

type Row =
    { String : string
      Colors : ColorInfo array // row.String.Length = row.Colors.Length
      CharOffsets : int array
      XOffsets : int array
      IsEndOfLine : bool }
    
    member row.SymbolCount = row.CharOffsets.Length - 1
    member row.GetSymbol(symbolIndex : int) =
        let ofs = row.CharOffsets.[symbolIndex]
        let len = row.CharOffsets.[symbolIndex + 1] - ofs
        row.String.Substring(ofs, len)
    member row.Width = row.XOffsets.[row.XOffsets.Length - 1]

type RowTreeInfo =
    { CharCount : int
      EndOfLineCount : int
      MaximumWidth : int }

type DocLayoutInfo =
    { FontName : string
      FontSize: int
      PageWidth : int
      TabWidth : int
      Padding : int
      YOffset : int }
    
    member x.LineHeight = x.FontSize + 2 * x.Padding

    static member Default pageWidth fontSize =
        let padding = fontSize / 8
        {
            FontName = "MS Gothic"
            FontSize = fontSize
            PageWidth = pageWidth
            TabWidth = 4 * fontSize
            Padding = padding
            YOffset = 1
        }          

type RowTree = MeasuredTreeList<Row, RowTreeInfo>
type RowTreeNode = MeasuredTreeListNode<Row, RowTreeInfo>

type Range =
    { Begin : int
      End : int }
    
    member r.Length = r.End - r.Begin
    member r.Contains x = r.Begin <= x && x < r.End
    member r.Clip x =
        if r.Length = 0 then dontcare()
        if x < r.Begin then r.Begin
        elif r.End <= x then r.End - 1
        else x

type Selection =
    { CaretPos : int
      AnchorPos : int }

    member x.Length =
        if x.AnchorPos <= x.CaretPos then
            x.CaretPos - x.AnchorPos
        else
            x.AnchorPos - x.CaretPos

    member x.ToRange() =
        if x.CaretPos < x.AnchorPos then
            { Begin = x.CaretPos; End = x.AnchorPos }
        else
            { Begin = x.AnchorPos; End = x.CaretPos }

type Doc =
    { LayoutInfo : DocLayoutInfo
      RowTree : RowTree
      Selection : Selection
      ContentId : int }
    
    member this.CharCount = this.RowTree.RootMeasure.CharCount
    member this.HasTrailingEmptyLine = this.RowTree.Count = 0 || this.RowTree.[this.RowTree.Count - 1].IsEndOfLine
    member this.RowCount = this.RowTree.Count + (if this.HasTrailingEmptyLine then 1 else 0)
    member this.LineCount = this.RowTree.RootMeasure.EndOfLineCount + 1

type Buf() =
    let mutable start = 0
    let mutable count = 0
    let mutable storage : char array = [||]

    member this.Count = count
    member this.Item with get i = storage.[start + i]

    member this.RemoveHead(n) =
        start <- start + n
        count <- count - n

    member this.AddRange(s : string) =
        if start + count + s.Length <= storage.Length then
            for i = 0 to s.Length - 1 do
                storage.[start + count + i] <- s.[i]
            count <- count + s.Length
        else
            let newCount = count + s.Length
            let newCapacity = FsMiniMAL.Misc.find_next_capacity_exn Int32.MaxValue newCount
            let newStorage = Array.zeroCreate<char> newCapacity
            Array.blit storage start newStorage 0 count
            for i = 0 to s.Length - 1 do
                newStorage.[count + i] <- s.[i]
            start <- 0
            count <- newCount
            storage <- newStorage
    
    member this.ReplaceRange(replBegin : int, replCount : int, replacement : string) =
        let newCount = count - replCount + replacement.Length
        let newStorage = Array.zeroCreate<char> newCount
        for i = 0 to replBegin - 1 do
            newStorage.[i] <- storage.[start + i]
        for i = 0 to replacement.Length - 1 do
            newStorage.[replBegin + i] <- replacement.[i]
        for i = 0 to count - replBegin - replCount - 1 do
            newStorage.[replBegin + replacement.Length + i] <- storage.[start + replBegin + replCount + i]
        start <- 0
        count <- newCount
        storage <- newStorage

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Doc =
    
    let ColorInfo_Default = { ciForeColor = None; ciBackColor = None; ciUnderlineColor = None; ciText = None }
    let RowTreeInfo_Zero = { CharCount = 0; EndOfLineCount = 0; MaximumWidth = 0 }
    let Selection_Zero = { CaretPos = 0; AnchorPos = 0 }

    /// Returns index of the table item which is equal to the key.
    /// If not found, returns index of the highest item of the items which is lower than the key.
    /// If there is no item which is lower than the key, returns -1.
    /// Therefore, -1 <= returnValue <= table.Length - 1.
    /// The table must be sorted in ascending order.
    let binchopLeft (table : int array) (key : int) =
        
        let rec loop i j =
            if j - i = 1 then
                if table.[i] <= key then
                    i
                else
                    i - 1
            else
                let k = i + (j - i) / 2
                match compare key table.[k] with
                | 0 -> k
                | -1 -> loop i k
                | 1 -> loop k j
                | _ -> dontcare()

        if table.Length = 0 then -1
        else loop 0 table.Length

    /// Returns index of the table item which is equal to the key.
    /// If not found, returns index of the lowest item of the items which is higher than the key.
    /// If there is no item which is higher than the key, returns table.Length.
    /// Therefore, 0 <= returnValue <= table.Length.
    /// The table must be sorted in ascending order.
    let binchopRight (table : int array) (key : int) =

        if table.Length = 0 then raise (ArgumentException())
        
        let rec loop i j =
            if j - i = 1 then
                if key <= table.[i] then
                    i
                else
                    i + 1
            else
                let k = i + (j - i) / 2
                match compare key table.[k] with
                | 0 -> k
                | -1 -> loop i k
                | 1 -> loop k j
                | _ -> dontcare()

        if table.Length = 0 then table.Length
        else loop 0 table.Length

    let dummy_bmp = new System.Drawing.Bitmap(1, 1)
    let dummy_g = Graphics.FromImage(dummy_bmp)
    let measure_cache = Dictionary<string, int>()
    let mutable latest_info = DocLayoutInfo.Default 800 20
    let createFont (info : DocLayoutInfo) = new Font(info.FontName, float32 info.FontSize, GraphicsUnit.Pixel)
    let mutable latest_font = createFont latest_info
    let measure (info : DocLayoutInfo) (symbol : string) =
        if not (LanguagePrimitives.PhysicalEquality info latest_info) then
            latest_info <- info
            latest_font <- createFont info
            measure_cache.Clear()
        match measure_cache.TryGetValue(symbol) with
        | true, w -> w
        | false, _ ->
            let symbol =
                match symbol with
                | "\r\n"
                | "\n"
                | " " -> "a"
                | "　" -> "あ"
                | _ -> symbol
            let f sym = int (round (dummy_g.MeasureString(sym, latest_font, PointF(0.0f, 0.0f), StringFormat.GenericTypographic).Width))
            let w = max (f symbol) (f "a")
            measure_cache.[symbol] <- w
            w
    
    let Row_empty =
        { String = ""
          Colors = [||]
          CharOffsets = [| 0 |]
          XOffsets = [| 0 |]
          IsEndOfLine = false }
    
    let rowTreeFunc (left : RowTreeInfo) (row : Row) (right : RowTreeInfo) =
        { CharCount = left.CharCount + row.String.Length + right.CharCount
          EndOfLineCount = left.EndOfLineCount + (if row.IsEndOfLine then 1 else 0) + right.EndOfLineCount
          MaximumWidth = max (max left.MaximumWidth row.Width) right.MaximumWidth }
    
    let mutable contentIdTop = 0
    let contentIdNew() =
        let newId = contentIdTop
        contentIdTop <- contentIdTop + 1
        newId
   
    let create info =
        { LayoutInfo = info
          RowTree = MeasuredTreeList<Row, RowTreeInfo>(rowTreeFunc, RowTreeInfo_Zero)
          Selection = { CaretPos = 0; AnchorPos = 0 }
          ContentId = contentIdNew() }
    
    let getRow (doc : Doc) rowIndex =
        if 0 <= rowIndex && rowIndex < doc.RowTree.Count then
            doc.RowTree.[rowIndex]
        elif rowIndex = doc.RowTree.Count && doc.HasTrailingEmptyLine then
            Row_empty
        else raise (IndexOutOfRangeException())
    
    let getCharRangeFromRowIndex (doc : Doc) rowIndex =
        if 0 <= rowIndex && rowIndex < doc.RowTree.Count then
            let bgn = doc.RowTree.MeasureRange(0, rowIndex).CharCount
            let row = doc.RowTree.[rowIndex]
            { Begin = bgn; End = bgn + row.String.Length }
        elif rowIndex = doc.RowTree.Count && doc.HasTrailingEmptyLine then
            let bgn = doc.RowTree.RootMeasure.CharCount
            { Begin = bgn; End = bgn }
        else raise (IndexOutOfRangeException())

    let getRowIndexFromCharPos (doc : Doc) charPos =
        if 0 <= charPos && charPos < doc.CharCount then
            let rec loop upleftRowCount node pos =
                match node with
                | Node (Left = left; Value = value; Right = right) ->
                    let leftCharCount = doc.RowTree.MeasureOf(left).CharCount
                    if pos < leftCharCount then
                        loop upleftRowCount left pos
                    elif pos < leftCharCount + value.String.Length then
                        upleftRowCount + left.Count
                    else
                        loop (upleftRowCount + left.Count + 1) right (pos - leftCharCount - value.String.Length)
                | Nil -> dontcare()
            loop 0 doc.RowTree.Root charPos
        elif charPos = doc.CharCount then
            if doc.HasTrailingEmptyLine then
                doc.RowTree.Count
            else
                doc.RowTree.Count - 1
        else raise (IndexOutOfRangeException())
    
    let validateCharPos (doc : Doc) (rightNotLeft : bool) charPos =
        if charPos < 0 then 0
        elif doc.CharCount < charPos then doc.CharCount
        else
            let rowIndex = getRowIndexFromCharPos doc charPos
            let row = getRow doc rowIndex
            let rowRange = getCharRangeFromRowIndex doc rowIndex
            let binchop = if rightNotLeft then binchopRight else binchopLeft
            let i = binchop row.CharOffsets (charPos - rowRange.Begin)
            rowRange.Begin + row.CharOffsets.[i]

    let setPos (doc : Doc) pos =
        { doc with Selection = { AnchorPos = pos; CaretPos = pos }}

    let getChar (doc : Doc) charIndex =
        if 0 <= charIndex && charIndex < doc.CharCount then
            let rowIndex = getRowIndexFromCharPos doc charIndex
            let row = getRow doc rowIndex
            let leftCharCount = doc.RowTree.MeasureRange(0, rowIndex).CharCount
            row.String.[charIndex - leftCharCount]
        else raise (IndexOutOfRangeException())

    let getColorInfo (doc : Doc) charIndex =
        if 0 <= charIndex && charIndex < doc.CharCount then
            let rowIndex = getRowIndexFromCharPos doc charIndex
            let row = getRow doc rowIndex
            let charRange = getCharRangeFromRowIndex doc rowIndex
            row.Colors.[charIndex - charRange.Begin]
        else raise (IndexOutOfRangeException())

    let symbolIsNewline (symbol : string) =
        let c0 = symbol.[0]
        c0 = '\r' || c0 = '\n'

    let symbolIsSpaceOrTab (symbol : string) =
        symbol = " " || symbol = "\t"

    //let symbolIsHalfwidth (symbol : string) =
    //    symbol.Length = 1 &&
    //    let c = symbol.[0]
    //    ('\u0020' <= c && c <= '\u007E') || // ascii
    //    ('\uFF66' <= c && c <= '\uFF9F') // 半角カナ
    
    let replace (doc : Doc) (replacement : string) =

        let charRange = doc.Selection.ToRange()

        let deconsrBeginCharPos =
            if (0 < charRange.Begin &&
                let c = getChar doc (charRange.Begin - 1)
                c = '\r' || Char.IsHighSurrogate(c))
            then charRange.Begin - 1
            else charRange.Begin
        
        let deconstrEndCharPos =
            if (charRange.End < doc.CharCount &&
                let c = getChar doc charRange.End
                c = '\n' || Char.IsLowSurrogate(c))
            then charRange.End + 1
            else charRange.End
        
        let deconstrRowIndexBegin = getRowIndexFromCharPos doc deconsrBeginCharPos
        let deconstrRowIndexEnd = getRowIndexFromCharPos doc deconstrEndCharPos + 1
        
        let mutable head = doc.RowTree.TakeFirst(deconstrRowIndexBegin)
        let mutable tail = doc.RowTree.TakeLast(max 0 (doc.RowTree.Count - deconstrRowIndexEnd))
        
        let buf = Buf()
        for rowIndex = deconstrRowIndexBegin to deconstrRowIndexEnd - 1 do
            buf.AddRange((getRow doc rowIndex).String)
        
        buf.ReplaceRange((charRange.Begin - head.RootMeasure.CharCount), (charRange.Length), replacement)
        let charCount = head.RootMeasure.CharCount + buf.Count + tail.RootMeasure.CharCount

        let rec rowTree_charAt_loop (tree : RowTree) (node : RowTreeNode) i =
            match node with
            | Node (Left = left; Value = value; Right = right) ->
                let leftCharCount = tree.MeasureOf(left).CharCount
                if i < leftCharCount then
                    rowTree_charAt_loop tree left i
                elif i < leftCharCount + value.String.Length then
                    value.String.[i - leftCharCount]
                else
                    rowTree_charAt_loop tree right (i - leftCharCount - value.String.Length)
            | Nil -> dontcare()
        
        let charAt i =
            let headCount = head.RootMeasure.CharCount
            let bufferCount = buf.Count
            if i < headCount then
                rowTree_charAt_loop head head.Root i
            elif i < headCount + bufferCount then
                buf.[i - headCount]
            else
                rowTree_charAt_loop tail tail.Root (i - headCount - bufferCount)

        while buf.Count > 0 do

            let scanStart = head.RootMeasure.CharCount

            let mutable charPos = 0            
            let mutable symbolPos = 0
            let mutable xOffset = 0
            let charPoss = List<int>()
            let xOffsets = List<int>()

            let add (symbol : string) (nextXOffset : int) =
                charPoss.Add(charPos)
                xOffsets.Add(xOffset)
                charPos <- charPos + symbol.Length
                symbolPos <- symbolPos + 1
                xOffset <- nextXOffset
            
            let mutable cont = true
            while cont && scanStart + charPos < charCount do

                let symbol =
                    let c0 = charAt (scanStart + charPos)
                    if (c0 = '\r' || Char.IsHighSurrogate(c0)) && scanStart + charPos + 1 < charCount then
                        let c1 = charAt (scanStart + charPos + 1)
                        if (c0 = '\r' && c1 = '\n') || Char.IsSurrogatePair(c0, c1) then
                            String([| c0; c1 |])
                        else String(c0, 1)
                    else String(c0, 1)
                
                match symbol with
                | "\t" ->
                    let xOffsetAfterAdd = ((xOffset / doc.LayoutInfo.TabWidth + 1) * doc.LayoutInfo.TabWidth)
                    if xOffsetAfterAdd <= doc.LayoutInfo.PageWidth then
                        add symbol xOffsetAfterAdd
                    else cont <- false                     
                | _ ->                  
                    let xOffsetAfterAdd = xOffset + measure doc.LayoutInfo symbol
                    if xOffsetAfterAdd <= doc.LayoutInfo.PageWidth then
                        add symbol xOffsetAfterAdd
                        if symbol.[0] = '\r' || symbol.[0] = '\n' then
                            cont <- false
                    else cont <- false
                
            charPoss.Add(charPos)
            xOffsets.Add(xOffset)

            let rowString = String(Array.init (charPoss.[charPoss.Count - 1] - charPoss.[0]) (fun i -> charAt (scanStart + i)))
            let symbolCount = charPoss.Count - 1
            let isEol =
                0 < scanStart + charPos - 1 &&
                let c = charAt (scanStart + charPos - 1)
                c = '\r' || c = '\n'

            let row =
                { String = rowString
                  Colors = Array.create rowString.Length ColorInfo_Default
                  CharOffsets = charPoss.ToArray()
                  XOffsets = xOffsets.ToArray()
                  IsEndOfLine = isEol }
            head <- head.Add(row)

            while buf.Count < rowString.Length do
                buf.AddRange(tail.[0].String)
                tail <- tail.RemoveAt(0)
                
            buf.RemoveHead(rowString.Length)

        let doc = { doc with RowTree = head.AddRange(tail); ContentId = contentIdNew() }
        let newPos = validateCharPos doc true (charRange.Begin + replacement.Length)
        { doc with Selection = { AnchorPos = newPos; CaretPos = newPos }}
    
    let createFromString info (s : string) = replace (create info) s

    let getSymbolFromCharPos (doc : Doc) (charPos : int) =
        let rowIndex = getRowIndexFromCharPos doc charPos
        let row = getRow doc rowIndex
        let rowRange = getCharRangeFromRowIndex doc rowIndex
        let symbolIndexInRow = binchopLeft row.CharOffsets (charPos - rowRange.Begin)
        let ofs = row.CharOffsets.[symbolIndexInRow]
        let len = row.CharOffsets.[symbolIndexInRow + 1] - row.CharOffsets.[symbolIndexInRow]
        row.String.Substring(ofs, len)

    let getSelectedString (doc : Doc) =
        let range = doc.Selection.ToRange()
        let sb = StringBuilder(range.Length)
        for i = range.Begin to range.End - 1 do
            sb.Add(getChar doc i)
        sb.ToString()

    let getAllString (doc : Doc) = String.Concat(Array.map (fun (row : Row) -> row.String) (doc.RowTree.ToArray()))

    let getPointFromCharPos (doc : Doc) (charPos : int) =
        let rowIndex = getRowIndexFromCharPos doc charPos
        let y = doc.LayoutInfo.LineHeight * rowIndex
        let row = getRow doc rowIndex
        let rowRange = getCharRangeFromRowIndex doc rowIndex
        let symbolIndex = binchopLeft row.CharOffsets (charPos - rowRange.Begin)
        let x = row.XOffsets.[symbolIndex]
        Point(x, y)

    let getCaretPoint (doc : Doc) = getPointFromCharPos doc doc.Selection.CaretPos

    let getCharIndexFromPoint (doc : Doc) (p : Point) =
        let rowIndex = { Begin = 0; End = doc.RowCount }.Clip(p.Y / doc.LayoutInfo.LineHeight)
        let row = getRow doc rowIndex
        let rowRange = getCharRangeFromRowIndex doc rowIndex
        let i = binchopLeft row.XOffsets p.X
        if i = -1 || i = row.XOffsets.Length - 1 then
            None
        else
            let charIndex =rowRange.Begin + row.CharOffsets.[i]
            if not (0 <= charIndex && charIndex < doc.CharCount) then dontcare()
            Some charIndex
    
    let getCharPosFromPoint (doc : Doc) (p : Point) =
        let rowIndex = { Begin = 0; End = doc.RowCount }.Clip(p.Y / doc.LayoutInfo.LineHeight)
        let row = getRow doc rowIndex
        let rowRange = getCharRangeFromRowIndex doc rowIndex
        let symbolPosInRow =
            let i = binchopLeft row.XOffsets p.X
            if i = -1 then 0 else i

        // if the point is on right half of bounding rectangle of the symbol, it is pointing next symbol.
        let symbolPosInRow =
            if symbolPosInRow < row.SymbolCount then
                let w = row.XOffsets.[symbolPosInRow + 1] - row.XOffsets.[symbolPosInRow]
                if row.XOffsets.[symbolPosInRow] + w / 2 <= p.X then
                    symbolPosInRow + 1
                else symbolPosInRow
            else symbolPosInRow
        
        // if the row contains newline symbol and the point is pointing the next of it, move to previous of it.
        let symbolPosInRow =
            if row.IsEndOfLine && symbolPosInRow = row.SymbolCount then
                symbolPosInRow - 1
            else symbolPosInRow
        
        rowRange.Begin + row.CharOffsets.[symbolPosInRow]
    
    let getWordSelection (doc : Doc) (p : Point) =
        if doc.CharCount > 0 then
            let rowIndex = { Begin = 0; End = doc.RowCount }.Clip(p.Y / doc.LayoutInfo.LineHeight)
            let row = getRow doc rowIndex
            let rowRange = getCharRangeFromRowIndex doc rowIndex
            if rowRange.Length > 0 then
                let symbolPosInRow =
                    let i = binchopLeft row.XOffsets p.X
                    if i = -1 then 0 else i
                let symbolIndexInRow = if symbolPosInRow = row.SymbolCount then row.SymbolCount - 1 else symbolPosInRow
                let sym = row.GetSymbol(symbolIndexInRow)
                if not (symbolIsNewline sym) then
                    let targetIsWhitespace = symbolIsSpaceOrTab sym
                    let mutable i = symbolIndexInRow
                    while i - 1 >= 0 && (symbolIsSpaceOrTab (row.GetSymbol(i - 1)) = targetIsWhitespace) do
                        i <- i - 1
                    let mutable j = symbolIndexInRow
                    while j + 1 < row.SymbolCount && (symbolIsSpaceOrTab (row.GetSymbol(j + 1)) = targetIsWhitespace) do
                        j <- j + 1
                    { AnchorPos = rowRange.Begin + row.CharOffsets.[i]; CaretPos = rowRange.Begin + row.CharOffsets.[j + 1] }
                else
                    let pos = rowRange.Begin + row.CharOffsets.[symbolIndexInRow]
                    { AnchorPos = pos; CaretPos = pos; }
            else { CaretPos = rowRange.Begin; AnchorPos = rowRange.Begin }
        else { CaretPos = 0; AnchorPos = 0 }
    
    let changeLayout (layoutInfo : DocLayoutInfo) (doc : Doc) =
        if layoutInfo <> doc.LayoutInfo then
            createFromString layoutInfo (getAllString doc)
        else doc

    let clearColor (doc : Doc) : Doc =
        let rows = Array.map (fun row -> { row with Colors = Array.create row.String.Length ColorInfo_Default }) (doc.RowTree.ToArray())
        { doc with RowTree = MeasuredTreeList<Row, RowTreeInfo>(rowTreeFunc, RowTreeInfo_Zero, rows) }

    let prevNext (nextNotPrev : bool) (doc : Doc) charPos =
        let newPos = validateCharPos doc nextNotPrev (if nextNotPrev then charPos + 1 else charPos - 1)
        if newPos <> charPos
        then Some newPos
        else None

    let leftRight (rightNotleft : bool) (doc : Doc) =
        let sel = doc.Selection
        if sel.Length = 0 then
            match prevNext rightNotleft doc sel.CaretPos with
            | Some left -> Some (setPos doc left)
            | None -> None
        else
            Some (setPos doc ((if rightNotleft then max else min) sel.AnchorPos sel.CaretPos))
    
    let shiftLeftRight (rightNotLeft : bool) (doc : Doc) =
        let sel = doc.Selection
        match prevNext rightNotLeft doc sel.CaretPos with
        | Some newPos -> Some { doc with Selection = { sel with CaretPos = newPos }}
        | None -> None

    let backDelete (deleteNotBack : bool) (doc : Doc) =
        let sel = doc.Selection
        let sel =
            if sel.Length = 0 then
                match prevNext deleteNotBack doc sel.CaretPos with
                | Some newPos -> { sel with CaretPos = newPos }
                | None -> sel
            else sel
        if sel.Length > 0 then
            Some (replace { doc with Selection = sel } "")
        else None
    
    let draw
            (g : Graphics)
            (bgColor : Color)
            (doc : Doc)
            (area : Rectangle)
            (linenoWidth : int)
            (leftMargin : int)
            (xOffset : int)
            (drawNewlineSymbol : bool)
            (drawTabSymbol : bool)
            (topRowIndex : int) =
        let li = doc.LayoutInfo
        use bgBrush = new SolidBrush(bgColor)
        g.FillRectangle(bgBrush, area)
        let bottomRowIndex = min (topRowIndex + area.Height / li.LineHeight) (doc.RowCount - 1)
        let mutable y = area.Y
        let x0 = area.X + linenoWidth + leftMargin - xOffset
        use font = createFont li
        let mutable lineno = doc.RowTree.MeasureRange(0, topRowIndex).EndOfLineCount + 1
        let mutable printLineno = topRowIndex = 0 || (getRow doc (topRowIndex - 1)).IsEndOfLine
        let hdc = g.GetHdc()
        Win32.SetBkMode(hdc, Win32.TRANSPARENT) |> ignore
        let hfont = font.ToHfont()
        let darkgreen_pen = Win32.CreatePen(0, 1, Win32.colorref_of_color(Color.DarkGreen))
        let old_hfont = Win32.SelectObject(hdc, hfont)
        let old_pen = Win32.SelectObject(hdc, darkgreen_pen)
        let selected_brush = Win32.CreateSolidBrush(Win32.colorref_of_color(Color.SkyBlue))

        let drawLineSymbol (p : Point) (lineSymbol : struct (PointF * PointF) array) =
            let k = float32 li.FontSize
            let round (x : float32) = int (Math.Floor(float x + 0.5))
            let translate (pf : PointF) = Point(p.X + round (k * pf.X), li.Padding + p.Y + round (k * pf.Y))
            for struct (a, b) in lineSymbol do
                let ta = translate a
                let tb = translate b
                Win32.MoveToEx(hdc, ta.X, ta.Y, IntPtr.Zero) |> ignore
                Win32.LineTo(hdc, tb.X, tb.Y) |> ignore
        
        for k = topRowIndex to bottomRowIndex do
            let row = getRow doc k
            let rowRange = getCharRangeFromRowIndex doc k
            Win32.SetTextColor(hdc, Win32.colorref_of_color Color.Black) |> ignore
            for i = 0 to row.SymbolCount - 1 do
                let colorInfo = row.Colors.[row.CharOffsets.[i]]
                let x = x0 + row.XOffsets.[i]
                let w = row.XOffsets.[i + 1] - row.XOffsets.[i]
                let range = doc.Selection.ToRange()
                let rectXOfs = -1
                if range.Contains (rowRange.Begin + row.CharOffsets.[i]) then
                    let mutable rect = Win32.RECT(left = x + rectXOfs, top = y, right = x + w, bottom = y + li.LineHeight)
                    Win32.FillRect(hdc, &rect, selected_brush) |> ignore
                elif Option.isSome colorInfo.ciBackColor then
                    let mutable rect = Win32.RECT(left = x + rectXOfs, top = y, right = x + w, bottom = y + li.LineHeight)
                    let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(colorInfo.ciBackColor.Value))
                    Win32.FillRect(hdc, &rect, brush) |> ignore
                    Win32.DeleteObject(brush) |> ignore
                if Option.isSome colorInfo.ciUnderlineColor then
                    let mutable rect = Win32.RECT(left = x + rectXOfs, top = y + li.LineHeight - (max 2 (li.FontSize / 10)) - li.Padding, right = x + w, bottom = y + li.LineHeight - li.Padding)
                    let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(colorInfo.ciUnderlineColor.Value))
                    Win32.FillRect(hdc, &rect, brush) |> ignore
                    Win32.DeleteObject(brush) |> ignore
                match row.String.[row.CharOffsets.[i]] with
                | '\r' | '\n' ->
                    if drawNewlineSymbol then
                        drawLineSymbol (Point(x0 + row.XOffsets.[i], y)) symbol_rn
                | '\t' ->
                    if drawTabSymbol then
                        drawLineSymbol (Point(x0 + row.XOffsets.[i + 1], y)) symbol_t
                | ' '
                | '　' -> ()
                | _ ->
                    let color =
                        if Option.isNone colorInfo.ciForeColor then
                            Color.Black
                        else
                            colorInfo.ciForeColor.Value
                    Win32.SetTextColor(hdc, Win32.colorref_of_color color) |> ignore
                    let ofs = row.CharOffsets.[i]
                    let len = row.CharOffsets.[i+1] - ofs
                    if not (0 <= ofs && ofs < row.String.Length && ofs + len <= row.String.Length) then dontcare()
                    Win32.TextOut(hdc, x0 + row.XOffsets.[i], li.Padding + y + li.YOffset, row.String.Substring(ofs,len), len) |> ignore

            if linenoWidth > 0 then
                let mutable rect = Win32.RECT(left = area.X, top = y, right = area.X + linenoWidth, bottom = y + li.LineHeight)
                let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(Color.DarkGray))
                Win32.FillRect(hdc, &rect, brush) |> ignore
                Win32.DeleteObject(brush) |> ignore
                if printLineno then
                    let s = sprintf "%4d" lineno
                    let s = if s.Length > 4 then s.Substring(s.Length - 4, 4) else s
                    Win32.SetTextColor(hdc, Win32.colorref_of_color Color.DarkGreen) |> ignore
                    Win32.TextOut(hdc, area.X, li.Padding + y + li.YOffset, s, s.Length) |> ignore
                if row.IsEndOfLine then
                    lineno <- lineno + 1
                    printLineno <- true
                else
                    printLineno <- false
                    
            y <- y + li.LineHeight

        for k = bottomRowIndex to topRowIndex + area.Height / li.LineHeight - 1 do
            if linenoWidth > 0 then
                let mutable rect = Win32.RECT(left = area.X, top = y, right = area.X + linenoWidth, bottom = y + li.LineHeight)
                let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(Color.DarkGray))
                Win32.FillRect(hdc, &rect, brush) |> ignore
                Win32.DeleteObject(brush) |> ignore

            y <- y + li.LineHeight

        Win32.DeleteObject(Win32.SelectObject(hdc, old_pen)) |> ignore
        Win32.DeleteObject(Win32.SelectObject(hdc, old_hfont)) |> ignore
        Win32.DeleteObject(selected_brush) |> ignore
        g.ReleaseHdc(hdc)
