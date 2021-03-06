; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Malmacs"
#define MyAppVersion "0.1"
#define MyAppExeName "Malmacs.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{BE1568A3-A1E1-4FB6-9884-2F0DF6C6B06B}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
DefaultDirName={userdocs}\{#MyAppName}
DisableProgramGroupPage=yes
OutputBaseFilename=MalmacsInstaller
Compression=lzma
SolidCompression=yes
PrivilegesRequired=lowest

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "Malmacs\bin\Release\Malmacs.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\Malmacs.exe.config"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\FSharp.Core.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\FsMiniMAL.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\Microsoft.WindowsAPICodePack.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\Microsoft.WindowsAPICodePack.Shell.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\System.Collections.Immutable.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\System.ValueTuple.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\bin\Release\Ude.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "Malmacs\Malmacs.config.Release.json"; DestDir: "{app}"; DestName: "Malmacs.config.json";Flags: ignoreversion
Source: "Malmacs\Mal\init.mal"; DestDir: "{app}\Mal"; Flags: ignoreversion
Source: "Malmacs\Mal\mal.mal"; DestDir: "{app}\Mal"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{userprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{userdesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

