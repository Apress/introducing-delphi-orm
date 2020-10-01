program CallCentre;

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppFMX,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {FormMain},
  EntityForm in 'Forms\EntityForm.pas' {FormEntity},
  Entities in 'Entities.pas',
  ConnectionModule in 'Forms\ConnectionModule.pas' {SQLiteConnection: TDataModule},
  Database.Session in 'Database.Session.pas',
  Database.Session.Types in 'Database.Session.Types.pas',
  Database.Utilities in 'Database.Utilities.pas',
  ImportFrame in 'Forms\ImportFrame.pas' {FrameImport: TFrame},
  Database.Import in 'Database.Import.pas',
  Database.Events in 'Database.Events.pas',
  Database.Events.Types in 'Database.Events.Types.pas',
  FileSelectForm in 'Forms\FileSelectForm.pas' {fmFileSelect};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TSQLiteConnection, SQLiteConnection);
  Application.Run;
end.








