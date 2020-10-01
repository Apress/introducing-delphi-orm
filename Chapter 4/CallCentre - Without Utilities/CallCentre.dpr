program CallCentre;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {FormMain},
  EntityForm in 'Forms\EntityForm.pas' {FormEntity},
  Entities in 'Entities.pas',
  ConnectionModule in 'Forms\ConnectionModule.pas' {SQLiteConnection: TDataModule},
  Database.Session in 'Database.Session.pas',
  Database.Session.Types in 'Database.Session.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TSQLiteConnection, SQLiteConnection);
  Application.Run;
end.


