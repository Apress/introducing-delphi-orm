program Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainClient in 'MainClient.pas' {FormMain},
  Entities in 'Entities.pas',
  Database.Utilities in 'Database.Utilities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.


