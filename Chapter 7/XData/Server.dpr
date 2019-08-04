program Server;

uses
  Vcl.Forms,
  Container in 'Container.pas' {ServerContainer: TDataModule},
  MainServer in 'MainServer.pas' {MainForm},
  Entities in 'Entities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
