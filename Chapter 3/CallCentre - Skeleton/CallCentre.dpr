program CallCentre;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'Forms\MainForm.pas' {FormMain},
  EntityForm in 'Forms\EntityForm.pas' {FormEntity};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.


