unit EntityForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TFormEntity = class(TForm)
    lbTitle: TLabel;
    Layout1: TLayout;
    lbID: TLabel;
    edCode: TEdit;
    edEntity: TEdit;
    lbEntity: TLabel;
    Layout2: TLayout;
    btCancel: TButton;
    btAction: TButton;
    procedure edEntityChangeTracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFormEntity.edEntityChangeTracking(Sender: TObject);
begin
  btAction.Enabled:= Trim(edEntity.Text) <> '';
end;

end.
