unit AgentForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TformAgent = class(TForm)
    lbTitle: TLabel;
    Layout1: TLayout;
    Label1: TLabel;
    edCode: TEdit;
    edEntity: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formAgent: TformAgent;

implementation

{$R *.fmx}

end.
