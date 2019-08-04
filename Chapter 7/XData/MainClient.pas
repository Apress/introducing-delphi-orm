unit MainClient;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Objects,
  FMX.Grid, FMX.ScrollBox, FMX.Layouts, XData.Client;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    lbURL: TLabel;
    edURL: TEdit;
    btFetch: TButton;
    Layout17: TLayout;
    sgAgents: TStringGrid;
    scAgentNr: TStringColumn;
    scAgentDescriptionn: TStringColumn;
    scAgentID: TStringColumn;
    Layout18: TLayout;
    Layout19: TLayout;
    imPhoto: TImage;
    Layout20: TLayout;
    Label4: TLabel;
    lbCreateUser: TLabel;
    Label5: TLabel;
    lbCreateTS: TLabel;
    Label9: TLabel;
    lbModifyTS: TLabel;
    Label8: TLabel;
    lbModifyUser: TLabel;
    procedure btFetchClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgAgentsSelectCell(Sender: TObject; const ACol, ARow: Integer; var
        CanSelect: Boolean);
  private
    fClient: TXDataClient;
    procedure loadAgents;
    procedure loadData(const aGUID: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Entities, System.Generics.Collections, System.SysUtils, Database.Utilities;

{$R *.fmx}

procedure TFormMain.btFetchClick(Sender: TObject);
begin
  fClient.Uri:=edURL.Text;
  loadAgents;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  fClient.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  fClient:=TXDataClient.Create;
end;

procedure TFormMain.loadAgents;
var
  list: TList<TAgent>;
  agent: TAgent;
begin
  sgAgents.RowCount:=0;
  list:=fClient.List<TAgent>;
  for agent in list do
  begin
    sgAgents.RowCount:=sgAgents.RowCount + 1;
    sgAgents.Cells[0, sgAgents.RowCount - 1]:= sgAgents.RowCount.ToString;
    sgAgents.Cells[1, sgAgents.RowCount - 1]:= agent.Description;
    sgAgents.Cells[2, sgAgents.RowCount - 1]:= agent.ID.ToString;
  end;
  list.Free;
end;

procedure TFormMain.loadData(const aGUID: string);
var
  agent: TAgent;
  bitmap: TBitmap;
begin
  agent:=fClient.Get<TAgent, TGUID>(StringToGUID(aGUID));
  if Assigned(agent) then
  begin
    if not agent.Photo.IsNull then
    begin
      bitmap:=TBitmap.Create;
      TDatabaseUtilities<TAgent>.blobToBitmap(agent.Photo, bitmap);
      imPhoto.Bitmap.Assign(bitmap);
      bitmap.Free;
    end;
    if agent.CreateUser.HasValue then
      lbCreateUser.Text:=agent.CreateUser.Value
    else
      lbCreateUser.Text:='Not Assigned';
    if agent.CreateTS.HasValue then
      lbCreateTS.Text:= FormatDateTime('DD/MM/YYYY, HH:MM', agent.CreateTS.Value)
    else
      lbCreateUser.Text:='Not Assigned';

    if agent.ModifyUser.HasValue then
      lbModifyUser.Text:=agent.ModifyUser.Value
    else
      lbModifyUser.Text:='Not Assigned';
    if agent.ModifyTS.HasValue then
      lbModifyTS.Text:= FormatDateTime('DD/MM/YYYY, HH:MM', agent.ModifyTS.Value)
    else
      lbModifyTS.Text:='Not Assigned';
  end;
end;

procedure TFormMain.sgAgentsSelectCell(Sender: TObject; const ACol, ARow: Integer;
    var CanSelect: Boolean);
begin
  loadData(sgAgents.Cells[2, ARow]);
end;

end.
