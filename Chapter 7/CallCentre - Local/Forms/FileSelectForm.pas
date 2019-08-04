unit FileSelectForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Layouts, FMX.ListBox,
  FMX.Controls.Presentation;

////////////////////////////
///  Source: https://stackoverflow.com/questions/30929489/open-file-manage-and-get-selected-file
////////////////////////////

type
  TCallback = procedure (ASelected: String) of object;

  TfmFileSelect = class(TForm)
    Panel1: TPanel;
    btnRefresh: TButton;
    btnSelect: TButton;
    edtCurrentFolder: TEdit;
    pnlDirectoryNotExist: TPanel;
    lblDirectoryNotExist: TLabel;
    lstItems: TListBox;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure lstItemsClick(Sender: TObject);
  private
    { Private declarations }
  public
    const
      CONST_STRING_PARENT = '..';
      CONST_X = '/'; { I know is function for this }

    var
      Callback: TCallback;

    function CD(AFolder: String): Boolean;
  end;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TfmFileSelect.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmFileSelect.FormCreate(Sender: TObject);
begin
  pnlDirectoryNotExist.Visible := False;
end;

procedure TfmFileSelect.btnRefreshClick(Sender: TObject);
begin
  if edtCurrentFolder.Text <> EmptyStr then
    CD(edtCurrentFolder.Text)
  else
    CD(TPath.GetDocumentsPath);
end;

procedure TfmFileSelect.btnSelectClick(Sender: TObject);
var
  LResult: String;
begin
  if Assigned(Callback) then
    begin
      if lstItems.ItemIndex > -1 then
        LResult := lstItems.Items[lstItems.ItemIndex];

      Callback(LResult);
    end;

  Close;
end;

{ TfmFileSelect }

function TfmFileSelect.CD(AFolder: String): Boolean;
var
  LParent: String;
  LDirs,
  LFiles: TStringDynArray;
  s: String;
begin
  lstItems.Clear;
  pnlDirectoryNotExist.Visible := False;
  if (AFolder <> EmptyStr) and (AFolder <> CONST_X) and (AFolder[AFolder.Length - 1] <> CONST_X) then
    AFolder := AFolder + CONST_X;
  edtCurrentFolder.Text := AFolder;

  { http://stackoverflow.com/questions/20318875/how-to-show-the-availble-files-in-android-memory-with-firemonkey }
  if not TDirectory.Exists(AFolder, True) then
    begin
      lblDirectoryNotExist.Text := 'Directory ' + AFolder + ' does not exist.';
      pnlDirectoryNotExist.Visible := True;
      Exit(False);
    end;

  { }
  LParent := TDirectory.GetParent(AFolder);

  { }
  if LParent <> AFolder then
    lstItems.Items.Add(CONST_STRING_PARENT);

  { }
  LDirs := TDirectory.GetDirectories(AFolder, '*');

  // Get all files. Non-Windows systems don't typically care about
  // extensions, so we just use a single '*' as a mask.
  LFiles := TDirectory.GetFiles(AFolder, '*');

  for s in LDirs do
    lstItems.Items.Add(s + CONST_X);

  for s in LFiles do
    lstItems.Items.Add(s);

  Result := True;

  btnSelect.Enabled:=False;
end;

procedure TfmFileSelect.lstItemsClick(Sender: TObject);
var
  s: String;
begin
  btnSelect.Enabled:= lstItems.ItemIndex > -1;

  if lstItems.ItemIndex = -1 then
    Exit;

  if SameText(lstItems.Items[lstItems.ItemIndex], CONST_STRING_PARENT) then
    { Or we need to  use global var for Parent }
    CD(TDirectory.GetParent(edtCurrentFolder.Text))
  else
    begin
      s := lstItems.Items[lstItems.ItemIndex];

      if s = EmptyStr then
        Exit;

      if s[s.Length - 1] = CONST_X then
        CD(s);
    end;
end;

end.

