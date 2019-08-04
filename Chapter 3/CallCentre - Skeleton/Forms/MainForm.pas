unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.TabControl, FMX.StdCtrls, FMX.Layouts, Aurelius.Mapping.Metadata,
  FMX.Objects, FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMXTee.Series;

type
  TWeeks = (wWeek1, wWeek2, wWeek3, wWeek4);

  TFormMain = class(TForm)
    TabControl1: TTabControl;
    tiDashboard: TTabItem;
    tiDepartments: TTabItem;
    tiAgents: TTabItem;
    sgAgents: TStringGrid;
    scAgentNr: TStringColumn;
    scAgentDescriptionn: TStringColumn;
    Layout1: TLayout;
    btAddAgent: TButton;
    btEditAgent: TButton;
    btDeleteAgent: TButton;
    Layout2: TLayout;
    sgAgentDetails: TStringGrid;
    scAgentDetailsNr: TStringColumn;
    scAgentDetailsCallID: TStringColumn;
    scAgentDetailsDate: TDateColumn;
    scAgentDetailsWaiting: TTimeColumn;
    scAgentDetailsDuration: TTimeColumn;
    scAgentDetailsResolved: TCheckColumn;
    scAgentDetailsRate: TIntegerColumn;
    scAgentDetailsEntry: TTimeColumn;
    lbAgentDetailsNoEntries: TLabel;
    Layout3: TLayout;
    scAgentID: TStringColumn;
    Layout4: TLayout;
    sgDepartments: TStringGrid;
    scDepartmentNr: TStringColumn;
    scDepartmentDescription: TStringColumn;
    scDepartmentID: TStringColumn;
    Layout5: TLayout;
    btAddDepartment: TButton;
    btEditDepartment: TButton;
    btDeleteDepartment: TButton;
    Layout6: TLayout;
    sgDepartmentDetails: TStringGrid;
    scDepartmentDetailsNr: TStringColumn;
    scDepartmentDetailsCallID: TStringColumn;
    scDepartmentDetailsDate: TDateColumn;
    scDepartmentDetailsEntry: TTimeColumn;
    scDepartmentDetailsWaiting: TTimeColumn;
    scDepartmentDetailsDuration: TTimeColumn;
    scDepartmentDetailsResolved: TCheckColumn;
    scDepartmentDetails: TIntegerColumn;
    lbDepartmentDetailsNoEntries: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    Layout7: TLayout;
    lbTotalCalls: TLabel;
    lbTotalCallsValue: TLabel;
    Layout8: TLayout;
    lbAnswerSpeed: TLabel;
    lbAnswerSpeedValue: TLabel;
    Layout9: TLayout;
    lbAbandonRate: TLabel;
    lbAbandonRateValue: TLabel;
    Layout10: TLayout;
    lbCallsMinute: TLabel;
    lbCallsMinuteValue: TLabel;
    Layout11: TLayout;
    Layout12: TLayout;
    Rectangle1: TRectangle;
    cbWeek1: TCornerButton;
    cbWeek2: TCornerButton;
    cbWeek4: TCornerButton;
    cbWeek3: TCornerButton;
    GridPanelLayout2: TGridPanelLayout;
    sgDashboardAgents: TStringGrid;
    scDashboardAgentsName: TStringColumn;
    scDashboardAgentsTotalCalls: TStringColumn;
    scDashboardAgentsCallsAnswered: TStringColumn;
    scDashboardAgentsSpeedAnswer: TStringColumn;
    scDashboardAgentsCallResolution: TStringColumn;
    scDashboardAgentsCRTrend: TStringColumn;
    Layout13: TLayout;
    lbSatisfaction: TLabel;
    lbSatisfactionValue: TLabel;
    ctSatisfactionScore: TChart;
    Series1: THorizBarSeries;
    ctAbandonRate: TChart;
    HorizBarSeries1: TBarSeries;
    Layout14: TLayout;
    Rectangle2: TRectangle;
    Label1: TLabel;
    Layout15: TLayout;
    lbAnsweredLess180: TLabel;
    Layout16: TLayout;
    lbSatisfactionScoreLess3: TLabel;
    Layout17: TLayout;
    Layout18: TLayout;
    imPhoto: TImage;
    Layout19: TLayout;
    btAddPhoto: TButton;
    btDeletePhoto: TButton;
    Label4: TLabel;
    Layout20: TLayout;
    lbCreateUser: TLabel;
    Label5: TLabel;
    lbCreateTS: TLabel;
    lbModifyUser: TLabel;
    lbModifyTS: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure btAddAgentClick(Sender: TObject);
    procedure btAddDepartmentClick(Sender: TObject);
    procedure btDeleteAgentClick(Sender: TObject);
    procedure btDeleteDepartmentClick(Sender: TObject);
    procedure btEditAgentClick(Sender: TObject);
    procedure btEditDepartmentClick(Sender: TObject);
    procedure cbWeek1Click(Sender: TObject);
    procedure cbWeek2Click(Sender: TObject);
    procedure cbWeek3Click(Sender: TObject);
    procedure cbWeek4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgAgentsSelectCell(Sender: TObject; const ACol, ARow: Integer; var
        CanSelect: Boolean);
    procedure sgDepartmentsSelectCell(Sender: TObject; const ACol, ARow: Integer;
        var CanSelect: Boolean);
    procedure TabControl1Change(Sender: TObject);
  private
    procedure setupGUI;
    procedure updateDashboard (const aWeek: TWeeks);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Generics.Collections, FMX.DialogService, EntityForm;

procedure TFormMain.btAddAgentClick(Sender: TObject);
var
  form: TFormEntity;
begin
  form:=TFormEntity.Create(self);
  form.lbTitle.Text:='Add Agent';
  form.Caption:='Add Agent';
  form.edCode.Text:='0';
  form.btAction.Text:='Save';
  form.lbEntity.Text:='Agent';

  if form.ShowModal = mrOk then
  begin
    // Add code here
  end;

  form.Free;
end;

procedure TFormMain.btAddDepartmentClick(Sender: TObject);
var
  form: TFormEntity;
begin
  form:=TFormEntity.Create(self);
  form.lbTitle.Text:='Add Department';
  form.Caption:='Add Department';
  form.edCode.Text:='0';
  form.btAction.Text:='Save';
  form.lbEntity.Text:='Department';

  if form.ShowModal = mrOk then
  begin
    // Add code here
  end;

  form.Free;
end;

procedure TFormMain.btDeleteAgentClick(Sender: TObject);
begin
  TDialogService.PreferredMode:=TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Do you really want to delete this entry?',
    TMsgDlgType.mtConfirmation, FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        mrYes: begin
                 // Add code here
               end;
      end;
    end);
end;

procedure TFormMain.btDeleteDepartmentClick(Sender: TObject);
begin
  TDialogService.PreferredMode:=TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Do you really want to delete this entry?',
    TMsgDlgType.mtConfirmation, FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        mrYes: begin
                 // Add code here
               end;
      end;
    end);
end;

procedure TFormMain.btEditAgentClick(Sender: TObject);
var
  form: TFormEntity;
begin
  form:=TFormEntity.Create(self);
  form.lbTitle.Text:='Edit Agent';
  form.Caption:='Edit Agent';
  form.edCode.Text:='GUID';
  form.edEntity.Text:='Description';
  form.btAction.Text:='Update';
  form.lbEntity.Text:='Agent';

  if form.ShowModal = mrOk then
  begin
    // Add code here
  end;

  form.Free;
end;

procedure TFormMain.btEditDepartmentClick(Sender: TObject);
var
  form: TFormEntity;
begin
  form:=TFormEntity.Create(self);
  form.lbTitle.Text:='Edit Department';
  form.Caption:='Edit Department';
  form.edCode.Text:='GUID';
  form.edEntity.Text:='Description';
  form.btAction.Text:='Update';
  form.lbEntity.Text:='Department';

  if form.ShowModal = mrOk then
  begin
    // Add code here
  end;

  form.Free;
end;

procedure TFormMain.cbWeek1Click(Sender: TObject);
begin
  updateDashboard(wWeek1);
end;

procedure TFormMain.cbWeek2Click(Sender: TObject);
begin
  updateDashboard(wWeek2);
end;

procedure TFormMain.cbWeek3Click(Sender: TObject);
begin
  updateDashboard(wWeek3);
end;

procedure TFormMain.cbWeek4Click(Sender: TObject);
begin
  updateDashboard(wWeek4);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  setupGUI;
end;

procedure TFormMain.setupGUI;
begin
  sgAgents.RowCount:=0;
  sgAgentDetails.RowCount:=0;

  sgDepartments.RowCount:=0;
  sgDepartmentDetails.RowCount:=0;

  updateDashboard(wWeek1);

  TabControl1.ActiveTab:=tiDashboard;
end;

procedure TFormMain.sgAgentsSelectCell(Sender: TObject; const ACol, ARow:
    Integer; var CanSelect: Boolean);
begin
  btEditAgent.Enabled := sgAgents.Selected >= 0;
  btDeleteAgent.Enabled := sgAgents.Selected >= 0;

  // Add Update of the calls grid

end;

procedure TFormMain.sgDepartmentsSelectCell(Sender: TObject; const ACol, ARow:
    Integer; var CanSelect: Boolean);
begin
  btEditDepartment.Enabled := sgDepartments.Selected >= 0;
  btDeleteDepartment.Enabled := sgDepartments.Selected >= 0;

  // Add Update of the calls grid
end;

procedure TFormMain.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab = tiDashboard then
  begin
    if cbWeek1.IsPressed then
      updateDashboard(wWeek1);
    if cbWeek2.IsPressed then
      updateDashboard(wWeek2);
    if cbWeek3.IsPressed then
      updateDashboard(wWeek3);
    if cbWeek4.IsPressed then
      updateDashboard(wWeek4);
  end;

  if TabControl1.ActiveTab = tiAgents then
    ; // Update Agents here
  if TabControl1.ActiveTab = tiDepartments then
    ; // Update Departments here
end;

procedure TFormMain.updateDashboard(const aWeek: TWeeks);
begin
  // Update the sidebar
  cbWeek1.IsPressed := cbWeek1.Tag = integer(aWeek);
  cbWeek2.IsPressed := cbWeek2.Tag = integer(aWeek);
  cbWeek3.IsPressed := cbWeek3.Tag = integer(aWeek);
  cbWeek4.IsPressed := cbWeek4.Tag = integer(aWeek);
end;

end.
