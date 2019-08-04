unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.TabControl, FMX.StdCtrls, FMX.Layouts, Aurelius.Mapping.Metadata,
  FMX.Objects, FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMXTee.Series,
  ImportFrame, Database.Utilities, Aurelius.Criteria.Base, Database.Session.Types;

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
    lbAnsweredLess180Value: TLabel;
    Layout16: TLayout;
    lbSatisfactionScoreLess3: TLabel;
    lbSatisfactionScoreLess3Value: TLabel;
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
    OpenDialog1: TOpenDialog;
    tiImport: TTabItem;
    FrameImport: TFrameImport;
    lbDepartmentTime: TLabel;
    lbAgentTime: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure btAddAgentClick(Sender: TObject);
    procedure btAddDepartmentClick(Sender: TObject);
    procedure btAddPhotoClick(Sender: TObject);
    procedure btDeleteAgentClick(Sender: TObject);
    procedure btDeleteDepartmentClick(Sender: TObject);
    procedure btDeletePhotoClick(Sender: TObject);
    procedure btEditAgentClick(Sender: TObject);
    procedure btEditDepartmentClick(Sender: TObject);
    procedure cbWeek1Click(Sender: TObject);
    procedure cbWeek2Click(Sender: TObject);
    procedure cbWeek3Click(Sender: TObject);
    procedure cbWeek4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FrameImportbtImportClick(Sender: TObject);
    procedure sgAgentsSelectCell(Sender: TObject; const ACol, ARow: Integer; var
        CanSelect: Boolean);
    procedure sgDepartmentsSelectCell(Sender: TObject; const ACol, ARow: Integer;
        var CanSelect: Boolean);
    procedure TabControl1Change(Sender: TObject);
  private
    photo: TBitmap;
    procedure calculateStatistics(const aWeek: TWeeks);
    procedure createViews(const dbSession: IDatabaseSession);
    procedure setupGUI;
    procedure updateDashboard (const aWeek: TWeeks);
    procedure updateDepartments;
    procedure updateAgents;
    procedure loadPhoto (const aGUID: string);
    procedure updateDepartmentList;
    procedure updateAgentList;
    function filter(const aWeek: TWeeks; const aCriteria: TCriteria): TCriteria;
    procedure loadAgentMetadata (const aGUID: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Generics.Collections, FMX.DialogService, EntityForm, ConnectionModule,
  Database.Session, Entities, Aurelius.Types.Blob,
  Database.Import, Aurelius.Criteria.Linq, System.Threading,
  System.Diagnostics, Aurelius.Criteria.Projections, Aurelius.Drivers.Interfaces;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(photo);
end;

procedure TFormMain.loadAgentMetadata(const aGUID: string);
var
  session: IDatabaseSession;
  agent: TAgent;
begin
  Assert(Trim(aGUID) <> '');

  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  agent:=session.objectManager.Find<TAgent>(Trim(aGUID));

  if Assigned(agent) then
  begin
    if agent.CreateUser.HasValue then
      lbCreateUser.Text:=agent.CreateUser.Value
    else
      lbCreateUser.Text:='Not Assigned';

    if agent.ModifyUser.HasValue then
      lbModifyUser.Text:=agent.ModifyUser.Value
    else
      lbModifyUser.Text:='Not Assigned';

    if agent.CreateTS.HasValue then
      lbCreateTS.Text:=FormatDateTime('DD/MM/YYYY, HH:MM', agent.CreateTS.Value)
    else
      lbCreateTS.Text:='Not Assigned';

    if agent.ModifyTS.HasValue then
      lbModifyTS.Text:=FormatDateTime('DD/MM/YYYY, HH:MM ', agent.ModifyTS.Value)
    else
      lbModifyTS.Text:='Not Assigned';
  end;
end;

procedure TFormMain.loadPhoto(const aGUID: string);
var
  session: IDatabaseSession;
  agent: TAgent;
begin
  Assert(Trim(aGUID) <> '');
  FreeAndNil(photo);

  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  agent:=session.objectManager.Find<TAgent>(Trim(aGUID));

  if Assigned(agent) and (not agent.Photo.IsNull) then
  begin
    photo:=TBitmap.Create;
    TDatabaseUtilities<TAgent>.blobToBitmap(agent.Photo, photo);
  end;

  imPhoto.Bitmap:=photo;
  btAddPhoto.Enabled:= not Assigned(photo);
  btDeletePhoto.Enabled:= Assigned(photo);
end;

procedure TFormMain.btAddAgentClick(Sender: TObject);
var
  form: TFormEntity;
  session: IDatabaseSession;
  agent: TAgent;
begin
  form:=TFormEntity.Create(self);
  form.lbTitle.Text:='Add Agent';
  form.Caption:='Add Agent';
  form.edCode.Text:='0';
  form.btAction.Text:='Save';
  form.lbEntity.Text:='Agent';

  if form.ShowModal = mrOk then
  begin
    session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
    agent:=TAgent.Create;
    agent.Description:=Trim(form.edEntity.Text);
    agent.CreateUser:='user';

    TDatabaseUtilities<TAgent>.edit(session.objectManager, agent);

    updateAgents;

  end;

  form.Free;
end;

procedure TFormMain.btAddDepartmentClick(Sender: TObject);
var
  form: TFormEntity;
  session: IDatabaseSession;
  department: TDepartment;
begin
  form:=TFormEntity.Create(self);
  form.lbTitle.Text:='Add Department';
  form.Caption:='Add Department';
  form.edCode.Text:='0';
  form.btAction.Text:='Save';
  form.lbEntity.Text:='Department';

  if form.ShowModal = mrOk then
  begin
    session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
    department:=TDepartment.Create;
    department.Description:=Trim(form.edEntity.Text);

    TDatabaseUtilities<TDepartment>.edit(session.objectManager, department);

    updateDepartments;

  end;

  form.Free;
end;

procedure TFormMain.btAddPhotoClick(Sender: TObject);
var
  session: IDatabaseSession;
  agent: TAgent;
  blob: TBlob;
begin
  OpenDialog1.Filter:='PNG image files|*.png';
  if OpenDialog1.Execute then
  begin
    session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
    agent:=session.objectManager.Find<TAgent>(StringToGUID(
                                        sgAgents.Cells[2, sgAgents.Selected]));
    if Assigned(agent) then
    begin
      imPhoto.Bitmap.LoadFromFile(OpenDialog1.FileName);
      TDatabaseUtilities<TAgent>.bitmapToBlob(imPhoto.Bitmap, 'png', blob);
      agent.Photo:=blob;
      TDatabaseUtilities<TAgent>.edit(session.objectManager, agent);
    end;
  end;
end;

procedure TFormMain.btDeleteAgentClick(Sender: TObject);
var
  session: IDatabaseSession;
  agent: TAgent;
begin
  TDialogService.PreferredMode:=TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Do you really want to delete this entry?',
    TMsgDlgType.mtConfirmation, FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        mrYes: begin
                 session:=TDatabaseSession.Create(
                                            SQLiteConnection.CreateConnection);
                 agent:=session.objectManager.Find<TAgent>(StringToGUID(
                                         sgAgents.Cells[2, sgAgents.Selected]));
                 if Assigned(agent) then
                 begin
                   session.objectManager.Remove(agent);
                   updateAgents;
                 end;
               end;
      end;
    end);
end;

procedure TFormMain.btDeleteDepartmentClick(Sender: TObject);
var
  session: IDatabaseSession;
  department: TDepartment;
begin
  TDialogService.PreferredMode:=TDialogService.TPreferredMode.Platform;
  TDialogService.MessageDialog('Do you really want to delete this entry?',
    TMsgDlgType.mtConfirmation, FMX.Dialogs.mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        mrYes: begin
                 session:=TDatabaseSession.Create(
                                            SQLiteConnection.CreateConnection);
                 department:=session.objectManager.Find<TDepartment>(
                  StringToGUID(sgDepartments.Cells[2, sgDepartments.Selected]));
                 if Assigned(department) then
                 begin
                   session.objectManager.Remove(department);
                   updateDepartments;
                 end;
               end;
      end;
    end);
end;

procedure TFormMain.btDeletePhotoClick(Sender: TObject);
var
  session: IDatabaseSession;
  agent: TAgent;
begin
  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  agent:=session.objectManager.Find<TAgent>(StringToGUID(
                                        sgAgents.Cells[2, sgAgents.Selected]));
  if Assigned(agent) then
  begin
    imPhoto.Bitmap:=nil;
    agent.Photo.IsNull:=True;
    TDatabaseUtilities<TAgent>.edit(session.objectManager, agent);
  end;
end;

procedure TFormMain.btEditAgentClick(Sender: TObject);
var
  form: TFormEntity;
  session: IDatabaseSession;
  agent: TAgent;
begin
  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  agent:=session.objectManager.Find<TAgent>(StringToGUID(
                                          sgAgents.Cells[2, sgAgents.Selected]));
  if Assigned(agent) then
  begin
    form:=TFormEntity.Create(self);
    form.lbTitle.Text:='Edit Agent';
    form.Caption:='Edit Agent';
    form.btAction.Text:='Update';
    form.lbEntity.Text:='Agent';

    form.edCode.Text:=GUIDToString(agent.ID);
    form.edEntity.Text:=agent.Description;

    if form.ShowModal = mrOk then
    begin
      agent.Description:=Trim(form.edEntity.Text);
      agent.ModifyUser:='user';

      TDatabaseUtilities<TAgent>.edit(session.objectManager, agent);

      updateAgents;
    end;

    form.Free;
  end;
end;

procedure TFormMain.btEditDepartmentClick(Sender: TObject);
var
  form: TFormEntity;
  session: IDatabaseSession;
  department: TDepartment;
begin
  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  department:=session.objectManager.Find<TDepartment>(StringToGUID(
                                    sgDepartments.Cells[2, sgDepartments.Selected]));
  if Assigned(department) then
  begin
    form:=TFormEntity.Create(self);
    form.lbTitle.Text:='Edit Department';
    form.Caption:='Edit Department';
    form.btAction.Text:='Update';
    form.lbEntity.Text:='Department';

    form.edCode.Text:=GUIDToString(department.ID);
    form.edEntity.Text:=department.Description;

    if form.ShowModal = mrOk then
    begin
      department.Description:=Trim(form.edEntity.Text);

      TDatabaseUtilities<TDepartment>.edit(session.objectManager, department);

      updateDepartments;
    end;

    form.Free;
  end;
end;

procedure TFormMain.calculateStatistics(const aWeek: TWeeks);
var
  session: IDatabaseSession;
  criteria: TCriteria;
  projRes: TCriteriaResult;
  totalCalls: integer;
  agentList: TObjectList<TCriteriaResult>;
  agentAnsweredList: TObjectList<TCriteriaResult>;
  agentResolvedList: TObjectList<TCriteriaResult>;
  agentAnswerSpeedList: TObjectList<TCriteriaResult>;
  agentCRRetrievedList: TObjectList<TCriteriaResult>;
  agentCRTrendList: TList<string>;
  index: Integer;
  callsDiff: integer;

  overallStatsList: TList<TOverallStatistics>;
  overallStats: TOverallStatistics;

  tempStats: TOverallStatistics;

begin
  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);

  criteria:=session.objectManager('Database').Find<TOverallStatistics>;
  criteria:=filter(aWeek, criteria);
  overallStatsList:=criteria.List<TOverallStatistics>;

  tempStats:=TOverallStatistics.Create;

  for overallStats in overallStatsList do
  begin
    tempStats.SatisfactionScore:=
      ( (tempStats.SatisfactionScore * tempStats.TotalCalls +
          overallStats.SatisfactionScore * overallStats.TotalCalls) ) /
            (tempStats.TotalCalls + overallStats.TotalCalls);

    tempStats.AnswerSpeed:=
      ( (tempStats.AnswerSpeed * tempStats.TotalCalls +
          overallStats.AnswerSpeed * overallStats.TotalCalls) ) /
            (tempStats.TotalCalls + overallStats.TotalCalls);

    tempStats.AbandonRate:=
      ( (tempStats.AbandonRate * tempStats.TotalCalls +
          overallStats.AbandonRate * overallStats.TotalCalls) ) /
            (tempStats.TotalCalls + overallStats.TotalCalls);

    tempStats.CallsLess180:= tempStats.CallsLess180 + overallStats.CallsLess180;

    tempStats.CallsLess3:=tempStats.CallsLess3 + overallStats.CallsLess3;

    tempStats.CallsLess3Perc:=
      ( (tempStats.CallsLess3Perc * tempStats.TotalCalls +
                                            overallStats.CallsLess3 ) ) /
            (tempStats.TotalCalls + overallStats.TotalCalls);

    tempStats.TotalCalls:=tempStats.TotalCalls + overallStats.TotalCalls;

    tempStats.CallsMinute:= tempStats.TotalCalls / 9 / 60;

  end;

  lbSatisfactionValue.Text:=format('%2.2f', [tempStats.SatisfactionScore]);
  lbTotalCallsValue.Text:=tempStats.TotalCalls.ToString;
  lbAnswerSpeedValue.Text:=FormatDateTime('n:ss', tempStats.AnswerSpeed);
  lbAbandonRateValue.Text:=format('%3.2f%%', [tempStats.AbandonRate]);
  lbCallsMinuteValue.Text:=format('%3.2f', [tempStats.CallsMinute]);
  lbAnsweredLess180Value.Text:=tempStats.CallsLess180.ToString;
      lbSatisfactionScoreLess3Value.Text:= format('%d (%3.2f%%)',
                [tempStats.CallsLess3,
                 tempStats.CallsLess3Perc * 100]);

  overallStatsList.Free;
  tempStats.Free;


{$REGION 'Agent List'}
  sgDashboardAgents.RowCount:=0;

  // Total Calls
  criteria:=session.objectManager.Find<TCall>
                                   .CreateAlias('AgentID', 'agent')
                                   .Select(TProjections.ProjectionList
                                     .Add(TProjections.Prop('agent.Description').As_('Name'))
                                     .Add(TProjections.Count('ID').As_('TotalCalls'))
                                     .Add(TProjections.Group('agent.ID'))
                                   )
                                   .OrderBy('agent.Description');

  criteria:=filter(aWeek, criteria);
  agentList:=criteria.ListValues;


  // Calls Answered
  criteria:=session.objectManager.Find<TCall>
                                   .CreateAlias('AgentID', 'agent')
                                   .Select(TProjections.ProjectionList
                                     .Add(TProjections.Count('ID').As_('AnsweredCalls'))
                                     .Add(TProjections.Group('agent.ID'))
                                   )
                                   .Where(Linq['Answered'] = 1)
                                   .OrderBy('agent.Description');

  criteria:=filter(aWeek, criteria);
  agentAnsweredList:=criteria.ListValues;

  // Answer Speed
  criteria:=session.objectManager.Find<TCall>
                                   .CreateAlias('AgentID', 'agent')
                                   .Select(TProjections.ProjectionList
                                     .Add(TProjections.Avg(Linq['QueueExitTime'] - Linq['QueueEntryTime']).As_('AnswerSpeed'))
                                     .Add(TProjections.Group('agent.ID'))
                                   )
                                   .Where(not Linq['QueueExitTime'].IsNull)
                                   .OrderBy('agent.Description');

  criteria:=filter(aWeek, criteria);
  agentAnswerSpeedList:=criteria.ListValues;


  // Call Resolution
  criteria:=session.objectManager.Find<TCall>
                                   .CreateAlias('AgentID', 'agent')
                                   .Select(TProjections.ProjectionList
                                     .Add(TProjections.Count('ID').As_('ResolvedCalls'))
                                     .Add(TProjections.Group('agent.ID'))
                                   )
                                   .Where(Linq['Resolved'] = 1)
                                   .OrderBy('agent.Description');

  criteria:=filter(aWeek, criteria);
  agentResolvedList:=criteria.ListValues;

  // CR Trends
  agentCRTrendList:=TList<string>.Create;
  agentCRTrendList.Count:=agentList.Count;

  if aWeek = wWeek1 then
    for index:=0 to agentCRTrendList.Count-1 do
      agentCRTrendList[index]:='Data not available'
  else
  begin
    agentCRRetrievedList:=session.objectManager.Find<TCall>
                                   .CreateAlias('AgentID', 'agent')
                                   .Select(TProjections.ProjectionList
                                     .Add(TProjections.Count('ID').As_('ResolvedCalls'))
                                     .Add(TProjections.Group('agent.ID'))
                                   )
                                   .Where(Linq['Resolved'] = 1)
                                   .OrderBy('agent.Description')
                                   .Add(Linq['Week'] = integer(aWeek))
                                   .ListValues;

    for index:=0 to agentCRRetrievedList.Count-1 do
    begin
      callsDiff:=Integer(agentCRRetrievedList[index].Values['ResolvedCalls']) -
          Integer(agentResolvedList[index].Values['ResolvedCalls']);
      if callsDiff > 0 then
        agentCRTrendList[index]:=' + '
      else
      if callsDiff <0 then
        agentCRTrendList[index]:=' - '
      else
        agentCRTrendList[index]:=' No change ';
    end;

    agentCRRetrievedList.Free;
  end;


  // Populate Grid
  for index:=0 to agentList.Count-1 do
  begin
    sgDashboardAgents.RowCount:=sgDashboardAgents.RowCount + 1;
    sgDashboardAgents.Cells[0, sgDashboardAgents.RowCount - 1]:=agentList[index].Values['Name'];
    sgDashboardAgents.Cells[1, sgDashboardAgents.RowCount - 1]:=agentList[index].Values['TotalCalls'];
    sgDashboardAgents.Cells[2, sgDashboardAgents.RowCount - 1]:=format('%3.2f%%',
                    [integer(agentAnsweredList[index].Values['AnsweredCalls']) /
                     integer(agentList[index].Values['TotalCalls']) * 100]);
    sgDashboardAgents.Cells[3, sgDashboardAgents.RowCount - 1]:=
                          FormatDateTime('n:ss',agentAnswerSpeedList[index].Values['AnswerSpeed']);
    sgDashboardAgents.Cells[4, sgDashboardAgents.RowCount - 1]:=format('%3.2f%%',
                    [integer(agentResolvedList[index].Values['ResolvedCalls']) /
                     integer(agentList[index].Values['TotalCalls']) * 100]);
    sgDashboardAgents.Cells[5, sgDashboardAgents.RowCount - 1]:= agentCRTrendList[index];

  end;

{$ENDREGION}

{$REGION 'Clean up'}
  agentList.Free;
  agentAnsweredList.Free;
  agentResolvedList.Free;
  agentAnswerSpeedList.Free;
  agentCRTrendList.Free;
{$ENDREGION}

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

procedure TFormMain.createViews(const dbSession: IDatabaseSession);
var
  statement: IDBStatement;
  sqlScript: string;
begin
  sqlScript:=
  'CREATE VIEW IF NOT EXISTS OverallStatistics AS select' +
  '  Week,' +
  '  avg(SatisfactionScore) as SatisfactionScore,' +
  '  Count(*) as TotalCalls,' +
  '  (Avg( case' +
  '          when QueueExitTime is not null then QueueExitTime - QueueEntryTime' +
  '        end )) as AnswerSpeed,' +
  '  (Count( case' +
  '            when QueueExitTime is null then 1' +
  '          end ) * 100.0 ) / count(*) as AbandonRate,' +
  '  (Count(*) / 9.00 / 60.00) as CallsMinute,' +
  '  (Count ( case' +
  '             when (QueueExitTime is not null) and ((QueueExitTime - QueueEntryTime) < 0.00208333333333333) then 1' +
  '           end )) as CallsLess180,' +
  '  (Count( case' +
  '            when SatisfactionScore < 3 then 1' +
  '          end )) as CallsLess3,' +
  '   (Count( case' +
  '             when SatisfactionScore < 3 then 1' +
  '           end ) * 100.0 / count(*) ) as CallsLess3Perc ' +
  'FROM' +
  '  Call ' +
  'GROUP BY' +
  '  Week;';

  statement:=dbSession.objectManager.Connection.CreateStatement;
  statement.SetSQLCommand(sqlScript);
  statement.Execute;
end;

function TFormMain.filter(const aWeek: TWeeks; const aCriteria: TCriteria):
    TCriteria;
begin
  if aWeek = wWeek4  then
    result:= aCriteria.Add(Linq['Week'] >= 4)
  else
    result:= aCriteria.Add(Linq['Week'] = integer(aWeek)+1);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  dbSession: IDatabaseSession;
begin
  SQLiteConnection:=TSQLiteConnection.Create(self);
  dbSession:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  dbSession.databaseManager.UpdateDatabase;

  createViews(dbSession);

  setupGUI;
end;

procedure TFormMain.FrameImportbtImportClick(Sender: TObject);
begin
  OpenDialog1.Filter:='CSV Files (*.csv)|*.csv';
  if OpenDialog1.Execute then
    importData(OpenDialog1.FileName, SQLiteConnection.CreateConnection, FrameImport);
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

  loadPhoto(sgAgents.Cells[2, ARow]);
  loadAgentMetadata(sgAgents.Cells[2, ARow]);

  updateAgentList;

end;

procedure TFormMain.sgDepartmentsSelectCell(Sender: TObject; const ACol, ARow:
    Integer; var CanSelect: Boolean);
begin
  btEditDepartment.Enabled := sgDepartments.Selected >= 0;
  btDeleteDepartment.Enabled := sgDepartments.Selected >= 0;

  updateDepartmentList;

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
    updateAgents;
  if TabControl1.ActiveTab = tiDepartments then
    updateDepartments;
end;

procedure TFormMain.updateAgentList;
var
  session: IDatabaseSession;
  agent: TAgent;
  call: TCall;
  watch: TStopwatch;
  time: TTime;
begin
  sgAgentDetails.RowCount:=0;

  if sgAgents.Selected < 0 then
    Exit;

  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);

  try
    watch:=TStopwatch.StartNew;
    agent:=session.objectManager.Find<TAgent>(
                          StringToGUID(sgAgents.Cells[2, sgAgents.Selected]));

    if Assigned(agent) then
    begin
      sgAgentDetails.BeginUpdate;

      for call in agent.CallList do
      begin

        sgAgentDetails.RowCount := sgAgentDetails.RowCount + 1;
        sgAgentDetails.Cells[0, sgAgentDetails.RowCount - 1]:= sgAgentDetails.RowCount.ToString;
        sgAgentDetails.Cells[1, sgAgentDetails.RowCount - 1]:= call.CallID;
        sgAgentDetails.Cells[2, sgAgentDetails.RowCount - 1]:=
                                           FormatDateTime('dd/mm/yyyy', call.Date);
        sgAgentDetails.Cells[3, sgAgentDetails.RowCount - 1]:=
                                           FormatDateTime('hh:mm:ss', call.QueueEntryTime);
        if call.QueueExitTime.HasValue then
          sgAgentDetails.Cells[4, sgAgentDetails.RowCount - 1]:=
                           FormatDateTime('hh:mm:ss', call.QueueExitTime.Value - call.QueueEntryTime);

        if call.ServiceStartTime.HasValue and call.ServiceEndTime.HasValue then
          sgAgentDetails.Cells[5, sgAgentDetails.RowCount - 1]:=
                           FormatDateTime('hh:mm:ss', call.ServiceEndTime.Value - call.ServiceStartTime.Value);

        if call.Resolved = 0 then
          sgAgentDetails.Cells[6, sgAgentDetails.RowCount - 1]:= 'N'
        else
          sgAgentDetails.Cells[6, sgAgentDetails.RowCount - 1]:= 'Y';

        if call.SatisfactionScore.HasValue then
          sgAgentDetails.Cells[7, sgAgentDetails.RowCount - 1]:=
                                           call.SatisfactionScore.Value.ToString;
      end;

    end;

  finally
    sgAgentDetails.EndUpdate;

    watch.Stop;

    time:= watch.Elapsed.TotalMilliseconds / 1000 / SecsPerDay;

    lbAgentTime.Text:=FormatDateTime('ss:zzz',time)+' (sec:msec)';

  end;

  lbAgentDetailsNoEntries.Visible := sgAgentDetails.RowCount = 0;
end;

procedure TFormMain.updateAgents;
var
  session: IDatabaseSession;
  agentList: TList<TAgent>;
  agent: TAgent;
begin
  sgAgents.RowCount:=0;
  sgAgentDetails.RowCount:=0;

  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  agentList:=session.ObjectManager.Find<TAgent>
                                  .OrderBy('Description')
                                  .List;
  for agent in agentList do
  begin
    sgAgents.RowCount := sgAgents.RowCount + 1;
    sgAgents.Cells[0, sgAgents.RowCount - 1]:= sgAgents.RowCount.ToString;
    sgAgents.Cells[1, sgAgents.RowCount - 1]:= agent.Description;
    sgAgents.Cells[2, sgAgents.RowCount - 1]:= GUIDToString(agent.ID);
  end;

  agentList.Free;

  btEditAgent.Enabled:= sgAgents.Selected>-1;
  btDeleteAgent.Enabled:= sgAgents.Selected>-1;
  lbAgentDetailsNoEntries.Visible:=sgAgentDetails.RowCount = 0;

  imPhoto.Bitmap:=nil;
  btAddPhoto.Enabled:= sgAgents.Selected>-1;
  btDeletePhoto.Enabled:= sgAgents.Selected>-1;
end;

procedure TFormMain.updateDashboard(const aWeek: TWeeks);
begin
  // Update the sidebar
  cbWeek1.IsPressed := cbWeek1.Tag = integer(aWeek);
  cbWeek2.IsPressed := cbWeek2.Tag = integer(aWeek);
  cbWeek3.IsPressed := cbWeek3.Tag = integer(aWeek);
  cbWeek4.IsPressed := cbWeek4.Tag = integer(aWeek);

  calculateStatistics (aWeek);
end;

procedure TFormMain.updateDepartmentList;
var
  session: IDatabaseSession;
  callList: TObjectList<TCall>;
  call: TCall;
  watch: TStopWatch;
  time: TTime;
begin
  sgDepartmentDetails.RowCount:=0;

  if sgDepartments.Selected < 0 then
    Exit;

  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);

  try
    watch:=TStopwatch.StartNew;
    callList:=session.objectManager.Find<TCall>
                              .CreateAlias('DepartmentID', 'department')
                                .Where(Linq['department.ID'] =
                                  sgDepartments.Cells[2, sgDepartments.Selected])
                              .List;

    sgDepartmentDetails.BeginUpdate;

    for call in callList do
    begin
      sgDepartmentDetails.RowCount := sgDepartmentDetails.RowCount + 1;
      sgDepartmentDetails.Cells[0, sgDepartmentDetails.RowCount - 1]:= sgDepartmentDetails.RowCount.ToString;
      sgDepartmentDetails.Cells[1, sgDepartmentDetails.RowCount - 1]:= call.CallID;
      sgDepartmentDetails.Cells[2, sgDepartmentDetails.RowCount - 1]:=
                                         FormatDateTime('dd/mm/yyyy', call.Date);
      sgDepartmentDetails.Cells[3, sgDepartmentDetails.RowCount - 1]:=
                                         FormatDateTime('hh:mm:ss', call.QueueEntryTime);
      if call.QueueExitTime.HasValue then
        sgDepartmentDetails.Cells[4, sgDepartmentDetails.RowCount - 1]:=
                         FormatDateTime('hh:mm:ss', call.QueueExitTime.Value - call.QueueEntryTime);

      if call.ServiceStartTime.HasValue and call.ServiceEndTime.HasValue then
        sgDepartmentDetails.Cells[5, sgDepartmentDetails.RowCount - 1]:=
                         FormatDateTime('hh:mm:ss', call.ServiceEndTime.Value - call.ServiceStartTime.Value);

      if call.Resolved = 0 then
        sgDepartmentDetails.Cells[6, sgDepartmentDetails.RowCount - 1]:= 'N'
      else
        sgDepartmentDetails.Cells[6, sgDepartmentDetails.RowCount - 1]:= 'Y';

      if call.SatisfactionScore.HasValue then
        sgDepartmentDetails.Cells[7, sgDepartmentDetails.RowCount - 1]:=
                                         call.SatisfactionScore.Value.ToString;
    end;

  finally
    sgDepartmentDetails.EndUpdate;

    callList.Free;

    watch.Stop;

    time:=watch.Elapsed.TotalMilliseconds / 1000 / SecsPerDay;

    lbDepartmentTime.Text:=FormatDateTime('ss:zzz', time) +' (sec:msec)';
  end;


  lbDepartmentDetailsNoEntries.Visible := sgDepartmentDetails.RowCount = 0;
end;

procedure TFormMain.updateDepartments;
var
  session: IDatabaseSession;
  departmentList: TList<TDepartment>;
  department: TDepartment;
begin
  sgDepartments.RowCount:=0;
  sgDepartmentDetails.RowCount:=0;

  session:=TDatabaseSession.Create(SQLiteConnection.CreateConnection);
  departmentList:=session.ObjectManager.Find<TDepartment>
                                  .OrderBy('Description')
                                  .List;
  for department in departmentList do
  begin
    sgDepartments.RowCount := sgDepartments.RowCount + 1;
    sgDepartments.Cells[0, sgDepartments.RowCount - 1]:= sgDepartments.RowCount.ToString;
    sgDepartments.Cells[1, sgDepartments.RowCount - 1]:= department.Description;
    sgDepartments.Cells[2, sgDepartments.RowCount - 1]:= GUIDToString(department.ID);
  end;

  departmentList.Free;

  btEditDepartment.Enabled:= sgDepartments.Selected>-1;
  btDeleteDepartment.Enabled:= sgDepartments.Selected>-1;
  lbDepartmentDetailsNoEntries.Visible:=sgDepartmentDetails.RowCount = 0;
end;

end.
