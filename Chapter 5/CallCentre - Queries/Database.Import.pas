unit Database.Import;

interface

uses
  Aurelius.Drivers.Interfaces,
  ImportFrame;

procedure importData (const aFilename: string; const aConnection: IDBConnection;
                      const aImportFrame: TFrameImport);
implementation

uses
  System.SysUtils, System.Generics.Collections, System.Rtti,
  Database.Session.Types, Aurelius.Engine.ObjectManager, Database.Session,
  Aurelius.Engine.DatabaseManager, Entities, FMX.Graphics, Database.Utilities,
  Aurelius.Types.Blob, FMX.StdCtrls, System.Threading, System.Classes,
  System.TimeSpan, System.DateUtils, System.Math, Aurelius.Types.Nullable,
  System.Diagnostics;

procedure updateLabel (const aLabel: TLabel; const aText: string);
begin
  Assert(aLabel <> nil);
  aLabel.Text:=aText;
end;

procedure updateProgressBar (const aBar: TProgressBar; const aValue: Single);
begin
  Assert(aBar <> nil);
  aBar.BeginUpdate;
  aBar.Value:=aValue;
  aBar.EndUpdate;
end;

procedure importData (const aFilename: string; const aConnection: IDBConnection;
                      const aImportFrame: TFrameImport);
const
  maxCalls = 1772;
  maxAgents = 8;
  maxDepartments = 5;

begin
  Assert(aConnection <> nil);
  Assert(aImportFrame <> nil);
  Assert(trim(aFilename) <> '');
  Assert(FileExists(aFilename));

  aImportFrame.btImport.Enabled:=False;
  aImportFrame.pbImport.Max:=5;

  TTask.Run(procedure
            var
              session: IDatabaseSession;
              dbManager: TDatabaseManager;
              objManager: TObjectManager;

              agent: TAgent;
              agentName: string;
              agentPhoto: TBitmap;
              agentBlob: TBlob;
              agentsDictionary: TDictionary<string, TAgent>;

              department: TDepartment;
              departmentName: string;
              departmentDictionary: TDictionary<string, TDepartment>;

              count: Integer;

              watch: TStopWatch;
              time: TTime;
              list: TStringList;
              item: string;
              timeSpan: TTimeSpan;
              itemArray: TArray<string>;
              transaction: IDBTransaction;
              call: TCall;
              format: TFormatSettings;
            begin
              session:=TDatabaseSession.Create(aConnection);
              dbManager:=session.databaseManager;
              objManager:=session.objectManager;

              TThread.Queue(nil, procedure
                           begin
                             updateLabel(aImportFrame.lbImport, 'Idle');
                             updateProgressBar(aImportFrame.pbImport, 0);
                           end);

              watch:=TStopwatch.StartNew;
              // Dump the database
              TThread.Queue(nil, procedure
                                       begin
                                         updateLabel(aImportFrame.lbImport, 'Dumping the database...');
                                         updateProgressBar(aImportFrame.pbImport, 1);
                                       end);
              dbManager.DestroyDatabase;

              // Create the database
              TThread.Queue(nil, procedure
                                       begin
                                         updateLabel(aImportFrame.lbImport, 'Creating fresh database...');
                                         updateProgressBar(aImportFrame.pbImport, 2);
                                       end);
              dbManager.UpdateDatabase;

              // Import Agents
              TThread.Queue(nil, procedure
                                       begin
                                         updateLabel(aImportFrame.lbImport, 'Adding Agents...')
                                       end);

              agentsDictionary:=TDictionary<string, TAgent>.Create;
              agentsDictionary.Add('Becky', nil);
              agentsDictionary.Add('Dan', nil);
              agentsDictionary.Add('Diane', nil);
              agentsDictionary.Add('Greg', nil);
              agentsDictionary.Add('Jim', nil);
              agentsDictionary.Add('Joe', nil);
              agentsDictionary.Add('Martha', nil);
              agentsDictionary.Add('Stewart', nil);

              count:=0;
              for agentName in agentsDictionary.Keys do
              begin
                agent:=TAgent.Create;
                agent.Description:=agentName;

                agentPhoto:=TBitmap.Create;
                if random(10) mod 2 = 0 then
                  agentPhoto.LoadFromFile('..\..\..\..\..\Misc\avatar-male.png')
                else
                  agentPhoto.LoadFromFile('..\..\..\..\..\Misc\avatar-female.png');
                TDatabaseUtilities<TAgent>.bitmapToBlob(agentPhoto, 'png', agentBlob);
                agent.Photo:=agentBlob;
                FreeAndNil(agentPhoto);

                TDatabaseUtilities<TAgent>.edit(objManager, agent);

                agentsDictionary[agentName]:=agent;

                Inc(count);
                TThread.Queue(nil, procedure
                                         begin
                                           updateProgressBar(aImportFrame.pbImport,
                                              aImportFrame.pbImport.Value +
                                                            count / maxAgents)
                                         end);
              end;

              TThread.Queue(nil, procedure
                           begin
                             updateProgressBar(aImportFrame.pbImport, 3);
                           end);

              // Import Departments
              TThread.Queue(nil, procedure
                                       begin
                                         updateLabel(aImportFrame.lbImport, 'Adding Departments...');
                                       end);
              departmentDictionary:=TDictionary<string, TDepartment>.Create;
              departmentDictionary.Add('Air Conditioner', nil);
              departmentDictionary.Add('Fridge', nil);
              departmentDictionary.Add('Television', nil);
              departmentDictionary.Add('Toaster', nil);
              departmentDictionary.Add('Washing Machine', nil);

              count:=0;
              for departmentName in departmentDictionary.Keys do
              begin
                department:=TDepartment.Create;
                department.Description:=departmentName;

                TDatabaseUtilities<TDepartment>.edit(objManager, department);

                departmentDictionary[departmentName]:=department;

                Inc(count);
                TThread.Queue(nil, procedure
                                       begin
                                         updateProgressBar(aImportFrame.pbImport, aImportFrame.pbImport.Value +
                                                            count / maxDepartments);
                                       end);
              end;

              TThread.Queue(nil, procedure
                                 begin
                                   updateProgressBar(aImportFrame.pbImport, 4);
                                 end);

              TThread.Queue(nil, procedure
                     begin
                       updateLabel(aImportFrame.lbImport, 'Adding calls...');
                     end);

              list:=TStringList.Create;
              list.LoadFromFile(aFilename);

              list.Delete(0);

              format.DateSeparator:='/';
              format.TimeSeparator:=':';
              format.ShortDateFormat:='DD/MM/YYYY';
              format.ShortTimeFormat:='HH:MM:SS';

              TThread.Queue(nil, procedure
               begin
                 aImportFrame.pbImport.Max:=list.Count;
                 aImportFrame.pbImport.Value:=0;
               end);

              transaction:=objManager.Connection.BeginTransaction;
              try
                count:=0;
                for item in list do
                begin
                  itemArray:=item.Split([',']);

                  call:=TCall.Create;
                  call.CallID:=itemArray[0];
                  call.Date:=StrToDate(itemArray[1], format);
                  call.Week:=WeekOfTheMonth(call.Date);
                  call.QueueEntryTime:=StrToTime(itemArray[2], format);

                  if trim(itemArray[3])<>'' then
                    call.QueueExitTime:=StrToTime(itemArray[3], format)
                  else
                    call.QueueExitTime:=SNull;

                  call.AgentID:=agentsDictionary[itemArray[4]];
                  call.DepartmentID:=departmentDictionary[itemArray[5]];
                  call.Answered:=integer(itemArray[6] = 'Y');
                  call.Resolved:=integer(itemArray[7] = 'Y');

                  call.ServiceStartTime:=call.QueueExitTime;

                  if (not call.ServiceStartTime.IsNull) and
                        (trim(itemArray[8])<>'') then
                  begin
                    timeSpan:=TTimeSpan.Parse(itemArray[8]);
                    call.ServiceEndTime:=IncSecond(call.ServiceStartTime,
                                            Ceil(timeSpan.TotalSeconds));
                  end
                  else
                    call.ServiceEndTime:=SNull;

                  if trim(itemArray[9])<>'' then
                    call.SatisfactionScore:=StrToInt(itemArray[9])
                  else
                    call.SatisfactionScore:=SNull;

                  objManager.Save(call);
                  // Update GUI
                  Inc(count);
                  TThread.Queue(nil, procedure
                     begin
                       updateLabel(aImportFrame.lbImport,
                          'Adding calls ('+count.ToString+
                                  '/'+maxCalls.ToString+')...');
                       updateProgressBar(aImportFrame.pbImport,
                          aImportFrame.pbImport.Value +
                                           count / maxCalls);
                     end);
                end;

                transaction.Commit;
              except
                transaction.Rollback;
              end;

              list.Free;

              watch.Stop;

              // Done
              agentsDictionary.Free;
              departmentDictionary.Free;

              time:=watch.Elapsed.TotalMilliseconds / 1000 / SecsPerDay;

              TThread.Queue(nil, procedure
                                       begin
                                         updateLabel(aImportFrame.lbTime,
                                                FormatDateTime('ss:zzz', time)+' (sec:msec)');
                                         updateLabel(aImportFrame.lbImport, 'Idle');
                                         updateProgressBar(aImportFrame.pbImport,
                                          aImportFrame.pbImport.Max);

                                         aImportFrame.btImport.Enabled:=True;
                                       end);
            end);
end;


end.
