unit Database.Events.Alternative;

interface

uses
  Database.Events.Types, Aurelius.Events.Manager;

type
  TDatabaseEvents = class (TInterfacedObject, IDatabaseEvents)
  private
    fInsertedProc: TInsertedProc;
    fUpdatedProc: TUpdatedProc;
  public
    constructor Create;
    destructor Destroy; override;
{$REGION 'Interface'}
    procedure subscribeEvents;
    procedure unsubscribeEvents;
{$ENDREGION}

  end;

implementation

uses
  Aurelius.Mapping.Explorer, Entities, Database.Session.Types,
  Aurelius.Drivers.Interfaces, ConnectionModule,
  Database.Session, System.SysUtils, Aurelius.Engine.ObjectManager;

constructor TDatabaseEvents.Create;
var
  statement: IDBStatement;
  objManager: TObjectManager;
  sqlScript: string;
begin
  inherited;
  fInsertedProc:= procedure (Args: TInsertedArgs)
                 begin
                   if Args.Entity is TAgent then
                   begin
                     statement:=(Args.Manager as TObjectManager).Connection.CreateStatement;
                     sqlScript:='update Base set CreateTS = '+
                              Double(Now).ToString+' where ID = '+
                                QuotedStr(TAgent(Args.Entity).ID.ToString);
                       statement.SetSQLCommand(sqlScript);
                       statement.Execute;
                   end;
                 end;
  fUpdatedProc:= procedure (Args: TUpdatedArgs)
                begin
                  if Args.Entity is TAgent then
                  begin
                    statement:=(Args.Manager as TObjectManager).Connection.CreateStatement;
                    sqlScript:='update Base set ModifyTS = '+
                              Double(Now).ToString+' where ID = '+
                                QuotedStr(TAgent(Args.Entity).ID.ToString);
                      statement.SetSQLCommand(sqlScript);
                      statement.Execute;
                  end;
                end;
end;

destructor TDatabaseEvents.Destroy;
begin
  unsubscribeEvents;
  fInsertedProc:=nil;
  fUpdatedProc:=nil;
  inherited;
end;

procedure TDatabaseEvents.subscribeEvents;
begin
  TMappingExplorer.Default.Events.OnInserted.Subscribe(fInsertedProc);
  TMappingExplorer.Default.Events.OnUpdated.Subscribe(fUpdatedProc);
end;

procedure TDatabaseEvents.unsubscribeEvents;
begin
  TMappingExplorer.Default.Events.OnInserted.Unsubscribe(fInsertedProc);
  TMappingExplorer.Default.Events.OnUpdated.Unsubscribe(fUpdatedProc);
end;

end.
