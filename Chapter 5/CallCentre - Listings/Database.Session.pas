unit Database.Session;

interface

uses
  Database.Session.Types,
  Aurelius.Engine.DatabaseManager,
  Aurelius.Drivers.Interfaces,
  Aurelius.Engine.ObjectManager;

type
  TDatabaseSession = class (TInterfacedObject, IDatabaseSession)
  private
    fConnection: IDBConnection;
    fDatabaseManager: TDatabaseManager;
    fObjectManager: TObjectManager;
  public
    constructor Create(const aConnection: IDBConnection);
    destructor Destroy; override;
{$REGION 'Interface'}
    function databaseManager: TDatabaseManager;
    function objectManager: TObjectManager;
{$ENDREGION}
  end;

implementation

constructor TDatabaseSession.Create(const aConnection: IDBConnection);
begin
  Assert(aConnection <> nil);

  inherited Create;
  fConnection:=aConnection;
end;

function TDatabaseSession.databaseManager: TDatabaseManager;
begin
  if not Assigned(fDatabaseManager) then
    fDatabaseManager:=TDatabaseManager.Create(fConnection);
  Result:=fDatabaseManager;
end;

destructor TDatabaseSession.Destroy;
begin
  fDatabaseManager.Free;
  fObjectManager.Free;
  inherited;
end;

function TDatabaseSession.objectManager: TObjectManager;
begin
  if not Assigned(fObjectManager) then
    fObjectManager:=TObjectManager.Create(fConnection);
  Result:=fObjectManager;
end;

end.
