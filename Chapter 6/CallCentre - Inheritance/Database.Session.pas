unit Database.Session;

interface

uses
  Database.Session.Types,
  Aurelius.Engine.DatabaseManager,
  Aurelius.Drivers.Interfaces,
  Aurelius.Engine.ObjectManager, System.Generics.Collections;

type
  TDatabaseSession = class (TInterfacedObject, IDatabaseSession)
  private
    fConnection: IDBConnection;
    fDatabaseManager: TDatabaseManager;
    fObjectManagerDictionary: TObjectDictionary<string, TObjectManager>;
  public
    constructor Create(const aConnection: IDBConnection);
    destructor Destroy; override;
{$REGION 'Interface'}
    function databaseManager: TDatabaseManager;
    function objectManager: TObjectManager; overload;
    function objectManager (const aModel: string): TObjectManager; overload;
{$ENDREGION}
  end;

implementation

uses
  System.SysUtils, Aurelius.Mapping.Explorer;

constructor TDatabaseSession.Create(const aConnection: IDBConnection);
begin
  Assert(aConnection <> nil);

  inherited Create;
  fConnection:=aConnection;
  fObjectManagerDictionary:=TObjectDictionary<string, TObjectManager>.Create([doOwnsValues]);
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
  fObjectManagerDictionary.Free;
  inherited;
end;

function TDatabaseSession.objectManager(const aModel: string): TObjectManager;
var
  cModel: string;
begin
  cModel:=Trim(UpperCase(aModel));
  if cModel='' then
    Result:=objectManager
  else
  begin
    if not fObjectManagerDictionary.ContainsKey(cModel) then
      if cModel = 'DEFAULT' then
        fObjectManagerDictionary.Add('DEFAULT', TObjectManager.Create(fConnection))
      else
        fObjectManagerDictionary.Add(cModel,
              TObjectManager.Create(fConnection, TMappingExplorer.Get(cModel)));
    Result:=fObjectManagerDictionary.Items[cModel];
  end;
end;

function TDatabaseSession.objectManager: TObjectManager;
begin
  result:=objectManager('default');
end;

end.

