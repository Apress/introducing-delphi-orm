unit Database.Session.Types;

interface

uses
  Aurelius.Engine.DatabaseManager,
  Aurelius.Engine.ObjectManager;

type
  IDatabaseSession = interface
    ['{7CA1B4A1-F339-47EE-AE17-9436853A618E}']
    function databaseManager: TDatabaseManager;
    function objectManager: TObjectManager; overload;
    function objectManager (const aModel: string): TObjectManager; overload;
  end;

implementation

end.
