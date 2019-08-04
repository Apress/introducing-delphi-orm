unit ConnectionModule;

interface

uses
  Aurelius.Drivers.Interfaces,
  Aurelius.Drivers.SQLite, 
  System.SysUtils, System.Classes, Aurelius.Comp.Connection;

type
  TSQLiteConnection = class(TDataModule)
    AureliusConnection1: TAureliusConnection;
  private
  public
    class function CreateConnection: IDBConnection;
    class function CreateFactory: IDBConnectionFactory;

  end;

var
  SQLiteConnection: TSQLiteConnection;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  Aurelius.Drivers.Base, System.IOUtils;

{$R *.dfm}

{ TMyConnectionModule }

class function TSQLiteConnection.CreateConnection: IDBConnection;
begin
{$IFDEF ANDROID}
  SQLiteConnection.AureliusConnection1.Params.Values['Database']:=
      TPath.Combine(TPath.GetDocumentsPath, 'database.db');
{$ENDIF}
  Result := SQLiteConnection.AureliusConnection1.CreateConnection;
end;

class function TSQLiteConnection.CreateFactory: IDBConnectionFactory;
begin
  Result := TDBConnectionFactory.Create(
    function: IDBConnection
    begin
      Result := CreateConnection;
    end
  );
end;



end.
