program User;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  IdHTTP,
  Entities in 'Entities.pas',
  Bcl.Json;

var
  idHTTP: TIdHTTP;
  newUser: TUsers;
  response: string;
begin
  ReportMemoryLeaksOnShutdown:=True;
  try
    idHTTP:=TIdHTTP.Create(nil);
    response:=idHTTP.Get('http://jsonplaceholder.typicode.com/users/1');
    newUser:=TJSON.Deserialize<TUsers>(response);

    newUser.address.geo.Free;
    newUser.address.Free;
    newUser.company.Free;

    newUser.Free;

    idHTTP.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
