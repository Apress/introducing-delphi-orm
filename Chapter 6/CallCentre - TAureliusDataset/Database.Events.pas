unit Database.Events;

interface

uses
  Database.Events.Types,
  Aurelius.Events.Manager;

type
  TDatabaseEvents = class (TInterfacedObject, IDatabaseEvents)
  private
    fInsertingProc: TInsertingProc;
    fUpdatingProc: TUpdatingProc;
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
  Entities,
  System.SysUtils,
  Aurelius.Mapping.Explorer;

constructor TDatabaseEvents.Create;
begin
  inherited;
  fInsertingProc:= procedure (Args: TInsertingArgs)
                 begin
                   if Args.Entity is TAgent then
                   begin
                     (Args.Entity as TAgent).CreateTS:=Now;
                   end;
                 end;
  fUpdatingProc:= procedure (Args: TUpdatingArgs)
                begin
                  if Args.Entity is TAgent then
                  begin
                    (Args.Entity as TAgent).ModifyTS:=Now;
                    Args.RecalculateState:=True;
                  end;
                end;
end;

destructor TDatabaseEvents.Destroy;
begin
  unsubscribeEvents;
  fInsertingProc:=nil;
  fUpdatingProc:=nil;
  inherited;
end;

procedure TDatabaseEvents.subscribeEvents;
begin
  TMappingExplorer.Default.Events.OnInserting.Subscribe(fInsertingProc);
  TMappingExplorer.Default.Events.OnUpdating.Subscribe(fUpdatingProc);
end;

procedure TDatabaseEvents.unsubscribeEvents;
begin
  TMappingExplorer.Default.Events.OnInserting.Unsubscribe(fInsertingProc);
  TMappingExplorer.Default.Events.OnUpdating.Unsubscribe(fUpdatingProc);
end;

end.
