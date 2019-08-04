unit Entities;

interface

uses
  SysUtils,
  Generics.Collections,
  Aurelius.Mapping.Attributes,
  Aurelius.Types.Blob,
  Aurelius.Types.DynamicProperties,
  Aurelius.Types.Nullable,
  Aurelius.Types.Proxy;

type
  TAbandonRatePerDepartment = class;
  TAgent = class;
  TBase = class;
  TCall = class;
  TDepartment = class;
  TOverallStatistics = class;

  [Entity]
  [Table('Base')]
  [Inheritance(TInheritanceStrategy.JoinedTables)]
  [Id('FID', TIdGenerator.SmartGuid)]
  TBase = class
  private
    [Column('ID', [TColumnProp.Required])]
    FID: TGuid;

    [Column('CreateUser', [], 50)]
    FCreateUser: Nullable<string>;

    [Column('CreateTS', [])]
    FCreateTS: Nullable<TDateTime>;

    [Column('ModifyUser', [], 50)]
    FModifyUser: Nullable<string>;

    [Column('ModifyTS', [])]
    FModifyTS: Nullable<TDateTime>;
  public
    property ID: TGuid read FID write FID;
    property CreateUser: Nullable<string> read FCreateUser write FCreateUser;
    property CreateTS: Nullable<TDateTime> read FCreateTS write FCreateTS;
    property ModifyUser: Nullable<string> read FModifyUser write FModifyUser;
    property ModifyTS: Nullable<TDateTime> read FModifyTS write FModifyTS;
  end;

  [Entity]
  [Table('AbandonRatePerDepartment')]
  [Model('Database')]
  [Id('FID', TIdGenerator.SmartGuid)]
  TAbandonRatePerDepartment = class
  private
    [Column('ID', [TColumnProp.Required])]
    FID: TGuid;
    
    [Column('Week', [TColumnProp.Required])]
    FWeek: Integer;
    
    [Column('Description', [TColumnProp.Required], 300)]
    FDescription: string;
    
    [Column('AbandonRate', [TColumnProp.Required])]
    FAbandonRate: Double;
  public
    property ID: TGUID read FID write FID;
    property Week: Integer read FWeek write FWeek;
    property Description: string read FDescription write FDescription;
    property AbandonRate: Double read FAbandonRate write FAbandonRate;
  end;
  
  [Entity]
  [Table('Agent')]
//  [Id('FID', TIdGenerator.SmartGuid)]
  TAgent = class (TBase)
  private
//    [Column('ID', [TColumnProp.Required])]
//    FID: TGuid;

    [Column('Description', [TColumnProp.Required], 255)]
    FDescription: string;

    [Column('Photo', [TColumnProp.Lazy])]
    FPhoto: TBlob;

    [ManyValuedAssociation([TAssociationProp.Lazy], CascadeTypeAll, 'FAgentID')]
    FCallList: Proxy<TList<TCall>>;
    function GetCallList: TList<TCall>;
  public
    constructor Create;
    destructor Destroy; override;
//    property ID: TGuid read FID write FID;
    property Description: string read FDescription write FDescription;
    property Photo: TBlob read FPhoto write FPhoto;
    property CallList: TList<TCall> read GetCallList;
  end;

  [Entity]
  [Table('Call')]
  [Id('FID', TIdGenerator.SmartGuid)]
  TCall = class
  private
    [Column('ID', [TColumnProp.Required])]
    FID: TGuid;
    
    [Column('Date', [TColumnProp.Required])]
    FDate: TDateTime;
    
    [Column('QueueEntryTime', [TColumnProp.Required])]
    FQueueEntryTime: TDateTime;
    
    [Column('QueueExitTime', [])]
    FQueueExitTime: Nullable<TDateTime>;
    
    [Column('ServiceStartTime', [])]
    FServiceStartTime: Nullable<TDateTime>;
    
    [Column('ServiceEndTime', [])]
    FServiceEndTime: Nullable<TDateTime>;
    
    [Column('Answered', [TColumnProp.Required])]
    FAnswered: Integer;
    
    [Column('Resolved', [TColumnProp.Required])]
    FResolved: Integer;
    
    [Column('SatisfactionScore', [])]
    FSatisfactionScore: Nullable<Integer>;
    
    [Column('CallID', [TColumnProp.Required], 50)]
    FCallID: string;
    
    [Column('Week', [TColumnProp.Required])]
    FWeek: Integer;
    
    [Association([TAssociationProp.Lazy], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('AgentID', [], 'ID')]
    FAgentID: Proxy<TAgent>;
    
    [Association([TAssociationProp.Lazy], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('DepartmentID', [], 'ID')]
    FDepartmentID: Proxy<TDepartment>;
    function GetAgentID: TAgent;
    procedure SetAgentID(const Value: TAgent);
    function GetDepartmentID: TDepartment;
    procedure SetDepartmentID(const Value: TDepartment);
  public
    property ID: TGuid read FID write FID;
    property Date: TDateTime read FDate write FDate;
    property QueueEntryTime: TDateTime read FQueueEntryTime write FQueueEntryTime;
    property QueueExitTime: Nullable<TDateTime> read FQueueExitTime write FQueueExitTime;
    property ServiceStartTime: Nullable<TDateTime> read FServiceStartTime write FServiceStartTime;
    property ServiceEndTime: Nullable<TDateTime> read FServiceEndTime write FServiceEndTime;
    property Answered: Integer read FAnswered write FAnswered;
    property Resolved: Integer read FResolved write FResolved;
    property SatisfactionScore: Nullable<Integer> read FSatisfactionScore write FSatisfactionScore;
    property CallID: string read FCallID write FCallID;
    property Week: Integer read FWeek write FWeek;
    property AgentID: TAgent read GetAgentID write SetAgentID;
    property DepartmentID: TDepartment read GetDepartmentID write SetDepartmentID;
  end;
  
  [Entity]
  [Table('Department')]
  [Id('FID', TIdGenerator.SmartGuid)]
  TDepartment = class
  private
    [Column('ID', [TColumnProp.Required])]
    FID: TGuid;

    [Column('Description', [TColumnProp.Required], 255)]
    FDescription: string;

    [ManyValuedAssociation([TAssociationProp.Lazy], CascadeTypeAll, 'FDepartmentID')]
    FCallList: Proxy<TList<TCall>>;
    function GetCallList: TList<TCall>;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: TGuid read FID write FID;
    property Description: string read FDescription write FDescription;
    property CallList: TList<TCall> read GetCallList;
  end;
  
  [Entity]
  [Table('OverallStatistics')]
  [Model('Database')]
  [Id('FWeek', TIdGenerator.None)]
  TOverallStatistics = class
  private
    [Column('Week', [TColumnProp.Required])]
    FWeek: Integer;
    
    [Column('SatisfactionScore', [TColumnProp.Required])]
    FSatisfactionScore: Double;
    
    [Column('TotalCalls', [TColumnProp.Required])]
    FTotalCalls: Integer;
    
    [Column('AnswerSpeed', [TColumnProp.Required])]
    FAnswerSpeed: Double;
    
    [Column('AbandonRate', [TColumnProp.Required])]
    FAbandonRate: Double;
    
    [Column('CallsMinute', [TColumnProp.Required])]
    FCallsMinute: Double;
    
    [Column('CallsLess180', [TColumnProp.Required])]
    FCallsLess180: Integer;
    
    [Column('CallsLess3', [TColumnProp.Required])]
    FCallsLess3: Integer;
    
    [Column('CallsLess3Perc', [TColumnProp.Required])]
    FCallsLess3Perc: Double;
  public
    property Week: Integer read FWeek write FWeek;
    property SatisfactionScore: Double read FSatisfactionScore write FSatisfactionScore;
    property TotalCalls: Integer read FTotalCalls write FTotalCalls;
    property AnswerSpeed: Double read FAnswerSpeed write FAnswerSpeed;
    property AbandonRate: Double read FAbandonRate write FAbandonRate;
    property CallsMinute: Double read FCallsMinute write FCallsMinute;
    property CallsLess180: Integer read FCallsLess180 write FCallsLess180;
    property CallsLess3: Integer read FCallsLess3 write FCallsLess3;
    property CallsLess3Perc: Double read FCallsLess3Perc write FCallsLess3Perc;
  end;
  

implementation

{ TAgent }

constructor TAgent.Create;
begin
  inherited;
  FCallList.SetInitialValue(TList<TCall>.Create);
end;

destructor TAgent.Destroy;
begin
  FCallList.DestroyValue;
  inherited;
end;

function TAgent.GetCallList: TList<TCall>;
begin
  result := FCallList.Value;
end;

{ TCall }

function TCall.GetAgentID: TAgent;
begin
  result := FAgentID.Value;
end;

procedure TCall.SetAgentID(const Value: TAgent);
begin
  FAgentID.Value := Value;
end;

function TCall.GetDepartmentID: TDepartment;
begin
  result := FDepartmentID.Value;
end;

procedure TCall.SetDepartmentID(const Value: TDepartment);
begin
  FDepartmentID.Value := Value;
end;

{ TDepartment }

constructor TDepartment.Create;
begin
  inherited;
  FCallList.SetInitialValue(TList<TCall>.Create);
end;

destructor TDepartment.Destroy;
begin
  FCallList.DestroyValue;
  inherited;
end;

function TDepartment.GetCallList: TList<TCall>;
begin
  result := FCallList.Value;
end;

initialization
  RegisterEntity(TDepartment);
  RegisterEntity(TAgent);
  RegisterEntity(TCall);
  RegisterEntity(TOverallStatistics);
  RegisterEntity(TBase);
  RegisterEntity(TAbandonRatePerDepartment);

finalization

end.
