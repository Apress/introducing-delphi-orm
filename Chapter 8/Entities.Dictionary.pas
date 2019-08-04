unit Entities.Dictionary;

interface

uses
  Aurelius.Criteria.Dictionary;

type
  TDepartmentTableDictionary = class;
  TAgentTableDictionary = class;
  TCallTableDictionary = class;
  
  TentitiesDicDictionary = class
  private
    FDepartment: TDepartmentTableDictionary;
    FAgent: TAgentTableDictionary;
    FCall: TCallTableDictionary;
    function GetDepartment: TDepartmentTableDictionary;
    function GetAgent: TAgentTableDictionary;
    function GetCall: TCallTableDictionary;
  public
    destructor Destroy; override;
    property Department: TDepartmentTableDictionary read GetDepartment;
    property Agent: TAgentTableDictionary read GetAgent;
    property Call: TCallTableDictionary read GetCall;
  end;
  
  TDepartmentTableDictionary = class
  private
    FID: TDictionaryAttribute;
    FDescription: TDictionaryAttribute;
  public
    constructor Create;
    property ID: TDictionaryAttribute read FID;
    property Description: TDictionaryAttribute read FDescription;
  end;
  
  TAgentTableDictionary = class
  private
    FID: TDictionaryAttribute;
    FDescription: TDictionaryAttribute;
    FPhoto: TDictionaryAttribute;
  public
    constructor Create;
    property ID: TDictionaryAttribute read FID;
    property Description: TDictionaryAttribute read FDescription;
    property Photo: TDictionaryAttribute read FPhoto;
  end;
  
  TCallTableDictionary = class
  private
    FID: TDictionaryAttribute;
    FDate: TDictionaryAttribute;
    FQueueEntryTime: TDictionaryAttribute;
    FQueueExitTime: TDictionaryAttribute;
    FServiceStartTime: TDictionaryAttribute;
    FServiceEndTime: TDictionaryAttribute;
    FAnswered: TDictionaryAttribute;
    FResolved: TDictionaryAttribute;
    FSatisfactionScore: TDictionaryAttribute;
    FCallID: TDictionaryAttribute;
    FWeek: TDictionaryAttribute;
    FAgentID: TDictionaryAssociation;
    FDepartmentID: TDictionaryAssociation;
  public
    constructor Create;
    property ID: TDictionaryAttribute read FID;
    property Date: TDictionaryAttribute read FDate;
    property QueueEntryTime: TDictionaryAttribute read FQueueEntryTime;
    property QueueExitTime: TDictionaryAttribute read FQueueExitTime;
    property ServiceStartTime: TDictionaryAttribute read FServiceStartTime;
    property ServiceEndTime: TDictionaryAttribute read FServiceEndTime;
    property Answered: TDictionaryAttribute read FAnswered;
    property Resolved: TDictionaryAttribute read FResolved;
    property SatisfactionScore: TDictionaryAttribute read FSatisfactionScore;
    property CallID: TDictionaryAttribute read FCallID;
    property Week: TDictionaryAttribute read FWeek;
    property AgentID: TDictionaryAssociation read FAgentID;
    property DepartmentID: TDictionaryAssociation read FDepartmentID;
  end;
  
function entitiesDic: TentitiesDicDictionary;

implementation

var
  __entitiesDic: TentitiesDicDictionary;

function entitiesDic: TentitiesDicDictionary;
begin
  if __entitiesDic = nil then __entitiesDic := TentitiesDicDictionary.Create;
  result := __entitiesDic
end;

{ TentitiesDicDictionary }

destructor TentitiesDicDictionary.Destroy;
begin
  if FCall <> nil then FCall.Free;
  if FAgent <> nil then FAgent.Free;
  if FDepartment <> nil then FDepartment.Free;
  inherited;
end;

function TentitiesDicDictionary.GetDepartment: TDepartmentTableDictionary;
begin
  if FDepartment = nil then FDepartment := TDepartmentTableDictionary.Create;
  result := FDepartment;
end;

function TentitiesDicDictionary.GetAgent: TAgentTableDictionary;
begin
  if FAgent = nil then FAgent := TAgentTableDictionary.Create;
  result := FAgent;
end;

function TentitiesDicDictionary.GetCall: TCallTableDictionary;
begin
  if FCall = nil then FCall := TCallTableDictionary.Create;
  result := FCall;
end;

{ TDepartmentTableDictionary }

constructor TDepartmentTableDictionary.Create;
begin
  inherited;
  FID := TDictionaryAttribute.Create('ID');
  FDescription := TDictionaryAttribute.Create('Description');
end;

{ TAgentTableDictionary }

constructor TAgentTableDictionary.Create;
begin
  inherited;
  FID := TDictionaryAttribute.Create('ID');
  FDescription := TDictionaryAttribute.Create('Description');
  FPhoto := TDictionaryAttribute.Create('Photo');
end;

{ TCallTableDictionary }

constructor TCallTableDictionary.Create;
begin
  inherited;
  FID := TDictionaryAttribute.Create('ID');
  FDate := TDictionaryAttribute.Create('Date');
  FQueueEntryTime := TDictionaryAttribute.Create('QueueEntryTime');
  FQueueExitTime := TDictionaryAttribute.Create('QueueExitTime');
  FServiceStartTime := TDictionaryAttribute.Create('ServiceStartTime');
  FServiceEndTime := TDictionaryAttribute.Create('ServiceEndTime');
  FAnswered := TDictionaryAttribute.Create('Answered');
  FResolved := TDictionaryAttribute.Create('Resolved');
  FSatisfactionScore := TDictionaryAttribute.Create('SatisfactionScore');
  FCallID := TDictionaryAttribute.Create('CallID');
  FWeek := TDictionaryAttribute.Create('Week');
  FAgentID := TDictionaryAssociation.Create('AgentID');
  FDepartmentID := TDictionaryAssociation.Create('DepartmentID');
end;

initialization

finalization
  if __entitiesDic <> nil then __entitiesDic.Free

end.
