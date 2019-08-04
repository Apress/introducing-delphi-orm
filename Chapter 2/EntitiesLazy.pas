unit EntitiesLazy;

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
  TPost = class;
  TUser = class;

  [Entity]
  [Table('Post')]
  [Id('FID', TIdGenerator.Guid)]
  TPost = class
  private
    [Column('ID', [TColumnProp.Required])]
    FID: TGuid;

    [Column('DateTime', [TColumnProp.Required])]
    FDateTime: TDateTime;

    [Column('Content', [], 50)]
    FContent: Nullable<string>;

    [Column('Image', [TColumnProp.Lazy])]
    FImage: TBlob;

    [Association([TAssociationProp.Lazy], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('User', [], 'ID')]
    FUser: Proxy<TUser>;
    function GetUser: TUser;
    procedure SetUser(const Value: TUser);
  public
    property ID: TGuid read FID write FID;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property Content: Nullable<string> read FContent write FContent;
    property Image: TBlob read FImage write FImage;
    property User: TUser read GetUser write SetUser;
  end;

  [Entity]
  [Table('User')]
  [Id('FID', TIdGenerator.Guid)]
  TUser = class
  private
    [Column('ID', [TColumnProp.Required])]
    FID: TGuid;

    [Column('Name', [TColumnProp.Required], 50)]
    FName: string;

    [ManyValuedAssociation([TAssociationProp.Lazy], [TCascadeType.SaveUpdate, TCascadeType.Merge], 'FUser')]
    FPosts: Proxy<TList<TPost>>;
    function GetPosts: TList<TPost>;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: TGuid read FID write FID;
    property Name: string read FName write FName;
    property Posts: TList<TPost> read GetPosts;
  end;


implementation

{ TPost }

function TPost.GetUser: TUser;
begin
  result := FUser.Value;
end;

procedure TPost.SetUser(const Value: TUser);
begin
  FUser.Value := Value;
end;

{ TUser }

constructor TUser.Create;
begin
  inherited;
  FPosts.SetInitialValue(TList<TPost>.Create);
end;

destructor TUser.Destroy;
begin
  FPosts.DestroyValue;
  inherited;
end;

function TUser.GetPosts: TList<TPost>;
begin
  result := FPosts.Value;
end;

initialization
  RegisterEntity(TPost);
  RegisterEntity(TUser);

finalization

end.
