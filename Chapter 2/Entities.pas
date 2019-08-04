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

    [Association([], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('User', [], 'ID')]
    FUser: TUser;
  public
    property ID: TGuid read FID write FID;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property Content: Nullable<string> read FContent write FContent;
    property Image: TBlob read FImage write FImage;
    property User: TUser read FUser write FUser;
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

    [ManyValuedAssociation([],
      [TCascadeType.SaveUpdate, TCascadeType.Merge, TCascadeType.Remove], 'FUser')]
    FPosts: TList<TPost>;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: TGuid read FID write FID;
    property Name: string read FName write FName;
    property Posts: TList<TPost> read FPosts;
  end;


implementation

{ TUser }

constructor TUser.Create;
begin
  inherited;
  FPosts := TList<TPost>.Create;
end;

destructor TUser.Destroy;
begin
  FPosts.Free;
  inherited;
end;

initialization
  RegisterEntity(TPost);
  RegisterEntity(TUser);

finalization

end.
