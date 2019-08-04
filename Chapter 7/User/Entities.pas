unit Entities;

interface

uses
  SysUtils, 
  Generics.Collections, 
  Aurelius.Mapping.Attributes,
  Aurelius.Types.DynamicProperties;

type
  TAddress = class;
  TCompany = class;
  TGeolocation = class;
  TUsers = class;
  
  [Entity]
  [Table('Address')]
  [Id('Fid', TIdGenerator.None)]
  TAddress = class
  private
    [Column('id', [TColumnProp.Required])]
    Fid: Integer;
    
    [Column('street', [], 255)]
    Fstreet: string;
    
    [Column('suite', [], 255)]
    Fsuite: string;
    
    [Column('city', [], 255)]
    Fcity: string;
    
    [Column('zipcode', [], 255)]
    Fzipcode: string;
    
    [Association([], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('geo', [], 'id')]
    Fgeo: TGeolocation;
  public
    property id: Integer read Fid write Fid;
    property street: string read Fstreet write Fstreet;
    property suite: string read Fsuite write Fsuite;
    property city: string read Fcity write Fcity;
    property zipcode: string read Fzipcode write Fzipcode;
    property geo: TGeolocation read Fgeo write Fgeo;
  end;
  
  [Entity]
  [Table('Company')]
  [Id('Fid', TIdGenerator.None)]
  TCompany = class
  private
    [Column('id', [TColumnProp.Required])]
    Fid: Integer;
    
    [Column('name', [], 255)]
    Fname: string;
    
    [Column('catchPhrase', [], 255)]
    FcatchPhrase: string;
    
    [Column('bs', [], 255)]
    Fbs: string;
  public
    property id: Integer read Fid write Fid;
    property name: string read Fname write Fname;
    property catchPhrase: string read FcatchPhrase write FcatchPhrase;
    property bs: string read Fbs write Fbs;
  end;
  
  [Entity]
  [Table('Geolocation')]
  [Id('Fid', TIdGenerator.None)]
  TGeolocation = class
  private
    [Column('id', [TColumnProp.Required])]
    Fid: Integer;

    [Column('lat', [], 255)]
    Flat: string;
    
    [Column('lng', [], 255)]
    Flng: string;
  public
    property id: Integer read Fid write Fid;
    property lat: string read Flat write Flat;
    property lng: string read Flng write Flng;
  end;
  
  [Entity]
  [Table('Users')]
  [Id('Fid', TIdGenerator.None)]
  TUsers = class
  private
    [Column('id', [TColumnProp.Required])]
    Fid: Integer;
    
    [Column('name', [], 255)]
    Fname: string;
    
    [Column('username', [], 255)]
    Fusername: string;
    
    [Column('email', [], 255)]
    Femail: string;
    
    [Column('phone', [], 255)]
    Fphone: string;
    
    [Column('website', [], 255)]
    Fwebsite: string;
    
    [Association([], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('company', [], 'id')]
    Fcompany: TCompany;
    
    [Association([], CascadeTypeAll - [TCascadeType.Remove])]
    [JoinColumn('address', [], 'id')]
    Faddress: TAddress;
  public
    property id: Integer read Fid write Fid;
    property name: string read Fname write Fname;
    property username: string read Fusername write Fusername;
    property email: string read Femail write Femail;
    property phone: string read Fphone write Fphone;
    property website: string read Fwebsite write Fwebsite;
    property company: TCompany read Fcompany write Fcompany;
    property address: TAddress read Faddress write Faddress;
  end;
  

implementation

initialization
  RegisterEntity(TGeolocation);
  RegisterEntity(TAddress);
  RegisterEntity(TUsers);
  RegisterEntity(TCompany);

finalization

end.
