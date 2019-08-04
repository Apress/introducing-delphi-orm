unit Database.Utilities;

interface

uses
  Aurelius.Engine.ObjectManager, FMX.Graphics, Aurelius.Types.Blob;

type
  TDatabaseUtilities<T: class> = class
    class procedure edit(const aObjManager: TObjectManager; const aEntity: T);
    class procedure bitmapToBlob (const aBmp: TBitmap; const aType: string;
                                                            var aBlob: TBlob);
    class procedure blobToBitmap (const aBlob: TBlob; var aBmp: TBitmap);
  end;
  
implementation

uses
  Aurelius.Mapping.Attributes, FMX.Surfaces, System.Classes, System.SysUtils;

{ TDatabaseUtilities<T> }

class procedure TDatabaseUtilities<T>.bitmapToBlob(const aBmp: TBitmap;
  const aType: string; var aBlob: TBlob);
var
  bmp: TBitmapSurface;
  bs: TBytesStream;
begin
  bmp := TBitmapSurface.create;
  try
    bmp.assign(aBmp);
    bs := TBytesStream.create;
    try
      TBitmapCodecManager.SaveToStream(bs, bmp, aType);
      aBlob.AsBytes := bs.Bytes;
    finally
      bs.free;
    end;
  finally
    bmp.free;
  end;

end;

class procedure TDatabaseUtilities<T>.blobToBitmap(const aBlob: TBlob;
  var aBmp: TBitmap);
var
  ms: TMemoryStream;
begin
  Assert(aBmp <> nil);
  ms := TMemoryStream.create;
  try
    aBlob.SaveToStream(ms);
    MS.Position := 0;
    aBmp.LoadFromStream(ms);
  finally
    ms.free;
  end;
end;

class procedure TDatabaseUtilities<T>.edit(const aObjManager: TObjectManager;
          const aEntity: T);
begin
  Assert(aObjManager <> nil);
  Assert(aEntity <> nil);

  try
    aObjManager.SaveOrUpdate(aEntity);
    aObjManager.Flush;
  except
    if not aObjManager.IsAttached(aEntity) then
        aEntity.Free;
  end;
end;

end.
