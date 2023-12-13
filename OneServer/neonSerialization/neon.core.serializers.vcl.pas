unit Neon.Core.Serializers.VCL;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Rtti, TypInfo, Generics.Collections, Neon.Core.Types,
  Neon.Core.Attributes, Neon.Core.Persistence, fpjson;

type
  TImageSerializer = class(TCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
      AContext: ISerializerContext): TJSONData; override;
    function Deserialize(AValue: TJSONData; const AData: TValue;
      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

implementation

uses
  ExtCtrls,
  Neon.Core.Utils;

{ TImageSerializer }

class function TImageSerializer.CanHandle(AType: PTypeInfo): boolean;
begin
  Result := TypeInfoIs(AType);
end;

class function TImageSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TImage.ClassInfo;
end;

function TImageSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONData;
var
  LImage: TImage;
  LStream: TMemoryStream;
  LBase64: string;
begin
  LImage := AValue.AsObject as TImage;

  if LImage.Picture = nil then
  begin
    case ANeonObject.NeonInclude.Value of
      IncludeIf.NotEmpty, IncludeIf.NotDefault: Exit(nil);
      else
        Exit(TJSONString.Create(''));
    end;
  end;

  LStream := TMemoryStream.Create;
  try
    LImage.Picture.SaveToStream(LStream);
    LStream.Position := soFromBeginning;
    LBase64 := TBase64.Encode(LStream);
  finally
    LStream.Free;
  end;
  Result := TJSONString.Create(LBase64);
end;

function TImageSerializer.Deserialize(AValue: TJSONData; const AData: TValue;
  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LImage: TImage;
  LStream: TMemoryStream;
begin
  Result := AData;
  LImage := AData.AsObject as TImage;

  if AValue.Value.IsEmpty then
  begin
    LImage.Picture := nil;
    Exit(AData);
  end;

  LStream := TMemoryStream.Create;
  try
    TBase64.Decode(AValue.Value, LStream);
    LStream.Position := soFromBeginning;
    LImage.Picture.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

end.
