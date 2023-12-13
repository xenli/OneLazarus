//unit Neon.Core.Serializers.RTL;
//
//{$mode ObjFPC}{$H+}
//
//interface
//
//
//uses
//  SysUtils, Classes, Rtti, SyncObjs, TypInfo, Generics.Collections,
//  Neon.Core.Types, Neon.Core.Attributes, Neon.Core.Persistence, fpjson;
//
//type
//  TGUIDSerializer = class(TCustomSerializer)
//  protected
//    class function GetTargetInfo: PTypeInfo; override;
//    class function CanHandle(AType: PTypeInfo): boolean; override;
//  public
//    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
//      AContext: ISerializerContext): TJSONData; override;
//    function Deserialize(AValue: TJSONData; const AData: TValue;
//      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
//  end;
//
//  TStreamSerializer = class(TCustomSerializer)
//  protected
//    class function GetTargetInfo: PTypeInfo; override;
//    class function CanHandle(AType: PTypeInfo): boolean; override;
//  public
//    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
//      AContext: ISerializerContext): TJSONData; override;
//    function Deserialize(AValue: TJSONData; const AData: TValue;
//      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
//  end;
//
//  TJSONValueSerializer = class(TCustomSerializer)
//  protected
//    class function GetTargetInfo: PTypeInfo; override;
//    class function CanHandle(AType: PTypeInfo): boolean; override;
//  public
//    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
//      AContext: ISerializerContext): TJSONData; override;
//    function Deserialize(AValue: TJSONData; const AData: TValue;
//      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
//  end;
//
//  TTValueSerializer = class(TCustomSerializer)
//  protected
//    class function GetTargetInfo: PTypeInfo; override;
//    class function CanHandle(AType: PTypeInfo): boolean; override;
//  public
//    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject;
//      AContext: ISerializerContext): TJSONData; override;
//    function Deserialize(AValue: TJSONData; const AData: TValue;
//      ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
//  end;
//
//procedure RegisterDefaultSerializers(ARegistry: TNeonSerializerRegistry);
//
//implementation
//
//uses
//  Neon.Core.Utils;
//
//procedure RegisterDefaultSerializers(ARegistry: TNeonSerializerRegistry);
//begin
//  ARegistry.RegisterSerializer(TGUIDSerializer);
//  ARegistry.RegisterSerializer(TStreamSerializer);
//end;
//
//{ TGUIDSerializer }
//
//class function TGUIDSerializer.GetTargetInfo: PTypeInfo;
//begin
//  Result := TypeInfo(TGUID);
//end;
//
//function TGUIDSerializer.Serialize(const AValue: TValue;
//  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONData;
//var
//  LGUID: TGUID;
//begin
//  AValue.AsString;
//  LGUID := TGUID(AValue.GetReferenceToRawData^);
//  Result := TJSONString.Create(Format('%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
//    [LGUID.D1, LGUID.D2, LGUID.D3, LGUID.D4[0], LGUID.D4[1], LGUID.D4[2],
//    LGUID.D4[3], LGUID.D4[4], LGUID.D4[5], LGUID.D4[6], LGUID.D4[7]]));
//end;
//
//class function TGUIDSerializer.CanHandle(AType: PTypeInfo): boolean;
//begin
//  if AType = GetTargetInfo then
//    Result := True
//  else
//    Result := False;
//end;
//
//function TGUIDSerializer.Deserialize(AValue: TJSONData; const AData: TValue;
//  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
//var
//  LGUID: TGUID;
//begin
//  LGUID := StringToGUID(Format('{%s}', [AValue.Value]));
//  Result := TValue.From<TGUID>(LGUID);
//end;
//
//{ TStreamSerializer }
//
//class function TStreamSerializer.GetTargetInfo: PTypeInfo;
//begin
//  Result := TStream.ClassInfo;
//end;
//
//class function TStreamSerializer.CanHandle(AType: PTypeInfo): boolean;
//begin
//  Result := TypeInfoIs(AType);
//end;
//
//function TStreamSerializer.Serialize(const AValue: TValue;
//  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONData;
//var
//  LStream: TStream;
//  LBase64: string;
//begin
//  LStream := AValue.AsObject as TStream;
//
//  if LStream.Size = 0 then
//  begin
//    case ANeonObject.NeonInclude.Value of
//      IncludeIf.NotEmpty, IncludeIf.NotDefault: Exit(nil);
//      else
//        Exit(TJSONString.Create(''));
//    end;
//  end;
//
//  LStream.Position := soFromBeginning;
//  LBase64 := TBase64.Encode(LStream);
//  Result := TJSONString.Create(LBase64);
//end;
//
//function TStreamSerializer.Deserialize(AValue: TJSONData; const AData: TValue;
//  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
//var
//  LStream: TStream;
//begin
//  Result := AData;
//  LStream := AData.AsObject as TStream;
//  LStream.Position := soFromBeginning;
//
//  TBase64.Decode(AValue.Value, LStream);
//end;
//
//{ TJSONValueSerializer }
//
//class function TJSONValueSerializer.CanHandle(AType: PTypeInfo): boolean;
//begin
//  Result := TypeInfoIs(AType);
//end;
//
//function TJSONValueSerializer.Deserialize(AValue: TJSONData;
//  const AData: TValue; ANeonObject: TNeonRttiObject;
//  AContext: IDeserializerContext): TValue;
//var
//  LJSONData: TJSONData;
//  LPair: TJSONPair;
//  LValue: TJSONData;
//begin
//  Result := AData;
//  LJSONData := Result.AsObject as TJSONData;
//
//  // Check the TypeInfo of AData as TJSONData and AValue
//  if not (LJSONData.ClassType = AValue.ClassType) then
//  begin
//    AContext.LogError(Format('TJSONValueSerializer: %s and %s not compatible',
//      [LJSONData.ClassName, AValue.ClassName]));
//    Exit;
//  end;
//
//  if LJSONData is TJSONObject then
//    for LPair in (AValue as TJSONObject) do
//      (LJSONData as TJSONObject).AddPair(LPair.Clone as TJSONPair)
//
//  else if LJSONData is TJSONArray then
//    for LValue in (AValue as TJSONArray) do
//      (LJSONData as TJSONArray).AddElement(LValue.Clone as TJSONData);
//
//  {
//  else if LJSONData is TJSONString then
//    (LJSONData as TJSONString). Value := (AValue as TJSONString).Value
//
//  else if LJSONData is TJSONNumber then
//    (LJSONData as TJSONNumber).Value := (AValue as TJSONNumber).Value
//
//  else if LJSONData is TJSONBool then
//    (LJSONData as TJSONString).Value := (AValue as TJSONString).Value
//
//  else if LJSONData is TJSONNull then
//    (LJSONData as TJSONString).Value := (AValue as TJSONString).Value
//  }
//end;
//
//class function TJSONValueSerializer.GetTargetInfo: PTypeInfo;
//begin
//  Result := TJSONData.ClassInfo;
//end;
//
//function TJSONValueSerializer.Serialize(const AValue: TValue;
//  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONData;
//var
//  LOriginalJSON: TJSONData;
//  LEmpty: boolean;
//begin
//  LEmpty := False;
//
//  LOriginalJSON := AValue.AsObject as TJSONData;
//
//  if LOriginalJSON is TJSONObject then
//    LEmpty := (LOriginalJSON as TJSONObject).Count = 0;
//
//  if LOriginalJSON is TJSONArray then
//    LEmpty := (LOriginalJSON as TJSONArray).Count = 0;
//
//  if LEmpty then
//    case ANeonObject.NeonInclude.Value of
//      IncludeIf.NotNull: Exit(nil);
//      IncludeIf.NotEmpty: Exit(nil);
//      IncludeIf.NotDefault: Exit(nil);
//    end;
//
//  Exit(LOriginalJSON.Clone as TJSONData);
//end;
//
//{ TTValueSerializer }
//
//class function TTValueSerializer.CanHandle(AType: PTypeInfo): boolean;
//begin
//  Result := AType = GetTargetInfo;
//end;
//
//function TTValueSerializer.Deserialize(AValue: TJSONData; const AData: TValue;
//  ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
//var
//  LType: TRttiType;
//  LValue: TValue;
//begin
//  LValue := TValue.Empty;
//
//  if AValue is TJSONNumber then
//  begin
//    LType := TRttiUtils.Context.GetType(TypeInfo(double));
//    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
//  end
//  else if AValue is TJSONString then
//  begin
//    LType := TRttiUtils.Context.GetType(TypeInfo(string));
//    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
//  end
//  else if AValue is TJSONBool then
//  begin
//    LType := TRttiUtils.Context.GetType(TypeInfo(boolean));
//    LValue := AContext.ReadDataMember(AValue, LType, AData, False);
//  end;
//
//  Result := LValue;
//end;
//
//class function TTValueSerializer.GetTargetInfo: PTypeInfo;
//begin
//  Result := TypeInfo(TValue);
//end;
//
//function TTValueSerializer.Serialize(const AValue: TValue;
//  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONData;
//begin
//  if AValue.Kind = tkRecord then
//    Result := AContext.WriteDataMember(AValue.AsType<TValue>, False)
//  else
//    Result := nil;
//end;
//
//end.
