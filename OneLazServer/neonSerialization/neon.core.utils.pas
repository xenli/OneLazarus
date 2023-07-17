unit Neon.Core.Utils;

{$IFDEF FPC}
{$MODE Delphi}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$ENDIF}{$H+}


interface


uses
  Classes, SysUtils, DB, Rtti, TypInfo, Generics.Collections, fpjson, jsonparser, base64;

type
  TProc<T> = procedure(Arg: T);
  TFunc<TResult> = function: TResult;
  TFunc<T, TResult> = function(Arg1: T): TResult;
  TFunc<T1, T2, TResult> = function(Arg1: T1; Arg2: T2): TResult;

  TRttiContextHelper = record helper for TRttiContext
  public
    function FindType(const AQualifiedName: string): TRttiType;
  end;

  TJSONUtils = class
  public
    class procedure Decode(const ASource: string; ADest: TStream); overload;
    class function Encode(const ASource: TStream): string; overload;

    class function ToJSON(AJSONValue: TJSONData): string; static;

    class function StringArrayToJsonArray(const AValues: TArray<string>): string; static;
    class function DoubleArrayToJsonArray(const AValues: TArray<Double>): string; static;
    class function IntegerArrayToJsonArray(const AValues: TArray<Integer>): string; static;

    class procedure JSONCopyFrom(ASource, ADestination: TJSONObject); static;

    class function BooleanToTJSON(AValue: Boolean): TJSONData;
    class function DateToJSON(ADate: TDateTime; AInputIsUTC: Boolean = True): string; static;
    class function DateToJSONValue(ADate: TDateTime; AInputIsUTC: Boolean = True): TJSONData; static;
    class function JSONToDate(const ADate: string; AReturnUTC: Boolean = True): TDateTime; static;
  end;

  TRttiUtils = class
  private
    class var FContext: TRttiContext;
  public
    class function FindAttribute<T: TCustomAttribute>(AType: TRttiObject): T; static;

    class function HasAttribute<T: TCustomAttribute>(AClass: TClass): Boolean; overload; static;

    class function HasAttribute<T: TCustomAttribute>(ARttiObj: TRttiObject): Boolean; overload; static;

    class function HasAttribute<T: TCustomAttribute>(ARttiObj: TRttiObject; ADoSomething: TProc<T>): Boolean; overload; static;

    class function ForEachAttribute<T: TCustomAttribute>(
      ARttiObj: TRttiObject; const ADoSomething: TProc<T>): Integer; overload; static;

    // TRttiType helpers functions
    class function ForEachMethodWithAttribute<T: TCustomAttribute>(
      ARttiType: TRttiType; const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer; static;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(
      ARttiType: TRttiType; const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer; overload; static;

    class function ForEachPropertyWithAttribute<T: TCustomAttribute>(
      ARttiType: TRttiType; const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer; overload; static;

    class function IsDynamicArrayOf<T: class>(ARttiType: TRttiType;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsDynamicArrayOf(ARttiType: TRttiType; const AClass: TClass;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsObjectOfType<T: class>(ARttiType: TRttiType;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    class function IsObjectOfType(ARttiType: TRttiType; const AClass: TClass;
      const AAllowInherithance: Boolean = True): Boolean; overload; static;

    // Create new value data
    class function CreateNewValue(AType: TRttiType): TValue; static;

    // Create instance of class with parameterless constructor
    class function CreateInstanceValue(AType: TRttiType): TValue; overload;

    // Create instance of class with parameterless constructor
    class function CreateInstance<T: class, constructor>: TObject; overload;
    class function CreateInstance(AClass: TClass): TObject; overload;
    class function CreateInstance(AType: TRttiType): TObject; overload;
    class function CreateInstance(const ATypeName: string): TObject; overload;

    // Create instance of class with one string parameter
    class function CreateInstance(AClass: TClass; const AValue: string): TObject; overload;
    class function CreateInstance(AType: TRttiType; const AValue: string): TObject; overload;
    class function CreateInstance(const ATypeName, AValue: string): TObject; overload;

    // Create instance of class with an array of TValue
    class function CreateInstance(AClass: TClass; const Args: array of TValue): TObject; overload;
    class function CreateInstance(AType: TRttiType; const Args: array of TValue): TObject; overload;
    class function CreateInstance(const ATypeName: string; const Args: array of TValue): TObject; overload;

    // Rtti general helper functions
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject): Boolean; overload;
    class function IfHasAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Boolean; overload;

    class function ForEachAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TProc<T>): Integer; overload;

    class function ForEachFieldWithAttribute<T: TCustomAttribute>(AInstance: TObject; const ADoSomething: TFunc<TRttiProperty, T, Boolean>)
      : Integer; overload;
    class function ForEachField(AInstance: TObject; const ADoSomething: TFunc<TRttiProperty, Boolean>): Integer;

    class function GetType(AObject: TRttiObject): TRttiType;
    class function GetSetElementType(ASetType: TRttiType): TRttiType;

    class function ClassDistanceFromRoot(AClass: TClass): Integer; overload; static;
    class function ClassDistanceFromRoot(AInfo: PTypeInfo): Integer; overload; static;

    class property Context: TRttiContext read FContext;
  end;

  TBase64 = class
    class function Encode(const ASource: TBytes): string; overload;
    class function Encode(const ASource: TStream): string; overload;

    class function Decode(const ASource: string): TBytes; overload;
    class procedure Decode(const ASource: string; ADest: TStream); overload;
  end;

  TDataSetUtils = class
  public
    class function RecordToJSONSchema(const ADataSet: TDataSet; AUseUTCDate: Boolean): TJSONObject; static;

    class function FieldToJSONValue(const AField: TField; AUseUTCDate: Boolean): TJSONData; static;
    class function RecordToJSONObject(const ADataSet: TDataSet; AUseUTCDate: Boolean): TJSONObject; static;
    class function DataSetToJSONArray(const ADataSet: TDataSet; AUseUTCDate: Boolean): TJSONArray; overload; static;
    class function DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>; AUseUTCDate: Boolean): TJSONArray;
      overload; static;

    class procedure JSONToRecord(AJSONObject: TJSONObject; ADataSet: TDataSet; AUseUTCDate: Boolean); static;
    class procedure JSONToDataSet(AJSONValue: TJSONData; ADataSet: TDataSet; AUseUTCDate: Boolean); static;
    class procedure JSONObjectToDataSet(AJSONValue: TJSONData; ADataSet: TDataSet; AUseUTCDate: Boolean); static;

    class function BlobFieldToBase64(ABlobField: TBlobField): string;
    class procedure Base64ToBlobField(const ABase64: string; ABlobField: TBlobField);
  end;

implementation

uses
  StrUtils, DateUtils, Neon.Core.Types;

function TRttiContextHelper.FindType(const AQualifiedName: string): TRttiType;
var
  ctx: TRttiContext;
  lType: TRttiType;
begin
  result := nil;
  // ctx := TRttiContext.Create;
  // try
  // for lType in ctx.GetTypes do
  // begin
  //
  // end;
  // finally
  // end;

end;

class function TRttiUtils.ClassDistanceFromRoot(AClass: TClass): Integer;
var
  LClass: TClass;
begin
  result := 0;
  LClass := AClass;
  while LClass <> TObject do
  begin
    LClass := LClass.ClassParent;
    Inc(result);
  end;
end;

class function TRttiUtils.ClassDistanceFromRoot(AInfo: PTypeInfo): Integer;
var
  lType: TRttiType;
begin
  result := -1;

  lType := TRttiUtils.Context.GetType(AInfo);
  if Assigned(lType) and (lType.TypeKind = tkClass) then
    result := TRttiUtils.ClassDistanceFromRoot(lType.AsInstance.MetaclassType);
end;

{ TRttiUtils }

class function TRttiUtils.CreateNewValue(AType: TRttiType): TValue;
var
  LAllocatedMem: Pointer;
begin
  case AType.TypeKind of
    tkEnumeration:
      result := TValue.From<Byte>(0);
    tkInteger:
      result := TValue.From<Integer>(0);
    tkInt64:
      result := TValue.From<Int64>(0);
    tkChar:
      result := TValue.From<UTF8Char>(#0);
    tkWChar:
      result := TValue.From<Char>(#0);
    tkFloat:
      result := TValue.From<Double>(0);
    tkString:
      result := TValue.From<UTF8String>('');
    tkWString:
      result := TValue.From<string>('');
    tkLString:
      result := TValue.From<UTF8String>('');
    tkUString:
      result := TValue.From<string>('');
    tkClass:
      result := CreateInstance(AType);
    tkRecord, tkDynArray:
      begin
        LAllocatedMem := AllocMem(AType.TypeSize);
        try
          TValue.Make(LAllocatedMem, AType.Handle, result);
        finally
          FreeMem(LAllocatedMem);
        end;
      end;
  else
    raise Exception.CreateFmt('Error creating type [%s]', [AType.Name]);
  end;
end;

class function TRttiUtils.CreateInstance(AClass: TClass): TObject;
var
  lType: TRttiType;
begin
  lType := FContext.GetType(AClass);
  result := CreateInstanceValue(lType).AsObject;
end;

class function TRttiUtils.CreateInstance(AType: TRttiType): TObject;
begin
  result := CreateInstanceValue(AType).AsObject;
end;

class function TRttiUtils.CreateInstance(const ATypeName: string): TObject;
var
  lType: TRttiType;
begin
  lType := Context.FindType(ATypeName);
  result := CreateInstanceValue(lType).AsObject;
end;

class function TRttiUtils.CreateInstance(AClass: TClass; const AValue: string): TObject;
var
  lType: TRttiType;
begin
  lType := FContext.GetType(AClass);
  result := CreateInstance(lType, AValue);
end;

class function TRttiUtils.CreateInstance(AType: TRttiType; const AValue: string): TObject;
var
  LMethod: TRttiMethod;
  LMetaClass: TClass;
begin
  result := nil;
  if Assigned(AType) then
  begin
    for LMethod in AType.GetMethods do
    begin
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      begin
        if Length(LMethod.GetParameters) = 1 then
        begin
          if LMethod.GetParameters[0].ParamType.TypeKind in [tkLString, tkUString, tkWString, tkString] then
          begin
            LMetaClass := AType.AsInstance.MetaclassType;
            Exit(LMethod.Invoke(LMetaClass, [AValue]).AsObject);
          end;
        end;
      end;
    end;
  end;
end;

class function TRttiUtils.CreateInstance(const ATypeName, AValue: string): TObject;
var
  lType: TRttiType;
begin
  lType := Context.FindType(ATypeName);
  result := CreateInstance(lType, AValue);
end;

class function TRttiUtils.CreateInstanceValue(AType: TRttiType): TValue;
var
  LMethod: TRttiMethod;
  LMetaClass: TClass;
begin
  result := nil;
  if Assigned(AType) then
    for LMethod in AType.GetMethods do
    begin
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      begin
        if Length(LMethod.GetParameters) = 0 then
        begin
          LMetaClass := AType.AsInstance.MetaclassType;
          Exit(LMethod.Invoke(LMetaClass, []));
        end;
      end;
    end;
end;

class function TRttiUtils.ForEachAttribute<T>(AInstance: TObject;
  const ADoSomething: TProc<T>): Integer;
var
  LContext: TRttiContext;
  lType: TRttiType;
begin
  result := 0;
  lType := LContext.GetType(AInstance.ClassType);
  if Assigned(lType) then
    result := TRttiUtils.ForEachAttribute<T>(lType, ADoSomething);
end;

class function TRttiUtils.ForEachField(AInstance: TObject;
  const ADoSomething: TFunc<TRttiProperty, Boolean>): Integer;
var
  LContext: TRttiContext;
  LField: TRttiProperty;
  lType: TRttiType;
  LBreak: Boolean;
begin
  result := 0;
  lType := LContext.GetType(AInstance.ClassType);
  for LField in lType.GetProperties do
  begin
    LBreak := False;

    if Assigned(ADoSomething) then
    begin
      if not ADoSomething(LField) then
        LBreak := True
      else
        Inc(result);
    end;

    if LBreak then
      Break;
  end;
end;

class function TRttiUtils.ForEachFieldWithAttribute<T>(AInstance: TObject;
  const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;
var
  LContext: TRttiContext;
  lType: TRttiType;
begin
  result := 0;
  lType := LContext.GetType(AInstance.ClassType);
  if Assigned(lType) then
    result := TRttiUtils.ForEachFieldWithAttribute<T>(lType, ADoSomething);
end;

class function TRttiUtils.IfHasAttribute<T>(AInstance: TObject): Boolean;
begin
  result := TRttiUtils.IfHasAttribute<T>(AInstance, nil);
end;

class function TRttiUtils.IfHasAttribute<T>(AInstance: TObject; ADoSomething: TProc<T>): Boolean;
var
  LContext: TRttiContext;
  lType: TRttiType;
begin
  result := False;
  lType := LContext.GetType(AInstance.ClassType);
  if Assigned(lType) then
    result := TRttiUtils.HasAttribute<T>(lType, ADoSomething);
end;

class function TRttiUtils.ForEachAttribute<T>(ARttiObj: TRttiObject;
  const ADoSomething: TProc<T>): Integer;
var
  LAttribute: TCustomAttribute;
begin
  result := 0;
  for LAttribute in ARttiObj.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      if Assigned(ADoSomething) then
        ADoSomething(T(LAttribute));
      Inc(result);
    end;
  end;
end;

class function TRttiUtils.HasAttribute<T>(ARttiObj: TRttiObject): Boolean;
begin
  result := HasAttribute<T>(ARttiObj, nil);
end;

class function TRttiUtils.HasAttribute<T>(ARttiObj: TRttiObject; const ADoSomething: TProc<T>): Boolean;
var
  LAttribute: TCustomAttribute;
begin
  result := False;
  for LAttribute in ARttiObj.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      result := True;

      if Assigned(ADoSomething) then
        ADoSomething(T(LAttribute));

      Break;
    end;
  end;
end;

class function TRttiUtils.ForEachFieldWithAttribute<T>(ARttiType: TRttiType;
  const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;
var
  LField: TRttiProperty;
  LBreak: Boolean;
begin
  for LField in ARttiType.GetProperties do
  begin
    LBreak := False;
    if TRttiUtils.HasAttribute<T>(LField,
      @
      procedure(AAttrib: T)
      begin
        if Assigned(ADoSomething) then
        begin
          if not ADoSomething(LField, AAttrib) then
            LBreak := True;
        end;
      end)
    then
      Inc(result);

    if LBreak then
      Break;
  end;
end;

class function TRttiUtils.ForEachMethodWithAttribute<T>(ARttiType: TRttiType;
const ADoSomething: TFunc<TRttiMethod, T, Boolean>): Integer;
var
  LMethod: TRttiMethod;
  LBreak: Boolean;
begin
  result := 0;
  for LMethod in ARttiType.GetMethods do
  begin
    LBreak := False;
    if TRttiUtils.HasAttribute<T>(LMethod,
      @
      procedure(AAttrib: T)
      begin
        if Assigned(ADoSomething) then
        begin
          if not ADoSomething(LMethod, AAttrib) then
            LBreak := True;
        end;
      end
      )
    then
      Inc(result);

    if LBreak then
      Break;
  end;
end;

class function TRttiUtils.ForEachPropertyWithAttribute<T>(ARttiType: TRttiType;
const ADoSomething: TFunc<TRttiProperty, T, Boolean>): Integer;
var
  LProperty: TRttiProperty;
  LBreak: Boolean;
begin
  result := 0;
  for LProperty in ARttiType.GetProperties do
  begin
    LBreak := False;
    if TRttiUtils.HasAttribute<T>(LProperty,
      @
      procedure(AAttrib: T)
      begin
        if Assigned(ADoSomething) then
        begin
          if not ADoSomething(LProperty, AAttrib) then
            LBreak := True;
        end;
      end
      )
    then
      Inc(result);

    if LBreak then
      Break;
  end;
end;

class function TRttiUtils.GetSetElementType(ASetType: TRttiType): TRttiType;
var
  LEnumInfo: PTypeInfo;
begin
  LEnumInfo := GetTypeData(ASetType.Handle)^.CompType;
  result := TRttiUtils.Context.GetType(LEnumInfo);
end;

class function TRttiUtils.GetType(AObject: TRttiObject): TRttiType;
begin
  if AObject is TRttiParameter then
    result := TRttiParameter(AObject).ParamType
  else if AObject is TRttiProperty then
    result := TRttiProperty(AObject).PropertyType
  else if AObject is TRttiProperty then
    result := TRttiProperty(AObject).PropertyType
    // else if AObject is TRttiManagedField then
    // Result := TRttiManagedField(AObject).FieldType
  else
    raise Exception.Create('Object doesn''t have a type');
end;

class function TRttiUtils.HasAttribute<T>(AClass: TClass): Boolean;
begin
  result := HasAttribute<T>(Context.GetType(AClass));
end;

class function TRttiUtils.IsDynamicArrayOf(ARttiType: TRttiType;
const AClass: TClass; const AAllowInherithance: Boolean): Boolean;
begin
  result := False;
  if ARttiType is TRttiDynamicArrayType then
    result := TRttiUtils.IsObjectOfType(
      TRttiDynamicArrayType(ARttiType).ElementType, AClass, AAllowInherithance);
end;

class function TRttiUtils.IsDynamicArrayOf<T>(ARttiType: TRttiType;
const AAllowInherithance: Boolean): Boolean;
begin
  result := TRttiUtils.IsDynamicArrayOf(ARttiType, TClass(T), AAllowInherithance);
end;

class function TRttiUtils.IsObjectOfType(ARttiType: TRttiType;
const AClass: TClass; const AAllowInherithance: Boolean): Boolean;
begin
  result := False;
  if ARttiType is TRttiInstanceType then
  begin
    if AAllowInherithance then
      result := TRttiInstanceType(ARttiType).MetaclassType.InheritsFrom(AClass)
    else
      result := TRttiInstanceType(ARttiType).MetaclassType = AClass;
  end;
end;

class function TRttiUtils.IsObjectOfType<T>(ARttiType: TRttiType;
const AAllowInherithance: Boolean): Boolean;
begin
  result := TRttiUtils.IsObjectOfType(ARttiType, TClass(T), AAllowInherithance);
end;

class function TRttiUtils.FindAttribute<T>(AType: TRttiObject): T;
var
  LAttribute: TCustomAttribute;
begin
  result := nil;
  for LAttribute in AType.GetAttributes do
  begin
    if LAttribute.InheritsFrom(TClass(T)) then
    begin
      result := LAttribute as T;

      Break;
    end;
  end;
end;

class function TRttiUtils.CreateInstance(AClass: TClass;
const Args: array of TValue): TObject;
var
  lType: TRttiType;
begin
  lType := FContext.GetType(AClass);
  result := CreateInstance(lType, Args);
end;

class function TRttiUtils.CreateInstance(AType: TRttiType; const Args: array of TValue): TObject;
var
  LMethod: TRttiMethod;
  LMetaClass: TClass;
begin
  result := nil;
  if Assigned(AType) then
  begin
    for LMethod in AType.GetMethods do
    begin
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
      begin
        if Length(LMethod.GetParameters) = Length(Args) then
        begin
          LMetaClass := AType.AsInstance.MetaclassType;
          Exit(LMethod.Invoke(LMetaClass, Args).AsObject);
        end;
      end;
    end;
  end;
  if not Assigned(result) then
    raise Exception.CreateFmt('TRttiUtils.CreateInstance: can''t create object [%s]', [AType.Name]);
end;

class function TRttiUtils.CreateInstance(const ATypeName: string; const Args: array of TValue): TObject;
var
  lType: TRttiType;
begin
  lType := Context.FindType(ATypeName);
  result := CreateInstance(lType, Args);
end;

class function TRttiUtils.CreateInstance<T>: TObject;
begin
  result := CreateInstance(TRttiUtils.Context.GetType(TClass(T)));
end;

class function TJSONUtils.BooleanToTJSON(AValue: Boolean): TJSONData;
begin
  if AValue then
    result := TJSONBoolean.Create(True)
  else
    result := TJSONBoolean.Create(False);
end;

class function TJSONUtils.DateToJSON(ADate: TDateTime; AInputIsUTC: Boolean = True): string;
begin
  result := '';
  if ADate <> 0 then
    result := DateToISO8601(ADate, AInputIsUTC);
end;

class function TJSONUtils.DateToJSONValue(ADate: TDateTime; AInputIsUTC: Boolean): TJSONData;
begin
  result := TJSONString.Create(TJSONUtils.DateToJSON(ADate, AInputIsUTC));
end;

class procedure TJSONUtils.Decode(const ASource: string; ADest: TStream);
var
  LDecoder: TBase64DecodingStream;
  LBase64Stream: TStringStream;
begin
  LBase64Stream := TStringStream.Create(ASource);
  LBase64Stream.Position := soFromBeginning;
  try
    LDecoder := TBase64DecodingStream.Create(LBase64Stream, bdmStrict);
    try

      ADest.CopyFrom(LDecoder, LDecoder.Size);
      ADest.Position := 0;
    finally
      LDecoder.Free;
    end;
  finally
    LBase64Stream.Free;
  end;
end;

class function TJSONUtils.DoubleArrayToJsonArray(const AValues: TArray<Double>): string;
var
  LArray: TJSONArray;
  LIndex: Integer;
begin
  LArray := TJSONArray.Create;
  try
    for LIndex := 0 to High(AValues) do
      LArray.Add(AValues[LIndex]);
    result := ToJSON(LArray);
  finally
    LArray.Free;
  end;
end;

class function TJSONUtils.Encode(const ASource: TStream): string;
var
  OutputStream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  result := '';
  OutputStream := TStringStream.Create('');
  Encoder := TBase64EncodingStream.Create(OutputStream);
  try
    ASource.Position := 0;
    Encoder.CopyFrom(ASource, ASource.Size);
    Encoder.Flush;
    OutputStream.Position := 0;
    result := OutputStream.DataString;
  finally
    Encoder.Free;
    OutputStream.Free;
  end;
end;

class function TJSONUtils.IntegerArrayToJsonArray(const AValues: TArray<Integer>): string;
var
  LArray: TJSONArray;
  LIndex: Integer;
begin
  LArray := TJSONArray.Create;
  try
    for LIndex := 0 to High(AValues) do
      LArray.Add(AValues[LIndex]);
    result := ToJSON(LArray);
  finally
    LArray.Free;
  end;
end;

class function TJSONUtils.JSONToDate(const ADate: string; AReturnUTC: Boolean = True): TDateTime;
begin
  result := 0.0;
  if ADate <> '' then
  begin
    try
      result := ISO8601ToDate(ADate, AReturnUTC);
    except
      trystrToDateTime(ADate, result);
    end;
  end;
end;

class function TJSONUtils.ToJSON(AJSONValue: TJSONData): string;
var
  LBytes: TBytes;
begin
  result := AJSONValue.AsJSON;
end;

class function TJSONUtils.StringArrayToJsonArray(const AValues: TArray<string>): string;
var
  LArray: TJSONArray;
  LIndex: Integer;
begin
  LArray := TJSONArray.Create;
  try
    for LIndex := 0 to High(AValues) do
      LArray.Add(AValues[LIndex]);
    result := ToJSON(LArray);
  finally
    LArray.Free;
  end;
end;

class procedure TJSONUtils.JSONCopyFrom(ASource, ADestination: TJSONObject);
var
  LPair: TJSONData;
  i: Integer;
begin
  // for LPair in ASource.Items.Count do
  // begin
  // //ADestination.add(LPair.Name,  LPair.Clone);
  // end;

end;

class function TBase64.Encode(const ASource: TBytes): string;
var
  lStream: TMemoryStream;
  OutputStream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  // Result := TNetEncoding.Base64.EncodeBytesToString(QBytes);
  result := '';
  lStream := TMemoryStream.Create;
  try
    lStream.Write(ASource, Length(ASource));

    OutputStream := TStringStream.Create('');
    Encoder := TBase64EncodingStream.Create(OutputStream);
    try
      lStream.Position := 0;
      Encoder.CopyFrom(lStream, lStream.Size);
      Encoder.Flush;
      OutputStream.Position := 0;
      result := OutputStream.DataString;
    finally
      Encoder.Free;
      OutputStream.Free;
    end;
  finally
    lStream.Free;
  end;
end;

class function TBase64.Encode(const ASource: TStream): string;
var
  OutputStream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  result := '';
  OutputStream := TStringStream.Create('');
  Encoder := TBase64EncodingStream.Create(OutputStream);
  try
    ASource.Position := 0;
    Encoder.CopyFrom(ASource, ASource.Size);
    Encoder.Flush;
    OutputStream.Position := 0;
    result := OutputStream.DataString;
  finally
    Encoder.Free;
    OutputStream.Free;
  end;
end;

class function TBase64.Decode(const ASource: string): TBytes;
begin
  // Result := TNetEncoding.Base64.DecodeStringToBytes(ASource);
end;

class procedure TBase64.Decode(const ASource: string; ADest: TStream);
var
  LDecoder: TBase64DecodingStream;
  LBase64Stream: TStringStream;
begin
  LBase64Stream := TStringStream.Create(ASource);
  LBase64Stream.Position := soFromBeginning;
  try
    LDecoder := TBase64DecodingStream.Create(LBase64Stream, bdmStrict);
    try
      ADest.CopyFrom(LDecoder, LDecoder.Size);
      ADest.Position := 0;
    finally
      LDecoder.Free;
    end;
  finally
    LBase64Stream.Free;
  end;
end;

class function TDataSetUtils.RecordToJSONObject(const ADataSet: TDataSet; AUseUTCDate: Boolean): TJSONObject;
var
  LField: TField;
  LPairName: string;
  LJSONValue: TJSONData;
begin
  result := TJSONObject.Create;

  for LField in ADataSet.Fields do
  begin
    LPairName := LField.FieldName;

    if ContainsStr(LPairName, '.') then
      Continue;

    LJSONValue := FieldToJSONValue(LField, AUseUTCDate);
    if Assigned(LJSONValue) then
      result.Add(LPairName, LJSONValue);
  end;
end;

class function TDataSetUtils.RecordToJSONSchema(const ADataSet: TDataSet; AUseUTCDate: Boolean): TJSONObject;
var
  LField: TField;
  LPairName: string;
  LJSONField: TJSONObject;
begin
  result := TJSONObject.Create;

  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  for LField in ADataSet.Fields do
  begin
    LPairName := LField.FieldName;

    if LPairName.Contains('.') then
      Continue;

    LJSONField := TJSONObject.Create;
    result.Add(LPairName, LJSONField);

    case LField.DataType of
      TFieldType.ftString:
        begin
          LJSONField.Add('type', 'string');
        end;

      TFieldType.ftSmallint,
        TFieldType.ftInteger,
        TFieldType.ftWord,
        TFieldType.ftLongWord,
        TFieldType.ftShortint,
        TFieldType.ftByte:
        begin
          LJSONField.Add('type', 'integer');
          LJSONField.Add('format', 'int32');
        end;

      TFieldType.ftBoolean:
        begin
          LJSONField.Add('type', 'boolean');
        end;

      TFieldType.ftFloat,
        TFieldType.ftSingle:
        begin
          LJSONField.Add('type', 'number');
          LJSONField.Add('format', 'float');
        end;

      TFieldType.ftCurrency,
        TFieldType.ftExtended:
        begin
          LJSONField.Add('type', 'number');
          LJSONField.Add('format', 'double');
        end;

      TFieldType.ftBCD:
        begin
          LJSONField.Add('type', 'number');
          LJSONField.Add('format', 'double');
        end;

      TFieldType.ftDate:
        begin
          LJSONField.Add('type', 'string');
          LJSONField.Add('format', 'date');
        end;

      TFieldType.ftTime:
        begin
          LJSONField.Add('type', 'string');
          LJSONField.Add('format', 'date-time');
        end;

      TFieldType.ftDateTime:
        begin
          LJSONField.Add('type', 'string');
          LJSONField.Add('format', 'date-time');
        end;

      // ftBytes: ;
      // ftVarBytes: ;

      TFieldType.ftAutoInc:
        begin
          LJSONField.Add('type', 'integer');
          LJSONField.Add('format', 'int32');
        end;

      // ftBlob: ;

      TFieldType.ftMemo,
        TFieldType.ftWideMemo:
        begin
          LJSONField.Add('type', 'string');
        end;

      // ftGraphic: ;
      // ftFmtMemo: ;
      // ftParadoxOle: ;
      // ftDBaseOle: ;
      // ftTypedBinary: ;
      // ftCursor: ;
      TFieldType.ftFixedChar,
        TFieldType.ftFixedWideChar,
        TFieldType.ftWideString:
        begin
          LJSONField.Add('type', 'string');
        end;

      TFieldType.ftLargeint:
        begin
          LJSONField.Add('type', 'integer');
          LJSONField.Add('format', 'int64');
        end;

      // ftADT: ;
      // ftArray: ;
      // ftReference: ;
      // ftDataSet: ;
      // ftOraBlob: ;
      // ftOraClob: ;

      TFieldType.ftVariant:
        begin
          LJSONField.Add('type', 'string');
        end;

      // ftInterface: ;
      // ftIDispatch: ;

      TFieldType.ftGuid:
        begin
          LJSONField.Add('type', 'string');
        end;

      TFieldType.ftTimeStamp:
        begin
          LJSONField.Add('type', 'string');
          LJSONField.Add('format', 'date-time');
        end;

      TFieldType.ftFMTBcd:
        begin
          LJSONField.Add('type', 'number');
          LJSONField.Add('format', 'double');
        end;

      // ftOraTimeStamp: ;
      // ftOraInterval: ;
      // ftConnection: ;
      // ftParams: ;
      // ftStream: ;
      // ftTimeStampOffset: ;
      // ftObject: ;
    end;
  end;
end;

class function TDataSetUtils.DataSetToJSONArray(const ADataSet: TDataSet; AUseUTCDate: Boolean): TJSONArray;
begin
  result := DataSetToJSONArray(ADataSet, nil, AUseUTCDate);
end;

class function TDataSetUtils.DataSetToJSONArray(const ADataSet: TDataSet; const AAcceptFunc: TFunc<Boolean>; AUseUTCDate: Boolean): TJSONArray;
var
  LBookmark: TBookmark;
begin
  result := TJSONArray.Create;
  if not Assigned(ADataSet) then
    Exit;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  try
    LBookmark := ADataSet.Bookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
        try
          if (not Assigned(AAcceptFunc)) or (AAcceptFunc()) then
            result.Add(RecordToJSONObject(ADataSet, AUseUTCDate));
        finally
          ADataSet.Next;
        end;
    finally
      ADataSet.GotoBookmark(LBookmark);
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

class function TDataSetUtils.FieldToJSONValue(const AField: TField; AUseUTCDate: Boolean): TJSONData;
begin
  result := nil;

  if AField.IsNull then
    Exit(TJSONNull.Create);

  case AField.DataType of
    TFieldType.ftString:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftSmallint:
      result := TJSONIntegerNumber.Create(AField.AsInteger);
    TFieldType.ftInteger:
      result := TJSONIntegerNumber.Create(AField.AsInteger);
    TFieldType.ftWord:
      result := TJSONIntegerNumber.Create(AField.AsInteger);
    TFieldType.ftBoolean:
      result := TJSONUtils.BooleanToTJSON(AField.AsBoolean);
    TFieldType.ftFloat:
      result := TJSONFloatNumber.Create(AField.AsFloat);
    TFieldType.ftCurrency:
      result := TJSONFloatNumber.Create(AField.AsCurrency);
    TFieldType.ftBCD:
      result := TJSONFloatNumber.Create(AField.AsFloat);
    TFieldType.ftDate:
      result := TJSONUtils.DateToJSONValue(AField.AsDateTime, AUseUTCDate);
    TFieldType.ftTime:
      result := TJSONUtils.DateToJSONValue(AField.AsDateTime, AUseUTCDate);
    TFieldType.ftDateTime:
      result := TJSONUtils.DateToJSONValue(AField.AsDateTime, AUseUTCDate);
    TFieldType.ftBytes:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    TFieldType.ftVarBytes:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    TFieldType.ftAutoInc:
      result := TJSONIntegerNumber.Create(AField.AsInteger);
    TFieldType.ftBlob:
      result := TJSONString.Create(BlobFieldToBase64(AField as TBlobField));
    TFieldType.ftMemo:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftGraphic:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    // TFieldType.ftFmtMemo: ;
    // TFieldType.ftParadoxOle: ;
    // TFieldType.ftDBaseOle: ;
    TFieldType.ftTypedBinary:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    // TFieldType.ftCursor: ;
    TFieldType.ftFixedChar:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftWideString:
      result := TJSONString.Create(AField.AsWideString);
    TFieldType.ftLargeint:
      result := TJSONInt64Number.Create(AField.AsLargeInt);
    TFieldType.ftADT:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    TFieldType.ftArray:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    // TFieldType.ftReference: ;
    // TFieldType.ftDataSet:
    // Result := DataSetToJSONArray((AField.AsDateTime).NestedDataSet, AUseUTCDate);
    TFieldType.ftOraBlob:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    TFieldType.ftOraClob:
      result := TJSONString.Create(TBase64.Encode(AField.AsBytes));
    TFieldType.ftVariant:
      result := TJSONString.Create(AField.AsString);
    // TFieldType.ftInterface: ;
    // TFieldType.ftIDispatch: ;
    TFieldType.ftGuid:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftTimeStamp:
      result := TJSONUtils.DateToJSONValue(AField.AsDateTime, AUseUTCDate);
    TFieldType.ftFMTBcd:
      result := TJSONFloatNumber.Create(AField.AsFloat);
    TFieldType.ftFixedWideChar:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftWideMemo:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftOraTimeStamp:
      result := TJSONUtils.DateToJSONValue(AField.AsDateTime, AUseUTCDate);
    TFieldType.ftOraInterval:
      result := TJSONString.Create(AField.AsString);
    TFieldType.ftLongWord:
      result := TJSONIntegerNumber.Create(AField.AsInteger);
    TFieldType.ftShortint:
      result := TJSONIntegerNumber.Create(AField.AsInteger);
    TFieldType.ftByte:
      result := TJSONInt64Number.Create(AField.AsInteger);
    TFieldType.ftExtended:
      result := TJSONFloatNumber.Create(AField.AsFloat);
    // TFieldType.ftConnection: ;
    // TFieldType.ftParams: ;
    // TFieldType.ftStream: ;
    // TFieldType.ftTimeStampOffset:
    // Result := TJSONString.Create(AField.AsString);
    // TFieldType.ftObject: ;
    TFieldType.ftSingle:
      result := TJSONFloatNumber.Create(AField.AsFloat);
  end;
end;

class procedure TDataSetUtils.JSONObjectToDataSet(AJSONValue: TJSONData; ADataSet: TDataSet; AUseUTCDate: Boolean);
var
  LJSONArray: TJSONArray;
begin
  if AJSONValue is TJSONObject then
  begin
    LJSONArray := TJSONArray.Create;
    try
      LJSONArray.Add(AJSONValue.Clone as TJSONData);
      JSONToDataSet(LJSONArray, ADataSet, AUseUTCDate);
    finally
      LJSONArray.Free;
    end;
  end;
end;

class procedure TDataSetUtils.JSONToDataSet(AJSONValue: TJSONData; ADataSet: TDataSet; AUseUTCDate: Boolean);
var
  LJSONArray: TJSONArray;
  LJSONItem: TJSONObject;
  LIndex: Integer;
begin
  if not(AJSONValue is TJSONArray) then
    raise ENeonException.Create('JSONToDataSet: The JSON must be an array');

  LJSONArray := AJSONValue as TJSONArray;

  for LIndex := 0 to LJSONArray.Count - 1 do
  begin
    LJSONItem := LJSONArray.Items[LIndex] as TJSONObject;

    JSONToRecord(LJSONItem, ADataSet, AUseUTCDate);
  end;
end;

class procedure TDataSetUtils.JSONToRecord(AJSONObject: TJSONObject; ADataSet: TDataSet; AUseUTCDate: Boolean);
var
  LJSONField: TJSONData;
  LIndex: Integer;
  LField: TField;
begin
  ADataSet.Append;
  for LIndex := 0 to ADataSet.Fields.Count - 1 do
  begin
    LField := ADataSet.Fields[LIndex];
    LJSONField := AJSONObject.Find(LField.FieldName);
    if not Assigned(LJSONField) then
      Continue;

    if LJSONField is TJSONNull then
    begin
      LField.Clear;
      Continue;
    end;

    case LField.DataType of
      // TFieldType.ftUnknown: ;
      TFieldType.ftString:
        LField.AsString := LJSONField.Value;
      TFieldType.ftSmallint:
        LField.AsString := LJSONField.Value;
      TFieldType.ftInteger:
        LField.AsString := LJSONField.Value;
      TFieldType.ftWord:
        LField.AsString := LJSONField.Value;
      TFieldType.ftBoolean:
        LField.AsString := LJSONField.Value;
      TFieldType.ftFloat:
        LField.AsString := LJSONField.Value;
      TFieldType.ftCurrency:
        LField.AsString := LJSONField.Value;
      TFieldType.ftBCD:
        LField.AsString := LJSONField.Value;
      TFieldType.ftDate:
        LField.AsDateTime := TJSONUtils.JSONToDate(LJSONField.Value, AUseUTCDate);
      TFieldType.ftTime:
        LField.AsDateTime := TJSONUtils.JSONToDate(LJSONField.Value, AUseUTCDate);
      TFieldType.ftDateTime:
        LField.AsDateTime := TJSONUtils.JSONToDate(LJSONField.Value, AUseUTCDate);
      TFieldType.ftBytes:
        ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      TFieldType.ftVarBytes:
        ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      TFieldType.ftAutoInc:
        LField.AsString := LJSONField.Value;
      TFieldType.ftBlob:
        TDataSetUtils.Base64ToBlobField(LJSONField.Value, ADataSet.Fields[LIndex] as TBlobField);
      TFieldType.ftMemo:
        LField.AsString := LJSONField.Value;
      TFieldType.ftGraphic:
        (ADataSet.Fields[LIndex] as TGraphicField).AsBytes := TBase64.Decode(LJSONField.Value);
      // TFieldType.ftFmtMemo: ;
      // TFieldType.ftParadoxOle: ;
      // TFieldType.ftDBaseOle: ;
      TFieldType.ftTypedBinary:
        ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      // TFieldType.ftCursor: ;
      TFieldType.ftFixedChar:
        LField.AsString := LJSONField.Value;
      TFieldType.ftWideString:
        LField.AsString := LJSONField.Value;
      TFieldType.ftLargeint:
        LField.AsString := LJSONField.Value;
      TFieldType.ftADT:
        ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      TFieldType.ftArray:
        ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      // TFieldType.ftReference: ;
      // TFieldType.ftDataSet:
      // JSONToDataSet(LJSONField, (ADataSet.Fields[LIndex] as TDataSetField).NestedDataSet, AUseUTCDate);
      TFieldType.ftOraBlob:
        TDataSetUtils.Base64ToBlobField(LJSONField.Value, ADataSet.Fields[LIndex] as TBlobField);
      TFieldType.ftOraClob:
        TDataSetUtils.Base64ToBlobField(LJSONField.Value, ADataSet.Fields[LIndex] as TBlobField);
      TFieldType.ftVariant:
        TDataSetUtils.Base64ToBlobField(LJSONField.Value, ADataSet.Fields[LIndex] as TBlobField);
      // TFieldType.ftInterface: ;
      // TFieldType.ftIDispatch: ;
      TFieldType.ftGuid:
        LField.AsString := LJSONField.Value;
      TFieldType.ftTimeStamp:
        LField.AsDateTime := TJSONUtils.JSONToDate(LJSONField.Value, AUseUTCDate);
      TFieldType.ftFMTBcd:
        ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      TFieldType.ftFixedWideChar:
        LField.AsString := LJSONField.Value;
      TFieldType.ftWideMemo:
        LField.AsString := LJSONField.Value;
      TFieldType.ftOraTimeStamp:
        LField.AsDateTime := TJSONUtils.JSONToDate(LJSONField.Value, AUseUTCDate);
      TFieldType.ftOraInterval:
        LField.AsString := LJSONField.Value;
      TFieldType.ftLongWord:
        LField.AsString := LJSONField.Value;
      TFieldType.ftShortint:
        LField.AsString := LJSONField.Value;
      TFieldType.ftByte:
        LField.AsString := LJSONField.Value;
      TFieldType.ftExtended:
        LField.AsString := LJSONField.Value;
      // TFieldType.ftConnection: ;
      // TFieldType.ftParams: ;
      // TFieldType.ftStream:
      // ADataSet.Fields[LIndex].AsBytes := TBase64.Decode(LJSONField.Value);
      // TFieldType.ftTimeStampOffset: ;
      // TFieldType.ftTimeStampOffset: ;
      // TFieldType.ftObject: ;
      TFieldType.ftSingle:
        LField.AsString := LJSONField.Value;
    end;
  end;

  try
    ADataSet.Post;
  except
    ADataSet.Cancel;
    raise;
  end;
end;

class procedure TDataSetUtils.Base64ToBlobField(const ABase64: string; ABlobField: TBlobField);
var
  LBinaryStream: TMemoryStream;
begin
  LBinaryStream := TMemoryStream.Create;
  try
    TBase64.Decode(ABase64, LBinaryStream);
    ABlobField.LoadFromStream(LBinaryStream);
  finally
    LBinaryStream.Free;
  end;
end;

class function TDataSetUtils.BlobFieldToBase64(ABlobField: TBlobField): string;
var
  LBlobStream: TMemoryStream;
begin
  LBlobStream := TMemoryStream.Create;
  try
    ABlobField.SaveToStream(LBlobStream);
    LBlobStream.Position := soFromBeginning;
    result := TBase64.Encode(LBlobStream);
  finally
    LBlobStream.Free;
  end;
end;

end.
