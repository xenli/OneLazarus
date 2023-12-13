unit OneOrmRtti;

{$mode DELPHI}{$H+}


interface

uses
  Rtti, Generics.Collections, StrUtils, SysUtils, TypInfo,
  OneAttribute, SyncObjs;

type
  TOneFieldRtti = class
  public
    //FFieldRtti: TRttiField;
    FPropertyRtti: TRttiProperty;
    FFieldName: string;
    FDBFieldName: string;
    FDBFieldNameLow: string;
    FDBFieldFormat: string;
    FJsonName: string;
    FJsonFormat: string;

    FIsProperty: boolean;
    // 是否是时间类型字段
    FIsDateTime: boolean;
    // 是否是布尔型
    FIsBool: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TOneOrmRttiItem = class(TObject)
  private
    FOrmName: string;
    FTableName: string;
    FPrimaryKey: string;
    FFields: TList<TOneFieldRtti>;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Fields: TList<TOneFieldRtti> read FFields;
    property OrmName: string read FOrmName;
    property TableName: string read FTableName;
    property PrimaryKey: string read FPrimaryKey;
  end;

  IOrmRtti = interface
    function GetOrmRtti(ATypeInfo: Pointer): TOneOrmRttiItem;
  end;

  TOneOrmRtti = class(TInterfacedObject, IOrmRtti)
  private
    FLockObj: TCriticalSection;
    FOrmRttiItemList: TDictionary<string, TOneOrmRttiItem>;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetInstance(): IOrmRtti; static;
  public
    function GetOrmRtti(ATypeInfo: Pointer): TOneOrmRttiItem;
  end;

var
  unit_OrmRtti: IOrmRtti = nil;

implementation

constructor TOneFieldRtti.Create;
begin
  inherited Create;
  //FFieldRtti := nil;
  FPropertyRtti := nil;
  FDBFieldName := '';
  FDBFieldNameLow := '';
  FDBFieldFormat := '';
  FJsonName := '';
  FJsonFormat := '';
  FIsProperty := False;
  FIsDateTime := False;
  FIsBool := False;
end;

destructor TOneFieldRtti.Destroy;
begin
  inherited Destroy;
end;

class function TOneOrmRtti.GetInstance(): IOrmRtti;
begin
  if unit_OrmRtti = nil then
  begin
    unit_OrmRtti := TOneOrmRtti.Create;
  end;
  Result := unit_OrmRtti;
end;

constructor TOneOrmRttiItem.Create;
begin
  inherited Create;
  FFields := TList<TOneFieldRtti>.Create;
end;

destructor TOneOrmRttiItem.Destroy;
var
  i: integer;
begin
  for i := FFields.Count - 1 downto 0 do
  begin
    FFields[i].Free;
  end;
  FFields.Clear;
  FFields.Free;
  inherited Destroy;
end;

constructor TOneOrmRtti.Create;
begin
  inherited Create;
  FOrmRttiItemList := TDictionary<string, TOneOrmRttiItem>.Create;
  FLockObj := TCriticalSection.Create;
end;

destructor TOneOrmRtti.Destroy;
var
  lItem: TOneOrmRttiItem;
begin
  for lItem in FOrmRttiItemList.Values do
  begin
    lItem.Free;
  end;
  FOrmRttiItemList.Clear;
  FOrmRttiItemList.Free;
  FLockObj.Free;
  inherited Destroy;
end;

function TOneOrmRtti.GetOrmRtti(ATypeInfo: Pointer): TOneOrmRttiItem;
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  lKey: string;
  lItem: TOneOrmRttiItem;
  //lFields: TArray<TRttiField>;
  //lField: TRttiField;
  lProperties: TArray<TRttiProperty>;
  lProper: TRttiProperty;
  lOneFieldRtti: TOneFieldRtti;
  i, iAttr: integer;
  isNotJoin: boolean;
  lAttributes: TArray<TObject>;
  lAttribute: TObject;
  lAttriDBFieldName: string;
begin
  Result := nil;
  lItem := nil;
  LRttiContext := TRttiContext.Create;
  LRttiType := LRttiContext.GetType(ATypeInfo);
  lKey := LRttiType.QualifiedClassName;
  FLockObj.Enter;
  try
    FOrmRttiItemList.TryGetValue(lKey, lItem);
    if lItem = nil then
    begin
      FOrmRttiItemList.Remove(lKey);
    end
    else
    begin
      Result := lItem;
      exit;
    end;
  finally
    FLockObj.leave;
  end;
  // 不存在添加
  lItem := TOneOrmRttiItem.Create;
  lItem.FOrmName := LRttiType.Name;
  //lFields := LRttiType.GetFields();
  lProperties := LRttiType.GetProperties;

  for i := 0 to length(lProperties) - 1 do
  begin
    isNotJoin := False;
    lProper := lProperties[i];
    // 非公开的直接跳过
    if not (lProper.Visibility in [mvPublic, mvPublished]) then
    begin
      isNotJoin := True;
      continue;
    end;
    case lProper.PropertyType.TypeKind of
      tkInteger, tkEnumeration, tkFloat,
      tkString, tkAString, tkChar, tkLString, tkUChar, tkUString,
      tkVariant, tkInt64:
      begin

      end
      else
      begin
        // tkMRecord,tkProcedure,tkPointer,tkClassRef,tkDynArray,tkArray, tkRecord, tkInterface, tkSet, tkClass, tkMethod,
        isNotJoin := True;
      end;
    end;
    if isNotJoin then
      continue;

    lOneFieldRtti := TOneFieldRtti.Create;
    lItem.FFields.Add(lOneFieldRtti);
    lOneFieldRtti.FPropertyRtti := lProper;
    lOneFieldRtti.FIsProperty := True;
    lOneFieldRtti.FFieldName := lProper.Name;
    lOneFieldRtti.FDBFieldName := lProper.Name;

    case lProper.PropertyType.TypeKind of
      tkFloat:
      begin
        if lProper.PropertyType.Handle = system.TypeInfo(TDateTime) then
        begin
          lOneFieldRtti.FIsDateTime := True;
        end;
      end;
      tkEnumeration:
      begin
        if lProper.PropertyType.Handle = system.TypeInfo(boolean) then
        begin
          lOneFieldRtti.FIsBool := True;
        end;
      end;
    end;
    // 注解取数据库字段
    // lAttribute := lProper.GetAttribute(TOneDBAttribute); 这种用法11版以下没有
    //lAttributes := lProper.GetAttributes;
    //for iAttr := 0 to length(lAttributes) - 1 do
    //begin
    //  lAttribute := lAttributes[iAttr];
    //  if lAttribute is TOneDBAttribute then
    //  begin
    //    lAttriDBFieldName := TOneDBAttribute(lAttribute).FieldName;
    //    if lAttriDBFieldName <> '' then
    //    begin
    //      lOneFieldRtti.FDBFieldName := lAttriDBFieldName;
    //      lOneFieldRtti.FDBFieldFormat := TOneDBAttribute(lAttribute).Format;
    //    end;
    //  end;
    //end;

    lOneFieldRtti.FDBFieldNameLow := lOneFieldRtti.FDBFieldName.ToLower;
  end;
  FLockObj.Enter;
  try
    if FOrmRttiItemList.ContainsKey(lKey) then
    begin
      // 多线程并发，同时写入的可能性
      lItem.Free;
      lItem := nil;
      // 取值
      FOrmRttiItemList.TryGetValue(lKey, lItem);
    end
    else
    begin
      FOrmRttiItemList.Add(lKey, lItem);
    end;
  finally
    FLockObj.Leave;
  end;
  Result := lItem;
end;

end.
