unit OneSerialization;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Generics.Collections, RTTI,
  TypInfo, Contnrs, OneDataJson, DB;

const
  DefalutString: string = '';
  DefalutInteger: integer = 0;
  DefalutInt64: int64 = 0;
  DefalutDouble: double = 0;
  DefalutBoolean: boolean = False;

type
  //emResultType = (unknowResult, numberResult, stringResult, boolResult,
  //  objResult, listResult, objListResult, genericsListResult,
  //  genericsObjListResult, mapResult, arrayResult, recordResult);

  emListT = (unknowListT, ListTObj, ListTInteger, ListTInt64, ListTString,
    ListTbool, ListTDouble, ListTCust, ObjListT);
  emList = (unknowList, ListCust, ObjList);

  evenListItemCreate = function(): TObject;

  TOneListClass = class
  private
    FListClass: TClass;
    FListItemClass: TClass;
    FItemCreateEven: evenListItemCreate;
    FItemRttiContext: TRttiContext;
    FItemRttiType: TRttiType;
  end;

  TOneSerializeRttiManage = class
  private
    FListClassDict: TDictionary<string, TOneListClass>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure AddListClass(QListClass: TClass; QListItemClass: TClass; QItemCreateEven: evenListItemCreate = nil);
function GetListClass(QClassName: string): TOneListClass;
//把一个对象序列化成JSON
function ObjectToJson(const QSource: TObject; var QErrMsg: string): TJsonData;
function ObjectToJsonString(const QSource: TObject): string;
//把一个JSON序列化成对象
function JsonToObject(const QSource: TObject; const QJsonData: TJsonData; var QErrMsg: string): boolean;
//判断对象是泛型，还是容器类型，还是map字典
function IsGenericsCollections(const QSource: TObject; QListRttiType: TRttiType; var QemList: emListT; var QListClass: TOneListClass): boolean;
function IsListCollections(QListRttiType: TRttiType; var QemList: emList): boolean;

//从文件中加载或保存JSON字符串和类互相序列化
function JSONToObjectFormFile(QObject: TObject; QFileName: string; var QErrMsg: string): boolean;
function ObjectToJsonFile(QObject: TObject; QFileName: string; var QErrMsg: string): boolean;

function FreeTValue(QTValue: TValue; QFreeListItem: boolean = True): boolean;

var
  unit_SerializeRttiManage: TOneSerializeRttiManage = nil;

implementation

constructor TOneSerializeRttiManage.Create;
begin
  inherited Create;
  self.FListClassDict := TDictionary<string, TOneListClass>.Create;
end;

destructor TOneSerializeRttiManage.Destroy;
var
  lListCust: TOneListClass;
begin
  for lListCust in FListClassDict.Values do
  begin
    lListCust.Free;
  end;
  FListClassDict.Clear;
  FListClassDict.Free;
  inherited Destroy;

end;

procedure AddListClass(QListClass: TClass; QListItemClass: TClass; QItemCreateEven: evenListItemCreate = nil);
var
  lKey: string;
  lListCust: TOneListClass;
begin
  if unit_SerializeRttiManage = nil then
  begin
    unit_SerializeRttiManage := TOneSerializeRttiManage.Create;
  end;
  lKey := LowerCase(QListClass.ClassName);
  if unit_SerializeRttiManage.FListClassDict.ContainsKey(lKey) then
    exit;
  lListCust := TOneListClass.Create;
  unit_SerializeRttiManage.FListClassDict.add(lKey, lListCust);
  lListCust.FListClass := QListClass;
  lListCust.FListItemClass := QListItemClass;
  lListCust.FItemCreateEven := QItemCreateEven;
  if lListCust.FListItemClass <> nil then
  begin
    lListCust.FItemRttiContext := TRttiContext.Create;
    lListCust.FItemRttiType :=
      lListCust.FItemRttiContext.GetType(lListCust.FListItemClass);
  end;
end;

function GetListClass(QClassName: string): TOneListClass;
var
  lListClass: TOneListClass;
begin
  Result := nil;
  if unit_SerializeRttiManage = nil then
  begin
    exit;
  end;
  QClassName := LowerCase(QClassName);
  lListClass := nil;
  if unit_SerializeRttiManage.FListClassDict.tryGetValue(QClassName, lListClass) then
  begin
    Result := lListClass;
  end;
end;

//把一个对象序列化成JSON
function ObjectToJson(const QSource: TObject; var QErrMsg: string): TJsonData;
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiProperty;
  LRttiProps: TArray<TRttiProperty>;
  LPropRttiType: TRttiType;

  LPropValue, LArrTValue: TValue;
  //获取相关Rtti信息
  LListClass: TOneListClass;
  LemListT: emListT;
  LemList: emList;
  LListItemRttiType: TRttiType;
  LMapKeyRttiType, lMapValueRttiType: TRttiType;
  //转化
  lJsonData: TJsonData;
  lJsonObj, lItemJsonObj: TJsonObject;
  lJsonArray: TJSonArray;
  //泛型类
  lListTObject: TList<TObject>;
  lObjListT: TObjectList<TObject>;
  lListTString: TList<string>;
  lListTInteger: TList<integer>;
  lListTInt64: TList<int64>;
  lListTDouble: TList<double>;
  lListTBoolean: TList<boolean>;
  //容器类
  lObjList: TObjectList;
  i, iArr: integer;
  tempStr: string;
begin
  Result := nil;
  QErrMsg := '';
  if QSource = nil then
    exit;
  if QSource is TDataSet then
  begin
    //数据集转化
    Result := OneDataJson.DataSetToJson(TDataSet(QSource));
    exit;
  end;
  LRttiContext := TRttiContext.Create;
  LRttiType := LRttiContext.GetType(QSource.ClassType);
  if not Assigned(LRttiType) then
  begin
    QErrMsg := '无法获取Rtti信息';
    Exit;
  end;
  if not (LRttiType.TypeKind in [tkClass, tkRecord]) then
  begin
    QErrMsg := '无法序列化对象类型' + GetEnumName(TypeInfo(TTypeKind), Ord(LRttiType.TypeKind));
    Exit;
  end;
  if LRttiType.TypeKind = tkClass then
  begin
    //判断是不是 TObjectList,TList这些泛型
    LListClass := nil;
    if IsGenericsCollections(QSource, LRttiType, LemListT, LListClass) then
    begin
      Result := TJSonArray.Create();
      lJsonArray := TJSonArray(Result);
      case LemListT of
        emListT.ListTObj:
        begin
          lListTObject := TList<TObject>(QSource);
          for i := 0 to lListTObject.Count - 1 do
          begin
            lJsonData := ObjectToJson(lListTObject.Items[i], QErrMsg);
            if lJsonData <> nil then
            begin
              lJsonArray.Add(lJsonData);
            end;
          end;
        end;
        emListT.ListTString:
        begin
          lListTString := TList<string>(QSource);
          for i := 0 to lListTString.Count - 1 do
          begin
            lJsonArray.Add(lListTString.Items[i]);
          end;
        end;
        emListT.ListTInteger:
        begin
          lListTInteger := TList<integer>(QSource);
          for i := 0 to lListTInteger.Count - 1 do
          begin
            lJsonArray.Add(lListTInteger.Items[i]);
          end;
        end;
        emListT.ListTInt64:
        begin
          lListTInt64 := TList<int64>(QSource);
          for i := 0 to lListTInt64.Count - 1 do
          begin
            lJsonArray.Add(lListTInt64.Items[i]);
          end;
        end;
        emListT.ListTDouble:
        begin
          lListTDouble := TList<double>(QSource);
          for i := 0 to lListTDouble.Count - 1 do
          begin
            lJsonArray.Add(lListTDouble.Items[i]);
          end;
        end;
        emListT.ListTbool:
        begin
          lListTBoolean := TList<boolean>(QSource);
          for i := 0 to lListTBoolean.Count - 1 do
          begin
            lJsonArray.Add(lListTBoolean.Items[i]);
          end;
        end;
        emListT.ObjListT:
        begin
          lObjListT := TObjectList<TObject>(QSource);
          for i := 0 to lObjListT.Count - 1 do
          begin
            lJsonData := ObjectToJson(lObjListT.Items[i], QErrMsg);
            if lJsonData <> nil then
            begin
              lJsonArray.Add(lJsonData);
            end;
          end;
        end;
        emListT.ListTCust:
        begin
          lObjListT := TObjectList<TObject>(QSource);
          for i := 0 to lObjListT.Count - 1 do
          begin
            lJsonData := ObjectToJson(lObjListT.Items[i], QErrMsg);
            if lJsonData <> nil then
            begin
              lJsonArray.Add(lJsonData);
            end;
          end;
        end
        else
        begin
          exit;
        end;
      end;
      exit;
    end
    else
    if IsListCollections(LRttiType, LemList) then
    begin
      Result := TJSonArray.Create();
      lJsonArray := TJSonArray(Result);
      case LemList of
        emList.ObjList:
        begin
          lObjList := TObjectList(QSource);
          for i := 0 to lObjList.Count - 1 do
          begin
            lJsonData := ObjectToJson(lObjList.Items[i], QErrMsg);
            if lJsonData <> nil then
            begin
              lJsonArray.Add(lJsonData);
            end;
          end;
        end;
      end;
      exit;
    end
    else
    begin
      Result := TJsonObject.Create();
    end;
  end;
  if Result = nil then
    exit;
  lJsonObj := TJsonObject(Result);
  LRttiProps := LRttiType.GetProperties;
  for i := 0 to length(LRttiProps) - 1 do
  begin
    LRttiProp := LRttiProps[i];
    if not LRttiProp.IsReadable then
      continue;
    if not (LRttiProp.Visibility in [mvPublic, mvPublished]) then
      continue;
    LPropRttiType := LRttiProp.PropertyType;
    LPropValue := LRttiProp.GetValue(QSource);
    case LPropRttiType.TypeKind of
      TTypeKind.tkClass:
      begin
        lJsonData := ObjectToJson(LPropValue.AsObject, QErrMsg);
        if lJsonData <> nil then
        begin
          lJsonObj.Add(LRttiProp.Name, lJsonData);
        end;
      end;
      tkSString, tkLString, tkAString, tkWString, tkWChar, tkUString, tkUChar:
      begin
        lJsonObj.Add(LRttiProp.Name, LPropValue.AsString);
      end;
      tkInteger:
      begin
        lJsonObj.Add(LRttiProp.Name, LPropValue.AsInteger);
      end;
      tkInt64:
      begin
        lJsonObj.Add(LRttiProp.Name, LPropValue.AsInt64);
      end;
      tkFloat, tkQWord:
      begin
        lJsonObj.Add(LRttiProp.Name, LPropValue.AsExtended);
      end;
      tkBool:
      begin
        lJsonObj.Add(LRttiProp.Name, LPropValue.AsBoolean);
      end;
      tkArray, tkDynArray:
      begin
        if LPropValue.GetArrayLength < 1 then
        begin
          lJsonObj.Add(LRttiProp.Name, TJSONArray.Create);
        end
        else
        begin
          lJsonArray := TJSONArray.Create;
          lJsonObj.Add(LRttiProp.Name, lJsonArray);
          for iArr := 0 to LPropValue.GetArrayLength - 1 do
          begin
            LArrTValue := LPropValue.GetArrayElement(iArr);
            if LArrTValue.Kind = tkBool then
              lJsonArray.Add(LArrTValue.AsBoolean)
            else if LArrTValue.Kind in [tkString, tkAString, tkChar, tkLString, tkUChar, tkUString, tkVariant] then
              lJsonArray.Add(LArrTValue.AsString)
            else if LArrTValue.Kind in [tkInteger, tkInt64] then
              lJsonArray.Add(LArrTValue.AsInt64)
            else if LArrTValue.Kind in [tkFloat] then
              lJsonArray.Add(LArrTValue.AsExtended)
            else if LArrTValue.Kind in [tkClass] then
            begin
              lJsonData := ObjectToJson(LArrTValue.AsObject, QErrMsg);
              if lJsonData <> nil then
              begin
                lJsonArray.Add(lJsonData);
              end;
            end;
          end;
        end;
      end
    end;
  end;
end;

function ObjectToJsonString(const QSource: TObject): string;
var
  lJsonData: TJsonData;
  lErrmsg: string;
begin
  Result := '';
  lJsonData := nil;
  lJsonData := ObjectToJson(QSource, lErrmsg);
  if lJsonData <> nil then
  begin
    Result := lJsonData.asJson;
  end;
end;

//把一个JSON序列化成对象
function JsonToObject(const QSource: TObject; const QJsonData: TJsonData; var QErrMsg: string): boolean;
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiProperty;
  LRttiProps: TArray<TRttiProperty>;
  LPropRttiType: TRttiType;
  tempObj: TObject;
  LPropValue, LArrItemTValue: TValue;
  //获取相关Rtti信息
  LemListT: emListT;
  LemList: emList;
  LListItemRttiType: TRttiType;
  LMapKeyRttiType, lMapValueRttiType: TRttiType;
  //转化
  lJsonData, tempJsonData: TJsonData;
  lJsonObj, lItemJsonObj: TJsonObject;
  lJsonArray: TJSonArray;
  //泛型类
  LRttiContextT: TRttiContext;
  LRttiTypeT: TRttiType;
  lListItemObj: TObject;
  lListTObject: TList<TObject>;
  lObjListT: TObjectList<TObject>;
  lListTString: TList<string>;
  lListTInteger: TList<integer>;
  lListTInt64: TList<int64>;
  lListTDouble: TList<double>;
  lListTBoolean: TList<boolean>;
  LListClass: TOneListClass;
  //数组类
  lArryType: PTypeInfo;
  LArrItemRttiContext: TRttiContext;
  LArrItemRttiType: TRttiType;
  lArryString: array of string;
  lArryObject: array of TObject;
  lArryItemObj: TObject;

  //容器类
  lObjList: TObjectList;
  i, iArr: integer;
  tempStr: string;

begin
  Result := False;
  if QSource = nil then
  begin
    QErrMsg := '序列化对象为nil';
    exit;
  end;
  if QJsonData = nil then
  begin
    QErrMsg := 'JSON对象为nil';
    exit;
  end;
  LRttiContext := TRttiContext.Create;
  LRttiType := LRttiContext.GetType(QSource.ClassType);
  if not Assigned(LRttiType) then
  begin
    QErrMsg := '无法获取Rtti信息';
    Exit;
  end;
  //判断是不是泛型,如果是泛型QJsonData必需是数组,否则是对象
  if not (LRttiType.TypeKind in [tkClass, tkRecord]) then
  begin
    QErrMsg := '无法序列化对象类型' + GetEnumName(TypeInfo(TTypeKind), Ord(LRttiType.TypeKind));
    Exit;
  end;
  if LRttiType.TypeKind = tkClass then
  begin
    //判断是不是 TObjectList,TList这些泛型
    LListClass := nil;
    if IsGenericsCollections(QSource, LRttiType, LemListT, LListClass) then
    begin
      if not (QJsonData is TJSonArray) then
      begin
        QErrMsg := '对象是个数组,但JSON非JSON数组';
        exit;
      end;
      lJsonArray := TJSonArray(QJsonData);
      case LemListT of
        emListT.ListTCust:
        begin
          if LListClass = nil then
          begin
            QErrMsg := LRttiType.AsInstance.MetaClassType.ClassName + '对像未注册,请先用OneSerialization.AddListClass方法注册';
            exit;
          end;
          if (LListClass.FListItemClass = nil) and (LListClass.FItemCreateEven = nil) then
          begin
            QErrMsg := LRttiType.AsInstance.MetaClassType.ClassName + '对像已注册,但TList<T>,T类型未注册';
            exit;
          end;
          lListTObject := TList<TObject>(QSource);
          //获取T的RTTI信息,然后动态生成对象
          for i := 0 to lJsonArray.Count - 1 do
          begin
            if @LListClass.FItemCreateEven <> nil then
            begin
              lListItemObj := LListClass.FItemCreateEven();
            end
            else
            if LListClass.FListItemClass <> nil then
            begin
              lListItemObj := TClass(LListClass.FListItemClass).Create;
            end;
            if not JsonToObject(lListItemObj, lJsonArray[i], QErrMsg) then
            begin
              exit;
            end;
            lListTObject.add(lListItemObj);
          end;
        end;
        emListT.ListTObj:
        begin
          //没办法判断对象类型跳过
          QErrMsg := '未支持的泛型对象';
          exit;
        end;
        emListT.ListTString:
        begin
          lListTString := TList<string>(QSource);
          lListTString.Clear;
          for i := 0 to lJsonArray.Count - 1 do
          begin
            lListTString.add(lJsonArray[i].AsString);
          end;
        end;
        emListT.ListTInteger:
        begin
          lListTInteger := TList<integer>(QSource);
          lListTInteger.Clear;
          for i := 0 to lJsonArray.Count - 1 do
          begin
            lListTInteger.add(lJsonArray[i].AsInteger);
          end;
        end;
        emListT.ListTInt64:
        begin
          lListTInt64 := TList<int64>(QSource);
          lListTInt64.Clear;
          for i := 0 to lJsonArray.Count - 1 do
          begin
            lListTInt64.add(lJsonArray[i].AsInt64);
          end;
        end;
        emListT.ListTDouble:
        begin
          lListTDouble := TList<double>(QSource);
          lListTDouble.Clear;
          for i := 0 to lJsonArray.Count - 1 do
          begin
            lListTDouble.add(lJsonArray[i].AsFloat);
          end;
        end;
        emListT.ListTbool:
        begin
          lListTBoolean := TList<boolean>(QSource);
          lListTBoolean.Clear;
          for i := 0 to lJsonArray.Count - 1 do
          begin
            lListTBoolean.add(lJsonArray[i].AsBoolean);
          end;
        end
        else
        begin
          QErrMsg := '未支持的泛型对象';
          exit;
        end;
      end;
    end
    else
    if IsListCollections(LRttiType, LemList) then
    begin
      QErrMsg := '未支持的TList对象';
      exit;
    end
    else
    begin
      //解析JSON对象
      if not (QJsonData is TJsonObject) then
      begin
        QErrMsg := '数据不为Json对象无法转化成对象[' + QSource.ClassName + ']';
        exit;
      end;
      lJsonObj := TJsonObject(QJsonData);
      LRttiProps := LRttiType.GetProperties;
      for i := 0 to length(LRttiProps) - 1 do
      begin
        LRttiProp := LRttiProps[i];

        if not LRttiProp.IsWritable then
          continue;
        if not (LRttiProp.Visibility in [mvPublic, mvPublished]) then
          continue;

        tempJsonData := lJsonObj.Find(LRttiProp.Name);
        if tempJsonData = nil then
        begin
          continue;
        end;

        LPropRttiType := LRttiProp.PropertyType;
        case LPropRttiType.TypeKind of
          TTypeKind.tkClass:
          begin
            LPropValue := LRttiProp.GetValue(QSource);
            tempObj := LPropValue.AsObject;
            if tempObj = nil then
            begin
              tempObj := TClass(LRttiProp.PropertyType.AsInstance.MetaClassType).Create;
              //设置属性
              LRttiProp.SetValue(QSource, tempObj);
            end;
            tempStr := LRttiProp.Name;
            if not JsonToObject(tempObj, tempJsonData, QErrMsg) then
            begin
              exit;
            end;
          end;
          tkSString, tkLString, tkAString, tkWString, tkWChar, tkUString, tkUChar:
          begin
            LRttiProp.SetValue(QSource, lJsonObj.Get(LRttiProp.Name, DefalutString));
          end;
          tkInteger:
          begin
            case tempJsonData.JSONType of
              jtNumber:
              begin
                LRttiProp.SetValue(QSource, lJsonObj.Get(LRttiProp.Name, DefalutInteger));
              end;
              else
              begin
                QErrMsg := '对象属性['+LRttiProp.Name+']是整型,但Json数据非整型,无法转化数据';
                exit;
              end
            end;

          end;
          tkInt64:
          begin
            LRttiProp.SetValue(QSource, lJsonObj.Get(LRttiProp.Name, DefalutInt64));
          end;
          tkFloat, tkQWord:
          begin
            LRttiProp.SetValue(QSource, lJsonObj.Get(LRttiProp.Name, DefalutDouble));
          end;
          tkBool:
          begin
            LRttiProp.SetValue(QSource, lJsonObj.Get(LRttiProp.Name, DefalutBoolean));
          end;
          tkDynArray:
          begin
            //后面在来
            if not lJsonObj.Find(LRttiProp.Name, lJsonArray) then
              continue;
            if lJsonArray.Count = 0 then
              exit;
            setLength(lArryObject, lJsonArray.Count);
            LPropValue := LRttiProp.GetValue(QSource);
            lArryType := LPropValue.TypeData^.elType2;
            LArrItemRttiContext := TRttiContext.Create;
            LArrItemRttiType := LArrItemRttiContext.GetType(lArryType);
            for iArr := 0 to lJsonArray.Count - 1 do
            begin
              lArryObject[iArr] := nil;
              lJsonData := lJsonArray.Items[iArr];
              if lJsonData is TJsonObject then
              begin
                if lArryType.Kind = tkClass then
                begin
                  lArryObject[iArr] :=
                    TClass(LArrItemRttiType.AsInstance.MetaClassType).Create;
                end;
              end;
            end;
            SetDynArrayProp(QSource, LRttiProp.Name, lArryObject);
          end
        end;
      end;
    end;
  end;
  Result := True;
end;

function IsGenericsCollections(const QSource: TObject; QListRttiType: TRttiType; var QemList: emListT; var QListClass: TOneListClass): boolean;
var
  LMethodGetEnumerator, LMethodAdd, LMethodClear: TRttiMethod;
  lClassName: string;
  LMethods: TArray<TRttiMethod>;
  lMethod: TRttiMethod;
  i: integer;
  lListClass: TOneListClass;
begin
  Result := False;
  QListClass := nil;
  QemList := emListT.unknowListT;
  lClassName := QListRttiType.AsInstance.MetaClassType.ClassName;

  if not lClassName.Contains('<') then
  begin
    exit;
  end;
  if not lClassName.Contains('>') then
  begin
    exit;
  end;
  lClassName := lClassName.ToLower;
  lClassName := lClassName.Replace('system.', '', [rfReplaceAll]);
  if lClassName.StartsWith('tactionresultt<') then
  begin
    exit;
  end;
  if lClassName.StartsWith('tobjectlist<') then
  begin
    QemList := emListT.ObjListT;
  end
  else
  if lClassName = 'tlist<tobject>' then
  begin
    QemList := emListT.ListTObj;
  end
  else
  if (lClassName = 'tlist<string>') or (lClassName = 'tlist<ansistring>') or (lClassName = 'tlist<widestring>') then
  begin
    QemList := emListT.ListTString;
  end
  else
  if (lClassName = 'tlist<integer>') or (lClassName = 'tlist<longint>') then
  begin
    QemList := emListT.ListTInteger;
  end
  else
  if lClassName = 'tlist<int64>' then
  begin
    QemList := emListT.ListTInt64;
  end
  else
  if lClassName = 'tlist<double>' then
  begin
    QemList := emListT.ListTDouble;
  end
  else
  if lClassName = 'tlist<boolean>' then
  begin
    QemList := emListT.ListTbool;
  end;
  if QemList = emListT.unknowListT then
  begin
    //判断是不是继承此类,此类的话是泛型
    if QSource is TCustomList<TObject> then
    begin
      QemList := emListT.ListTObj;
    end;
  end;
  if QemList = emListT.unknowListT then
  begin
    //获取自定义类型
    QemList := emListT.ListTCust;
    lListClass := GetListClass(lClassName);
    if lListClass <> nil then
    begin
      QListClass := lListClass;
    end;
  end;
  Result := QemList <> emListT.unknowListT;
end;

function IsListCollections(QListRttiType: TRttiType; var QemList: emList): boolean;
var
  LMethodGetEnumerator, LMethodAdd, LMethodClear: TRttiMethod;
  lClassName: string;
begin
  Result := False;
  QemList := emList.unknowList;
  lClassName := QListRttiType.AsInstance.MetaClassType.ClassName;
  if lClassName.Contains('<') then
  begin
    exit;
  end;
  if lClassName.Contains('>') then
  begin
    exit;
  end;
  lClassName := lClassName.ToLower;
  if lClassName = 'tobjectlist' then
  begin
    QemList := emList.ObjList;
  end
  else
  if lClassName = 'tlist' then
  begin
    QemList := emList.ListCust;
  end;
  Result := (QemList <> emList.unknowList);
end;

function JSONToObjectFormFile(QObject: TObject; QFileName: string; var QErrMsg: string): boolean;
var
  lJsonData: TJsonData;
  lStream: TStringStream;
  lJsonStr: string;
begin
  Result := False;
  lJsonData := nil;
  QErrMsg := '';
  if QObject = nil then
  begin
    QErrMsg := '请先创建对象';
    exit;
  end;
  if QFileName = '' then
  begin
    QErrMsg := '保存的文件名称不可为空';
    exit;
  end;
  if not FileExists(QFileName) then
  begin
    QErrMsg := '文件不存在';
    exit;
  end;
  lJsonData := nil;
  lStream := TStringStream.Create();
  try
    lStream.LoadFromFile(QFileName);
    lStream.Position := 0;
    lJsonStr := lStream.DataString;
    if lJsonStr = '' then
      exit;
    lJsonData := GetJSON(lJsonStr);
    Result := JsonToObject(QObject, lJsonData, QErrMsg);
  finally
    lStream.Clear;
    lStream.Free;
    if lJsonData <> nil then
    begin
      lJsonData.Clear;
      lJsonData.Free;
    end;
  end;
end;

function ObjectToJsonFile(QObject: TObject; QFileName: string; var QErrMsg: string): boolean;
var
  lJsonData: TJsonData;
  lStream: TStringStream;
  lStr: string;
begin
  Result := False;
  QErrMsg := '';
  if QObject = nil then
  begin
    QErrMsg := '请先创建对象';
    exit;
  end;
  if QFileName = '' then
  begin
    QErrMsg := '保存的文件名称不可为空';
    exit;
  end;
  lJsonData := nil;
  lStream := TStringStream.Create();
  try
    lJsonData := ObjectToJson(QObject, QErrMsg);
    if lJsonData = nil then
      exit;
    lStr := string(lJsonData.AsJSON);
    lStream.WriteString(lStr);
    lStream.Position := 0;
    lStream.SaveToFile(QFileName);
    Result := True;
  finally
    lStream.Clear;
    lStream.Free;
    if lJsonData <> nil then
    begin
      lJsonData.Clear;
      lJsonData.Free;
    end;
  end;
end;

function FreeTValue(QTValue: TValue; QFreeListItem: boolean = True): boolean;
var
  lObj: TObject;
  lRttiContext: TRttiContext;
  lRttiType: TRttiType;
  lCustClass: TOneListClass;
  i: integer;
  //泛型类
  lemListT: emListT;
  lemList: emList;
  lListTObject: TList<TObject>;
  lObjListT: TObjectList<TObject>;
  lListTString: TList<string>;
  lListTInteger: TList<integer>;
  lListTInt64: TList<int64>;
  lListTDouble: TList<double>;
  lListTBoolean: TList<boolean>;
  lList: TList;
  lObjList: TObjectList;
begin
  Result := False;
  if not QTValue.IsObject then
  begin
    Result := True;
    exit;
  end;
  lObj := QTValue.AsObject;
  if lObj = nil then
    exit;
  lRttiContext := TRttiContext.Create;
  try
    lRttiType := lRttiContext.GetType(lObj.ClassInfo);
    if IsGenericsCollections(lObj, lRttiType, lemListT, lCustClass) then
    begin
      case LemListT of
        emListT.ListTObj:
        begin
          //明细要释放
          lListTObject := TList<TObject>(lObj);
          if QFreeListItem then
          begin
            for i := 0 to lListTObject.Count - 1 do
            begin
              lListTObject[i].Free;
            end;
          end;
          TList<TObject>(lObj).Clear;
          lObj.Free;
        end;
        emListT.ListTString, emListT.ListTInteger, emListT.ListTInt64,
        emListT.ListTDouble, emListT.ListTbool:
        begin
          //字符串不用释放明细
          lObj.Free;
        end;
        emListT.ObjListT:
        begin
          lObjListT := TObjectList<TObject>(lObj);
          if not lObjListT.OwnsObjects and QFreeListItem then
          begin
            for i := 0 to lObjListT.Count - 1 do
            begin
              lObjListT[i].Free;
            end;
          end;
          lObjListT.Free;
        end;
        emListT.ListTCust:
        begin
          if QFreeListItem then
          begin
            lListTObject := TList<TObject>(lObj);
            for i := 0 to lListTObject.Count - 1 do
            begin
              lListTObject[i].Free;
            end;
            lListTObject.Clear;
            lListTObject.Free;
          end;
        end;
      end;
    end
    else if IsListCollections(lRttiType, lemList) then
    begin
      if lemList = emList.ListCust then
      begin
        lList := TList(lObj);
        // 里面是不是对象要遍历判断，这边不处理
        // 主要放 string,int 这些常用类型
        // 自已释放
        if QFreeListItem then
        begin
          for i := 0 to lList.Count - 1 do
          begin
            if TObject(lList[i]) <> nil then
              TObject(lList[i]).Free;
          end;
        end;
        lList.Clear;
        lList.Free;
      end
      else if lemList = emList.ObjList then
      begin
        lObjList := TObjectList(lObj);
        if not lObjList.OwnsObjects then
        begin
          if QFreeListItem then
          begin
            for i := 0 to lObjList.Count - 1 do
            begin
              if lObjList[i] <> nil then
                lObjList[i].Free;
            end;
          end;

        end;
        lObjList.Clear;
        lObjList.Free;
      end;
    end
    else
    begin
      lObj.Free;
    end;
  finally

  end;
end;

end.
