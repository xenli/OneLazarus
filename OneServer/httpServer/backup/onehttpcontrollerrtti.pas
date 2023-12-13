unit OneHttpControllerRtti;

{$mode DELPHI}{$H+}


// 此单元的作用就是跟据反射解析继承 OneHttpController.TOneHttpController
// 把控制层类的反射信息相保存起来,放在缓存中
interface

uses
  TypInfo, Generics.Collections, Classes, Rtti, SysUtils;

type
  // 标准方法类型一等公民 resultProcedure=TEvenControllerProcedure
  emOneMethodType = (unknow, resultProcedure, sysProcedure, sysFunction);
  emOneMethodResultType = (unknowResult, numberResult, stringResult, boolResult,
    objResult, listResult, objListResult, genericsListResult,
    genericsObjListResult, mapResult, arrayResult, recordResult);
  // OneAll代表没有以下面为特殊规则的，不控制什么HTTPMethod可以访问,低版不采用注解
  // 方法头部 OneGetxxxx代表只支持Get方法
  // 方法头部 OnePost代表只支持Post方法
  // 方法头部 OneForm代表只支持Post方法,且数据是表单形式 key1=value1&key2=value2&...
  // 方法头部 OneUpload代表是MulitePart提交包含文件
  emOneHttpMethodMode = (OneAll, OneGet, OnePost, OneForm, OneUpload, OneDownload);

type
  TOneMethodRtti = class;
  TOneControllerRtti = class;

  TOneMethodRtti = class
  private
    FMethodType: emOneMethodType;
    FHttpMethodType: emOneHttpMethodMode;
    FRttiMethod: TRttiMethod;
    FTypeInfo: PTypeInfo;
    FHaveClassParam: boolean;
    FParamClassList: TList<TClass>;

    FResultRtti: TRttiType;
    FResultType: emOneMethodResultType;
    // 容器map key值类型
    FResultCollectionsKeyType: emOneMethodResultType;
    // 容器值类型
    FResultCollectionsValueType: emOneMethodResultType;
    FErrMsg: string;
  public
    constructor Create();
    destructor Destroy; override;
  public
    property MethodType: emOneMethodType read FMethodType;
    property HttpMethodType: emOneHttpMethodMode read FHttpMethodType;
    property RttiMethod: TRttiMethod read FRttiMethod;
    property HaveClassParam: boolean read FHaveClassParam;
    property ParamClassList: TList<TClass> read FParamClassList;
    property ResultType: emOneMethodResultType read FResultType;
    property ResultRtti: TRttiType read FResultRtti;
    property ResultCollectionsKeyType: emOneMethodResultType
      read FResultCollectionsKeyType;
    property ResultCollectionsValueType: emOneMethodResultType
      read FResultCollectionsValueType;
    property ErrMsg: string read FErrMsg;
    property TypeInfo: PTypeInfo read FTypeInfo;
  end;

  TOneControllerRtti = class
  private
    // 方法反射信息 (方法名称,反射信息)
    FMethodList: TDictionary<string, TOneMethodRtti>;
    // 控制层反射信息
    FRttiContext: TRttiContext;
    FRttiType: TRttiType;
  private

  public
    // 判断是不是泛型容器
    class function GetRttiTypeToResultType(qRttiType: TRttiType;
      var QResultType: emOneMethodResultType;
      var QItemResultType: emOneMethodResultType): boolean; static;
    constructor Create(QInterfaceTypeInfo: PTypeInfo); overload;
    destructor Destroy; override;
    function IsGenericsCollections(qListType: TRttiType;
      QOneMethodRtti: TOneMethodRtti): boolean;
    // 判断是不是List容器
    function IsListCollections(qListType: TRttiType;
      QOneMethodRtti: TOneMethodRtti): boolean;
    // 判断是不是map容器
    function IsMapCollections(QMapType: TRttiType;
      QOneMethodRtti: TOneMethodRtti): boolean;

    // 跟据控制层类,获取控制类的方法反射信息
    procedure GetMethodList(QInterfaceTypeInfo: PTypeInfo);
    // 跟据方法名称获取方法的反射信息
    function GetRttiMethod(QMethodName: string): TOneMethodRtti;
  public
    property MethodList: TDictionary<string, TOneMethodRtti> read FMethodList;
  end;

function ResultTypeIsGenericsCollections(qListType: TRttiType;
  var QResultType: emOneMethodResultType;
  var QItemResultType: emOneMethodResultType): boolean;
// 判断是不是List容器
function ResultTypeIsListCollections(qListType: TRttiType;
  var QResultType: emOneMethodResultType;
  var QItemResultType: emOneMethodResultType): boolean;
// 判断是不是map容器
function ResultTypeIsMapCollections(QMapType: TRttiType;
  var QResultType: emOneMethodResultType): boolean;


implementation

constructor TOneMethodRtti.Create();
begin
  inherited Create;
  FParamClassList := TList<TClass>.Create;
end;

destructor TOneMethodRtti.Destroy;
var
  i: integer;
begin
  for i := 0 to FParamClassList.Count - 1 do
  begin

  end;
  FParamClassList.Clear;
  FParamClassList.Free;
  inherited Destroy;
end;

constructor TOneControllerRtti.Create(QInterfaceTypeInfo: PTypeInfo);
begin
  inherited Create;
  FMethodList := TDictionary<string, TOneMethodRtti>.Create;
  self.GetMethodList(QInterfaceTypeInfo);
end;

function TOneControllerRtti.GetRttiMethod(QMethodName: string): TOneMethodRtti;
var
  lItem: TOneMethodRtti;
begin
  Result := nil;
  if FMethodList.TryGetValue(QMethodName.ToLower, lItem) then
  begin
    Result := lItem;
  end;
end;

destructor TOneControllerRtti.Destroy;
var
  lItem: TOneMethodRtti;
begin
  for lItem in FMethodList.Values do
  begin
    try
      if lItem.FRttiMethod <> nil then
      begin
        // 这边不用释放,底程RTTI会自行管理
        lItem.FRttiMethod := nil;
        // lItem.FRttiMethod.Free;
      end;
    except

    end;
    lItem.FRttiMethod := nil;
    lItem.Free;
  end;
  FMethodList.Clear;
  FMethodList.Free;
  inherited Destroy;
end;

class function TOneControllerRtti.GetRttiTypeToResultType(qRttiType: TRttiType;
  var QResultType: emOneMethodResultType;
  var QItemResultType: emOneMethodResultType): boolean;
var
  LMethodGetEnumerator, LMethodAdd, LMethodClear: TRttiMethod;
begin
  Result := False;
  QResultType := emOneMethodResultType.unknowResult;
  QItemResultType := emOneMethodResultType.unknowResult;
  case qRttiType.TypeKind of
    tkInteger, tkFloat, tkInt64:
    begin
      QResultType := emOneMethodResultType.numberResult;
    end;
    tkSString, tkLString, tkAString, tkWString, tkWChar, tkUString, tkUChar:
    begin
      QResultType := emOneMethodResultType.stringResult;
    end;
    tkClass:
    begin
      QResultType := emOneMethodResultType.objResult;
      if ResultTypeIsGenericsCollections(qRttiType, QResultType, QItemResultType) then
      else
      if ResultTypeIsListCollections(qRttiType, QResultType, QItemResultType) then;
    end;
    tkRecord:
    begin
      QResultType := emOneMethodResultType.recordResult;
    end;
    tkArray, tkDynArray:
    begin
      QResultType := emOneMethodResultType.arrayResult;
    end;
  end;
  Result := True;
end;

function TOneControllerRtti.IsGenericsCollections(qListType: TRttiType;
  QOneMethodRtti: TOneMethodRtti): boolean;
var
  lClassName: string;
begin
  Result := False;
  lClassName := qListType.AsInstance.MetaClassType.ClassName;
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
  if lClassName.StartsWith('tobjectlist<') then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsObjListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.objResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<tobject>' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.objResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<string>' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.stringResult;
    Result := True;
  end
  else
  if (lClassName = 'tlist<integer>') or (lClassName = 'tlist<longint>') then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.numberResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<int64>' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.numberResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<double>' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.numberResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<boolean>' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.boolResult;
    Result := True;
  end
  else
  if lClassName.StartsWith('tlist<') then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.genericsListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.objResult;
    Result := True;
  end;
end;

function TOneControllerRtti.IsListCollections(qListType: TRttiType;
  QOneMethodRtti: TOneMethodRtti): boolean;
var
  LMethodGetEnumerator, LMethodAdd, LMethodClear: TRttiMethod;
  LItemType: TRttiType;
  lClassName: string;
begin
  Result := False;
  lClassName := qListType.AsInstance.MetaClassType.ClassName;
  if lClassName.Contains('<') then
  begin
    exit;
  end;
  if lClassName.Contains('>') then
  begin
    exit;
  end;
  lClassName := lClassName.ToLower;
  if lClassName = 'tobjectList' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.objListResult;
    QOneMethodRtti.FResultCollectionsValueType := emOneMethodResultType.objResult;
    Result := True;
  end
  else
  if lClassName = 'tlist' then
  begin
    QOneMethodRtti.FResultType := emOneMethodResultType.listResult;
    Result := True;
  end;

end;

function TOneControllerRtti.IsMapCollections(QMapType: TRttiType;
  QOneMethodRtti: TOneMethodRtti): boolean;
var
  LKeyType, LValType: TRttiType;
  LKeyProp, LValProp: TRttiProperty;
  LAddMethod: TRttiMethod;
begin
  Result := False;
  if not QMapType.Name.Contains('<') then
  begin
    exit;
  end;
  if not QMapType.Name.Contains('>') then
  begin
    exit;
  end;
  LKeyProp := QMapType.GetProperty('Keys');
  if not Assigned(LKeyProp) then
    exit;

  LValProp := QMapType.GetProperty('Values');
  if not Assigned(LValProp) then
    exit;

  LAddMethod := QMapType.GetMethod('Add');
  if not Assigned(LAddMethod) or (Length(LAddMethod.GetParameters) <> 2) then
    exit;
  // 键类型
  LKeyType := LAddMethod.GetParameters[0].ParamType;
  LValType := LAddMethod.GetParameters[1].ParamType;
  case LKeyType.TypeKind of
    tkInteger, tkFloat, tkInt64:
    begin
      QOneMethodRtti.FResultCollectionsKeyType :=
        emOneMethodResultType.numberResult;
    end;
    tkString, tkAString, tkChar, tkLString, tkUChar, tkUString, tkVariant:
    begin
      QOneMethodRtti.FResultCollectionsKeyType :=
        emOneMethodResultType.stringResult;
    end;
    tkClass:
    begin
      QOneMethodRtti.FResultCollectionsKeyType :=
        emOneMethodResultType.objResult;
      // 有可以判断是不是泛型，不做这么多层处理
    end
  end;
  case LValType.TypeKind of
    tkInteger, tkFloat, tkInt64:
    begin
      QOneMethodRtti.FResultCollectionsValueType :=
        emOneMethodResultType.numberResult;
    end;
    tkString, tkAString, tkChar, tkLString, tkUChar, tkUString, tkVariant:
    begin
      QOneMethodRtti.FResultCollectionsValueType :=
        emOneMethodResultType.stringResult;
    end;
    tkClass:
    begin
      QOneMethodRtti.FResultCollectionsValueType :=
        emOneMethodResultType.objResult;
      // 有可以判断是不是泛型，不做这么多层处理
    end
  end;

  QOneMethodRtti.FResultType := emOneMethodResultType.mapResult;
  Result := True;
end;

procedure TOneControllerRtti.GetMethodList(QInterfaceTypeInfo: PTypeInfo);
var
  lMethodName: string;
  // vRttiType: TRttiType;
  lRttiMethods: TArray<TRttiMethod>;
  vRttiMethod: TRttiMethod;
  lParameters: TArray<TRttiParameter>;
  lParam: TRttiParameter;
  i, iMethod, iParam: integer;
  lOneMethodRtti: TOneMethodRtti;
  lVisibility: TMemberVisibility;
  tempStr:string;
begin
  FRttiContext := TRttiContext.Create;
  FRttiType := FRttiContext.GetType(QInterfaceTypeInfo);
  if FRttiType = nil then
    exit;
  lRttiMethods := FRttiType.GetDeclaredMethods;
  for iMethod := 0 to Length(lRttiMethods) - 1 do
  begin
    vRttiMethod := lRttiMethods[iMethod];
    if vRttiMethod = nil then
      continue;
    lMethodName := vRttiMethod.Name.ToLower;
    lVisibility := vRttiMethod.Visibility;
    //if not (vRttiMethod.Visibility = mvPublic) then
    //begin
    //  // 非公共方法
    //  continue;
    //end;
    if not (vRttiMethod.MethodKind in [mkProcedure, mkFunction]) then
    begin
      // 非方法函数
      continue;
    end;
    // vRttiMethod.Package.Name
    // 不把父级方法放出来
    //if vRttiMethod.Parent.Name <> QPersistentClass.ClassName then
    // continue;
    // 获取参数
    lParameters := vRttiMethod.GetParameters;
    lOneMethodRtti := TOneMethodRtti.Create;
    lOneMethodRtti.FErrMsg := '';
    lOneMethodRtti.FHttpMethodType := emOneHttpMethodMode.OneAll;
    lOneMethodRtti.FRttiMethod := vRttiMethod;
    lOneMethodRtti.FHaveClassParam := False;
    lOneMethodRtti.FResultRtti := vRttiMethod.ReturnType;
    lOneMethodRtti.FTypeInfo := QInterfaceTypeInfo;
    // 分析方法名称
    // emOneHttpMethodMode =(OneAll,OneGet,OnePost,OneForm, OneFile);
    if lMethodName.StartsWith('oneget') then
    begin
      lOneMethodRtti.FHttpMethodType := emOneHttpMethodMode.OneGet;
    end
    else if lMethodName.StartsWith('onepost') then
    begin
      lOneMethodRtti.FHttpMethodType := emOneHttpMethodMode.OnePost;
    end
    else if lMethodName.StartsWith('oneform') then
    begin
      lOneMethodRtti.FHttpMethodType := emOneHttpMethodMode.OneForm;
    end
    else if lMethodName.StartsWith('oneupload') then
    begin
      lOneMethodRtti.FHttpMethodType := emOneHttpMethodMode.OneUpload;
    end
    else if lMethodName.StartsWith('onedownload') then
    begin
      lOneMethodRtti.FHttpMethodType := emOneHttpMethodMode.OneDownload;
    end;
    // 分析方法或函数类型
    case vRttiMethod.MethodKind of
      mkProcedure:
        lOneMethodRtti.FMethodType := emOneMethodType.sysProcedure;
      mkFunction:
        lOneMethodRtti.FMethodType := emOneMethodType.sysFunction;
      else
        lOneMethodRtti.FMethodType := emOneMethodType.unknow;
    end;
    // 分析参数
    if Length(lParameters) = 2 then
    begin
      lParam := lParameters[0];
      tempStr := lParam.ParamType.ToString;
      if (vRttiMethod.GetParameters[0].ParamType.Name.ToLower = 'thttpctxt') and
        (vRttiMethod.GetParameters[1].ParamType.Name.ToLower = 'thttpresult') then
      begin
        lOneMethodRtti.FMethodType := emOneMethodType.resultProcedure;
      end;
    end;
    // 判断参数是不是合法类型
    for iParam := Low(lParameters) to High(lParameters) do
    begin
      lOneMethodRtti.FParamClassList.Add(nil);
      lParam := lParameters[iParam];
      case lParam.ParamType.TypeKind of
        tkInteger, tkFloat, tkInt64:
        begin
        end;
        tkString, tkAString, tkChar, tkLString, tkUChar, tkUString, tkVariant:
        begin
        end;
        tkClass:
        begin
          lOneMethodRtti.FParamClassList[iParam] :=
            lParam.ParamType.AsInstance.MetaclassType;
          lOneMethodRtti.FHaveClassParam := True;
        end;
        tkRecord:
        begin
          lOneMethodRtti.FHaveClassParam := True;
        end;
      end;
    end;
    // 分析返回值类型
    if lOneMethodRtti.FResultRtti <> nil then
    begin
      case lOneMethodRtti.FResultRtti.TypeKind of
        tkInteger, tkFloat, tkInt64:
        begin
          lOneMethodRtti.FResultType := emOneMethodResultType.numberResult;
        end;
        tkString, tkAString, tkChar, tkLString, tkUChar, tkUString, tkVariant:
        begin
          lOneMethodRtti.FResultType := emOneMethodResultType.stringResult;
        end;
        tkClass:
        begin
          lOneMethodRtti.FResultType := emOneMethodResultType.objResult;
          // 判断是否是List<T>泛型
          if IsGenericsCollections(lOneMethodRtti.FResultRtti, lOneMethodRtti) then
          // 判断是否是List容器
          else if IsListCollections(lOneMethodRtti.FResultRtti, lOneMethodRtti) then
          // 判断是不是字典容器
          else if IsMapCollections(lOneMethodRtti.FResultRtti, lOneMethodRtti) then
          else
          begin
          end;
        end;
        else
        begin
          lOneMethodRtti.FResultType := emOneMethodResultType.unknowResult;
        end;
      end;

    end;

    if not FMethodList.ContainsKey(lMethodName) then
    begin
      { 增加RTTI方法,参数,返回值,自定义属性 }
      FMethodList.Add(lMethodName, lOneMethodRtti);
    end
    else
    begin
      // 释放
      lOneMethodRtti.Free;
    end;
  end;
end;

function ResultTypeIsGenericsCollections(qListType: TRttiType;
  var QResultType: emOneMethodResultType;
  var QItemResultType: emOneMethodResultType): boolean;
var
  lClassName: string;
begin
  Result := False;
  lClassName := qListType.AsInstance.MetaClassType.ClassName;
  if not lClassName.Contains('<') then
  begin
    exit;
  end;
  if not lClassName.Contains('>') then
  begin
    exit;
  end;

  lClassName := lClassName.ToLower;
  if lClassName.StartsWith('tobjectList<') then
  begin
    QResultType := emOneMethodResultType.genericsObjListResult;
    QItemResultType := emOneMethodResultType.objResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<tobject>' then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.objResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<string>' then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.stringResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<integer>' then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.numberResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<int64>' then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.numberResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<double>' then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.numberResult;
    Result := True;
  end
  else
  if lClassName = 'tlist<boolean>' then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.boolResult;
    Result := True;
  end
  else
  if lClassName.StartsWith('tlist<') then
  begin
    QResultType := emOneMethodResultType.genericsListResult;
    QItemResultType := emOneMethodResultType.objResult;
    Result := True;
  end;
end;

// 判断是不是List容器
function ResultTypeIsListCollections(qListType: TRttiType;
  var QResultType: emOneMethodResultType;
  var QItemResultType: emOneMethodResultType): boolean;
var
  lClassName: string;
begin
  Result := False;
  lClassName := qListType.AsInstance.MetaClassType.ClassName;
  if lClassName.Contains('<') then
  begin
    exit;
  end;
  if lClassName.Contains('>') then
  begin
    exit;
  end;
  lClassName := lClassName.ToLower;
  if lClassName = 'tobjectList' then
  begin
    QResultType := emOneMethodResultType.objListResult;
    QItemResultType := emOneMethodResultType.objResult;
    Result := True;
  end
  else
  if lClassName = 'tlist' then
  begin
    QResultType := emOneMethodResultType.listResult;
    Result := True;
  end;
end;

// 判断是不是map容器
function ResultTypeIsMapCollections(QMapType: TRttiType;
  var QResultType: emOneMethodResultType): boolean;
begin

end;

end.
