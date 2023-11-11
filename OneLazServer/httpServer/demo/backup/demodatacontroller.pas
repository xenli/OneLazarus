unit DemoDataController;

{$mode DELPHI}{$H+}

// 结果返回是jsonobject,jsonArray事例
interface


uses OneHttpController, OneHttpCtxtResult, OneHttpRouterManage, SysUtils,
  Generics.Collections, Contnrs, Classes, DB, BufDataset, Rtti, OneDataJson, memds;

type
  {$M+}
  IDemoDataController = interface
    ['{289F0C31-F626-4799-8330-2325611B5BFD}']
    function GetData(): TBufDataSet;
  end;

  {$M-}
  TDemoDataController = class(TOneControllerBase, IDemoDataController)
  public
    // override标识很重要
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
      override;
    // 返回结果 [{"name":"flm0"},{"name":"flm1"},{"name":"flm2"}]
    function GetData(): TBufDataSet;
  end;

function CreateNewDemoDataController(QRouterItem: TOneRouterItem): TObject;

implementation

function CreateNewDemoDataController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TDemoDataController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TDemoDataController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TDemoDataController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lDemoController: IDemoDataController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //转化成接口
  lDemoController := self as IDemoDataController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lDemoController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'GetData' then
  begin
    Result := self.GetData();
  end
  else
  if lMethodName = 'GetJsonArray' then
  begin
    Result := self.GetJsonArray();
  end
  else
  if lMethodName = 'GetJsonParam' then
  begin
    Result := self.GetJsonParam(TJsonObject(aArgs[0].AsObject));
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;
end;

function TDemoDataController.GetData(): TBufDataSet;
var
  i: integer;
begin
  Result := TBufDataSet.Create(nil);
  Result.FieldDefs.Add('name', ftString, 20, False);
  Result.FieldDefs.Add('age', ftInteger, 0, False);
  Result.CreateDataSet();
  for i := 0 to 9 do
  begin
    Result.Append;
    Result.FieldByName('name').AsString := 'flm' + i.ToString();
    Result.FieldByName('age').AsInteger := i;
    Result.Post;
  end;
end;

// 注册到路由
initialization

  // 注意，路由名称 不要一样，否则会判定已注册过，跳过
  // 多例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPPoolWork('DemoData',
    TDemoDataController, TypeInfo(IDemoDataController), 10, CreateNewDemoDataController);

finalization

end.
