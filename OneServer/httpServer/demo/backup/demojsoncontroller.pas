unit DemoJsonController;

{$mode DELPHI}{$H+}
interface

uses
  OneHttpController, OneHttpRouterManage, SysUtils, Classes, Rtti, fpjson, jsonparser;

type
  {$M+}
  IDemoJsonController = interface
    ['{26163650-43D9-4079-9418-262EF3F61F9E}']
    function GetJsonObject(): TJsonObject;
    function GetJsonArray(): TJsonArray;
    function GetJsonParam(QJsonObj: TJsonObject): string;
  end;

  {$M-}
  TDemoJsonController = class(TOneControllerBase, IDemoJsonController)
  public
    // override标识很重要
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
      override;
    // 返回结果 {"name":"flm0"}
    function GetJsonObject(): TJsonObject;
    // 返回结果 [{"name":"flm0"},{"name":"flm1"},{"name":"flm2"}]
    function GetJsonArray(): TJsonArray;

    // 最好是 TJsonValue做参数,如果传进的是一个数组也能正常接收
    // 然后在逻辑判断是JSONOBJECT还是JSONARRAY
    function GetJsonParam(QJsonObj: TJsonObject): string;
  end;

function CreateNewDemoJsonController(QRouterItem: TOneRouterItem): TObject;

implementation

function CreateNewDemoJsonController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TDemoJsonController;
begin
  Result := nil;
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TDemoJsonController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TDemoJsonController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lDemoController: IDemoJsonController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lDemoController := self as IDemoJsonController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lDemoController, aArgs);
  exit;
  {$endif}

  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'GetJsonObject' then
  begin
    Result := self.GetJsonObject();
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

function TDemoJsonController.GetJsonObject(): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('name', 'flm');
end;

function TDemoJsonController.GetJsonArray(): TJsonArray;
var
  lJsonObj: TJsonObject;
  i: integer;
begin
  Result := TJsonArray.Create;
  for i := 0 to 9 do
  begin
    lJsonObj := TJsonObject.Create;
    lJsonObj.Add('name', 'flm' + i.ToString);
    Result.Add(lJsonObj);
  end;
end;

function TDemoJsonController.GetJsonParam(QJsonObj: TJsonObject): string;
begin
  Result := QJsonObj.ToString;
end;

// 注册到路由
initialization

  // 注意，路由名称 不要一样，否则会判定已注册过，跳过
  // 多例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPPoolWork('DemoJson',
    TDemoJsonController, TypeInfo(IDemoJsonController), 10, CreateNewDemoJsonController);

finalization

end.
