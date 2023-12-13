unit OneFastApiController;

{$mode DELPHI}{$H+}
interface

uses
  SysUtils, Classes, fpjson, jsonparser, Rtti,
  OneHttpController, OneHttpRouterManage, OneTokenManage,
  OneControllerResult;

type
  {$M+}
  IFastApiController = interface
    ['{FB237DDC-EB31-4B87-9A6D-0BAE53583853}']
    function DoFastApi(QPostJson: TJsonObject): string;
    function RefreshApiInfo(QApiCode: string): TActionResultString;
    function RefreshApiInfoAll(): TActionResultString;
  end;

  {$M-}

  TFastApiController = class(TOneControllerBase, IFastApiController)
  public
    //各个方法实现
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue; override;
    function DoFastApi(QPostJson: TJsonObject): string;
    function RefreshApiInfo(QApiCode: string): TActionResultString;
    function RefreshApiInfoAll(): TActionResultString;
  end;

implementation

uses OneGlobal, OneFastApiManage, OneFastApiDo;

function CreateNewFastApiController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TFastApiController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TFastApiController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;


function TFastApiController.DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
var
  lIController: IFastApiController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lIController := self as IFastApiController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lIController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'DoFastApi'.ToLower() then
  begin
    Result := self.DoFastApi(TJsonObject(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'RefreshApiInfo'.ToLower() then
  begin
    Result := self.RefreshApiInfo(aArgs[0].AsString);
  end
  else
  if lMethodName = 'RefreshApiInfoAll'.ToLower() then
  begin
    Result := self.RefreshApiInfoAll();
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;

end;

function TFastApiController.DoFastApi(QPostJson: TJsonObject): string;
var
  lTokenItem: TOneTokenItem;
  lErrMsg: string;
  lJsonValue: TJsonData;
begin
  lTokenItem := nil;
  lJsonValue := nil;
  try
    lTokenItem := self.GetCureentToken(lErrMsg);
    lJsonValue := OneFastApiDo.DoFastApi(lTokenItem, QPostJson);
    try
      Result := lJsonValue.tostring;
    finally
      if lJsonValue <> nil then
      begin
        lJsonValue.Free;
      end;
    end;
  except
    on e: Exception do
    begin
      Result := '发生异常,异常消息[' + e.Message + ']';
    end;
  end;
end;

function TFastApiController.RefreshApiInfo(QApiCode: string): TActionResultString;
var
  lApiManage: TOneFastApiManage;
  lErrMsg: string;
begin
  Result := TActionResultString.Create;
  lApiManage := OneFastApiManage.UnitFastApiManage();
  if not lApiManage.RefreshApiInfo(QApiCode, lErrMsg) then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  Result.ResultData := '刷新FastApi模板[' + QApiCode + ']板信息成功';
  Result.SetResultTrue;
end;

function TFastApiController.RefreshApiInfoAll(): TActionResultString;
var
  lApiManage: TOneFastApiManage;
  lErrMsg: string;
begin
  Result := TActionResultString.Create;
  lApiManage := OneFastApiManage.UnitFastApiManage();
  if not lApiManage.RefreshApiInfoAll(lErrMsg) then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  Result.ResultData := '刷新FastApi模板信息成功';
  Result.SetResultTrue;
end;

initialization
  // 单例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork('OneServer/FastApi',
    TFastApiController, TypeInfo(IFastApiController), 0, CreateNewFastApiController);

finalization

end.
