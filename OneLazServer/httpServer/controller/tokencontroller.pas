unit TokenController;

{$mode DELPHI}{$H+}
interface

uses
  Classes, SysUtils,
  OneHttpController, OneHttpRouterManage, OneHttpCtxtResult, OneTokenManage,
  Generics.Collections, OneControllerResult, Rtti;

type

  TClientConnect = class
  private
    FConnectSecretkey: string;
    FClientIP: string; // 客户端传上来的客户端IP
    FClientMac: string; // 客户端传上来的客户端Mac地址
    FTokenID: string; // 服务端返回的TokenID
    FPrivateKey: string; // 服务端返回的私钥
  published
    property ConnectSecretkey: string read FConnectSecretkey write FConnectSecretkey;
    property ClientIP: string read FClientIP write FClientIP;
    property ClientMac: string read FClientMac write FClientMac;
    property TokenID: string read FTokenID write FTokenID;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
  end;

  TClientLogin = class

  private
    FTokenID: string;
    // ClientConnect返回的tokenID如果有先进行 ClientConnect, 有的话进行绑定
    FLoginUserCode: string;
    FLoginPass: string;
  published
    property TokenID: string read FTokenID write FTokenID;
    // ClientConnect返回的tokenID如果有先进行 ClientConnect, 有的话进行绑定
    property LoginUserCode: string read FLoginUserCode write FLoginUserCode;
    property LoginPass: string read FLoginPass write FLoginPass;
  end;

  {$M+}
  IOneTokenController = interface
    ['{8C64E597-56D8-4B72-B213-640950A2C464}']
    function ClientConnect(QCleintConnect: TClientConnect): TActionResultObject;
    function ClientDisConnect(TokenID: string): TActionResultstring;
    function ClientPing(): TActionResultstring;
    function ClientConnectPing(QCleintConnect: TClientConnect): TActionResultstring;
  end;

  {$M-}
  TOneTokenController = class(TOneControllerBase, IOneTokenController)
  public
    //各个方法实现
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue; override;
    function ClientConnect(QCleintConnect: TClientConnect): TActionResultObject;
    function ClientDisConnect(TokenID: string): TActionResultstring;
    function ClientPing(): TActionResultstring;
    function ClientConnectPing(QCleintConnect: TClientConnect): TActionResultstring;
  end;

function CreateNewOneTokenController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal;

function CreateNewOneTokenController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TOneTokenController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TOneTokenController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TOneTokenController.DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
var
  lIController: IOneTokenController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lIController := self as IOneTokenController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lIController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'ClientConnect' then
  begin
    Result := self.ClientConnect(TClientConnect(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'ClientDisConnect' then
  begin
    Result := self.ClientDisConnect(aArgs[0].AsString);
  end
  else
  if lMethodName = 'ClientPing' then
  begin
    Result := self.ClientPing();
  end
  else
  if lMethodName = 'ClientConnectPing' then
  begin
    Result := self.ClientConnectPing(TClientConnect(aArgs[0].AsObject));
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;

end;

function TOneTokenController.ClientConnect(QCleintConnect: TClientConnect): TActionResultObject;
var
  lOneGlobal: TOneGlobal;
  lTokenItem: TOneTokenItem;
  lList: TList<TOneTokenItem>;
  i: integer;
  lClientConnect: TClientConnect;
begin
  // 第一个参数true代表 Result.ResultData用完要释放
  Result := TActionResultObject.Create(True, False);
  lOneGlobal := TOneGlobal.GetInstance();
  if lOneGlobal.ServerSet.ConnectSecretkey <> QCleintConnect.ConnectSecretkey then
  begin
    // 安全密钥不一至
    Result.ResultMsg := '安全密钥不一至,无法连接服务端!!!';
    exit;
  end;
  lTokenItem := lOneGlobal.TokenManage.AddConnectToken();
  lTokenItem.LoginIP := QCleintConnect.ClientIP;
  lTokenItem.LoginMac := QCleintConnect.ClientMac;
  lClientConnect := TClientConnect.Create;
  Result.ResultData := lClientConnect;
  lClientConnect.TokenID := lTokenItem.TokenID;
  lClientConnect.PrivateKey := lTokenItem.PrivateKey;
  lClientConnect.ConnectSecretkey := '';
  Result.SetResultTrue();
end;

function TOneTokenController.ClientDisConnect(TokenID: string): TActionResultString;
var
  lOneGlobal: TOneGlobal;
begin
  Result := TActionResultString.Create;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.TokenManage.RemoveToken(TokenID);
  Result.ResultData := 'Token删除成功';
  Result.SetResultTrue();
end;

function TOneTokenController.ClientPing(): TActionResultString;
begin
  Result := TActionResultString.Create;
  Result.ResultData := '';
  Result.SetResultTrue();
end;

function TOneTokenController.ClientConnectPing(QCleintConnect: TClientConnect): TActionResultstring;
var
  lOneGlobal: TOneGlobal;
begin
  result := TActionResultstring.Create();
  lOneGlobal := TOneGlobal.GetInstance();
  if lOneGlobal.ServerSet.ConnectSecretkey <> QCleintConnect.ConnectSecretkey then
  begin
    // 安全密钥不一至
    result.ResultMsg := '安全密钥不一至,无法连接服务端!!!';
    exit;
  end;
  result.ResultData := '';
  result.SetResultTrue();
end;

initialization

  // 单例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork('/OneServer/Token',
    TOneTokenController, TypeInfo(IOneTokenController), 0, CreateNewOneTokenController);

finalization

end.
