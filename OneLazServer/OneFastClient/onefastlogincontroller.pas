unit OneFastLoginController;

{$mode DELPHI}{$H+}

interface


uses
  Classes, SysUtils,
  OneHttpController, OneHttpRouterManage, OneHttpCtxtResult,
  Generics.Collections, OneControllerResult, OneTokenManage, OneZTManage, Rtti,
  OneCrypto;

type

  TFastLogin = class
  private
    FloginCode: string; // 登陆代码
    FloginPass: string; // 登陆密码
    FloginZTCode: string; // 指定登陆账套
    FSecretkey: string;
    FtokenID: string; // 返回去的TokenID
    FprivateKey: string; // 返回去的私钥
    FAdminID: string; // 返回去的用户名称
    FAdminCode: string;
    FAdminName: string;
    // 如果有其它信息自已加，本示例只是个demo
  published
    // 前后端参数及返回结果大写小请保证一至 ,不要问我为什么JSON是区分大小写的,
    property loginCode: string read FloginCode write FloginCode;
    property loginPass: string read FloginPass write FloginPass;
    property loginZTCode: string read FloginZTCode write FloginZTCode;
    property secretkey: string read FSecretkey write FSecretkey;
    property tokenID: string read FtokenID write FtokenID;
    property privateKey: string read FprivateKey write FprivateKey;
    property adminID: string read FAdminID write FAdminID;
    property adminCode: string read FAdminCode write FAdminCode;
    property adminName: string read FAdminName write FAdminName;
  end;

   {$M+}
  IFastLoginController = interface
    ['{2DDB4418-9AF3-41BE-8314-0544AE24F14A}']
    //登陆接口
    function Login(QLogin: TFastLogin): TActionResultObject;
    // 登出接口
    function LoginOut(QLogin: TFastLogin): TActionResultString;

    function LoginOutToken(): TActionResultString;
  end;

  {$M-}
  TFastLoginController = class(TOneControllerBase, IFastLoginController)
  public
    //各个方法实现
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue; override;
    //登陆接口
    function Login(QLogin: TFastLogin): TActionResultObject;
    // 登出接口
    function LoginOut(QLogin: TFastLogin): TActionResultString;

    function LoginOutToken(): TActionResultString;
  end;

function CreateNewFastLoginController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal;

function CreateNewFastLoginController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TFastLoginController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TFastLoginController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TFastLoginController.DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
var
  lIController: IFastLoginController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lIController := self as IFastLoginController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lIController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'login' then
  begin
    Result := self.Login(TFastLogin(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'loginout' then
  begin
    Result := self.LoginOut(TFastLogin(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'loginouttoken' then
  begin
    Result := self.LoginOutToken();
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;

end;


//登陆接口
function TFastLoginController.Login(QLogin: TFastLogin): TActionResultObject;
var
  lOneGlobal: TOneGlobal;
  lOneZTMange: TOneZTManage;
  lOneTokenManage: TOneTokenManage;
  lZTItem: TOneZTItem;
  lFDQuery: TFDQuery;
  lOneTokenItem: TOneTokenItem;
  lErrMsg: string;
  lNow: TDateTime;
  lFastLogin:TFastLogin;
begin
  Result := TActionResultObject.Create(True, False);
  lOneGlobal := TOneGlobal.GetInstance();
  lErrMsg := '';
  if QLogin.loginCode = '' then
  begin
    Result.resultMsg := '用户代码不可为空';
    exit;
  end;
  if QLogin.loginPass = '' then
  begin
    Result.resultMsg := '用户密码不可为空';
    exit;
  end;
  // if lOneGlobal.ServerSet.ConnectSecretkey <> QLogin.secretkey then
  // begin
  // // 安全密钥不一至
  // result.resultMsg := '安全密钥不一至,无法连接服务端!!!';
  // exit;
  // end;
  // 判断是不是超级管理员
  if QLogin.loginCode = 'SuperAdmin' then
  begin
    // 超级管理员
    if QLogin.loginPass.ToLower = OneCrypto.MD5Endcode(lOneGlobal.ServerSet.SuperAdminPass, False) then
    begin
      // 正确增加Token返回相关的toeknID及私钥
      lOneTokenManage := TOneGlobal.GetInstance().TokenManage;
      // true允许同个账号共用token,测试接口共享下防止踢来踢去
      lOneTokenItem := lOneTokenManage.AddLoginToken('fastClient', QLogin.loginCode, True, lErrMsg);
      if lOneTokenItem = nil then
      begin
        Result.resultMsg := lErrMsg;
        exit;
      end;
      // 为Token设置相关信息
      lOneTokenItem.LoginUserCode := QLogin.loginCode;
      lOneTokenItem.ZTCode := QLogin.loginZTCode; // 指定账套
      lOneTokenItem.SysUserID := QLogin.loginCode;
      lOneTokenItem.SysUserName := '超级管理员';
      lOneTokenItem.SysUserType := '超级管理员';
      lOneTokenItem.SysUserCode := QLogin.loginCode;
      // 返回信息设置
      lFastLogin :=TFastLogin.Create;
      Result.resultData := lFastLogin;
      lFastLogin.loginCode := QLogin.loginCode;
      lFastLogin.tokenID := lOneTokenItem.tokenID;
      lFastLogin.privateKey := lOneTokenItem.privateKey;

      lFastLogin.adminID := QLogin.loginCode;
      lFastLogin.adminCode := QLogin.loginCode;
      lFastLogin.adminName := '超级管理员';

      Result.SetResultTrue;
      exit;
    end;

  end;
  lOneGlobal.Log.WriteLog('Login', '2');
  // 验证账号密码,比如数据库
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  // 账套为空时,默认取主账套,多账套的话可以固定一个账套代码
  lZTItem := lOneZTMange.LockZTItem(QLogin.loginZTCode, lErrMsg);
  if lZTItem = nil then
  begin
    Result.resultMsg := lErrMsg;
    exit;
  end;
  lOneGlobal.Log.WriteLog('Login', '3');
  try
    // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
    lFDQuery := lZTItem.ADQuery;
    // 这边改成你的用户表
    lFDQuery.SQL.Text := 'select FAdminID,FAdminCode,FAdminName,FAdminPass,FAdminType,FIsEnable,FIsLimit,FLimtStartTime,FLimtEndTime,FIsMultiLogin ' + ' from onefast_admin where FAdminCode=:FAdminCode';
    lFDQuery.Params[0].AsString := QLogin.loginCode;
    lFDQuery.Open;
    lOneGlobal.Log.WriteLog('Login', '4');
    if lFDQuery.RecordCount = 0 then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode + ']不存在,请检查';
      exit;
    end;
    if lFDQuery.RecordCount > 1 then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode + ']重复,请联系管理员检查数据';
      exit;
    end;
    if not lFDQuery.FieldByName('FIsEnable').AsBoolean then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode + ']未启用,请联系管理员启用';
      exit;
    end;
    if lFDQuery.FieldByName('FIsLimit').AsBoolean then
    begin

      lNow := now;
      if lNow >= lFDQuery.FieldByName('FLimtEndTime').AsDateTime then
      begin
        Result.resultMsg := '当前用户[' + QLogin.loginCode + ']已到期,请联系管理员';
        exit;
      end;
      if lNow < lFDQuery.FieldByName('FLimtStartTime').AsDateTime then
      begin
        Result.resultMsg := '当前用户[' + QLogin.loginCode + ']限期开始时间' + FormatDateTime('yyyy-MM-dd hh:mm:ss', lFDQuery.FieldByName('FLimtStartTime').AsDateTime) + ',请联系管理员';
        exit;
      end;
      // 判断限时
      Result.resultMsg := '当前用户[' + QLogin.loginCode + ']未启用,请联系管理员启用';
      exit;
    end;
    lOneGlobal.Log.WriteLog('Login', '5');
    // 为一条时要验证密码,前端一般是MD5加密的,后端也是保存MD5加密的
    if QLogin.loginPass.ToLower <> lFDQuery.FieldByName('FAdminPass').AsString.ToLower then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode + ']密码不正确,请检查';
      exit;
    end;
    // 正确增加Token返回相关的toeknID及私钥
    lOneTokenManage := TOneGlobal.GetInstance().TokenManage;
    // true允许同个账号共用token,测试接口共享下防止踢来踢去
    lOneTokenItem := lOneTokenManage.AddLoginToken('fastClient', QLogin.loginCode, lFDQuery.FieldByName('FIsMultiLogin').AsBoolean, lErrMsg);
    if lOneTokenItem = nil then
    begin
      Result.resultMsg := lErrMsg;
      exit;
    end;
    lOneGlobal.Log.WriteLog('Login', '6');
    // 为Token设置相关信息
    lOneTokenItem.LoginUserCode := QLogin.loginCode;
    lOneTokenItem.ZTCode := QLogin.loginZTCode; // 指定账套
    lOneTokenItem.SysUserID := lFDQuery.FieldByName('FAdminID').AsString;
    lOneTokenItem.SysUserName := lFDQuery.FieldByName('FAdminName').AsString;
    lOneTokenItem.SysUserCode := lFDQuery.FieldByName('FAdminCode').AsString;
    lOneTokenItem.SysUserType := lFDQuery.FieldByName('FAdminType').AsString;
    // 返回信息设置
    lFastLogin := TFastLogin.Create;
    Result.resultData := lFastLogin;
    lFastLogin.loginCode := QLogin.loginCode;
    lFastLogin.tokenID := lOneTokenItem.tokenID;
    lFastLogin.privateKey := lOneTokenItem.privateKey;
    lFastLogin.adminID := lFDQuery.FieldByName('FAdminID').AsString;
    lFastLogin.adminCode := lFDQuery.FieldByName('FAdminCode').AsString;
    lFastLogin.adminName := lFDQuery.FieldByName('FAdminName').AsString;

    Result.SetResultTrue;
    lOneGlobal.Log.WriteLog('Login', '7');
  finally
    // 解锁,归还池很重要
    lZTItem.UnLockWork;
    lOneGlobal.Log.WriteLog('Login', '8');
  end;
end;
// 登出接口
function TFastLoginController.LoginOut(QLogin: TFastLogin): TActionResultString;
var
  lOneGlobal: TOneGlobal;
begin
  result := TActionResultString.Create();
  if QLogin.tokenID = '' then
  begin
    result.resultMsg := 'tokenID为空请上传tokenID';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.TokenManage.RemoveToken(QLogin.tokenID);
  result.resultData := 'Token删除成功';
  result.SetResultTrue();
end;

function TFastLoginController.LoginOutToken(): TActionResultString;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
  lToken: TOneTokenItem;
begin
  result := TActionResultString.Create();
  lToken := self.GetCureentToken(lErrMsg);
  if lToken = nil then
  begin
    result.resultMsg := lErrMsg;
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.TokenManage.RemoveToken(lToken.tokenID);
  result.resultData := 'Token删除成功';
  result.SetResultTrue();
end;


initialization

  // 单例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork('/FastClient/Login',
    TFastLoginController, TypeInfo(IFastLoginController), 0, CreateNewFastLoginController);

finalization

end.
