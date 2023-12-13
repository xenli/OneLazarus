unit FastLoginController;

{$mode Delphi}{$H+}


interface

uses OneClientConnect, fpjson, OneClientResult;

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
    FErrMsg: string;
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
    property errMsg: string read FErrMsg write FErrMsg;
  end;

function Login(QLogin: TFastLogin): boolean;

implementation

uses OneSerialization, OneCrypto;

function Login(QLogin: TFastLogin): boolean;
var
  lConnection: TOneConnection;
  lLoginPass, lErrMsg: string;
  lJsonData: TJsonData;
  lActionResult: TActionResult<TFastLogin>;
begin
  Result := False;
  lConnection := OneClientConnect.Unit_Connection;
  if lConnection = nil then
  begin
    QLogin.errMsg := 'OneClientConnect.Unit_Connection未挂勾TOneConnection控件。';
    exit;
  end;
  lLoginPass := QLogin.FloginPass;
  try
    QLogin.FloginPass := OneCrypto.MD5Endcode(lLoginPass, False);
    lJsonData := lConnection.PostResulTJsonData('/FastClient/Login/Login', QLogin, lErrMsg);
    if lJsonData = nil then
    begin
      QLogin.errMsg := lErrMsg;
      exit;
    end;
    lActionResult := TActionResult<TFastLogin>.Create;
    try
      lActionResult.ResultDataT := QLogin;
      if not OneSerialization.JsonToObject(lActionResult, lJsonData, lErrMsg) then
      begin
        QLogin.errMsg := lErrMsg;
        exit;
      end;
      Result := lActionResult.ResultSuccess;
      if not Result then
        QLogin.errMsg := lActionResult.ResultMsg
      else
      begin
        lConnection.tokenID := lActionResult.ResultDataT.tokenID;
        lConnection.privateKey := lActionResult.ResultDataT.privateKey;
        if lConnection.tokenID = '' then
        begin
          Result := False;
          QLogin.errMsg := '请求成功但返回的TokenID为空,当前返回的数据:' + lJsonData.AsString;
          exit;
        end;
        QLogin.tokenID := lConnection.tokenID;
      end;
    finally
      lJsonData.Free;
      lActionResult.Free;
    end;
  finally
    QLogin.FloginPass := lLoginPass;
  end;
end;

end.
