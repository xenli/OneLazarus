unit OneClientConnect;

 {$mode DELPHI}{$H+}

interface

uses
  Classes, OneClientConst, SysUtils, Variants, ZLib, DateUtils,
  Generics.Collections, DB, fpjson, jsonparser, URIParser, OneSerialization,
  TypInfo, OneClientResult, OneClientDataInfo, OneStreamString,
  OneSQLCrypto, ZStream, Math, fphttpclient, BufDataset, Zipper, openSSl;

const
  HTTP_URL_TokenName = 'token';
  HTTP_URL_TokenTime = 'time';
  HTTP_URL_TokenSign = 'sign';
  // 常用函数交互
  URL_HTTP_HTTPServer_TOKEN_ClientConnect = 'OneServer/Token/ClientConnect';
  URL_HTTP_HTTPServer_TOKEN_ClientConnectPing = 'OneServer/Token/ClientConnectPing';
  URL_HTTP_HTTPServer_TOKEN_ClientPing = 'OneServer/Token/ClientPing';
  URL_HTTP_HTTPServer_TOKEN_ClientDisConnect = 'OneServer/Token/ClientDisConnect';

  URL_HTTP_HTTPServer_DATA_OpenDatas = 'OneServer/Data/OpenDatas';
  URL_HTTP_HTTPServer_DATA_SaveDatas = 'OneServer/Data/SaveDatas';
  URL_HTTP_HTTPServer_DATA_ExecStored = 'OneServer/Data/ExecStored';
  URL_HTTP_HTTPServer_DATA_DownLoadDataFile = 'OneServer/Data/DownLoadDataFile';
  URL_HTTP_HTTPServer_DATA_DelDataFile = 'OneServer/Data/DelDataFile';
  // 二层事务先关事件
  URL_HTTP_HTTPServer_DATA_LockTranItem = 'OneServer/Data/LockTranItem';
  URL_HTTP_HTTPServer_DATA_UnLockTranItem = 'OneServer/Data/UnLockTranItem';
  URL_HTTP_HTTPServer_DATA_StartTranItem = 'OneServer/Data/StartTranItem';
  URL_HTTP_HTTPServer_DATA_CommitTranItem = 'OneServer/Data/CommitTranItem';
  URL_HTTP_HTTPServer_DATA_RollbackTranItem = 'OneServer/Data/RollbackTranItem';

  // 文件相关上传下载
  URL_HTTP_HTTPServer_DATA_UploadFile = 'OneServer/VirtualFile/UploadFile';
  URL_HTTP_HTTPServer_DATA_DownloadFile = 'OneServer/VirtualFile/DownloadFile';
  URL_HTTP_HTTPServer_DATA_GetTaskID = 'OneServer/VirtualFile/GetTaskID';
  URL_HTTP_HTTPServer_DATA_UploadChunkFile = 'OneServer/VirtualFile/UploadChunkFile';
  URL_HTTP_HTTPServer_DATA_DownloadChunkFile = 'OneServer/VirtualFile/DownloadChunkFile';

type
  OneContentType = (ContentTypeText, ContentTypeHtml, ContentTypeStream, ContentTypeZip);

  TOneResultBytes = class
  private
    FIsOK: boolean;
    FBytes: TBytes;
    FContentType: string;
    FIsFile: boolean;
    FErrMsg: string;
  public
    constructor Create();
    destructor Destroy; override;
  public
    property IsOK: boolean read FIsOK write FIsOK;
    property Bytes: TBytes read FBytes write FBytes;
    property ContentType: string read FContentType write FContentType;
    property IsFile: boolean read FIsFile write FIsFile;
    property ErrMsg: string read FErrMsg write FErrMsg;
  end;


  TOneConnection = class(TComponent)
  private
    // 服务端是否正常
    FConnected: boolean;
    // 客户端IP
    FClientIP: string;
    // 客户端MAC地址
    FClientMac: string;

    FIsHttps: boolean;
    FHTTPHost: string;
    FHTTPPort: integer;
    // 安全密钥
    FConnectSecretkey: string;
    FToKenID: string;
    FPrivateKey: string;
    // HTTP请求超时设
    FConnectionTimeout: integer;
    FResponseTimeout: integer;
    // 全局账套代码，如果数据集有取数据集的,没有取全局的
    FZTCode: string;
    FErrMsg: string;
  private
    // 组装URL
    function MakeUrl(var QUrl: string; var QErrMsg: string): boolean;
    procedure SetErrTrueResult(var QErrMsg: string);
    function IsErrTrueResult(QErrMsg: string): boolean;
    // 获取最终状态
    function GetZTCode(QDataSetZTCode: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    function DoConnect(qForceConnect: boolean = False): boolean;
    function DoConnectPing(): boolean;
    function DoPing(): boolean;
    procedure DisConnect; overload;
    // 提交bytes返回Bytes
    function PostResultBytes(const QUrl: string; QPostDataBtye: TBytes): TOneResultBytes;
      overload;
    function PostResultBytes(const QUrl: string; QPostData: string): TOneResultBytes;
      overload;
    // 提交字符串，返回JSONValue
    function PostResultJsonData(const QUrl: string; QPostData: string; var QErrMsg: string): TJsonData; overload;
    // 提交Bytes，返回JSONValue
    function PostResultJsonData(const QUrl: string; QPostData: TBytes; var QErrMsg: string): TJsonData; overload;
    //提交对象,返回JsonValue
    function PostResultJsonData(const QUrl: string; QObject: TObject; var QErrMsg: string): TJsonData; overload;

    function PostResultContent(const QUrl: string; QPostData: string; var QResultData: string): boolean;
    // Get相关事件
    function GetResultBytes(const QUrl: string; var QContentType: string; var QErrMsg: string): TBytes;
    function GetResulTJsonData(const QUrl: string; var QErrMsg: string): TJsonData;
    function GetResultContent(const QUrl: string; var QResultData: string): boolean;
  public
    procedure DataSetToOpenData(Sender: TObject; QDataOpen: TOneDataOpen);
    // 跟据OneDataSet打开数据集
    function OpenData(Sender: TObject): boolean;
    // 跟据List<OneDataSet>打开数据集
    function OpenDatas(QObjectList: TList<TObject>; var QErrMsg: string): boolean;
      overload;
    // 以文件流方式打开数据
    function DownLoadDataFile(QFileID: string; var QErrMsg: string): TMemoryStream;
    // 跟据List<TOneDataOpen(数据集信息收集)>打开数据集
    function OpenDatasPost(QDataOpens: TList<TOneDataOpen>): TOneDataResult; overload;

    function ExecStored(Sender: TObject): boolean;
    // 执行存储过程
    function ExecStoredPost(QDataOpen: TOneDataOpen): TOneDataResult;
    // 跟据dataSet保存数据
    function SaveData(Sender: TObject): boolean; overload;
    // 跟据List<OneDataSet>打开数据集
    function SaveDatas(QObjectList: TList<TObject>; var QErrMsg: string): boolean;
    function SaveDatasPost(QSaveDMLDatas: TList<TOneDataSaveDML>): TOneDataResult;
      overload;
    // 把返回的结构转化成dataset
    function DataResultToDataSets(DataResult: TOneDataResult; QObjectList: TList<TObject>; QIsSave: boolean; var QErrMsg: string): boolean;
    function DataResultToDataSet(QDataResult: TOneDataResult; QObject: TObject; var QErrMsg: string): boolean;

    // 文件上传下载
    function UploadFile(QVirtualInfo: TVirtualInfo): boolean;
    function DownloadFile(QVirtualInfo: TVirtualInfo): boolean;
    function GetTaskID(QVirtualTask: TVirtualTask): boolean;
    function UploadChunkFile(QVirtualTask: TVirtualTask; QUpDownChunkCallBack: EvenUpDownChunkCallBack): boolean;
    function DownloadChunkFile(QVirtualTask: TVirtualTask; QUpDownChunkCallBack: EvenUpDownChunkCallBack): boolean;
    // *********二层事务自由控制***********
    /// <summary>
    /// 事务控制第一步:获取账套连接,标识成事务账套
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function LockTran(QTranInfo: TOneTran): boolean;
    // 2.用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
    function UnLockTran(QTranInfo: TOneTran): boolean;
    // 3.开启账套连接事务
    function StartTran(QTranInfo: TOneTran): boolean;
    // 4.提交账套连接事务
    function CommitTran(QTranInfo: TOneTran): boolean;
    // 5.回滚账套连接事务
    function RollbackTran(QTranInfo: TOneTran): boolean;
  published
    /// <param name="Connected">DoConnect连接成功的标识,就是一开始确定服务端HTTP连接是否正常</param>
    property Connected: boolean read FConnected;
    /// <param name="ClientIP">DoConnect连接或代上此参数,请自行赋值在连接前，可以为空</param>
    property ClientIP: string read FClientIP write FClientIP;
    /// <param name="ClientMac">DoConnect连接或代上此参数,请自行赋值在连接前，可以为空</param>
    property ClientMac: string read FClientMac write FClientMac;
    /// <param name="IsHttps">是否HTTP访问</param>
    property IsHttps: boolean read FIsHttps write FIsHttps;
    /// <param name="HTTPHost">服务端地址或域名</param>
    property HTTPHost: string read FHTTPHost write FHTTPHost;
    /// <param name="HTTPPort">服务端端口</param>
    property HTTPPort: integer read FHTTPPort write FHTTPPort;
    /// <param name="ConnectSecretkey">服务端连接安全秘钥,DoConnect时需要</param>
    property ConnectSecretkey: string read FConnectSecretkey write FConnectSecretkey;
    /// <param name="TokenID">DoConnect连接成功后返回的tokenID</param>
    property TokenID: string read FToKenID write FToKenID;
    /// <param name="PrivateKey">DoConnect连接成功后返回的PrivateKey</param>
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    /// <param name="ConnectionTimeout">连接超时时间</param>
    property ConnectionTimeout: integer read FConnectionTimeout write FConnectionTimeout;
    /// <param name="ResponseTimeout">请求结果超时时间</param>
    property ResponseTimeout: integer read FResponseTimeout write FResponseTimeout;
    /// <param name="ZTCode">连接账套,如果数据集有账套优先取数据集的</param>
    property ZTCode: string read FZTCode write FZTCode;
    /// <param name="ErrMsg">错误信息存放</param>
    property ErrMsg: string read FErrMsg write FErrMsg;
  end;



var
  Unit_Connection: TOneConnection = nil;

procedure Register;

implementation

uses OneClientDataSet, OneFileHelper, OneCrypto;

procedure Register;
begin
  RegisterComponents('OneClient', [TOneConnection]);
end;

constructor TOneResultBytes.Create();
begin
  inherited Create();
  self.FIsOK := False;
  self.FErrMsg := '';
  self.FContentType := '';
end;

destructor TOneResultBytes.Destroy;
begin
  self.FBytes := nil;
  inherited Destroy;
end;

constructor TOneConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

// 连接服务端获取Token
function TOneConnection.DoConnect(qForceConnect: boolean = False): boolean;
var
  lJsonObj: TJsonObject;
  lResulTJsonData: TJsonData;
  lErrMsg: string;
  lActionResult: TActionResult<TClientConnect>;
begin
  Result := False;
  if (not qForceConnect) then
  begin
    if self.FConnected then
    begin
      // 如果已经连接过就不在连接，
      // 除非主动断掉
      self.FErrMsg :=
        '已连接,无需在连接,如需重新连接请执行DisConnect或者强制重新连接DoConnect(true)';
      // 这边还是返回true
      Result := True;
      exit;
    end;
  end;
  lResulTJsonData := nil;
  lActionResult := TActionResult<TClientConnect>.Create;
  lJsonObj := TJsonObject.Create;
  try
    lJsonObj.Add('ClientIP', self.ClientIP);
    lJsonObj.Add('ClientMac', self.ClientMac);
    lJsonObj.Add('ConnectSecretkey', self.ConnectSecretkey);
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_TOKEN_ClientConnect, lJsonObj.AsJSON, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    lActionResult.ResultDataT := TClientConnect.Create;
    if not OneSerialization.JsonToObject(lActionResult, lResulTJsonData, lErrMsg) then
    begin
      self.FErrMsg :=
        '返回的数据解析成TResult<TClientConnect>出错,无法知道结果,数据:' + lResulTJsonData.AsJSON;
      exit;
    end;
    if not lActionResult.ResultSuccess then
    begin
      self.FErrMsg := '服务端消息:' + lActionResult.ResultMsg;
      exit;
    end;
    self.TokenID := lActionResult.ResultDataT.TokenID;
    self.PrivateKey := lActionResult.ResultDataT.PrivateKey;
    if self.TokenID = '' then
    begin
      self.FErrMsg := '请求成功但返回的TokenID为空,当前返回的数据:' + lResulTJsonData.AsJSON;
      exit;
    end;
    self.FConnected := True;
    Result := True;
  finally
    lJsonObj.Free;
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lActionResult.ResultData <> nil then
    begin
      lActionResult.ResultData.Free;
    end;
    lActionResult.Free;
  end;
end;

function TOneConnection.DoConnectPing(): boolean;
var
  lJsonObj: TJsonObject;
  lResultJsonValue: TJsonData;
  lErrMsg: string;
  lClientConnect: TClientConnect;
  lServerResult: TActionResultString;
begin
  Result := False;
  lResultJsonValue := nil;
  lServerResult := TActionResultString.Create();
  lJsonObj := TJsonObject.Create;
  try
    lJsonObj.Add('ConnectSecretkey', self.ConnectSecretkey);
    lResultJsonValue := self.PostResultJsonData(URL_HTTP_HTTPServer_TOKEN_ClientConnectPing, lJsonObj.AsJSON, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;

    if not OneSerialization.JsonToObject(lServerResult, lResultJsonValue, lErrMsg) then
    begin
      self.FErrMsg := '返回的数据解析成TResult<string>出错,无法知道结果,数据:' + lResultJsonValue.AsJSON;
      exit;
    end;
    if not lServerResult.ResultSuccess then
    begin
      self.FErrMsg := '服务端消息:' + lServerResult.ResultMsg;
      exit;
    end;
    self.FConnected := True;
    Result := True;
  finally
    lJsonObj.Free;
    if lResultJsonValue <> nil then
    begin
      lResultJsonValue.Free;
    end;
    lServerResult.Free;
  end;
end;

function TOneConnection.DoPing(): boolean;
var
  lResultJsonValue: TJsonData;
  lErrMsg: string;
  lClientConnect: TClientConnect;
  lServerResult: TActionResultString;
begin
  Result := False;
  lResultJsonValue := nil;
  lServerResult := TActionResultString.Create();
  try
    lResultJsonValue := self.PostResultJsonData(URL_HTTP_HTTPServer_TOKEN_ClientPing, '', lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;

    if not OneSerialization.JsonToObject(lServerResult, lResultJsonValue, lErrMsg) then
    begin
      self.FErrMsg := '返回的数据解析成TResult<string>出错,无法知道结果,数据:' + lResultJsonValue.AsJSON;
      exit;
    end;
    if not lServerResult.ResultSuccess then
    begin
      self.FErrMsg := '服务端消息:' + lServerResult.ResultMsg;
      exit;
    end;
    Result := True;
  finally
    self.FConnected := Result;
    if lResultJsonValue <> nil then
    begin
      lResultJsonValue.Free;
    end;
    lServerResult.Free;
  end;
end;

// 断开服务端，本质上是踢除Token，HTTP多是基于短连接
procedure TOneConnection.DisConnect;
var
  lOldTokenID: string;
begin
  lOldTokenID := self.TokenID;
  self.TokenID := '';
  self.PrivateKey := '';
  self.FConnected := False;
  //踢除
end;

// 调整URL
function TOneConnection.MakeUrl(var QUrl: string; var QErrMsg: string): boolean;
var
  tempStr: string;
  lURI: TURI;
  lTimeStr: string;
  lSign: string;
begin
  Result := False;
  QErrMsg := '';
  tempStr := '';
  if not QUrl.StartsWith('http') then
  begin
    if self.FHTTPHost = '' then
    begin
      QErrMsg := '服务端地址未设置';
      exit;
    end;
    if (self.FHTTPHost.StartsWith('https://')) or (self.FHTTPHost.StartsWith('http://')) then
    begin
      tempStr := self.FHTTPHost;
    end
    else
    begin
      if self.FIsHttps then
      begin
        tempStr := tempStr + 'https://';
      end
      else
      begin
        tempStr := tempStr + 'http://';
      end;
      tempStr := tempStr + self.FHTTPHost;
    end;

    if self.FHTTPPort > 0 then
    begin
      tempStr := tempStr + ':' + self.FHTTPPort.ToString();
    end;
    if QUrl.StartsWith('/') then
    begin
      tempStr := tempStr + QUrl;
    end
    else
    begin
      tempStr := tempStr + '/' + QUrl;
    end;
  end
  else
  begin
    tempStr := QUrl;
  end;
  try
    lURI := ParseURI(tempStr);
    // 加上Token相关参数以及签名
    if self.FToKenID <> '' then
    begin
      lTimeStr := DateTimeToUnix(now).ToString;
      if lURI.Params = '' then
        lURI.Params := HTTP_URL_TokenName + '=' + self.FToKenID
      else
        lURI.Params := lURI.Params + '&' + HTTP_URL_TokenName + '=' + self.FToKenID;
      lURI.Params := lURI.Params + '&' + HTTP_URL_TokenTime + '=' + lTimeStr;

      // 签名
      lSign := self.FToKenID + lTimeStr + self.FPrivateKey;
      lSign := OneCrypto.MD5Endcode(lSign);
      lURI.Params := lURI.Params + '&' + HTTP_URL_TokenSign + '=' + lSign;
    end;
    // 转换回来
    QUrl := EncodeURI(lURI);
    Result := True;
  except
    on e: Exception do
    begin
      QErrMsg := 'URL解析异常,最终组装的URL为:' + tempStr;
    end;
  end;

end;

procedure TOneConnection.SetErrTrueResult(var QErrMsg: string);
begin
  QErrMsg := 'true';
end;

function TOneConnection.IsErrTrueResult(QErrMsg: string): boolean;
begin
  Result := QErrMsg = 'true';
end;

// 获取账套 ，如果数据集有取数据集的,没有取全局的
function TOneConnection.GetZTCode(QDataSetZTCode: string): string;
begin
  if QDataSetZTCode.Trim = '' then
    Result := self.ZTCode
  else
    Result := QDataSetZTCode.Trim;
end;

function TOneConnection.PostResultBytes(const QUrl: string; QPostData: string): TOneResultBytes;
var
  lPostBytes, lResultBytes: TBytes;
begin
  Result := nil;
  lPostBytes := TEncoding.UTF8.GetAnsiBytes(QPostData);
  Result := self.PostResultBytes(QUrl, lPostBytes);
end;

// 提交bytes返回Bytes
function TOneConnection.PostResultBytes(const QUrl: string; QPostDataBtye: TBytes): TOneResultBytes;
var
  lUrl: string;
  LNetHttp: TFPHTTPClient;
  LResponseStream: TBytesStream;
  LRequestStream: TBytesStream;
  lBytes: TBytes;
  tempA: TArray<string>;
  lErrMsg: string;
  lContentType: string;
  iSize: int64;
begin
  Result := TOneResultBytes.Create;
  lErrMsg := '';
  lUrl := QUrl;
  if not self.MakeUrl(lUrl, lErrMsg) then
  begin
    Result.ErrMsg := lErrMsg;
    exit;
  end;
  LNetHttp := TFPHTTPClient.Create(nil);
  LRequestStream := TBytesStream.Create(QPostDataBtye);
  LResponseStream := TBytesStream.Create;
  try
    try
      LNetHttp.AddHeader('Content-Type', 'text/plain; charset=UTF-8');
      //LRequestStream.Write(QPostDataBtye, length(QPostDataBtye));
      LRequestStream.Position := 0;
      LNetHttp.AllowRedirect := True;
      LNetHttp.RequestBody := LRequestStream;
      LNetHttp.Post(lUrl, LResponseStream);
      if LNetHttp.ResponseStatusCode = 200 then
      begin
        // LResponse.Headers
        // 'text/plain;charset=UTF-8'
        Result.ContentType := LNetHttp.ResponseHeaders.Values['Content-Type'];
        tempA := Result.ContentType.Split([';']);
        if length(tempA) = 2 then
        begin
          if not tempA[0].StartsWith('charset') then
            Result.ContentType := tempA[0]
          else
            Result.ContentType := tempA[1];
        end;
        Result.ContentType := Result.ContentType.Trim();
        Result.IsFile := (LNetHttp.ResponseHeaders.Values['OneOutMode'] = 'OUTFILE');
        if not Result.IsFile then
        begin
          if (Result.ContentType.ToLower <> 'application/json') and (Result.ContentType.ToLower <> 'text/plain') then
          begin
            Result.IsFile := True;
          end;
        end;
        // 判断是文件还是不是
        lBytes := LResponseStream.Bytes;
        setLength(lBytes, LResponseStream.Size);
        Result.Bytes := lBytes;
        Result.IsOK := True;
      end
      else
      begin
        LResponseStream.Position := 0;
        lBytes := LResponseStream.Bytes;
        Result.ErrMsg := '返回结果错误,错误代码:' + LNetHttp.ResponseStatusCode.ToString + ';错误状态:' +
          LNetHttp.ResponseStatusText + ';服务端错误:' + TEncoding.UTF8.GetString(lBytes);
      end;
    except
      on e: Exception do
      begin
        Result.ErrMsg := '请求发生异常:' + e.Message;
      end;
    end;
  finally
    LRequestStream.Clear;
    LRequestStream.Free;
    LResponseStream.Clear;
    LResponseStream.Free;
    LNetHttp.Free;
  end;
end;

// 提交字符串，返回JSONValue
function TOneConnection.PostResultJsonData(const QUrl: string; QPostData: string; var QErrMsg: string): TJsonData;
var
  lPostBytes: TBytes;
  lResultBytes: TOneResultBytes;
  lJsonValue: TJsonData;
  lJSonStr: string;
begin
  Result := nil;
  lJsonValue := nil;
  lResultBytes := nil;
  QErrMsg := '';
  // 数据上传压缩
  lPostBytes := TEncoding.UTF8.GetAnsiBytes(QPostData);
  lResultBytes := self.PostResultBytes(QUrl, lPostBytes);
  if not lResultBytes.IsOK then
  begin
    QErrMsg := lResultBytes.ErrMsg;
    exit;
  end;
  try
    try
      lJSonStr := TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
      lJsonValue := GetJson(lJSonStr);
      if lJsonValue = nil then
      begin
        QErrMsg := '结果转化JSON发生异常:结果为nil,返回信息:' + TEncoding.UTF8.GetString(lResultBytes.Bytes);
        exit;
      end;
      self.SetErrTrueResult(QErrMsg);
      Result := lJsonValue;
    except
      on e: Exception do
      begin
        QErrMsg := '结果转化JSON发生异常:' + e.Message;
      end;
    end;
  finally
    if lResultBytes <> nil then
    begin
      lResultBytes.Free;
    end;
  end;
end;

// 提交Bytes，返回JSONValue
function TOneConnection.PostResultJsonData(const QUrl: string; QPostData: TBytes; var QErrMsg: string): TJsonData;
var
  lPostBytes: TBytes;
  lResultBytes: TOneResultBytes;
  lJsonValue: TJsonData;
  lJSonStr: string;
begin
  Result := nil;
  lJsonValue := nil;
  lResultBytes := nil;
  QErrMsg := '';
  lResultBytes := self.PostResultBytes(QUrl, lPostBytes);
  if not lResultBytes.IsOK then
  begin
    QErrMsg := lResultBytes.ErrMsg;
    exit;
  end;
  try
    try
      lJSonStr := TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
      lJsonValue := GetJson(lJSonStr);
      if lJsonValue = nil then
      begin
        QErrMsg := '结果转化JSON发生异常:结果为nil,返回信息:' + TEncoding.UTF8.GetString(lResultBytes.Bytes);
        exit;
      end;
      self.SetErrTrueResult(QErrMsg);
      Result := lJsonValue;
    except
      on e: Exception do
      begin
        QErrMsg := '结果转化JSON发生异常:' + e.Message;
      end;
    end;
  finally
    if lResultBytes <> nil then
    begin
      lResultBytes.Free;
    end;
  end;
end;

function TOneConnection.PostResultJsonData(const QUrl: string; QObject: TObject; var QErrMsg: string): TJsonData;
var
  lTempJson: TJsonData;
  lTempStr: string;
  lPostBytes: TBytes;
  lResultBytes: TOneResultBytes;
  lJsonValue: TJsonData;
begin
  Result := nil;
  lTempJson := nil;
  lJsonValue := nil;
  QErrMsg := '';
  lTempJson := OneSerialization.ObjectToJson(QObject, QErrMsg);
  if lTempJson = nil then
    exit;
  try
    lTempStr := lTempJson.AsJSON;
  finally
    lTempJson.Free;
    lTempJson := nil;
  end;
  lPostBytes := TEncoding.UTF8.GetBytes(lTempStr);
  lResultBytes := self.PostResultBytes(QUrl, lPostBytes);
  if not lResultBytes.IsOK then
  begin
    QErrMsg := lResultBytes.ErrMsg;
    exit;
  end;
  try
    try
      lTempStr := TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
      lJsonValue := GetJson(lTempStr);
      if lJsonValue = nil then
      begin
        QErrMsg := '结果转化JSON发生异常:结果为nil,返回信息:' + lTempStr;
        exit;
      end;
      self.SetErrTrueResult(QErrMsg);
      Result := lJsonValue;
    except
      on e: Exception do
      begin
        QErrMsg := '结果转化JSON发生异常:' + e.Message;
      end;
    end;
  finally
    if lResultBytes <> nil then
    begin
      lResultBytes.Free;
    end;
  end;
end;

function TOneConnection.PostResultContent(const QUrl: string; QPostData: string; var QResultData: string): boolean;
var
  lPostBytes: TBytes;
  lResultBytes: TOneResultBytes;
begin
  Result := False;
  lResultBytes := nil;
  QResultData := '';
  lPostBytes := TEncoding.UTF8.GetAnsiBytes(QPostData);
  lResultBytes := self.PostResultBytes(QUrl, lPostBytes);
  try
    if not lResultBytes.IsOK then
    begin
      QResultData := lResultBytes.ErrMsg;
      exit;
    end;
    QResultData := TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
    Result := True;
  finally
    if lResultBytes <> nil then
    begin
      lResultBytes.Free;
    end;
  end;
end;

function TOneConnection.GetResultBytes(const QUrl: string; var QContentType: string; var QErrMsg: string): TBytes;
var
  lUrl: string;
  LNetHttp: TFPHTTPClient;
  LResponseStream: TMemoryStream;
  lBytes: TBytes;
begin
  Result := nil;
  QErrMsg := '';
  QContentType := '';
  lUrl := QUrl;
  if not self.MakeUrl(lUrl, QErrMsg) then
    exit;
  LNetHttp := TFPHTTPClient.Create(nil);
  LResponseStream := TMemoryStream.Create;
  try
    try
      // LNetHttp.ContentType := 'text/plain; charset=utf-8';
      LNetHttp.AddHeader('Content-Type', 'text/plain; charset=UTF-8');
      LNetHttp.Get(lUrl, LResponseStream);
      if LNetHttp.ResponseStatusCode = 200 then
      begin
        // 判断是文件还是不是
        LResponseStream.Position := 0;
        setLength(lBytes, LResponseStream.Size);
        LResponseStream.Read(lBytes, LResponseStream.Size);
        self.SetErrTrueResult(QErrMsg);
        Result := lBytes;
      end
      else
      begin
        QErrMsg := '返回结果错误,错误代码:' + LNetHttp.ResponseStatusCode.ToString + ';错误状态:' + LNetHttp.ResponseStatusText;
      end;
    except
      on e: Exception do
      begin
        QErrMsg := '请求发生异常:' + e.Message;
      end;
    end;
  finally
    LResponseStream.Clear;
    LResponseStream.Free;
    LNetHttp.Free;
  end;
end;

function TOneConnection.GetResulTJsonData(const QUrl: string; var QErrMsg: string): TJsonData;
var
  lPostBytes, lResultBytes: TBytes;
  lJsonValue: TJsonData;
  QContentType, lJsonStr: string;
begin
  Result := nil;
  lJsonValue := nil;
  QErrMsg := '';
  lResultBytes := self.GetResultBytes(QUrl, QContentType, QErrMsg);
  if not self.IsErrTrueResult(QErrMsg) then
  begin
    exit;
  end;
  try
    try
      lJsonStr := TEncoding.UTF8.GetAnsiString(lResultBytes);
      lJsonValue := GetJson(lJsonStr);
      if lJsonValue = nil then
      begin
        QErrMsg := '结果转化JSON发生异常:结果为nil,返回信息:' + TEncoding.UTF8.GetString(lResultBytes);
        exit;
      end;
      self.SetErrTrueResult(QErrMsg);
      Result := lJsonValue;
    except
      on e: Exception do
      begin
        QErrMsg := '结果转化JSON发生异常:' + e.Message;
      end;
    end;
  finally
    lResultBytes := nil;
  end;
end;

function TOneConnection.GetResultContent(const QUrl: string; var QResultData: string): boolean;
var
  lPostBytes, lResultBytes: TBytes;
  QContentType, lErrMsg: string;
begin
  Result := False;
  lErrMsg := '';
  lResultBytes := self.GetResultBytes(QUrl, QContentType, lErrMsg);
  if not self.IsErrTrueResult(lErrMsg) then
  begin
    exit;
  end;
  QResultData := TEncoding.UTF8.GetAnsiString(lResultBytes);
  Result := True;
end;

procedure TOneConnection.DataSetToOpenData(Sender: TObject; QDataOpen: TOneDataOpen);
var
  lOneDataSet: TOneDataSet;
  iParam: integer;
  lFDParam: TParam;
  lOneParam: TOneParam;
begin

  lOneDataSet := TOneDataSet(Sender);
  QDataOpen.TranID := lOneDataSet.DataInfo.TranID;
  QDataOpen.ZTCode := self.GetZTCode(lOneDataSet.DataInfo.ZTCode);
  // SQL进行打乱
  QDataOpen.OpenSQL := OneSQLCrypto.SwapCrypto(lOneDataSet.SQL.Text);
  QDataOpen.SPName := lOneDataSet.DataInfo.StoredProcName;
  QDataOpen.SPIsOutData := lOneDataSet.DataInfo.IsReturnData;
  // 增加相关参数
  for iParam := 0 to lOneDataSet.Params.Count - 1 do
  begin
    lFDParam := lOneDataSet.Params[iParam];
    lOneParam := TOneParam.Create;
    QDataOpen.Params.Add(lOneParam);
    lOneParam.ParamName := lFDParam.Name;
    lOneParam.ParamType := GetEnumName(TypeInfo(TParamType), Ord(lFDParam.ParamType));
    lOneParam.ParamDataType := GetEnumName(TypeInfo(TFieldType), Ord(lFDParam.DataType));
    lOneParam.ParamSize := lFDParam.Size;
    // 参数赋值
    case lFDParam.DataType of
      ftUnknown:
      begin
        if lFDParam.IsNull then
          lOneParam.ParamValue := const_OneParamIsNull_Value
        else
          lOneParam.ParamValue := varToStr(lFDParam.Value);
      end;
      ftBlob:
      begin
        // 转化成Base64
        if lFDParam.IsNull then
          lOneParam.ParamValue := const_OneParamIsNull_Value
        else
          lOneParam.ParamValue :=
            OneStreamString.BytesToBase64(lFDParam.AsBlob);
      end;
      else
      begin
        if lFDParam.IsNull then
          lOneParam.ParamValue := const_OneParamIsNull_Value
        else
          lOneParam.ParamValue := varToStr(lFDParam.Value);
      end;
    end;
  end;
  QDataOpen.PageSize := lOneDataSet.DataInfo.PageSize;
  QDataOpen.PageIndex := lOneDataSet.DataInfo.PageIndex;
  QDataOpen.PageRefresh := False;
  QDataOpen.DataReturnMode := GetEnumName(TypeInfo(TDataReturnMode), Ord(lOneDataSet.DataInfo.DataReturnMode));
  ;
end;

// 跟据OneDataSet打开数据集
function TOneConnection.OpenData(Sender: TObject): boolean;
var
  QObjectList: TList<TObject>;
  lErrMsg: string;
  lDataSet: TOneDataSet;
begin
  Result := False;
  if not (Sender is TOneDataSet) then
    exit;
  lDataSet := TOneDataSet(Sender);
  QObjectList := TList<TObject>.Create;
  try
    QObjectList.Add(lDataSet);
    Result := self.OpenDatas(QObjectList, lErrMsg);
    lDataSet.DataInfo.ErrMsg := lErrMsg;
  finally
    QObjectList.Clear;
    QObjectList.Free;
  end;
end;

// 跟据List<OneDataSet>打开数据集
function TOneConnection.OpenDatas(QObjectList: TList<TObject>; var QErrMsg: string): boolean;
var
  lOneDataSets: TList<TOneDataSet>;
  lOneDataSet: TOneDataSet;
  lFDParam: TParam;
  i, iParam: integer;
  lDataOpens: TList<TOneDataOpen>;
  lDataOpen: TOneDataOpen;
  lOneParam: TOneParam;
  lDataResult: TOneDataResult;
begin
  Result := False;
  QErrMsg := '';
  if not FConnected then
  begin
    QErrMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  lDataResult := nil;
  lDataOpens := TList<TOneDataOpen>.Create;
  lOneDataSets := TList<TOneDataSet>.Create;
  try
    for i := 0 to QObjectList.Count - 1 do
    begin
      if QObjectList[i] is TOneDataSet then
      begin
        lOneDataSets.Add(TOneDataSet(QObjectList[i]));
      end
      else
      begin
        QErrMsg := '非OneDataSet不可使用';
        exit;
      end;
    end;
    // 校验
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      if lOneDataSet.SQL.Text.Trim = '' then
      begin
        QErrMsg := '数据集:' + lOneDataSet.Name + 'SQL为空,无法打开数据';
        exit;
      end;
    end;
    // 组装参数
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      lDataOpen := TOneDataOpen.Create;
      lDataOpens.Add(lDataOpen);
      self.DataSetToOpenData(lOneDataSet, lDataOpen);
    end;
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
    end;
    // 打开数据
    lDataResult := self.OpenDatasPost(lDataOpens);
    if not lDataResult.ResultOK then
    begin
      QErrMsg := lDataResult.ResultMsg;
      exit;
    end;
    // 解析数据到dataSet
    if not self.DataResultToDataSets(lDataResult, QObjectList, False, QErrMsg) then
    begin
      exit;
    end;
    Result := True;
  finally
    for i := 0 to lDataOpens.Count - 1 do
    begin
      lDataOpens[i].Free;
    end;
    lDataOpens.Clear;
    lDataOpens.Free;
    lOneDataSets.Clear;
    lOneDataSets.Free;
    if lDataResult <> nil then
      lDataResult.Free;
  end;

end;

function TOneConnection.OpenDatasPost(QDataOpens: TList<TOneDataOpen>): TOneDataResult;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    Result.ResultMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QDataOpens = nil then
  begin
    Result.ResultMsg := '传入的请求数据的信息为nil';
    exit;
  end;
  if QDataOpens.Count = 0 then
  begin
    Result.ResultMsg := '传入的请求数据的信息为0个信息';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QDataOpens, lErrMsg);
    if lErrMsg <> '' then
    begin
      Result.ResultMsg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_OpenDatas, lPosTJsonData.AsJSON, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      Result.ResultMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(Result, lResulTJsonData, lErrMsg) then
    begin
      Result.ResultMsg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJSON;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

function TOneConnection.ExecStored(Sender: TObject): boolean;
var
  lDataOpen: TOneDataOpen;
  lDataResult: TOneDataResult;
  lErrMsg: string;
begin
  Result := False;
  if not (Sender is TOneDataSet) then
    exit;
  lDataOpen := TOneDataOpen.Create;
  lDataResult := nil;
  try
    self.DataSetToOpenData(Sender, lDataOpen);
    lDataResult := self.ExecStoredPost(lDataOpen);
    if not lDataResult.ResultOK then
    begin
      TOneDataSet(Sender).DataInfo.ErrMsg := '服务端消息:' + lDataResult.ResultMsg;
      exit;
    end;
    if not self.DataResultToDataSet(lDataResult, Sender, lErrMsg) then
    begin
      TOneDataSet(Sender).DataInfo.ErrMsg := lErrMsg;
      exit;
    end;
    Result := True;
  finally
    lDataOpen.Free;
    if lDataResult <> nil then
      lDataResult.Free;
  end;
end;

function TOneConnection.ExecStoredPost(QDataOpen: TOneDataOpen): TOneDataResult;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    Result.ResultMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QDataOpen = nil then
  begin
    Result.ResultMsg := '传入的请求数据的信息为nil';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QDataOpen, lErrMsg);
    if lErrMsg <> '' then
    begin
      Result.ResultMsg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_ExecStored, lPosTJsonData.AsJSON, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      Result.ResultMsg := lErrMsg;
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(Result, lResulTJsonData, lErrMsg) then
    begin
      Result.ResultMsg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJSON;
      exit;
    end;
    if Result.ResultMsg <> '' then
    begin
      Result.ResultMsg := '服务端消息:' + Result.ResultMsg;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

// ***********保存数据*********************//
function TOneConnection.SaveData(Sender: TObject): boolean;
var
  QObjectList: TList<TObject>;
  lErrMsg: string;
  lDataSet: TOneDataSet;
begin
  Result := False;
  if not (Sender is TOneDataSet) then
    exit;
  lDataSet := TOneDataSet(Sender);
  QObjectList := TList<TObject>.Create;
  try
    QObjectList.Add(lDataSet);
    Result := self.SaveDatas(QObjectList, lErrMsg);
    lDataSet.DataInfo.ErrMsg := lErrMsg;
  finally
    QObjectList.Clear;
    QObjectList.Free;
  end;
end;

function TOneConnection.SaveDatas(QObjectList: TList<TObject>; var QErrMsg: string): boolean;
var
  lOneDataSets: TList<TOneDataSet>;
  lOneDataSet: TOneDataSet;
  lFDParam: TParam;
  i, iField, iParam: integer;
  lSaveDMLs: TList<TOneDataSaveDML>;
  lSaveDML: TOneDataSaveDML;
  lOneParam: TOneParam;
  lDataResult: TOneDataResult;
  lTemStream: TMemoryStream;
  lProviderFlags: TProviderFlags;
  lFlags: integer;
begin
  Result := False;
  QErrMsg := '';
  if not FConnected then
  begin
    QErrMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  lDataResult := nil;
  lSaveDMLs := TList<TOneDataSaveDML>.Create;
  lOneDataSets := TList<TOneDataSet>.Create;
  try
    for i := 0 to QObjectList.Count - 1 do
    begin
      if QObjectList[i] is TOneDataSet then
      begin
        lOneDataSets.Add(TOneDataSet(QObjectList[i]));
      end
      else
      begin
        QErrMsg := '非OneDataSet不可使用';
        exit;
      end;
    end;
    // 校验
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      if lOneDataSet.DataInfo.SaveMode = TDataSaveMode.SaveData then
      begin
        if not lOneDataSet.Active then
        begin
          QErrMsg := '数据集:' + lOneDataSet.Name + '保存数据模式:数据未运行';
          exit;
        end;
        if lOneDataSet.DataInfo.TableName = '' then
        begin
          QErrMsg := '数据集:' + lOneDataSet.Name + '保存数据模式:表名不可为空';
          exit;
        end;
        if lOneDataSet.DataInfo.PrimaryKey = '' then
        begin
          QErrMsg := '数据集:' + lOneDataSet.Name + '保存数据模式:主键不可为空';
          exit;
        end;
        if lOneDataSet.State in dsEditModes then
          lOneDataSet.Post;
      end
      else if lOneDataSet.DataInfo.SaveMode = TDataSaveMode.saveDML then
      begin
        if lOneDataSet.SQL.Text.Trim = '' then
        begin
          QErrMsg := '数据集:' + lOneDataSet.Name + 'DML操作:SQL不可为空';
          exit;
        end;
      end
      else
      begin
        QErrMsg := '数据集:' + lOneDataSet.Name + '未设计的操作模式';
        exit;
      end;
    end;
    // 组装参数
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      lSaveDML := TOneDataSaveDML.Create;
      lSaveDMLs.Add(lSaveDML);
      lSaveDML.TranID := lOneDataSet.DataInfo.TranID;
      lSaveDML.ZTCode := self.GetZTCode(lOneDataSet.DataInfo.ZTCode);
      lSaveDML.DataSaveMode := GetEnumName(TypeInfo(TDataSaveMode), Ord(lOneDataSet.DataInfo.SaveMode));
      lSaveDML.TableName := lOneDataSet.DataInfo.TableName;
      lSaveDML.PrimaryKey := lOneDataSet.DataInfo.PrimaryKey;
      lSaveDML.OtherKeys := lOneDataSet.DataInfo.OtherKeys;
      if lOneDataSet.DataInfo.SaveMode = TDataSaveMode.SaveData then
      begin
        lTemStream := TMemoryStream.Create;
        try
          lOneDataSet.SaveDeltaToStream(lTemStream);
          lSaveDML.SaveData := OneStreamString.StreamToBase64Str(lTemStream);
        finally
          lTemStream.Clear;
          lTemStream.Free;
        end;
        //字段提交标识处理
        for iField := 0 to lOneDataSet.Fields.Count - 1 do
        begin
          //pfInkey,pfInUpdate,pfInWhere
          lFlags := 222;
          lProviderFlags := lOneDataSet.Fields[iField].ProviderFlags;
          if pfInkey in lProviderFlags then
          begin
            lFlags := lFlags - 100;
          end;
          if pfInUpdate in lProviderFlags then
          begin
            lFlags := lFlags - 10;
          end;
          if pfInWhere in lProviderFlags then
          begin
            lFlags := lFlags - 1;
          end;
          lSaveDML.FieldProviderFlags.Add(lFlags);
        end;
      end
      else
      begin

      end;
      lSaveDML.UpdateMode := GetEnumName(TypeInfo(TUpdateMode), Ord(lOneDataSet.UpdateMode));
      lSaveDML.AffectedMaxCount := lOneDataSet.DataInfo.AffectedMaxCount;
      lSaveDML.AffectedMustCount := lOneDataSet.DataInfo.AffectedMustCount;
      ;
      lSaveDML.SaveDataInsertSQL := '';
      lSaveDML.SaveDataUpdateSQL := '';
      lSaveDML.SaveDataDelSQL := '';
      lSaveDML.IsReturnData := lOneDataSet.DataInfo.IsReturnData;
      lSaveDML.IsAutoID := lOneDataSet.DataInfo.IsReturnData;
      // SQL进行打乱
      lSaveDML.SQL := OneSQLCrypto.SwapCrypto(lOneDataSet.SQL.Text);
      // 增加相关参数
      for iParam := 0 to lOneDataSet.Params.Count - 1 do
      begin
        lFDParam := lOneDataSet.Params[iParam];
        lOneParam := TOneParam.Create;
        lSaveDML.Params.Add(lOneParam);
        lOneParam.ParamName := lFDParam.Name;
        lOneParam.ParamType :=
          GetEnumName(TypeInfo(TParamType), Ord(lFDParam.ParamType));
        lOneParam.ParamDataType :=
          GetEnumName(TypeInfo(TFieldType), Ord(lFDParam.DataType));
        // 参数赋值
        case lFDParam.DataType of
          ftUnknown:
          begin
            if lFDParam.IsNull then
              lOneParam.ParamValue := const_OneParamIsNull_Value
            else
              lOneParam.ParamValue := varToStr(lFDParam.Value);
          end;
          ftBlob:
          begin
            // 转化成Base64
            if lFDParam.IsNull then
              lOneParam.ParamValue := const_OneParamIsNull_Value
            else
              lOneParam.ParamValue :=
                OneStreamString.BytesToBase64(lFDParam.AsBlob);
          end;
          else
          begin
            if lFDParam.IsNull then
              lOneParam.ParamValue := const_OneParamIsNull_Value
            else
              lOneParam.ParamValue := varToStr(lFDParam.Value);
          end;
        end;
      end;
    end;
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
    end;
    // 打开数据
    lDataResult := self.SaveDatasPost(lSaveDMLs);
    if not lDataResult.ResultOK then
    begin
      QErrMsg := '服务端消息:' + lDataResult.ResultMsg;
      exit;
    end;
    // 解析数据到dataSet
    if not self.DataResultToDataSets(lDataResult, QObjectList, True, QErrMsg) then
      exit;
    Result := True;
  finally
    for i := 0 to lSaveDMLs.Count - 1 do
    begin
      lSaveDMLs[i].Free;
    end;
    lSaveDMLs.Clear;
    lSaveDMLs.Free;
    lOneDataSets.Clear;
    lOneDataSets.Free;
    if lDataResult <> nil then
      lDataResult.Free;
  end;

end;

function TOneConnection.SaveDatasPost(QSaveDMLDatas: TList<TOneDataSaveDML>): TOneDataResult;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    Result.ResultMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QSaveDMLDatas = nil then
  begin
    Result.ResultMsg := '传入的保存的数据为nil';
    exit;
  end;
  if QSaveDMLDatas.Count = 0 then
  begin
    Result.ResultMsg := '传入的保存的数据的个数为0';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QSaveDMLDatas, lErrMsg);
    if lErrMsg <> '' then
    begin
      Result.ResultMsg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_SaveDatas, lPosTJsonData.AsJSON, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(Result, lResulTJsonData, lErrMsg) then
    begin
      Result.ResultMsg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJSON;
      exit;
    end;
    if Result.ResultMsg <> '' then
    begin
      Result.ResultMsg := '服务端消息:' + Result.ResultMsg;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

function TOneConnection.DataResultToDataSets(DataResult: TOneDataResult; QObjectList: TList<TObject>; QIsSave: boolean;
  var QErrMsg: string): boolean;
var
  lOneDataSets: TList<TOneDataSet>;
  lOneDataSet: TOneDataSet;
  tempOneDataSet: TBufDataset;
  lDataResultItem: TOneDataResultItem;
  i, iField: integer;
  bCacle: boolean;
  lCacleList: TList<boolean>;
  lTemStream: TMemoryStream;
begin
  Result := False;
  QErrMsg := '';
  lTemStream := nil;
  if DataResult.ResultItems.Count <> QObjectList.Count then
  begin
    QErrMsg := '结果返回数据集个数与请求的个数不相等';
    exit;
  end;
  lCacleList := TList<boolean>.Create;
  lOneDataSets := TList<TOneDataSet>.Create;
  try
    for i := 0 to QObjectList.Count - 1 do
    begin
      lOneDataSets.Add(TOneDataSet(QObjectList[i]));
    end;
    // DisableControls
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      lOneDataSet.DisableControls;
    end;
    // 关闭
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      bCacle := False;
      for iField := 0 to lOneDataSet.Fields.Count - 1 do
      begin
        if lOneDataSet.Fields[iField].FieldKind in [fkInternalCalc] then
        begin
          bCacle := True;
          break;
        end;
      end;
      lCacleList.Add(bCacle);
      if not QIsSave then
      begin
        if lOneDataSet.Active then
          lOneDataSet.Close;
      end;
    end;
    // 处理
    for i := 0 to DataResult.ResultItems.Count - 1 do
    begin
      lDataResultItem := DataResult.ResultItems[i];
      lOneDataSet := lOneDataSets[i];
      if lDataResultItem.ResultPage then
      begin
        // 分页信息
        lOneDataSet.DataInfo.PageTotal := lDataResultItem.ResultTotal;
        lOneDataSet.DataInfo.PageCount := 0;
        if lOneDataSet.DataInfo.PageSize > 0 then
        begin
          lOneDataSet.DataInfo.PageCount :=
            ceil(lDataResultItem.ResultTotal / lOneDataSet.DataInfo.PageSize);
        end;
      end;
      lOneDataSet.DataInfo.RowsAffected := lDataResultItem.RecordCount;
      if lDataResultItem.ResultDataMode = const_DataReturnMode_Stream then
      begin
        lTemStream := TMemoryStream.Create;
        try
          if not OneStreamString.StreamWriteBase64Str(lTemStream, lDataResultItem.ResultContext) then
          begin
            QErrMsg := '数据集[' + lOneDataSet.Name + ']流写入字符串出错';
            exit;
          end;
          tempOneDataSet := TBufDataset.Create(nil);
          try
            lTemStream.Position := 0;
            tempOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
            if lCacleList[i] then
            begin
              // 有计算字段的比较特殊加载模式
              lOneDataSet.CopyDataSetOnlyData(tempOneDataSet);
            end
            else
            begin
              // 无计算字段的整个加载,判断现有结构是否一至
              if lOneDataSet.IsSameStruct(tempOneDataSet) then
              begin
                lTemStream.Position := 0;
                lOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
              end
              else
              begin
                lOneDataSet.CopyDataSetOnlyData(tempOneDataSet);
              end;
            end;
          finally
            tempOneDataSet.Free;
          end;

        finally
          lTemStream.Clear;
          lTemStream.Free;
        end;
      end
      else if lDataResultItem.ResultDataMode = const_DataReturnMode_File then
      begin
        // 下载文件，比较适合较大数据加载
        lTemStream := self.DownLoadDataFile(lDataResultItem.ResultContext, QErrMsg);
        try
          if not self.IsErrTrueResult(QErrMsg) then
          begin
            exit;
          end;
          lTemStream.Position := 0;
          if lCacleList[i] then
          begin
            // 有计算字段的比较特殊加载模式
            tempOneDataSet := TBufDataset.Create(nil);
            try
              tempOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
            finally
              tempOneDataSet.Free;
            end;
          end
          else
          begin
            // 无计算字段的整个加载
            lOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
          end;
        finally
          if lTemStream <> nil then
          begin
            lTemStream.Clear;
            lTemStream.Free;
          end;
        end;
      end
      else if lDataResultItem.ResultDataMode = const_DataReturnMode_JSON then
      begin
        // JSON序列化
      end;
    end;

    Result := True;
  finally
    for i := 0 to lOneDataSets.Count - 1 do
    begin
      lOneDataSet := lOneDataSets[i];
      if lOneDataSet.Active then
        lOneDataSet.MergeChangeLog;
      lOneDataSet.EnableControls;
    end;
    lOneDataSets.Clear;
    lOneDataSets.Free;
    lCacleList.Clear;
    lCacleList.Free;
  end;
end;

function TOneConnection.DataResultToDataSet(QDataResult: TOneDataResult; QObject: TObject; var QErrMsg: string): boolean;
var
  lOneDataSet: TOneDataSet;
  tempOneDataSet: TBufDataset;
  lDataResultItem: TOneDataResultItem;
  i, iParam, iField: integer;
  bCacle: boolean;
  lTemStream: TMemoryStream;
  iDataCount: integer;
  isMulite: boolean;

  lDictParam: TDictionary<string, TOneParam>;
  lOneParam: TOneParam;
  lParamName: string;
begin
  Result := False;
  QErrMsg := '';
  lTemStream := nil;
  bCacle := False;
  isMulite := False;
  lOneDataSet := TOneDataSet(QObject);
  lOneDataSet.DisableControls;
  lDictParam := TDictionary<string, TOneParam>.Create;
  try
    if lOneDataSet.Active then
      lOneDataSet.Close;
    // 关闭
    for iField := 0 to lOneDataSet.Fields.Count - 1 do
    begin
      if lOneDataSet.Fields[iField].FieldKind in [fkInternalCalc] then
      begin
        bCacle := True;
        break;
      end;
    end;
    // 参数赋值
    iDataCount := 0;
    for i := 0 to QDataResult.ResultItems.Count - 1 do
    begin
      lDataResultItem := QDataResult.ResultItems[i];
      if lDataResultItem.ResultDataMode = const_DataReturnMode_Stream then
      begin
        iDataCount := iDataCount + 1;
      end
      else if lDataResultItem.ResultDataMode = const_DataReturnMode_File then
      begin
        iDataCount := iDataCount + 1;
      end
      else if lDataResultItem.ResultDataMode = const_DataReturnMode_JSON then
      begin
        iDataCount := iDataCount + 1;
      end;
    end;
    isMulite := iDataCount > 1;
    if isMulite then
    begin
      if lOneDataSet.MultiData = nil then
      begin
        lOneDataSet.MultiData := TList<TBufDataset>.Create;
      end;
    end;
    if lOneDataSet.MultiData <> nil then
    begin
      for i := 0 to lOneDataSet.MultiData.Count - 1 do
      begin
        lOneDataSet.MultiData[i].Free;
      end;
      lOneDataSet.MultiData.Clear;
    end;
    lOneDataSet.MultiIndex := 0;
    // 处理
    for i := 0 to QDataResult.ResultItems.Count - 1 do
    begin
      lDataResultItem := QDataResult.ResultItems[i];
      if (i = 0) then
      begin
        // 分页信息
        if lDataResultItem.ResultPage then
        begin
          lOneDataSet.DataInfo.PageTotal := lDataResultItem.ResultTotal;
          lOneDataSet.DataInfo.PageCount := 0;
          if lOneDataSet.DataInfo.PageSize > 0 then
          begin
            lOneDataSet.DataInfo.PageCount :=
              ceil(lDataResultItem.ResultTotal / lOneDataSet.DataInfo.PageSize);
          end;
        end;
        // 参数赋值
        if lDataResultItem.ResultParams.Count > 0 then
        begin
          for iParam := 0 to lDataResultItem.ResultParams.Count - 1 do
          begin

            lOneParam := lDataResultItem.ResultParams[iParam];
            lDictParam.Add(lOneParam.ParamName.ToLower, lOneParam);
          end;
          for iParam := 0 to lOneDataSet.Params.Count - 1 do
          begin

            lParamName := lOneDataSet.Params[iParam].Name.ToLower;
            if lDictParam.TryGetValue(lParamName, lOneParam) then
            begin
              lOneDataSet.Params[iParam].Value := lOneParam.ParamValue;
            end;
          end;
        end;
      end;
      if lDataResultItem.ResultDataMode = const_DataReturnMode_Stream then
      begin

        lTemStream := TMemoryStream.Create;
        try
          if not OneStreamString.StreamWriteBase64Str(lTemStream, lDataResultItem.ResultContext) then
          begin
            QErrMsg := '数据集[' + lOneDataSet.Name + ']流写入字符串出错';
            exit;
          end;
          lTemStream.Position := 0;
          if isMulite then
          begin
            tempOneDataSet := TBufDataset.Create(nil);
            tempOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
            lOneDataSet.MultiData.Add(tempOneDataSet);
          end
          else
          begin
            if bCacle then
            begin
              // 有计算字段的比较特殊加载模式
              tempOneDataSet := TBufDataset.Create(nil);
              try
                tempOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
              finally
                tempOneDataSet.Free;
              end;
            end
            else
            begin
              // 无计算字段的整个加载
              lOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
            end;
          end;
        finally
          lTemStream.Clear;
          lTemStream.Free;
        end;
      end
      else if lDataResultItem.ResultDataMode = const_DataReturnMode_File then
      begin
        // 下载文件，比较适合较大数据加载
        lTemStream := self.DownLoadDataFile(lDataResultItem.ResultContext, QErrMsg);
        try
          if not self.IsErrTrueResult(QErrMsg) then
          begin
            exit;
          end;
          lTemStream.Position := 0;
          if isMulite then
          begin
            tempOneDataSet := TBufDataset.Create(nil);
            tempOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
            lOneDataSet.MultiData.Add(tempOneDataSet);
          end
          else
          begin
            if bCacle then
            begin
              // 有计算字段的比较特殊加载模式
              tempOneDataSet := TBufDataset.Create(nil);
              try
                tempOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
              finally
                tempOneDataSet.Free;
              end;
            end
            else
            begin
              // 无计算字段的整个加载
              lOneDataSet.LoadFromStream(lTemStream, TDataPacketFormat.dfBinary);
            end;
          end;
        finally
          if lTemStream <> nil then
          begin
            lTemStream.Clear;
            lTemStream.Free;
          end;
        end;
      end
      else if lDataResultItem.ResultDataMode = const_DataReturnMode_JSON then
      begin
        // JSON序列化
      end;
    end;
    if isMulite then
    begin
      lOneDataSet.MultiIndex := 0;
    end;
    Result := True;
  finally
    lOneDataSet.EnableControls;
    lDictParam.Clear;
    lDictParam.Free;
  end;
end;

function TOneConnection.DownLoadDataFile(QFileID: string; var QErrMsg: string): TMemoryStream;
var
  lUnZipStream: TBytesStream;
  lZipStream: TBytesStream;
  lJonsObj: TJsonObject;
  lJsonValue: TJsonData;
  lOutBytes: TBytes;
  lResultBytes: TOneResultBytes;
  //fpc 3.3 是 TGZipDecompressionStream ，3.2是 Tdecompressionstream
  lDecompress: TInflater;
  lJsonStr: string;
  tempMsg: string;

begin
  Result := nil;
  lResultBytes := nil;
  lOutBytes := nil;
  lJonsObj := TJsonObject.Create;
  try
    lJonsObj.Add('fileID', QFileID);
    lResultBytes := self.PostResultBytes(URL_HTTP_HTTPServer_DATA_DownLoadDataFile, lJonsObj.AsJSON);
    if not lResultBytes.IsOK then
    begin
      QErrMsg := lResultBytes.ErrMsg;
      exit;
    end;
    if not lResultBytes.IsFile then
    begin
      lJsonStr := TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
      lJsonValue := GetJson(lJsonStr);
      if lJsonValue = nil then
      begin
        QErrMsg := '结果转化JSON发生异常:结果为nil,返回信息:' + TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
        exit;
      end;
      QErrMsg := TJsonObject(lJsonValue).Get('ResultMsg', '');
      exit;
    end
    else if (lResultBytes.ContentType.ToLower = 'application/zip') or (lResultBytes.ContentType.ToLower = 'application/octet-stream') then
    begin
      lUnZipStream := TBytesStream.Create;
      lZipStream := TBytesStream.Create(lResultBytes.Bytes);
      lDecompress := nil;
      try
        lZipStream.Position := 0;
        tempMsg := lZipStream.Size.ToString;
        lDecompress := TInflater.Create(lZipStream, lUnZipStream, lZipStream.Size);
        lDecompress.DeCompress;
        lUnZipStream.Position := 0;
        Result := lUnZipStream;
        self.SetErrTrueResult(QErrMsg);
      finally
        if lDecompress <> nil then
          lDecompress.Free;
        lZipStream.Free;
        if not self.IsErrTrueResult(QErrMsg) then
        begin
          lUnZipStream.Free;
          lUnZipStream := nil;
        end;
      end;
      exit;
    end
    else
    begin
      QErrMsg := '未解析的头部内型[' + lResultBytes.ContentType + ']';
      exit;
    end;
  finally
    lJonsObj.Free;
    if lResultBytes <> nil then
      lResultBytes.Free;
    lOutBytes := nil;
  end;

end;

// *********二层事务自由控制***********
// 1.先获取一个账套连接,标记成事务账套
function TOneConnection.LockTran(QTranInfo: TOneTran): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
  lDataResult: TOneDataResult;
begin
  Result := False;
  lErrMsg := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QTranInfo.Msg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  QTranInfo.ZTCode := self.GetZTCode(QTranInfo.ZTCode);
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lDataResult := TOneDataResult.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QTranInfo, lErrMsg);
    if lErrMsg <> '' then
    begin
      QTranInfo.Msg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_LockTranItem, lPosTJsonData.AsJSON, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lDataResult, lResulTJsonData, lErrMsg) then
    begin
      QTranInfo.Msg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lDataResult.ResultMsg <> '' then
    begin
      QTranInfo.Msg := '服务端消息:' + lDataResult.ResultMsg;
    end;
    if lDataResult.ResultOK then
    begin
      QTranInfo.TranID := lDataResult.ResultData;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

// 2.用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
function TOneConnection.UnLockTran(QTranInfo: TOneTran): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
  lDataResult: TOneDataResult;
begin
  Result := False;
  lErrMsg := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QTranInfo.Msg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QTranInfo.TranID = '' then
  begin
    QTranInfo.Msg := '事务TranID为空,请检查.';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lDataResult := TOneDataResult.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QTranInfo, lErrMsg);
    if lErrMsg <> '' then
    begin
      QTranInfo.Msg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_UnLockTranItem, lPosTJsonData.AsJson, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lDataResult, lResulTJsonData, lErrMsg) then
    begin
      QTranInfo.Msg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lDataResult.ResultMsg <> '' then
    begin
      QTranInfo.Msg := '服务端消息:' + lDataResult.ResultMsg;
    end;
    if lDataResult.ResultOK then
    begin
      QTranInfo.TranID := lDataResult.ResultData;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

// 3.开启账套连接事务
function TOneConnection.StartTran(QTranInfo: TOneTran): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
  lDataResult: TOneDataResult;
begin
  Result := False;
  lErrMsg := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QTranInfo.Msg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QTranInfo.TranID = '' then
  begin
    QTranInfo.Msg := '事务TranID为空,请检查.';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lDataResult := TOneDataResult.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QTranInfo, lErrMsg);
    if lErrMsg <> '' then
    begin
      QTranInfo.Msg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_StartTranItem, lPosTJsonData.AsJson, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lDataResult, lResulTJsonData, lErrMsg) then
    begin
      QTranInfo.Msg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lDataResult.ResultMsg <> '' then
    begin
      QTranInfo.Msg := '服务端消息:' + lDataResult.ResultMsg;
    end;
    if lDataResult.ResultOK then
    begin
      QTranInfo.TranID := lDataResult.ResultData;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

// 4.提交账套连接事务
function TOneConnection.CommitTran(QTranInfo: TOneTran): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
  lDataResult: TOneDataResult;
begin
  Result := False;
  lErrMsg := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QTranInfo.Msg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QTranInfo.TranID = '' then
  begin
    QTranInfo.Msg := '事务TranID为空,请检查.';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lDataResult := TOneDataResult.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QTranInfo, lErrMsg);
    if lErrMsg <> '' then
    begin
      QTranInfo.Msg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_CommitTranItem, lPosTJsonData.AsJson, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lDataResult, lResulTJsonData, lErrMsg) then
    begin
      QTranInfo.Msg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lDataResult.ResultMsg <> '' then
    begin
      QTranInfo.Msg := '服务端消息:' + lDataResult.ResultMsg;
    end;
    if lDataResult.ResultOK then
    begin
      QTranInfo.TranID := lDataResult.ResultData;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

// 5.回滚账套连接事务
function TOneConnection.RollbackTran(QTranInfo: TOneTran): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lErrMsg: string;
  lDataResult: TOneDataResult;
begin
  Result := False;
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QTranInfo.Msg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QTranInfo.TranID = '' then
  begin
    QTranInfo.Msg := '事务TranID为空,请检查.';
    exit;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lDataResult := TOneDataResult.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QTranInfo, lErrMsg);
    if lErrMsg <> '' then
    begin
      QTranInfo.Msg := lErrMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_RollbackTranItem, lPosTJsonData.AsJson, lErrMsg);
    if not self.IsErrTrueResult(lErrMsg) then
    begin
      self.FErrMsg := lErrMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lDataResult, lResulTJsonData, lErrMsg) then
    begin
      QTranInfo.Msg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lDataResult.ResultMsg <> '' then
    begin
      QTranInfo.Msg := '服务端消息:' + lDataResult.ResultMsg;
    end;
    if lDataResult.ResultOK then
    begin
      QTranInfo.TranID := lDataResult.ResultData;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
  end;
end;

function TOneConnection.UploadFile(QVirtualInfo: TVirtualInfo): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lActionResult: TActionResultString;
  tempMsg, lFileName: string;
  TempStream: TMemoryStream;
begin
  Result := False;
  QVirtualInfo.ErrMsg := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QVirtualInfo.ErrMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QVirtualInfo.VirtualCode = '' then
  begin
    QVirtualInfo.ErrMsg := '虚拟路径代码[VirtualCode]为空,请检查';
    exit;
  end;
  if QVirtualInfo.LocalFile = '' then
  begin
    QVirtualInfo.ErrMsg := '本地路径文件[LocalFile]为空,请检查';
    exit;
  end;
  // 判断有没有要下载的文件
  if not OneFileHelper.HasExtension(QVirtualInfo.LocalFile) then
  begin
    QVirtualInfo.ErrMsg :=
      '本地路径文件[LocalFile]文件扩展名为空,无法确定是路径还是文件';
    exit;
  end;
  if not FileExists(QVirtualInfo.LocalFile) then
  begin
    QVirtualInfo.ErrMsg := '本地路径文件[LocalFile]文件不存在';
    exit;
  end;
  if QVirtualInfo.RemoteFile = '' then
  begin
    QVirtualInfo.ErrMsg := '路径文件[RemoteFile]为空,请检查';
    exit;
  end;
  // 统一用liunx格式
  // win默认文件路径用\,liun是/ 但win支持/
  QVirtualInfo.LocalFile := OneFileHelper.FormatPath(QVirtualInfo.LocalFile);
  QVirtualInfo.RemoteFile := OneFileHelper.FormatPath(QVirtualInfo.RemoteFile);
  if not OneFileHelper.HasExtension(QVirtualInfo.RemoteFile) then
  begin
    lFileName := ExtractFileName(QVirtualInfo.LocalFile);
    QVirtualInfo.RemoteFile :=
      OneFileHelper.CombinePath(QVirtualInfo.RemoteFile, lFileName);
  end;
  // 开始提交
  TempStream := TMemoryStream.Create;
  try
    TempStream.LoadFromFile(QVirtualInfo.LocalFile);
    TempStream.Position := 0;
    QVirtualInfo.StreamBase64 := OneStreamString.StreamToBase64Str(TempStream);
  finally
    TempStream.Clear;
    TempStream.Free;
  end;
  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lActionResult := TActionResultString.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QVirtualInfo, tempMsg);
    if tempMsg <> '' then
    begin
      QVirtualInfo.ErrMsg := tempMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_UploadFile, lPosTJsonData.AsJson, tempMsg);
    if not self.IsErrTrueResult(tempMsg) then
    begin
      QVirtualInfo.ErrMsg := tempMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lActionResult, lResulTJsonData, tempMsg) then
    begin
      QVirtualInfo.ErrMsg :=
        '返回的数据解析成TOneDataResult出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lActionResult.ResultMsg <> '' then
    begin
      QVirtualInfo.ErrMsg := '服务端消息:' + lActionResult.ResultMsg;
    end;
    if lActionResult.ResultSuccess then
    begin
      // 有可能上传文件名称和最后文件名称不一至
      QVirtualInfo.RemoteFileName := lActionResult.ResultData;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
    lActionResult.Free;
  end;
end;

function TOneConnection.DownloadFile(QVirtualInfo: TVirtualInfo): boolean;
var
  lPosTJsonData, lJsonValue: TJsonData;
  lResultBytes: TOneResultBytes;
  lActionResult: TActionResultString;
  lInStream: TMemoryStream;
  tempMsg, lLocalPath, lAFileName: string;
  lJsonStr: string;
begin
  Result := False;
  lResultBytes := nil;
  QVirtualInfo.ErrMsg := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QVirtualInfo.ErrMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QVirtualInfo.VirtualCode = '' then
  begin
    QVirtualInfo.ErrMsg := '虚拟路径代码[VirtualCode]为空,请检查';
    exit;
  end;
  if QVirtualInfo.RemoteFile = '' then
  begin
    QVirtualInfo.ErrMsg := '路径文件[RemoteFile]为空,请检查';
    exit;
  end;
  // 判断有没有要下载的文件
  if not OneFileHelper.HasExtension(QVirtualInfo.RemoteFile) then
  begin
    QVirtualInfo.ErrMsg :=
      '路径文件[RemoteFile]文件扩展名为空,无法确定是路径还是文件';
    exit;
  end;
  if QVirtualInfo.LocalFile = '' then
  begin
    self.ErrMsg := '本地路径文件[LocalFile]为空,请检查';
    exit;
  end;

  QVirtualInfo.RemoteFile := OneFileHelper.FormatPath(QVirtualInfo.RemoteFile);
  QVirtualInfo.LocalFile := OneFileHelper.FormatPath(QVirtualInfo.LocalFile);
  lAFileName := ExtractFileName(QVirtualInfo.RemoteFile);
  if lAFileName = '' then
  begin
    self.ErrMsg := '远程文件名称为空';
    exit;
  end;
  if not OneFileHelper.HasExtension(QVirtualInfo.LocalFile) then
  begin
    // 组装本地文件
    QVirtualInfo.LocalFile := OneFileHelper.CombinePath(QVirtualInfo.LocalFile, lAFileName);
  end;

  lJsonValue := nil;
  lPosTJsonData := nil;
  lResultBytes := nil;
  lActionResult := TActionResultString.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QVirtualInfo, tempMsg);
    if tempMsg <> '' then
    begin
      QVirtualInfo.ErrMsg := tempMsg;
      exit;
    end;
    lResultBytes := self.PostResultBytes(URL_HTTP_HTTPServer_DATA_DownloadFile, lPosTJsonData.AsJson);
    if not lResultBytes.IsOK then
    begin
      QVirtualInfo.ErrMsg := lResultBytes.ErrMsg;
      exit;
    end;
    lJsonStr := TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
    lJsonValue := GetJson(lJsonStr);
    if lJsonValue = nil then
    begin
      QVirtualInfo.ErrMsg := '结果转化JSON发生异常:结果为nil,返回信息:' + TEncoding.UTF8.GetAnsiString(lResultBytes.Bytes);
      exit;
    end;
    if not OneSerialization.JsonToObject(lActionResult, lJsonValue, tempMsg) then
    begin
      QVirtualInfo.ErrMsg := tempMsg;
      exit;
    end;
    if not lActionResult.ResultSuccess then
    begin
      QVirtualInfo.ErrMsg := '服务端消息:' + lActionResult.ResultMsg;
      exit;
    end;
    lInStream := TMemoryStream.Create;
    try
      OneStreamString.StreamWriteBase64Str(lInStream, lActionResult.ResultData);
      lInStream.Position := 0;
      lLocalPath := ExtractFilePath(QVirtualInfo.LocalFile);
      if not DirectoryExists(lLocalPath) then
      begin
        ForceDirectories(lLocalPath);
      end
      else
      begin
        if FileExists(QVirtualInfo.LocalFile) then
        begin
          DeleteFile(QVirtualInfo.LocalFile);
        end;
      end;
      lInStream.SaveToFile(QVirtualInfo.LocalFile);
      Result := True;
    finally
      lInStream.Free;
    end;
  finally
    if lJsonValue <> nil then
    begin
      lJsonValue.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
    if lResultBytes <> nil then
      lResultBytes.Free;
    lActionResult.Free;
  end;
end;

function TOneConnection.GetTaskID(QVirtualTask: TVirtualTask): boolean;
var
  lPosTJsonData, lResulTJsonData: TJsonData;
  lActionResult: TActionResult<TVirtualTask>;
  tempMsg: string;
  lFileStream: TFileStream;
  lLocalPath, lFileName: string;
begin
  Result := False;
  QVirtualTask.ErrMsg := '';
  QVirtualTask.TaskID := '';
  if not self.FConnected then
  begin
    // 如果已经连接过就不在连接，
    // 除非主动断掉
    QVirtualTask.ErrMsg := '未连接,请先执行DoConnect事件.';
    exit;
  end;
  if QVirtualTask.FileChunSize <= 0 then
    QVirtualTask.FileChunSize := 1024 * 1024 * 1;
  if (QVirtualTask.UpDownMode <> '上传') and (QVirtualTask.UpDownMode <> '下载') then
  begin
    QVirtualTask.ErrMsg := '文件模式[UpDownMode]只能是上传或下载';
    exit;
  end;
  if QVirtualTask.VirtualCode = '' then
  begin
    QVirtualTask.ErrMsg := '虚拟路径代码[VirtualCode]为空,请检查';
    exit;
  end;
  if QVirtualTask.LocalFile = '' then
  begin
    QVirtualTask.ErrMsg := '本地文件[LocateFile]为空,请检查';
    exit;
  end;
  if (QVirtualTask.UpDownMode = '上传') then
  begin
    if not OneFileHelper.HasExtension(QVirtualTask.LocalFile) then
    begin
      QVirtualTask.ErrMsg :=
        '上传本地文件[LocateFile]不能只是路径,无法确定要上传的文件';
      exit;
    end;
    if not FileExists(QVirtualTask.LocalFile) then
    begin
      QVirtualTask.ErrMsg := '不存在本地文件[' + QVirtualTask.LocalFile + '],请检查';
      exit;
    end;
    // 读取文件大小
    lFileStream := TFileStream.Create(QVirtualTask.LocalFile, fmopenRead);
    try
      QVirtualTask.FileTotalSize := lFileStream.Size;
    finally
      lFileStream.Free;
    end;
  end;

  if QVirtualTask.RemoteFile = '' then
  begin
    QVirtualTask.RemoteFile := ExtractFileName(QVirtualTask.LocalFile);
  end;
  if OneFileHelper.DriveExists(QVirtualTask.RemoteFile) then
  begin
    QVirtualTask.ErrMsg :=
      '路径文件[RemoteFile]不可包含驱动，只能是路径/xxx/xxxx/xx';
    exit;
  end;
  if not OneFileHelper.HasExtension(QVirtualTask.RemoteFile) then
  begin
    if (QVirtualTask.UpDownMode = '上传') then
    begin
      // 把RemoteFile组装上文件名称，从本地 LocateFile
      lFileName := ExtractFileName(QVirtualTask.LocalFile);
      QVirtualTask.RemoteFile :=
        OneFileHelper.CombinePath(QVirtualTask.RemoteFile, lFileName);
    end
    else if (QVirtualTask.UpDownMode = '下载') then
    begin

      QVirtualTask.ErrMsg :=
        '下载路径文件[RemoteFile]不包括文件名,无法知道要下载的文件';
      exit;
    end;
  end;

  if (QVirtualTask.UpDownMode = '下载') then
  begin
    if not OneFileHelper.HasExtension(QVirtualTask.LocalFile) then
    begin
      lFileName := ExtractFileName(QVirtualTask.RemoteFile);
      QVirtualTask.LocalFile :=
        OneFileHelper.CombinePath(QVirtualTask.LocalFile, lFileName);
    end;
    lLocalPath := ExtractFilePath(QVirtualTask.LocalFile);
    if not DirectoryExists(lLocalPath) then
    begin
      ForceDirectories(lLocalPath);
    end
    else
    begin
      // 删除老的文件
      if FileExists(QVirtualTask.LocalFile) then
      begin
        DeleteFile(QVirtualTask.LocalFile);
      end;
    end;
  end;

  lResulTJsonData := nil;
  lPosTJsonData := nil;
  lActionResult := TActionResult<TVirtualTask>.Create;
  lActionResult.ResultDataT := TVirtualTask.Create;
  try
    lPosTJsonData := OneSerialization.ObjectToJson(QVirtualTask, tempMsg);
    if tempMsg <> '' then
    begin
      QVirtualTask.ErrMsg := tempMsg;
      exit;
    end;
    lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_GetTaskID, lPosTJsonData.AsJson, tempMsg);
    if not self.IsErrTrueResult(tempMsg) then
    begin
      QVirtualTask.ErrMsg := tempMsg;
      exit;
    end;
    if not OneSerialization.JsonToObject(lActionResult, lResulTJsonData, tempMsg) then
    begin
      QVirtualTask.ErrMsg :=
        '返回的数据解析成TResult<TVirtualTask>出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
      exit;
    end;
    if lActionResult.ResultMsg <> '' then
    begin
      QVirtualTask.ErrMsg := '服务端消息:' + lActionResult.ResultMsg;
    end;
    if lActionResult.ResultSuccess then
    begin
      // 赋值服务端一些相关属性
      QVirtualTask.TaskID := lActionResult.ResultDataT.TaskID;
      QVirtualTask.FileTotalSize := lActionResult.ResultDataT.FileTotalSize;
      QVirtualTask.FileName := lActionResult.ResultDataT.FileName;
      QVirtualTask.NewFileName := lActionResult.ResultDataT.NewFileName;
      Result := True;
    end;
  finally
    if lResulTJsonData <> nil then
    begin
      lResulTJsonData.Free;
    end;
    if lPosTJsonData <> nil then
      lPosTJsonData.Free;
    lActionResult.ResultData.Free;
    lActionResult.Free;
  end;
end;

function TOneConnection.UploadChunkFile(QVirtualTask: TVirtualTask; QUpDownChunkCallBack: EvenUpDownChunkCallBack): boolean;
var
  lFileStream: TFileStream;
  tempMsg: string;
  lPosTJsonData, lResulTJsonData: TJsonData;
  lResult: TActionResultString;
  iChunSize: int64;
  isEnd: boolean;
  tempStream: TMemoryStream;
begin
  Result := False;
  tempMsg := '';
  QVirtualTask.UpDownMode := '上传';
  if QVirtualTask.TaskID = '' then
  begin
    if not self.GetTaskID(QVirtualTask) then
    begin
      if Assigned(QUpDownChunkCallBack) then
      begin
        QUpDownChunkCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownErr,
          0, 0, QVirtualTask.ErrMsg);
      end;
      exit;
    end;
    QVirtualTask.FilePosition := 0;
  end
  else
  begin
    // 不为空情况下可能是续传
  end;
  // 开始读取文件上传
  lFileStream := TFileStream.Create(QVirtualTask.LocalFile, fmopenRead);
  lResult := TActionResultString.Create;
  tempStream := TMemoryStream.Create;
  try
    QVirtualTask.FileTotalSize := lFileStream.Size;
    if Assigned(QUpDownChunkCallBack) then
    begin
      QUpDownChunkCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownStart,
        QVirtualTask.FileTotalSize, 0, '开始上传');
    end;
    while QVirtualTask.FilePosition < QVirtualTask.FileTotalSize do
    begin
      lResult.ResultSuccess := False;
      lResult.ResultMsg := '';
      lResult.ResultCode := '';
      lResult.ResultData := '';
      // 读取本次上传的数据
      lFileStream.Position := QVirtualTask.FilePosition;
      iChunSize := QVirtualTask.FileChunSize;
      if lFileStream.Position + iChunSize > QVirtualTask.FileTotalSize then
      begin
        iChunSize := QVirtualTask.FileTotalSize - lFileStream.Position;
      end;
      tempStream.Clear;
      tempStream.CopyFrom(lFileStream, iChunSize);
      QVirtualTask.StreamBase64 := OneStreamString.StreamToBase64Str(tempStream);
      // 开始上传数据
      lPosTJsonData := nil;
      lResulTJsonData := nil;
      tempMsg := '';
      try
        lPosTJsonData := OneSerialization.ObjectToJson(QVirtualTask, tempMsg);
        if tempMsg <> '' then
        begin
          QVirtualTask.ErrMsg := tempMsg;
          exit;
        end;

        lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_UploadChunkFile, lPosTJsonData.AsJson, tempMsg);
        if not self.IsErrTrueResult(tempMsg) then
        begin
          QVirtualTask.ErrMsg := tempMsg;
          exit;
        end;

        if not OneSerialization.JsonToObject(lResult, lResulTJsonData, tempMsg) then
        begin
          QVirtualTask.ErrMsg :=
            '返回的数据解析成TActionResultString出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
          exit;
        end;
        if lResult.ResultMsg <> '' then
        begin
          QVirtualTask.ErrMsg := '服务端消息:' + lResult.ResultMsg;
        end;
        if not lResult.ResultSuccess then
        begin
          QVirtualTask.ErrMsg := lResult.ResultMsg;
          exit;
        end;
      finally
        if lPosTJsonData <> nil then
          lPosTJsonData.Free;
        if lResulTJsonData <> nil then
          lResulTJsonData.Free;
        // 读取位置加上去
        QVirtualTask.StreamBase64 := '';
        if lResult.ResultSuccess then
          QVirtualTask.FilePosition := QVirtualTask.FilePosition + iChunSize;
        if Assigned(QUpDownChunkCallBack) then
        begin
          if lResult.ResultSuccess then
          begin
            isEnd := QVirtualTask.FilePosition >= QVirtualTask.FileTotalSize;
            if isEnd then
            begin
              QUpDownChunkCallBack(emUpDownMode.UpLoad,
                emUpDownChunkStatus.upDownEnd, QVirtualTask.FileTotalSize,
                QVirtualTask.FilePosition, QVirtualTask.ErrMsg);
            end
            else
            begin
              QUpDownChunkCallBack(emUpDownMode.UpLoad,
                emUpDownChunkStatus.upDownProcess, QVirtualTask.FileTotalSize,
                QVirtualTask.FilePosition, QVirtualTask.ErrMsg);
            end;
          end
          else
          begin
            QUpDownChunkCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownErr,
              QVirtualTask.FileTotalSize, QVirtualTask.FilePosition, QVirtualTask.ErrMsg);
          end;
        end;
      end;
    end;
    Result := True;
  finally
    lFileStream.Free;
    lResult.Free;
    tempStream.Clear;
    tempStream.Free;
  end;
end;

function TOneConnection.DownloadChunkFile(QVirtualTask: TVirtualTask; QUpDownChunkCallBack: EvenUpDownChunkCallBack): boolean;
var
  lFileStream: TFileStream;
  tempMsg: string;
  lPosTJsonData, lResulTJsonData: TJsonData;
  lResult: TActionResultString;
  iChunSize: int64;
  lInStream: TMemoryStream;
  isEnd: boolean;
begin
  Result := False;
  tempMsg := '';
  QVirtualTask.UpDownMode := '下载';
  if QVirtualTask.TaskID = '' then
  begin
    if not self.GetTaskID(QVirtualTask) then
    begin
      if Assigned(QUpDownChunkCallBack) then
      begin
        QUpDownChunkCallBack(emUpDownMode.DownLoad, emUpDownChunkStatus.upDownErr,
          0, 0, QVirtualTask.ErrMsg);
      end;
      exit;
    end;
    QVirtualTask.FilePosition := 0;
  end
  else
  begin
    // 续下载
  end;
  // 开始读取文件上传
  lFileStream := TFileStream.Create(QVirtualTask.LocalFile, fmCreate or fmOpenReadWrite);
  lResult := TActionResultString.Create;
  try
    if Assigned(QUpDownChunkCallBack) then
    begin
      QUpDownChunkCallBack(emUpDownMode.DownLoad, emUpDownChunkStatus.upDownStart,
        QVirtualTask.FileTotalSize, 0, '开始下载');
    end;
    while QVirtualTask.FilePosition < QVirtualTask.FileTotalSize do
    begin
      lResult.ResultSuccess := False;
      lResult.ResultMsg := '';
      lResult.ResultCode := '';
      lResult.ResultData := '';
      // 开始上传数据
      lPosTJsonData := nil;
      lResulTJsonData := nil;
      lInStream := TMemoryStream.Create;
      try
        lPosTJsonData := OneSerialization.ObjectToJson(QVirtualTask, tempMsg);
        if tempMsg <> '' then
        begin
          QVirtualTask.ErrMsg := tempMsg;
          exit;
        end;

        lResulTJsonData := self.PostResultJsonData(URL_HTTP_HTTPServer_DATA_DownloadChunkFile, lPosTJsonData.AsJson, tempMsg);
        if not self.IsErrTrueResult(tempMsg) then
        begin
          QVirtualTask.ErrMsg := tempMsg;
          exit;
        end;

        if not OneSerialization.JsonToObject(lResult, lResulTJsonData, tempMsg) then
        begin
          QVirtualTask.ErrMsg :=
            '返回的数据解析成TActionResultString出错,无法知道结果,数据:' + lResulTJsonData.AsJson;
          exit;
        end;
        if lResult.ResultMsg <> '' then
        begin
          QVirtualTask.ErrMsg := '服务端消息:' + lResult.ResultMsg;
        end;
        if not lResult.ResultSuccess then
        begin
          exit;
        end;
        // 写数据
        OneStreamString.StreamWriteBase64Str(lInStream, lResult.ResultData);
        lInStream.Position := 0;
        lFileStream.Position := QVirtualTask.FilePosition;
        lFileStream.CopyFrom(lInStream, lInStream.Size);
      finally
        if lPosTJsonData <> nil then
          lPosTJsonData.Free;
        if lResulTJsonData <> nil then
          lResulTJsonData.Free;
        lInStream.Free;
        if lResult.ResultSuccess then
        begin
          QVirtualTask.FilePosition :=
            QVirtualTask.FilePosition + QVirtualTask.FileChunSize;
          if Assigned(QUpDownChunkCallBack) then
          begin
            isEnd := QVirtualTask.FilePosition >= QVirtualTask.FileTotalSize;
            if isEnd then
              QUpDownChunkCallBack(emUpDownMode.DownLoad,
                emUpDownChunkStatus.upDownEnd, QVirtualTask.FileTotalSize,
                QVirtualTask.FilePosition, QVirtualTask.ErrMsg)
            else
              QUpDownChunkCallBack(emUpDownMode.DownLoad,
                emUpDownChunkStatus.upDownProcess, QVirtualTask.FileTotalSize,
                QVirtualTask.FilePosition, QVirtualTask.ErrMsg);
          end;
        end
        else
        begin
          QUpDownChunkCallBack(emUpDownMode.DownLoad, emUpDownChunkStatus.upDownErr,
            QVirtualTask.FileTotalSize, QVirtualTask.FilePosition, QVirtualTask.ErrMsg);
        end;
      end;
    end;
    Result := True;
  finally
    lFileStream.Free;
    lResult.Free;
  end;
end;

end.
