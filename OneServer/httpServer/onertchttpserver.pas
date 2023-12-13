unit onertchttpserver;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, rtcHttpSrv, rtcConn, rtcSystem,
  rtcInfo, rtcDataSrv, oneILog, httpprotocol, URIParser,
  OneHttpCtxtResult, OneMultipart, OneFileHelper;

type
  TOneHttpServer = class
  private
    // HTTP服务是否启动
    FStarted: boolean;
    // 拒绝请求
    FStopRequest: boolean;
    // 绑定HTTP端口，默认9090
    FPort: integer;
    // 线程池 ThreadPoolCount<0 将使用单个线程来对其进行全部规则, =0将为每个连接创建一个线程,>0将利用线程池
    // 一般设定大于0用线程池性能最好
    FThreadPoolCount: integer;
    // 默认30000毫秒，即30秒 连接保持活动的时间
    FKeepAliveTimeOut: integer;
    // 队列默认1000,请求放进队列,然后有线程消费完成
    FHttpQueueLength: integer;
    FHttpServer: TRtcHttpServer;
    FHttpDataProvider: TRtcDataProvider;
    // 错误消息
    FErrMsg: string;
    FLog: IOneLog;
  protected
    // 重写请求事件
    procedure onCheckRequest(Sender: TRtcConnection);
    procedure OnRequest(Ctxt: TRtcConnection);
  public
    constructor Create(QLog: IOneLog); overload;
    destructor Destroy; override;
  public
    // 启动服务
    function ServerStart(): boolean;
    // 停止服务
    function ServerStop(): boolean;
    // 拒绝任何请求
    function ServerStopRequest(): boolean;
  published
    // HTTP服务是否启动
    property Started: boolean read FStarted;
    // 拒绝请求
    property StopRequest: boolean read FStopRequest;
    // 绑定HTTP端口，默认9090
    property Port: integer read FPort write FPort;
    // 线程池
    property ThreadPoolCount: integer read FThreadPoolCount write FThreadPoolCount;
    // 默认30000毫秒，即30秒 连接保持活动的时间
    property KeepAliveTimeOut: integer read FKeepAliveTimeOut write FKeepAliveTimeOut;
    // 队列默认1000,请求放进队列,然后有线程消费完成
    property HttpQueueLength: integer read FHttpQueueLength write FHttpQueueLength;
    // 错误消息
    property ErrMsg: string read FErrMsg write FErrMsg;
  end;

function CreateNewHTTPCtxt(Ctxt: TRtcConnection): THTTPCtxt;

implementation

uses OneStopwatch, OneHttpRouterManage, OneHttpController, OneGlobal;

{ TOneHttpServer }


constructor TOneHttpServer.Create(QLog: IOneLog);
begin
  inherited Create;
  self.FLog := QLog;
  self.FStarted := False;
  self.FStopRequest := False;
  self.FPort := 9090;
  self.FThreadPoolCount := 100;
  self.FKeepAliveTimeOut := 30000;
  self.FHttpQueueLength := 1000;
end;

destructor TOneHttpServer.Destroy;
begin
  if FHttpServer <> nil then
  begin
    FHttpServer.Free;
  end;
  inherited Destroy;
end;

procedure TOneHttpServer.onCheckRequest(Sender: TRtcConnection);
begin
  Sender.Accept;
end;

procedure TOneHttpServer.OnRequest(Ctxt: TRtcConnection);
var
  lErrMsg: string; // 错误消息
  lIsErr: boolean; // 是否产生错误
  lwatchTimer: TStopwatch; // 时间计数器
  lRequestMilSec: integer;

  lURI: TURI;
  lUrlPath: string; // URL路径
  lRouterUrlPath: TOneRouterUrlPath; // URL路径对应的路由信息
  lRouterItem: TOneRouterItem;
  lWorkObj: TObject; // 控制器对象
  lOneControllerWork: TOneControllerBase; // 控制器对象

  lHTTPResult: THTTPResult; // HTTP执行结果，统一接口格式化
  lHTTPCtxt: THTTPCtxt; // HTTP请求相关信息,转成内部一个类处理
  // 静态文件输出
  lFileName, lFileCode, lPhy: string;
  tempI: integer;
  tempMsg: string;
  lStatusCode: integer;
begin
  if not Ctxt.Request.Complete then
  begin
    exit;
  end;
  lErrMsg := '';
  lIsErr := False;
  lHTTPResult := nil;
  lHTTPCtxt := nil;
  lStatusCode := 0;
  if self.FStopRequest then
  begin
    // 停止请求，返回404
    Ctxt.Response.StatusCode := 404;
    exit;
  end;
  lURI := ParseURI('http://' + Ctxt.Request.URL);
  lUrlPath := lURI.Path + lURI.Document;
  lUrlPath := OneHttpCtxtResult.FormatRootName(lUrlPath);
  lwatchTimer := TStopwatch.StartNew;
  try
    try
      // 解析URL调用相关路由方法
      // lUrlPath='/'+注册RootName根路径/控制器方法名称
      lRouterUrlPath := OneHttpRouterManage.GetInitRouterManage().GetRouterUrlPath(lUrlPath, lErrMsg);
      if lRouterUrlPath <> nil then
      begin
        lRouterItem := lRouterUrlPath.RouterItem;
        lHTTPResult := CreateNewHTTPResult;
        lHTTPCtxt := CreateNewHTTPCtxt(Ctxt);
        lHTTPCtxt.ControllerMethodName := lRouterUrlPath.MethodName;
        // 跟据路由模式锁定不同模式干活
        if (lRouterItem.RouterMode = emRouterMode.pool) or (lRouterItem.RouterMode = emRouterMode.single) then
        begin
          lWorkObj := lRouterItem.LockWorkItem(lErrMsg);
          if lErrMsg <> '' then
          begin
            Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
            Ctxt.Write(UTF8Encode(lErrMsg));
            lStatusCode := 500;
            lIsErr := True;
            exit;
          end;

          try
            if (lWorkObj = nil) then
            begin
              Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
              Ctxt.Write('获取的控制器对象为nil');
              lStatusCode := 500;
              lIsErr := True;
              exit;
            end;
            if not (lWorkObj is TOneControllerBase) then
            begin
              Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
              Ctxt.Write('控制器请继承TOneControllerBase');
              lStatusCode := 500;
              lIsErr := True;
              exit;
            end;
            lOneControllerWork := TOneControllerBase(lWorkObj);
            lStatusCode := lOneControllerWork.DoWork(lHTTPCtxt, lHTTPResult, lRouterItem);
          finally
            // 归还控制器
            lRouterItem.UnLockWorkItem(lWorkObj);
          end;
        end
        else
        begin
          Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
          Ctxt.Write('[TOneHttpServer.OnRequest]未设计的路由模式');
          lStatusCode := 500;
          lIsErr := True;
          exit;
        end;
      end
      else
      begin
        if (lUrlPath = '') or (lUrlPath = '/one') then
        begin
          Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
          tempMsg := '欢迎来到OneLaz世界!!!!';
          Ctxt.Write(UTF8Encode(tempMsg));
          lStatusCode := 200;
          exit;
        end;
        if lUrlPath.StartsWith('/oneweb/') then
        begin
          lFileName := lUrlPath.Substring(8, lUrlPath.Length - 8);
          // 有中文进行解码
          lFileName := HTTPDecode(lFileName);
          lFileName := OneFileHelper.CombineExeRunPathB('OnePlatform\OneWeb', lFileName);
          Ctxt.Write(Read_File(lFileName));
          lStatusCode := 200;
          exit;
        end;
        if lUrlPath.StartsWith('/onewebv/') then
        begin
          // 返回虚拟目录站点文件/onewebv/虚拟文件代码/文件路径
          lUrlPath := lUrlPath.Substring(9, lUrlPath.Length - 9);
          // 取出虚拟文件代码,和虚拟文件路径
          tempI := lUrlPath.IndexOf('/');
          lFileCode := lUrlPath.Substring(0, tempI);
          lFileName := lUrlPath.Substring(tempI, lUrlPath.Length - tempI);

          lPhy := TOneGlobal.GetInstance().VirtualManage.GetVirtualPhy(lFileCode, lErrMsg);
          if lErrMsg <> '' then
          begin
            Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
            Ctxt.Write(UTF8Encode(lErrMsg));
            exit;
          end;
          lFileName := HTTPDecode(lFileName);
          lFileName := OneFileHelper.CombinePath(lPhy, lFileName);
          Ctxt.Write(Read_File(lFileName));
          //Ctxt.OutCustomHeaders :=
          //  GetMimeContentTypeHeader('', Ctxt.OutContent) + #13#10 + 'OneOutMode: OUTFILE';
          lStatusCode := 200;
          exit;
        end;
        Ctxt.Write(UTF8Encode(lErrMsg));
        Ctxt.Response.ContentType := 'text/plain; charset=UTF-8';
        lIsErr := True;
        exit;
      end;
      if lStatusCode = 0 then
      begin
        // DoWork里面也有可能返回来
        lStatusCode := 200;
      end;
    finally
      if lHTTPResult <> nil then
      begin
        if lStatusCode = 500 then
        begin
          // 服务端异常不处理结果
          Ctxt.Write(UTF8Encode(lHTTPResult.ResultException));
        end
        else if lHTTPResult.ResultOutMode = THTTPResultMode.OUTFILE then
        begin
          Ctxt.Write(Read_File(lHTTPCtxt.OutContent));
          lStatusCode := 200;
        end
        else if lHTTPResult.ResultOutMode = THTTPResultMode.HTML then
        begin
          Ctxt.Write(lHTTPCtxt.OutContent);
          Ctxt.Response.ContentType := 'text/html; charset=UTF-8';
          lStatusCode := 200;
        end
        else
        begin
          Ctxt.Response.ContentType :=
            'text/plain;charset=' + lHTTPCtxt.RequestAcceptCharset;
          Ctxt.Response.HeaderText := lHTTPCtxt.ResponCustHeaderList;
          Ctxt.Write(lHTTPCtxt.OutContent);
        end;
        lHTTPResult.Free;
      end;
      if lHTTPCtxt <> nil then
        lHTTPCtxt.Free;
      if lIsErr then
      begin
        if lStatusCode = 500 then
        begin
          // 抛出异常到前端
          // raise Exception.Create(Ctxt.OutContent);
        end
        else
        begin
          lStatusCode := 404;
        end;
      end;
    end;
  finally
    Ctxt.Response.StatusCode := lStatusCode;
    lwatchTimer.Stop;
    lRequestMilSec := lwatchTimer.ElapsedMilliseconds;
    if (self.FLog <> nil) and (self.FLog.IsHTTPLog) then
    begin
      self.FLog.WriteHTTPLog('请求用时:[' + lRequestMilSec.ToString + ']毫秒');
      self.FLog.WriteHTTPLog('请求URL:[' + Ctxt.Request.URL + ']');
      if not IsMultipartForm(Ctxt.Request.ContentType) then
      begin
        self.FLog.WriteHTTPLog('请求内容:' + Ctxt.Request.ContentType);
        // self.FLog.WriteHTTPLog('输出内容:' + Ctxt.OutContent);
      end;
    end;
  end;
end;

// 启动服务
function TOneHttpServer.ServerStart(): boolean;
var
  CertificateFile, PrivateKeyFile: string;
begin
  Result := False;
  // 已经启动
  if FStarted then
  begin
    self.FErrMsg := 'HTTP服务已启动无需在启动';
    exit;
  end;
  if (self.FPort <= 0) then
  begin
    self.FErrMsg := 'HTTP服务端口未设置,请先设置,当前绑定端口【' + self.FPort.ToString() + '】';
    exit;
  end;
  if self.FThreadPoolCount > 1000 then
    self.FThreadPoolCount := 1000;
  if self.FHttpQueueLength <= 0 then
    self.FHttpQueueLength := 1000;
  // 创建HTTP服务
  try
    //HTTP服务
    self.FHttpServer := TRtcHttpServer.Create(nil);
    self.FHttpServer.ServerPort := self.FPort.ToString();
    //HTTP服务数据处理
    self.FHttpDataProvider := TRtcDataProvider.Create(nil);
    self.FHttpDataProvider.Server := self.FHttpServer;
    self.FHttpDataProvider.OnCheckRequest := SELF.onCheckRequest;
    self.FHttpDataProvider.OnDataReceived := self.OnRequest;
    //开启HTTP服务
    self.FHttpServer.Listen();
    self.FStopRequest := False;
    self.FStarted := True;
    Result := True;
  except
    on e: Exception do
    begin
      self.FErrMsg := '启动服务器失败,原因:' + e.Message +
        ';解决方案可偿试管理员启动程序或换个端口临听（端口重复绑定）。';
      self.FHttpServer.Free;
      self.FHttpServer := nil;
    end;
  end;

end;

// 停止服务
function TOneHttpServer.ServerStop(): boolean;
begin
  Result := False;
  if self.FHttpServer <> nil then
  begin
    self.FHttpServer.StopListen;
  end;
  if self.FHttpDataProvider <> nil then
  begin
    self.FHttpDataProvider.Free;
  end;
  if self.FHttpServer <> nil then
  begin
    self.FHttpServer.Free;
    self.FHttpServer := nil;
  end;
  self.FStarted := False;
  Result := True;
end;

// 拒绝任何请求
function TOneHttpServer.ServerStopRequest(): boolean;
begin
  // 返回  HTTP_NOTFOUND 404
  Result := False;
  self.FStopRequest := True;
  Result := True;
end;

// 提到外面来底程和什么通讯无关
function CreateNewHTTPCtxt(Ctxt: TRtcConnection): THTTPCtxt;
begin
  Result := THTTPCtxt.Create;
  Result.UrlParams := TStringList.Create;
  Result.HeadParamList := TStringList.Create;
  Result.ResponCustHeaderList := '';
  Result.RequestContentTypeCharset := 'UTF-8';
  Result.RequestAcceptCharset := 'UTF-8';
  // 解析
  Result.Method := Ctxt.Request.Method;
  Result.RequestContentType := Ctxt.Request.ContentType;

  Result.Url := Ctxt.Request.URL;
  Result.SetUrlParams();
  Result.RequestInHeaders := Ctxt.Request.HeaderText;
  Result.SetHeadParams();
  // String类型，不要直接等于  Ctxt.InContent 当参数传进去处理
  // 不难有可能编码会被打乱,碰到一种就加一种在  SetInContent统一处理
  // Result.RequestInContent := Ctxt.InContent;
  // 如果没有你想要的编码解析,联系群主，加上或者你搞好了，发给群主合并
  Result.SetInContent(Ctxt.Read);
  Result.TokenUserCode := '';
end;

end.
