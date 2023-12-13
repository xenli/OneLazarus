unit OneGlobal;

{$mode DELPHI}{$H+}


// 一些共用的全局变量统一管理
interface

uses
  SysUtils, Generics.Collections, Classes,
   {$IFDEF ONERTC}
   onertchttpserver,
         {$ELSE}
  OneHttpServer, {$ENDIF}  OneZTManage, OneFileHelper,
  OneTokenManage, OneVirtualFile, OneILog, OneLog, OneSerialization;

type

  // 服务HTTP配置
  TOneServerSet = class
  private
    FHTTPPort: integer; // HTTP端口
    FHTTPPool: integer; // HTTP池数量
    FHTTPQueue: integer; // HTTP队列大小
    FHTTPAutoWork: boolean; // 是否自启动
    FConnectSecretkey: string; // 连接安全密钥
    FTokenIntervalSec: integer;
    // 连接有效安全时间,<=0代表无限长有效,但三天72小时必踢掉，如果没交互
    FWinTaskStart: boolean; // win随开机任务启动,无界面
    FWinRegisterStart: boolean; // win随开机进入界面启动,注册表方式

    FSuperAdminPass: string;
  public
    constructor Create;
  published
    //laz下，发布的属性才会被序列化
    property HTTPPort: integer read FHTTPPort write FHTTPPort;
    property HTTPPool: integer read FHTTPPool write FHTTPPool;
    property HTTPQueue: integer read FHTTPQueue write FHTTPQueue;
    property HTTPAutoWork: boolean read FHTTPAutoWork write FHTTPAutoWork;
    property ConnectSecretkey: string read FConnectSecretkey write FConnectSecretkey;
    property TokenIntervalSec: integer read FTokenIntervalSec write FTokenIntervalSec;
    property WinTaskStart: boolean read FWinTaskStart write FWinTaskStart;
    property WinRegisterStart: boolean read FWinRegisterStart write FWinRegisterStart;

    property SuperAdminPass: string read FSuperAdminPass write FSuperAdminPass;
  end;

  TOneGlobal = class
  private
    // 服务端运行路径
    FExeRunPath: string;
    // 服务端运行EXE名称
    FExeName: string;
    // 日记:
    FLogSet: TOneLogSet;
    FLog: IOneLog;
    // 服务端配置
    FServerSet: TOneServerSet;
    // 账套管理配置
    FZTMangeSet: TOneZTMangeSet;
    // 虚拟目录配置
    FVirtualSet: TOneVirtualSet;
    // HTT服务
    FHTTPServer: TOneHttpServer;
    // 账套管理服务
    FZTManage: TOneZTManage;
    // TOken管理服务
    FTokenManage: TOneTokenManage;

    FVirtualManage: TOneVirtualManage;
    FIsConsole: boolean;
  public
    class function GetInstance(QIsConsole: boolean = False): TOneGlobal; static;
  public
    constructor Create();
    destructor Destroy; override;
    procedure IntiFile();
    function LoadLogSet(): boolean;
    function SaveLogSet(var QErrMsg: string): boolean;
    function LoadServerSet(): boolean;
    function SaveServerSet(var QErrMsg: string): boolean;
    function LoadZTMangeSet(): boolean;
    function SaveZTMangeSet(var QErrMsg: string): boolean;
    function LoadVirtualSet(): boolean;
    function SaveVirtualSet(var QErrMsg: string): boolean;

    function HTTPServerStart(var QErrMsg: string): boolean;
    // 开始工作
    function StarWork(var QErrMsg: string): boolean;
  public
    property LogSet: TOneLogSet read FLogSet;
    property ServerSet: TOneServerSet read FServerSet;
    property ZTMangeSet: TOneZTMangeSet read FZTMangeSet;
    property VirtualSet: TOneVirtualSet read FVirtualSet;
    property HttpServer: TOneHttpServer read FHTTPServer;
    property Log: IOneLog read FLog;
    property ZTManage: TOneZTManage read FZTManage;
    property TokenManage: TOneTokenManage read FTokenManage;
    property VirtualManage: TOneVirtualManage read FVirtualManage;

    property IsConsole: boolean read FIsConsole;
  end;

var
  Unit_OneGlobal: TOneGlobal;
  const_OnePlatform: string = 'OnePlatform';
  const_OneSet: string = 'OneSet';

implementation

uses OneOrm;

constructor TOneServerSet.Create;
begin
  inherited Create;
  FHTTPPort := 9090;
  FHTTPPool := 32;
  FHTTPQueue := 1000;
end;

class function TOneGlobal.GetInstance(QIsConsole: boolean = False): TOneGlobal;
begin
  if Unit_OneGlobal = nil then
  begin
    Unit_OneGlobal := TOneGlobal.Create;
    Unit_OneGlobal.FIsConsole := QIsConsole;
  end;
  Result := Unit_OneGlobal;
end;

constructor TOneGlobal.Create();
var
  lOneLog: TOneLog;
begin
  inherited Create;
  // 日记先实例化
  self.FExeRunPath := OneFileHelper.GetExeRunPath();
  self.FExeName := OneFileHelper.GetExeName;
  // 配置实例化
  self.FLogSet := TOneLogSet.Create;
  self.FServerSet := TOneServerSet.Create;
  self.FZTMangeSet := TOneZTMangeSet.Create;
  self.FVirtualSet := TOneVirtualSet.Create;
  self.IntiFile();
  // 加载日志配置
  self.LoadLogSet();
  // 创建日志
  lOneLog := TOneLog.Create(self.FLogSet);
  // 日志最先工作的
  lOneLog.StarWork;
  self.FLog := lOneLog;
  // 加载HTTP配置
  self.LoadServerSet();
  // 加载账套配置
  self.LoadZTMangeSet();
  // 加载虚拟目录配置
  self.LoadVirtualSet();
  // 服务实例化
  FHTTPServer := TOneHttpServer.Create(self.FLog);
  FZTManage := TOneZTManage.Create(self.FLog);

  FTokenManage := TOneTokenManage.Create(self.FLog);
  FVirtualManage := TOneVirtualManage.Create();
  // 给ORM提供ZT取数据功能
  OneOrm.unit_OrmZTManage := self.FZTManage;
end;

destructor TOneGlobal.Destroy;
begin
  OneOrm.unit_OrmZTManage := nil;
  if FServerSet <> nil then
    FServerSet.Free;
  if FZTMangeSet <> nil then
    FZTMangeSet.Free;
  if FVirtualSet <> nil then
    FVirtualSet.Free;
  if FHTTPServer <> nil then
  begin
    FHTTPServer.Free;
  end;
  if FZTManage <> nil then
  begin
    FZTManage.Free;
  end;
  if FTokenManage <> nil then
  begin
    FTokenManage.Free;
  end;
  if FVirtualManage <> nil then
  begin
    FVirtualManage.Free;
  end;
  if FLog <> nil then
    FLog.StopWork;
  inherited Destroy;
end;

procedure TOneGlobal.IntiFile();
const
  // fireDac需要各自DLL驱动，把驱动放到Exe相关目录下面即可
  const_OraOciDll32: string = 'OnePhyDBDLL\OracleDll\32';
  const_OraOciDll64: string = 'OnePhyDBDLL\OracleDll\64';
  const_Libmysql32: string = 'OnePhyDBDLL\mySQLDLL\32';
  const_Libmysql64: string = 'OnePhyDBDLL\mySQLDLL\64';
  const_Libpg32: string = 'OnePhyDBDLL\pgDLL\32';
  const_Libpg64: string = 'OnePhyDBDLL\pgDLL\64';
  const_Libfb32: string = 'OnePhyDBDLL\fbDLL\32';
  const_Libfb64: string = 'OnePhyDBDLL\fbDLL\64';
  const_Libfbb32: string = 'OnePhyDBDLL\fbbDLL\32';
  const_Libfbb64: string = 'OnePhyDBDLL\fbbDLL\64';
  const_3SQLite32: string = 'OnePhyDBDLL\SQLite3\32';
  const_3SQLite64: string = 'OnePhyDBDLL\SQLite3\64';
  const_ASA32: string = 'OnePhyDBDLL\ASA\32';
  const_ASA64: string = 'OnePhyDBDLL\ASA\64';
  const_ODBC32: string = 'OnePhyDBDLL\ODBC\32';
  const_ODBC64: string = 'OnePhyDBDLL\ODBC\64';
var
  tempPath: string;
  lListPath: TList<string>;
  i: integer;
begin
  tempPath := OneFileHelper.CombinePath(self.FExeRunPath, const_OnePlatform);
  if not DirectoryExists(tempPath) then
    ForceDirectories(tempPath);
  lListPath := TList<string>.Create;
  try
    // 配置默认目录
    lListPath.Add(const_OneSet);
    // 日记默认目录
    lListPath.Add('OneLogs');
    // 临时存储数据的
    lListPath.Add('OneDataTemp');
    lListPath.Add('OneWeb');
    // 以下多是驱动的目录
    lListPath.Add(const_OraOciDll32);
    lListPath.Add(const_OraOciDll64);
    lListPath.Add(const_Libmysql32);
    lListPath.Add(const_Libmysql64);
    lListPath.Add(const_Libpg32);
    lListPath.Add(const_Libpg64);
    lListPath.Add(const_Libfb32);
    lListPath.Add(const_Libfb64);
    lListPath.Add(const_Libfbb32);
    lListPath.Add(const_Libfbb64);
    lListPath.Add(const_3SQLite32);
    lListPath.Add(const_3SQLite64);
    lListPath.Add(const_ASA32);
    lListPath.Add(const_ASA64);
    lListPath.Add(const_ODBC64);
    lListPath.Add(const_ODBC64);
    for i := 0 to lListPath.Count - 1 do
    begin
      tempPath := OneFileHelper.CombinePathC(self.FExeRunPath, const_OnePlatform, lListPath[i]);
      if not DirectoryExists(tempPath) then
        ForceDirectories(tempPath);
    end;
  finally
    lListPath.Clear;
    lListPath.Free;
  end;
end;

function TOneGlobal.LoadLogSet(): boolean;
var
  lLogSetFileName, lErrMsg: string;
begin
  lLogSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneLogSet.JSON');
  // 加载配置
  OneSerialization.JSONToObjectFormFile(self.FLogSet, lLogSetFileName, lErrMsg);
  // 序列化
  OneSerialization.ObjectToJsonFile(self.FLogSet, lLogSetFileName, lErrMsg);
end;

function TOneGlobal.SaveLogSet(var QErrMsg: string): boolean;
var
  lLogSetFileName: string;
begin
  lLogSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneLogSet.JSON');
  Result := OneSerialization.ObjectToJsonFile(self.FLogSet, lLogSetFileName, QErrMsg);
end;

function TOneGlobal.LoadServerSet(): boolean;
var
  lServerSetFileName, lErrMsg: string;
begin
  lServerSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneServerSet.JSON');
  // 加载配置
  OneSerialization.JSONToObjectFormFile(self.FServerSet,
    lServerSetFileName, lErrMsg);
  // 序列化
  OneSerialization.ObjectToJsonFile(self.FServerSet, lServerSetFileName, lErrMsg);
  // self.SaveServerSet(lErrMsg);
  // 保存
end;

function TOneGlobal.SaveServerSet(var QErrMsg: string): boolean;
var
  lServerSetFileName: string;
begin
  lServerSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneServerSet.JSON');
  Result := OneSerialization.ObjectToJsonFile(self.FServerSet, lServerSetFileName, QErrMsg);
end;

function TOneGlobal.StarWork(var QErrMsg: string): boolean;
begin
  Result := False;
  QErrMsg := '';
  if self.IsConsole then
    writeln(Utf8DeCode('**启动账套**'));
  if FZTManage <> nil then
  begin
    if FZTMangeSet.AutoWork then
    begin
      // 开启添加账套
      FZTManage.StarWork(FZTMangeSet.ZTSetList, QErrMsg);
      if self.IsConsole then
        writeln(Utf8DeCode('启动账套成功'));
    end
    else
    begin
      if self.IsConsole then
        writeln(Utf8DeCode('账套未启动,原因账套设置不是自启动'));
    end;
  end;
  if self.IsConsole then
    writeln(Utf8DeCode('**启动HTTP服务**'));
  if FHTTPServer <> nil then
  begin
    if self.IsConsole then
      writeln(Utf8DeCode('HTTP端口:' + IntToStr(FServerSet.FHTTPPort)));
    FHTTPServer.Port := FServerSet.FHTTPPort;
    FHTTPServer.ThreadPoolCount := FServerSet.FHTTPPool;
    FHTTPServer.HttpQueueLength := FServerSet.FHTTPQueue;
    if FServerSet.FHTTPAutoWork then
    begin
      if not FHTTPServer.ServerStart then
      begin
        QErrMsg := FHTTPServer.ErrMsg;
        if self.IsConsole then
          writeln(Utf8DeCode('启动HTTP异常原因:' + QErrMsg));
      end
      else
      begin
        if self.IsConsole then
          writeln(Utf8DeCode('启动HTTP服务成功'));
      end;
    end
    else
    begin
      if self.IsConsole then
        writeln(Utf8DeCode('HTTP服务未启动,原因HTTP服务设置不是自启动'));
    end;
  end;
  if FVirtualManage <> nil then
  begin
    FVirtualManage.StarWork(self.FVirtualSet.VirtualSetList);
  end;
  Result := True;
end;

function TOneGlobal.LoadZTMangeSet(): boolean;
var
  lZTMangeSetFileName, lErrMsg: string;
begin
  // 读取配置
  lZTMangeSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneZTMangeSet.JSON');
  OneSerialization.JSONToObjectFormFile(self.FZTMangeSet,
    lZTMangeSetFileName, lErrMsg);
  // 保存配置
  OneSerialization.ObjectToJsonFile(self.FZTMangeSet, lZTMangeSetFileName,
    lErrMsg);
end;

function TOneGlobal.SaveZTMangeSet(var QErrMsg: string): boolean;
var
  lZTMangeSetFileName: string;
begin
  lZTMangeSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneZTMangeSet.JSON');
  Result := OneSerialization.ObjectToJsonFile(self.FZTMangeSet, lZTMangeSetFileName, QErrMsg);
end;

function TOneGlobal.LoadVirtualSet(): boolean;
var
  lVirtualSetFileName, lErrMsg: string;
begin
  // 读取配置
  lVirtualSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneVirtualSet.JSON');
  OneSerialization.JSONToObjectFormFile(self.FVirtualSet,
    lVirtualSetFileName, lErrMsg);
  // 保存配置
  OneSerialization.ObjectToJsonFile(self.FVirtualSet, lVirtualSetFileName,
    lErrMsg);
end;

function TOneGlobal.SaveVirtualSet(var QErrMsg: string): boolean;
var
  lVirtualSetFileName: string;
begin
  lVirtualSetFileName := OneFileHelper.CombinePathD(self.FExeRunPath, const_OnePlatform, const_OneSet, 'OneVirtualSet.JSON');
  Result := OneSerialization.ObjectToJsonFile(self.FVirtualSet, lVirtualSetFileName, QErrMsg);
end;

function TOneGlobal.HTTPServerStart(var QErrMsg: string): boolean;
begin
  Result := False;
  QErrMsg := '';
  if FHTTPServer.Started then
  begin
    QErrMsg := '已运行，无需在运行，当前运行端口:' + FHTTPServer.Port.ToString();
    exit;
  end;

  FHTTPServer.Port := FServerSet.FHTTPPort;
  FHTTPServer.ThreadPoolCount := FServerSet.FHTTPPool;
  FHTTPServer.HttpQueueLength := FServerSet.FHTTPQueue;
  if not FHTTPServer.ServerStart then
  begin
    QErrMsg := FHTTPServer.ErrMsg;
    exit;
  end;
  Result := True;
end;

end.
