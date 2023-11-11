unit OneZTManage;

{$mode DELPHI}{$H+}

interface

uses
  Generics.Collections, SyncObjs, DB, SysUtils, SQLDB,
  Classes, StrUtils, Variants, TypInfo,
  OneDataInfo, OneThread, OneGUID, OneFileHelper,
  OneStreamString, OneILog, DateUtils, BufDataset, fpjson, jsonparser,
  Zipper, zstream, SQLDBLib, OneSerialization, RegExpr, OneDataJson,
  SQLite3Conn,
  PQConnection,
  oracleconnection,
  odbcconn,
  mysql40conn,
  mysql41conn,
  mysql50conn,
  mysql51conn,
  mysql55conn,
  mysql56conn,
  mysql57conn,
  mysql80conn,
  MSSQLConn;

const
  Driver_MSSQLServer = 'MSSQLServer';
  Driver_MySQL = 'MySQL';
  Driver_Oracle = 'Oracle';
  Driver_PostgreSQL = 'PostgreSQL';
  Driver_SQLite3 = 'SQLite3';
  Driver_Sybase = 'Sybase';
  Driver_Firebird = 'Firebird';

type
  TOneZTSet = class;
  TOneZTMangeSet = class;
  TOneZTItem = class;
  TOneZTPool = class;
  TOneZTManage = class;
  TOneFDException = class;
  emZTKeepMode = (keepTran, keepTempData);

  TSQLInfo = record
    FDriver: string;
    FDriverVersion: string;
    FPageIndex: integer;
    FPageSize: integer;
    FSQL: string;
    FOrderByLine: integer;
    FOrderSQL: string;
    FPageField: string;
    FErrMsg: string;
  end;

  TFDConnection = class(TSQLConnector)

  end;

  TFDTransaction = class(TSQLTransaction)
  private
    FIsInTran: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure TranStart;
    procedure TranCommit;
    procedure TranRollback;
  public
    property IsInTran: boolean read FIsInTran write FIsInTran;
  end;

  TFDQuery = class(TSQLQuery)
  private
    FTableName: string;
    FKeyFields: string;
  public
    property TableName: string read FTableName write FTableName;
    property KeyFields: string read FKeyFields write FKeyFields;
  end;

  TFDScript = class(TSQLScript)

  end;

  TFDStoredProc = class(TSQLQuery)
  private
    FStoredProcName: string;
    FPackageName: string;
  public
    property StoredProcName: string read FStoredProcName write FStoredProcName;
    property PackageName: string read FPackageName write FPackageName;
  end;

  TFDMemtable = class(TBufDataset)

  end;

  TFDParam = class(TParams)

  end;

  TOneFDException = class(TObject)
  public
    FErrmsg: string;
  end;

  TZTKeepInfo = class
  private
    FKeepID: string;
    FKeepMode: emZTKeepMode;
    FKeepSec: integer;
    FLastTime: TDateTime;
  end;

  // 账套配置
  TOneZTSet = class
  private
    FZTCode: string; // 账套代码
    FZTCaption: string; // 账套标签
    FInitPoolCount: integer; // 初始化账套池数量
    FMaxPoolCount: integer; // 最大账套池数量
    FConnectionStr: string; // 连接字符串
    FIsEnable: boolean; // 是否启用
    FIsMain: boolean; // 默认主账套
    FPhyDriver: string; // 驱动类型
    FDBType: string;
    FDBVersion: string;
    FDBHostName: string;
    FDBCharSet: string;
    FDBHostPort: integer;
    FDBName: string;
    FDBUserName: string;
    FDBUserPass: string;
    FDBKeepConnect: boolean;
    FDBOtherParams: string;
  published
    property ZTCode: string read FZTCode write FZTCode;
    property ZTCaption: string read FZTCaption write FZTCaption;
    property InitPoolCount: integer read FInitPoolCount write FInitPoolCount;
    property MaxPoolCount: integer read FMaxPoolCount write FMaxPoolCount;
    property ConnectionStr: string read FConnectionStr write FConnectionStr;
    property IsEnable: boolean read FIsEnable write FIsEnable;
    property IsMain: boolean read FIsMain write FIsMain;
    property PhyDriver: string read FPhyDriver write FPhyDriver;
    property DBType: string read FDBType write FDBType;
    property DBVersion: string read FDBVersion write FDBVersion;
    property DBHostName: string read FDBHostName write FDBHostName;
    property DBCharSet: string read FDBCharSet write FDBCharSet;
    property DBHostPort: integer read FDBHostPort write FDBHostPort;
    property DBName: string read FDBName write FDBName;
    property DBUserName: string read FDBUserName write FDBUserName;
    property DBUserPass: string read FDBUserPass write FDBUserPass;
    property DBKeepConnect: boolean read FDBKeepConnect write FDBKeepConnect;
    property DBOtherParams: string read FDBOtherParams write FDBOtherParams;
  end;

  TOneZTMangeSet = class
  private
    FAutoWork: boolean;
    FZTSetList: TList<TOneZTSet>;
  public
    constructor Create();
    destructor Destroy; override;
  published
    property AutoWork: boolean read FAutoWork write FAutoWork;
    property ZTSetList: TList<TOneZTSet> read FZTSetList write FZTSetList;
  end;

  { 一个连接 }
  TOneZTItem = class(TObject)
  private
    FCreateID: string; // 每个一个编号

    FOwnerZTPool: TOneZTPool;
    FIsWorking: boolean; // 正在工作中
    // FD套件,每个连接创建多有这几个，拿来就用，省去创建的过程
    FDConnection: TFDConnection;
    FDTransaction: TFDTransaction;
    FDQuery: TFDQuery;
    FDScript: TFDScript;
    FDStoredProc: TFDStoredProc;
    // 临时保存流的地方
    FTempStream: TMemoryStream;
    FException: TOneFDException;

    FCharacterSet: string;
    // 二层一样控由客户端控制事务
    FCustTran: boolean;
    // 锁定最长时间,毫秒为单位
    // -1无限长,客户端调起释放才释放，如果事务还在执行中
    // 0 代表事务不得超过30分钟，如果30分钟还没事务完成,服务端回滚事务
    // >0  代表事务不得超过>0分钟，如果>0分钟还没事务完成,服务端回滚事务
    FCustTranMaxSpanSec: integer;
    // 最后交互时间
    FLastTime: TDateTime;
    FZTSet: TOneZTSet;
  private
    // 单纯获取一个连接 FDConnection
    function GetADConnection: TFDConnection;
    // 获取一个事务连接 FDTransaction其中connection已绑定 FDConnection
    function GetADTransaction: TFDTransaction;
    // 获取一个Query查询 其中connection已绑定 FDConnection
    function GetQuery: TFDQuery;
    // 获取一个Script角本执行 其中connection已绑定 FDConnection
    function GetScript: TFDScript;
    // 获取一个Stored存储过程执行器 其中connection已绑定 FDConnection
    function GetStoredProc: TFDStoredProc;
    function GetTempStream: TMemoryStream;
    procedure FDQueryError(ASender, AInitiator: TObject; var AException: Exception);
    procedure FDScriptError(ASender, AInitiator: TObject; var AException: Exception);
  public
    constructor Create(AOwner: TOneZTPool; QZTSet: TOneZTSet); overload;
    destructor Destroy; override;
    procedure UnLockWork();
  public
    property ADConnection: TFDConnection read FDConnection;
    property ADTransaction: TFDTransaction read GetADTransaction;
    property ADQuery: TFDQuery read GetQuery;
    property ADScript: TFDScript read GetScript;
    property ADStoredProc: TFDStoredProc read GetStoredProc;
    property IsWorking: boolean read FIsWorking write FIsWorking;
    property DataStream: TMemoryStream read GetTempStream;
    property ZTSet: TOneZTSet read fZTSet;
  end;

  { 一个账套池 }
  TOneZTPool = class(TObject)
  private
    FZTManage: TOneZTManage;
    FZTCode: string; // 账套代码
    FInitPoolCount: integer; // 初如化池数量
    FMaxPoolCount: integer; // 最大池数量
    FPoolCreateCount: integer; // 已创建池数量
    FPoolWorkCount: integer; // 正在工作的数量

    FPhyDriver: string; // 数据库驱动
    FConnectionStr: string; // 数据库连接
    FStop: boolean; // 停止运作
    FLockObj: TCriticalSection; // 锁
    FZTItems: TList<TOneZTItem>; // 账套池
    FZTSet: TOneZTSet;
  public
    // 创建一个池
    constructor Create(QZTManage: TOneZTManage; QZTSet: TOneZTSet); overload;
    destructor Destroy; override;
    // 从池中锁定一个账套出来
    function LockZTItem(var QErrMsg: string): TOneZTItem;
    procedure UnLockWorkCount();
  public

  public
    property Stop: boolean read FStop write FStop;
    property ZTCode: string read FZTCode write FZTCode;
    property InitPoolCount: integer read FInitPoolCount; // 初如化池数量
    property MaxPoolCount: integer read FMaxPoolCount; // 最大池数量
    property PoolCreateCount: integer read FPoolCreateCount; // 已创建池数量
    property PoolWorkCount: integer read FPoolWorkCount; // 正在工作的数量
  end;

  { 账套管理-采用hase管理 }
  TOneZTManage = class(TObject)
  private
    FZTMain: string;
    FStop: boolean;
    FZTPools: TDictionary<string, TOneZTPool>;
    FTranZTItemList: TDictionary<string, TOneZTItem>;
    FLockObject: TCriticalSection;
    FLog: IOneLog;
    FKeepList: TList<TZTKeepInfo>;
    // 在事务太久的,自动最还连接
    FTimerThread: TOneTimerThread;
    //驱动
    FLibrarys: TDictionary<string, TSQLDBLibraryLoader>;
  private
    procedure onTimerWork(Sender: TObject);
    procedure InitPhyDriver(QDriverName: string);
    procedure BuildStoredSQL(QPhyDirver: string; QOpenData: TOneDataOpen);
  public
    constructor Create(QOneLog: IOneLog); overload;
    destructor Destroy; override;
    function StarWork(QZTSetList: TList<TOneZTSet>; var QErrMsg: string): boolean;
  public
    // 某个账套停止工作
    function StopZT(QZTCode: string; QStop: boolean; var QErrMsg: string): boolean;
    // 获取一个账套
    function LockZTItem(QZTCode: string; var QErrMsg: string): TOneZTItem;
    // ************ 对接客户端的事务,有点像二层事务,一般很少用,额外福利***********//
    // 锁定一个账套,由客户端自由控制
    function LockTranItem(QZTCode: string; QMaxSpanSec: integer; var QErrMsg: string): string;
    // 归还一个账套,由客户端自由控制
    function UnLockTranItem(QTranID: string; var QErrMsg: string): boolean;
    // 账套开启事务,由客户端自由控制
    function StartTranItem(QTranID: string; var QErrMsg: string): boolean;
    // 账套提交事务,由客户端自由控制
    function CommitTranItem(QTranID: string; var QErrMsg: string): boolean;
    // 账套回滚事务,由客户端自由控制
    function RollbackTranItem(QTranID: string; var QErrMsg: string): boolean;
    // 获取客户自由管理的账套
    function GetTranItem(QTranID: string; var QErrMsg: string): TOneZTItem;
    // 跟据账套代码创建账套池
    // **********************************************//
    function InitZTPool(QZTSet: TOneZTSet; var QErrMsg: string): boolean;
    function HaveZT(QZTCode: string): boolean;
  public
    function GetZTMain: string;
    // 打开数据
    function OpenData(QOpenData: TOneDataOpen; QOneDataResult: TOneDataResult): boolean;
      overload;
    // IsServer是不是服务端自已发起的请求
    function OpenDatas(QOpenDatas: TList<TOneDataOpen>; var QOneDataResult: TOneDataResult): boolean;
    // 保存数据
    function SaveDatas(QSaveDMLDatas: TList<TOneDataSaveDML>; var QOneDataResult: TOneDataResult): boolean;
    // 执行存储过程
    function ExecStored(QOpenData: TOneDataOpen; var QOneDataResult: TOneDataResult): boolean;
  public
    // 主要提供给Orm用的
    function OpenData(QOpenData: TOneDataOpen; QParams: array of variant; var QErrMsg: string): TFDMemtable; overload;
    function ExecSQL(QDataSaveDML: TOneDataSaveDML; QParams: array of variant; var QErrMsg: string): integer;
  public
    property ZTMain: string read FZTMain write FZTMain;
    property ZTPools: TDictionary<string, TOneZTPool> read FZTPools write FZTPools;
  end;

// 去 Order by SQL
function ClearOrderBySQL(QSQL: string): string;
//设置解析SQL
function SetSQLInfo(var QSQLInfo: TSQLInfo): boolean;
procedure InitSQLInfo(var QSQLInfo: TSQLInfo);
//分析更新语句

var
  Var_ADOZTMgr: TOneZTManage;
  // 驱动加载
  Var_MSSQLDriverLink: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink4_0: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink4_1: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink5_0: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink5_1: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink5_5: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink5_6: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink5_7: TSQLDBLibraryLoader = nil;
  Var_MySQLDriverLink8_0: TSQLDBLibraryLoader = nil;
  Var_OracleDriverLink: TSQLDBLibraryLoader = nil;
  var_PGDriverLink: TSQLDBLibraryLoader = nil;
  var_FireBirdDriverLinK: TSQLDBLibraryLoader = nil;
  var_FireBirdDriverLinKB: TSQLDBLibraryLoader = nil;
  var_SQLiteDriverLinK: TSQLDBLibraryLoader = nil;
  var_ASADriverLink: TSQLDBLibraryLoader = nil;
  var_ODBCDriverLink: TSQLDBLibraryLoader = nil;
  var_MSAccDriverLink: TSQLDBLibraryLoader = nil;

implementation

uses OneStopwatch;

constructor TFDTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.Options := [stoUseImplicit];
  self.FIsInTran := False;
end;

procedure TFDTransaction.TranStart;
begin
  if self.FIsInTran then
    exit;
  if self.Active then
  begin
    self.FIsInTran := True;
    exit;
  end;
  self.StartTransaction;
  self.FIsInTran := True;
end;

procedure TFDTransaction.TranCommit;
begin
  if self.FIsInTran then
  begin
    self.CommitRetaining;
    self.FIsInTran := False;
  end;
end;

procedure TFDTransaction.TranRollback;
begin
  if self.FIsInTran then
  begin
    self.RollbackRetaining;
    self.FIsInTran := False;
  end;
end;

constructor TOneZTMangeSet.Create();
begin
  inherited Create;
  FZTSetList := TList<TOneZTSet>.Create;
  //注册到RTTI中
  OneSerialization.AddListClass(TList<TOneZTSet>, TOneZTSet, nil);
end;

destructor TOneZTMangeSet.Destroy;
var
  i: integer;
begin
  for i := 0 to FZTSetList.Count - 1 do
  begin
    FZTSetList[i].Free;
  end;
  FZTSetList.Clear;
  FZTSetList.Free;
  inherited Destroy;
end;


constructor TOneZTItem.Create(AOwner: TOneZTPool; QZTSet: TOneZTSet);
var
  LSetList: TStringList;
  i, iPortIndex: integer;
begin
  inherited Create;
  FCreateID := OneGUID.GetGUID32();
  self.FCustTran := False;
  self.FLastTime := Now;
  self.FOwnerZTPool := AOwner;
  self.FZTSet := QZTSet;
  { 数据库才有这些 }
  FDConnection := TFDConnection.Create(nil);
  LSetList := TStringList.Create();
  try
    LSetList.LineBreak := ';';
    LSetList.Text := QZTSet.DBOtherParams;
    FDConnection.LoginPrompt := False;
    //数据库类型
    FDConnection.ConnectorType := QZTSet.PhyDriver;
    //数据库编码
    FDConnection.CharSet := QZTSet.DBCharSet;
    //数据库地址
    FDConnection.HostName := QZTSet.DBHostName;
    //数据库名称
    FDConnection.DatabaseName := QZTSet.DBName;
    //数据库账号
    FDConnection.UserName := QZTSet.DBUserName;
    //数据库密码
    FDConnection.Password := QZTSet.DBUserPass;
    //数据库连接
    FDConnection.KeepConnection := QZTSet.DBKeepConnect;

    if QZTSet.DBHostPort > 0 then
    begin
      FDConnection.params.Values['Port'] := QZTSet.DBHostPort.ToString;
    end
    else
    if FDConnection.params.IndexOfName('Port') > -1 then
    begin
      iPortIndex := FDConnection.params.IndexOfName('Port');
      FDConnection.params.Delete(iPortIndex);
    end;
    for i := 0 to LSetList.Count - 1 do
    begin
      FDConnection.Params.Add(LSetList[i]);
    end;
    //加载驱动DLL目录
    self.FOwnerZTPool.FZTManage.InitPhyDriver(FDConnection.ConnectorType);
    //FDConnection.LIB := InitPhyDriver(FDConnection.Protocol);
  finally
    LSetList.Free;
  end;

  FDTransaction := TFDTransaction.Create(nil);
  // 解析是什么编码
  //FCharacterSet := FDConnection.Params.Values['CharacterSet'];
  //FCharacterSet := FCharacterSet.ToLower;
  FDQuery := TFDQuery.Create(nil);
  FDStoredProc := TFDStoredProc.Create(nil);

  // FDStoredProc.FetchOptions.Items := [fiBlobs,fiDetails,fiMeta];
  FTempStream := TMemoryStream.Create;

  FException := TOneFDException.Create;
end;

destructor TOneZTItem.Destroy;
begin
  FOwnerZTPool := nil;
  if FDTransaction <> nil then
    FDTransaction.Free;
  if FDQuery <> nil then
    FDQuery.Free;
  if FDStoredProc <> nil then
    FDStoredProc.Free;
  if FDConnection <> nil then
  begin
    FDConnection.Connected := False;
    FDConnection.Free;
  end;
  if FTempStream <> nil then
  begin
    FTempStream.Clear;
    FTempStream.Free;
  end;
  FException.Free;
  inherited Destroy;
end;

function TOneZTItem.GetADConnection: TFDConnection;
begin
  Result := nil;
  if not FDConnection.Connected then
    FDConnection.Connected := True;
  if FDConnection.Connected then
    Result := FDConnection;
end;

function TOneZTItem.GetADTransaction: TFDTransaction;
begin
  Result := FDTransaction;
  FDTransaction.DataBase := FDConnection;
end;

function TOneZTItem.GetQuery: TFDQuery;
begin
  self.GetADConnection;
  Result := FDQuery;
  if FDQuery.Active then
  begin
    FDQuery.Close;
  end;
  FDQuery.SQL.Clear;
  FDQuery.Params.Clear;
  FDConnection.Transaction := FDTransaction;
  FDQuery.DataBase := FDConnection;
end;

function TOneZTItem.GetScript: TFDScript;
begin
  Result := FDScript;
  FDTransaction.DataBase := FDConnection;
  FDScript.DataBase := FDConnection;
  FDScript.Transaction := FDTransaction;
  FDTransaction.CloseDataSets;
end;

function TOneZTItem.GetStoredProc: TFDStoredProc;
begin
  Result := nil;
  if (FDStoredProc <> nil) then
  begin
    FDStoredProc.Free;
    FDStoredProc := nil;
  end;
  // 每次多新建一个
  FDStoredProc := TFDStoredProc.Create(nil);
  if FDStoredProc.Active then
    FDStoredProc.Close;
  FDStoredProc.Params.Clear;
  FDTransaction.DataBase := FDConnection;
  FDStoredProc.DataBase := FDConnection;
  FDStoredProc.Transaction := FDTransaction;
  Result := FDStoredProc;
end;

function TOneZTItem.GetTempStream: TMemoryStream;
begin
  FTempStream.Clear;
  FTempStream.Position := 0;
  Result := FTempStream;
end;

procedure TOneZTItem.FDQueryError(ASender, AInitiator: TObject; var AException: Exception);
begin
  if (AException <> nil) and (AException.Message <> '') then
  begin
    self.FException.FErrmsg := AException.Message;
  end;
end;

procedure TOneZTItem.FDScriptError(ASender, AInitiator: TObject; var AException: Exception);
begin

end;

procedure TOneZTItem.UnLockWork();
begin
  // 事务自行控制
  // UnLockTranItem没进行解锁是不释放的
  if self.FCustTran then
    exit;
  // 放在任务理

  // 释放时事务没提交,回滚
  if FDTransaction.IsInTran then
  begin
    FDTransaction.TranRollback;
  end;
  if FDQuery.Active then
  begin
    FDQuery.Close;
  end;
  FDQuery.SQL.Clear;
  FDQuery.Params.Clear;

  // 自增清空
  if FDStoredProc.Active then
  begin
    FDStoredProc.Close;
  end;
  if FDStoredProc.Params.Count > 0 then  FDStoredProc.Params.Clear;
  FDStoredProc.DataBase := nil;
  if FTempStream <> nil then
  begin
    FTempStream.Position := 0;
    FTempStream.Clear;
  end;
  // 默认1800秒事务锁
  FCustTranMaxSpanSec := 30 * 60;
  FIsWorking := False;
  self.FOwnerZTPool.UnLockWorkCount();
end;

// 一个账磁连接池
constructor TOneZTPool.Create(QZTManage: TOneZTManage; QZTSet: TOneZTSet);
var
  i: integer;
  lZTItem: TOneZTItem;
begin
  inherited Create;
  self.FZTManage := QZTManage;
  if QZTSet.InitPoolCount <= 0 then
    QZTSet.InitPoolCount := 5;
  if QZTSet.MaxPoolCount <= 0 then
    QZTSet.MaxPoolCount := 10;
  FZTSet := QZTSet;
  FZTCode := QZTSet.ZTCode;
  FInitPoolCount := QZTSet.InitPoolCount;
  FMaxPoolCount := QZTSet.MaxPoolCount;
  FPoolCreateCount := 0; // 已创建池数量
  FPoolWorkCount := 0;
  // 正在工作的数量
  FPhyDriver := QZTSet.PhyDriver;
  FConnectionStr := QZTSet.ConnectionStr;
  self.FZTItems := TList<TOneZTItem>.Create();
  FLockObj := TCriticalSection.Create;
  for i := 0 to FInitPoolCount - 1 do
  begin
    lZTItem := TOneZTItem.Create(self, QZTSet);
    lZTItem.FLastTime := Now;
    self.FZTItems.Add(lZTItem);
    self.FPoolCreateCount := self.FPoolCreateCount + 1;
  end;
end;

destructor TOneZTPool.Destroy;
var
  i: integer;
begin
  if FLockObj <> nil then
    FLockObj.Free;
  if self.FZTItems <> nil then
  begin
    for i := 0 to FZTItems.Count - 1 do
    begin
      FZTItems[i].Free;
    end;
    FZTItems.Clear;
    FZTItems.Free;
  end;
  inherited Destroy;
end;

function TOneZTPool.LockZTItem(var QErrMsg: string): TOneZTItem;
var
  i: integer;
  lZTItem: TOneZTItem;
begin
  Result := nil;
  lZTItem := nil;
  QErrMsg := '';
  FLockObj.Enter;
  try
    if self.FPoolWorkCount >= self.FMaxPoolCount then
    begin
      QErrMsg := '账套池[' + self.FZTCode + ']已达到最大工作量[' + FPoolWorkCount.ToString() + ']';
      exit;
    end;
    // 遍历看哪个是空闲的
    for i := 0 to self.FZTItems.Count - 1 do
    begin
      if self.FZTItems[i].IsWorking then
        continue;
      lZTItem := self.FZTItems[i];
      // 找到一个中断
      break;
    end;
    if lZTItem = nil then
    begin
      // 没找到就开始创建一个新的
      lZTItem := TOneZTItem.Create(self, self.FZTSet);
      // 加入到池中
      self.FZTItems.Add(lZTItem);
      self.FPoolCreateCount := self.FPoolCreateCount + 1;
    end;
    lZTItem.FLastTime := Now;
    // 工作量加1
    lZTItem.IsWorking := True;
    self.FPoolWorkCount := self.FPoolWorkCount + 1;
    Result := lZTItem;
  finally
    FLockObj.Leave;
  end;
end;

procedure TOneZTPool.UnLockWorkCount();
begin
  FLockObj.Enter;
  try
    self.FPoolWorkCount := self.FPoolWorkCount - 1;
  finally
    FLockObj.Leave;
  end;
end;

constructor TOneZTManage.Create(QOneLog: IOneLog);
begin
  inherited Create;
  self.FLog := QOneLog;
  FZTPools := TDictionary<string, TOneZTPool>.Create;
  FTranZTItemList := TDictionary<string, TOneZTItem>.Create;
  FLockObject := TCriticalSection.Create;
  FKeepList := TList<TZTKeepInfo>.Create;
  FTimerThread := TOneTimerThread.Create(self.onTimerWork);
  FLibrarys := TDictionary<string, TSQLDBLibraryLoader>.Create;
end;

destructor TOneZTManage.Destroy;
var
  i: integer;
  lZTPool: TOneZTPool;
  lZTItem: TOneZTItem;
  lLib: TSQLDBLibraryLoader;
begin
  if FTimerThread <> nil then
    FTimerThread.FreeWork;
  // 二层事务的
  for lZTItem in FTranZTItemList.Values do
  begin
    if lZTItem.ADTransaction.IsInTran then
    begin
      lZTItem.ADTransaction.TranRollback;
    end;
  end;
  FTranZTItemList.Clear;
  FTranZTItemList.Free;
  // 释放连接池
  for lZTPool in FZTPools.Values do
  begin
    lZTPool.Free;
  end;
  FZTPools.Clear;
  FZTPools.Free;
  FLockObject.Free;
  for i := 0 to FKeepList.Count - 1 do
  begin
    FKeepList[i].Free;
  end;
  FKeepList.Clear;
  FKeepList.Free;
  for lLib in FLibrarys.values do
  begin
    lLib.Free;
  end;
  FLibrarys.Clear;
  FLibrarys.Free;
  inherited Destroy;
end;

procedure TOneZTManage.onTimerWork(Sender: TObject);
var
  lZTItem: TOneZTItem;
  lNow: TDateTime;
  lSpanSec: integer;
begin
  FLockObject.Enter;
  try
    lNow := Now;
    // 二层事务的
    for lZTItem in FTranZTItemList.Values do
    begin
      if lZTItem.FCustTranMaxSpanSec < 0 then
        continue;

      if lZTItem.FCustTranMaxSpanSec = 0 then
      begin
        lZTItem.FCustTranMaxSpanSec := 30 * 60;
      end;
      // 事务到期了,还没操作归还
      if SecondsBetween(lNow, lZTItem.FLastTime) >= lZTItem.FCustTranMaxSpanSec then
      begin
        if lZTItem.ADTransaction.IsInTran then
        begin
          // 回滚
          lZTItem.ADTransaction.TranRollback;
        end;
        lZTItem.FCustTran := False;
        lZTItem.UnLockWork;
        FTranZTItemList.Remove(lZTItem.FCreateID);
      end;
    end;
  finally
    FLockObject.leave;
  end;
end;

function TOneZTManage.GetZTMain: string;
begin
  Result := FZTMain;
end;

function TOneZTManage.LockZTItem(QZTCode: string; var QErrMsg: string): TOneZTItem;
var
  lZTPool: TOneZTPool;
begin
  Result := nil;
  QErrMsg := '';
  FLockObject.Enter;
  try
    if FStop then
    begin
      QErrMsg := '账套管理目前状态处于停止工作状态!!!';
      exit;
    end;
    if QZTCode = '' then
    begin
      QZTCode := self.ZTMain;
    end;
    if QZTCode = '' then
    begin
      QErrMsg := '账套代码为空或者请设置一个主账套';
      exit;
    end;
    if FZTPools.TryGetValue(QZTCode.ToUpper, lZTPool) then
    begin
      if lZTPool.Stop then
      begin
        QErrMsg := '账套[' + QZTCode + ']池目前状态处于停止工作状态';
        exit;
      end;
      Result := lZTPool.LockZTItem(QErrMsg);
      if Result = nil then
      begin
        exit;
      end;
    end
    else
    begin
      QErrMsg := '账套管理不存在账套代码[' + QZTCode + ']';
    end;
  finally
    FLockObject.leave;
  end;
  if Result <> nil then
  begin
    if not Result.ADConnection.Connected then
    begin
      // 不是连接状态，连下
      Result.ADConnection.Connected := False;
      Result.ADConnection.Connected := True;
    end;
  end;
end;

function TOneZTManage.LockTranItem(QZTCode: string; QMaxSpanSec: integer; var QErrMsg: string): string;
var
  lFireZTPool: TOneZTPool;
  lZTItem: TOneZTItem;
  lStartTranID: string;
begin
  Result := '';
  QErrMsg := '';
  lZTItem := nil;
  lZTItem := self.LockZTItem(QZTCode, QErrMsg);
  if lZTItem = nil then
  begin
    exit;
  end;
  FLockObject.Enter;
  try
    lZTItem.FCustTran := True;
    lZTItem.FCustTranMaxSpanSec := QMaxSpanSec;
    self.FTranZTItemList.Add(lZTItem.FCreateID, lZTItem);
    Result := lZTItem.FCreateID;
  finally
    FLockObject.leave;
  end;
end;

function TOneZTManage.UnLockTranItem(QTranID: string; var QErrMsg: string): boolean;
var
  lZTItem: TOneZTItem;
begin
  Result := False;
  QErrMsg := '';
  FLockObject.Enter;
  try
    if FTranZTItemList.TryGetValue(QTranID, lZTItem) then
    begin
      if lZTItem.ADTransaction.IsInTran then
      begin
        lZTItem.ADTransaction.TranRollback;
        lZTItem.FLastTime := Now;
      end;
      lZTItem.FCustTran := False;
      lZTItem.UnLockWork;
      Result := True;
    end
    else
    begin
      QErrMsg := '不存在此事务的账套连接';
      Result := False;
    end;
  finally
    FLockObject.leave;
    FTranZTItemList.Remove(QTranID);

  end;

end;

function TOneZTManage.GetTranItem(QTranID: string; var QErrMsg: string): TOneZTItem;
var
  lZTItem: TOneZTItem;
begin
  Result := nil;
  QErrMsg := '';
  FLockObject.Enter;
  try
    if FTranZTItemList.TryGetValue(QTranID, lZTItem) then
    begin
      Result := lZTItem;
      Result.FLastTime := Now;
    end
    else
    begin
      QErrMsg := '不存在此事务的账套连接';
    end;
  finally
    FLockObject.leave;
  end;
end;

// 此种模式用在客户端开启事务,相当于两层写事务
function TOneZTManage.StartTranItem(QTranID: string; var QErrMsg: string): boolean;
var
  lZTItem: TOneZTItem;
begin
  Result := False;
  QErrMsg := '';
  FLockObject.Enter;
  try
    if FTranZTItemList.TryGetValue(QTranID, lZTItem) then
    begin
      lZTItem.ADTransaction.TranStart;
      lZTItem.FLastTime := Now;
      Result := True;
    end
    else
    begin
      QErrMsg := '不存在此事务的账套连接';
      Result := False;
    end;
  finally
    FLockObject.leave;
  end;
end;

function TOneZTManage.CommitTranItem(QTranID: string; var QErrMsg: string): boolean;
var
  lZTItem: TOneZTItem;
begin
  Result := False;
  QErrMsg := '提交事务开始';
  if self.FTranZTItemList.TryGetValue(QTranID, lZTItem) then
  begin
    try
      if lZTItem.ADTransaction.IsInTran then
      begin
        lZTItem.ADTransaction.TranCommit;
        lZTItem.FLastTime := Now;
        Result := True;
      end
      else
      begin
        Result := True;
      end;
    except
      on e: Exception do
      begin
        QErrMsg := '事务异常,原因:' + e.Message;
      end;
    end;
  end
  else
  begin
    QErrMsg := '此事务已经不存在';
  end;
end;

function TOneZTManage.RollbackTranItem(QTranID: string; var QErrMsg: string): boolean;
var
  lZTItem: TOneZTItem;
begin
  Result := False;
  QErrMsg := '回滚事务开始';
  if self.FTranZTItemList.TryGetValue(QTranID, lZTItem) then
  begin
    try
      if 1 = 1 then
      begin
        lZTItem.ADTransaction.TranRollback;
        lZTItem.FLastTime := Now;
        Result := True;
      end
      else
      begin
        Result := True;
      end;
    except
      on e: Exception do
      begin
        QErrMsg := '事务异常,原因:' + e.Message;
      end;
    end;
  end
  else
  begin
    QErrMsg := '此事务已经不存在';
  end;
end;

function TOneZTManage.StopZT(QZTCode: string; QStop: boolean; var QErrMsg: string): boolean;
var
  lZTPool: TOneZTPool;
begin
  Result := False;
  QErrMsg := '';
  QZTCode := QZTCode.ToUpper;
  if FZTPools.TryGetValue(QZTCode, lZTPool) then
  begin
    lZTPool.Stop := QStop;
    Result := True;
  end
  else
  begin
    QErrMsg := '找不到相关账套';
  end;
end;

function TOneZTManage.InitZTPool(QZTSet: TOneZTSet; var QErrMsg: string): boolean;
var
  lZTPool: TOneZTPool;
  lZTCode: string;
begin
  Result := False;
  QErrMsg := '';
  if QZTSet.ZTCode.Trim = '' then
    exit;
  lZTCode := QZTSet.ZTCode.ToUpper;
  FLockObject.Enter;
  try
    if FZTPools.TryGetValue(lZTCode, lZTPool) then
    begin
      lZTPool.Free;
      FZTPools.Remove(lZTCode);
    end;
    lZTPool := TOneZTPool.Create(self, QZTSet);
    FZTPools.Add(lZTCode, lZTPool);
    Result := True;
  finally
    FLockObject.leave;
  end;
end;

function TOneZTManage.HaveZT(QZTCode: string): boolean;
begin
  QZTCode := QZTCode.ToUpper;
  Result := FZTPools.ContainsKey(QZTCode);
end;

function TOneZTManage.StarWork(QZTSetList: TList<TOneZTSet>; var QErrMsg: string): boolean;
var
  i: integer;
  lZTSet: TOneZTSet;
  lZTPool: TOneZTPool;
begin
  Result := False;
  QErrMsg := '';
  try
    // 先释放原有的
    FLockObject.Enter;
    try
      for lZTPool in FZTPools.Values do
      begin
        // 全停止
        lZTPool.Stop := True;
        if lZTPool.FPoolWorkCount > 0 then
        begin
          QErrMsg := lZTPool.ZTCode + '还有正在运行的连接,不可重新加载';
          exit;
        end;
      end;
      for lZTPool in FZTPools.Values do
      begin
        lZTPool.Free;
      end;
      FZTPools.Clear;
    finally
      FLockObject.leave;
    end;

    for i := 0 to QZTSetList.Count - 1 do
    begin
      lZTSet := QZTSetList[i];
      lZTSet.ZTCode := lZTSet.ZTCode.Trim;
      if not lZTSet.IsEnable then
        continue;
      if lZTSet.ZTCode = '' then
        continue;
      if lZTSet.ConnectionStr = '' then
        continue;
      if self.HaveZT(lZTSet.ZTCode) then
        continue;
      self.InitZTPool(lZTSet, QErrMsg);
      if lZTSet.IsMain then
      begin
        self.FZTMain := lZTSet.FZTCode.ToUpper;
      end;
    end;
    FTimerThread.StartWork;
    Result := True;
  except
    on e: Exception do
    begin
      QErrMsg := e.Message;
    end;
  end;
end;

function TOneZTManage.OpenData(QOpenData: TOneDataOpen; QOneDataResult: TOneDataResult): boolean;
var
  lOpenDatas: TList<TOneDataOpen>;
begin
  lOpenDatas := TList<TOneDataOpen>.Create;
  try
    lOpenDatas.Add(QOpenData);
    Result := self.OpenDatas(lOpenDatas, QOneDataResult)
  finally
    // 只释放容器 QOneOpenData不释放
    lOpenDatas.Clear;
    lOpenDatas.Free;
  end;
end;

function TOneZTManage.OpenData(QOpenData: TOneDataOpen; QParams: array of variant; var QErrMsg: string): TFDMemtable;
var
  lZTItem: TOneZTItem;
  LZTQuery: TFDQuery;
  iParam: integer;
begin
  Result := nil;
  QErrMsg := '';
  lZTItem := self.LockZTItem(QOpenData.ZTCode, QErrMsg);
  if lZTItem = nil then
  begin
    if QErrMsg = '' then
      QErrMsg := '获取账套' + QOpenData.ZTCode + '连接失败,原因未知';
    exit;
  end;

  try
    // 打开数据屏B删除,插入,删除功能防止打开数据的SQL里面放有执行DML语句
    LZTQuery := lZTItem.GetQuery;
    if (QOpenData.PageSize > 0) and (QOpenData.PageIndex > 0) then
    begin
      LZTQuery.PacketRecords := QOpenData.PageSize;
    end
    else
    begin
      if QOpenData.PageSize > 0 then
        LZTQuery.PacketRecords := QOpenData.PageSize
      else
        LZTQuery.PacketRecords := -1;
    end;
    LZTQuery.SQL.Text := QOpenData.OpenSQL;
    // 参数赋值
    if LZTQuery.Params.Count <> length(QParams) then
    begin
      QErrMsg := 'SQL最终参数个数和传进的参数个数不一至';
      exit;
    end;
    for iParam := 0 to length(QParams) - 1 do
    begin
      LZTQuery.Params[iParam].Value := QParams[iParam];
    end;

    try
      LZTQuery.Open;
      if not LZTQuery.Active then
      begin
        QErrMsg := '数据打开失败';
        exit;
      end;
      Result := TFDMemtable.Create(nil);
    except
      on e: Exception do
      begin
        QErrMsg := '打开数据发生异常,原因：' + e.Message;
        exit;
      end;
    end;
  finally
    lZTItem.UnLockWork;
  end;
end;

function TOneZTManage.OpenDatas(QOpenDatas: TList<TOneDataOpen>; var QOneDataResult: TOneDataResult): boolean;
var
  i, iParam, iErr: integer;
  lOpenData: TOneDataOpen;
  lZTItem: TOneZTItem;
  LZTQuery: TFDQuery;
  lMemoryStream, lParamStream: TMemoryStream;
  lRequestMilSec: integer;
  LJsonValue: TJSONData;
  lFileName, lFileGuid: string;
  lZip: TDeflater;

  lSQL: string;
  lErrMsg: string;
  lDataResultItem: TOneDataResultItem;
  lwatchTimer: TStopwatch;
  LFDParam: TParam;
  LOneParam: TOneParam;
  LSQLInfo: TSQLInfo;
  LPageField: TField;
  tempMsg: string;

  lFileStream: TFileStream;
begin
  Result := False;
  lErrMsg := '';
  if QOneDataResult = nil then
  begin
    QOneDataResult := TOneDataResult.Create;
  end;
  //计时器开始
  if QOpenDatas.Count = 0 then
  begin
    QOneDataResult.ResultMsg := '无相关打开数据集';
    exit;
  end;
  lwatchTimer := TStopwatch.StartNew;
  try
    // 遍历打开多个数据集
    for i := 0 to QOpenDatas.Count - 1 do
    begin
      lDataResultItem := TOneDataResultItem.Create;
      QOneDataResult.ResultCount := QOneDataResult.ResultCount + 1;
      QOneDataResult.ResultItems.Add(lDataResultItem);
      lOpenData := QOpenDatas[i];
      if lOpenData.ZTCode = '' then
        lOpenData.ZTCode := self.ZTMain;
      lOpenData.ZTCode := lOpenData.ZTCode.ToUpper;
      lZTItem := nil;
      // 客户端发起事务,像两层一样运作
      if lOpenData.TranID <> '' then
      begin
        lZTItem := self.GetTranItem(lOpenData.TranID, lErrMsg);
      end
      else
      begin
        lZTItem := self.LockZTItem(lOpenData.ZTCode, lErrMsg);
      end;
      if lZTItem = nil then
      begin
        if lErrMsg = '' then
          lErrMsg := '获取账套' + lOpenData.ZTCode + '连接失败,原因未知';
        exit;
      end;

      try
        // lZTItem.ADQuery 获取连接现成的Query直接用
        // lZTItem.FDConnection 获取连接
        LZTQuery := lZTItem.ADQuery;
        InitSQLInfo(LSQLInfo);
        LSQLInfo.FDriver := lZTItem.FZTSet.FPhyDriver;
        LSQLInfo.FDriverVersion := lZTItem.FZTSet.FDBVersion;
        LSQLInfo.FPageIndex := lOpenData.PageIndex;
        LSQLInfo.FPageSize := lOpenData.PageSize;
        LSQLInfo.FSQL := lOpenData.OpenSQL;
        if not SetSQLInfo(LSQLInfo) then
        begin
          lErrMsg := LSQLInfo.FErrMsg;
          exit;
        end;
        LZTQuery.SQL.Text := LSQLInfo.FSQL;
        if LZTQuery.Params.Count <> 0 then
        begin
          if lOpenData.Params.Count <> LZTQuery.Params.Count then
          begin
            lErrMsg := '参数提供不全';
            exit;
          end;
          // 参数赋值
          for iParam := 0 to LZTQuery.Params.Count - 1 do
          begin
            LFDParam := LZTQuery.Params[iParam];
            LOneParam := lOpenData.Params[iParam];
            // 格式转化
            LFDParam.DataType :=
              TFieldType(GetEnumValue(TypeInfo(TFieldType), LOneParam.ParamDataType));
            LFDParam.ParamType :=
              TParamType(GetEnumValue(TypeInfo(TParamType), LOneParam.ParamType));
            // 值赋值
            if LOneParam.ParamValue = const_OneParamIsNull_Value then
            begin
              // Null值情况
              LFDParam.Clear();
            end
            else
            begin
              case LFDParam.DataType of
                ftUnknown:
                begin
                  LFDParam.Value := LOneParam.ParamValue;
                end;
                ftBlob:
                begin
                  //lParamStream := TMemoryStream.Create;
                  //OneStreamString.StreamWriteBase64Str(
                  //  lParamStream, LOneParam.ParamValue);
                  //LFDParam.AsBlob := lParamStream.;
                end;
                else
                begin
                  LFDParam.Value := LOneParam.ParamValue;
                end;
              end;
            end;

          end;
        end;
        try
          LZTQuery.Open;
          if not LZTQuery.Active then
          begin
            lErrMsg := '打开数据失败';
            exit;
          end;

          lDataResultItem.RecordCount := LZTQuery.RecordCount;
          lDataResultItem.ResultDataCount :=
            lDataResultItem.ResultDataCount + 1;
          //lOpenData.DataReturnMode := const_DataReturnMode_File;
          if lOpenData.DataReturnMode = const_DataReturnMode_File then
          begin
            lDataResultItem.ResultDataMode := const_DataReturnMode_File;
            lFileGuid := OneGUID.GetGUID32;
            lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneDataTemp\' + lFileGuid + '.zip');

            lFileStream := TFileStream.Create(lFileName, fmCreate);
            lMemoryStream := TMemoryStream.Create;
            // 进行文件压缩
            lZip := nil;
            try
              LZTQuery.SaveToStream(lMemoryStream, TDataPacketFormat.dfBinary);
              lMemoryStream.Position := 0;

              lZip := TDeflater.Create(lMemoryStream, lFileStream, lMemoryStream.Size);
              lZip.Compress;
              lDataResultItem.ResultContext := lFileGuid;
            finally
              lFileStream.Free;
              lMemoryStream.Free;
              if lZip <> nil then
                lZip.Free;
            end;
          end
          else if lOpenData.DataReturnMode = const_DataReturnMode_Stream then
          begin
            lMemoryStream := TMemoryStream.Create;
            LZTQuery.SaveToStream(lMemoryStream, TDataPacketFormat.dfBinary);
            if lMemoryStream.Size >= 1024 * 1024 * 1 then
            begin
              lDataResultItem.ResultDataMode := const_DataReturnMode_File;
              lFileGuid := OneGUID.GetGUID32;
              lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneDataTemp\' + lFileGuid + '.zip');
              // 进行文件压缩
              lFileStream := TFileStream.Create(lFileName, fmCreate);
              lZip := nil;
              try
                lMemoryStream.Position := 0;
                lZip := TDeflater.Create(lMemoryStream, lFileStream, lMemoryStream.Size);
                lZip.Compress;
                lDataResultItem.ResultContext := lFileGuid;
              finally
                lFileStream.Free;
                lMemoryStream.Free;
                if lZip <> nil then
                  lZip.Free;
              end;
            end
            else
            begin
              lDataResultItem.ResultDataMode := const_DataReturnMode_Stream;
              lDataResultItem.SetStream(lMemoryStream);
            end;
          end
          else if lOpenData.DataReturnMode = const_DataReturnMode_JSON then
          begin
            LJsonValue := OneDataJson.DataSetToJson(LZTQuery);
            try
              lDataResultItem.ResultContext := LJsonValue.AsJSON;
            finally
              LJsonValue.Free;
            end;
          end;
          // 分页取总数，第一页才去取
          if ((lOpenData.PageSize > 0) and (lOpenData.PageIndex = 1)) or (lOpenData.PageRefresh) then
          begin
            lDataResultItem.ResultPage := True;
            LZTQuery := lZTItem.GetQuery;
            //LZTQuery.FetchOptions.RecsSkip := -1;
            //LZTQuery.FetchOptions.RecsMax := -1;
            lSQL := ClearOrderBySQL(lOpenData.OpenSQL);
            lSQL := 'select count(1) from ( ' + lSQL + ' ) tempCount';
            LZTQuery.SQL.Text := lSQL;
            for iParam := 0 to LZTQuery.Params.Count - 1 do
            begin
              LZTQuery.Params[iParam].Value :=
                lOpenData.Params[iParam].ParamValue;
            end;
            LZTQuery.Open();
            lSQL := LZTQuery.RecordCount.ToString;
            lSQL := LZTQuery.Fields.Count.ToString;
            lSQL := LZTQuery.Fields[0].AsString;
            lDataResultItem.ResultTotal := LZTQuery.Fields[0].AsInteger;
          end;
        except
          on e: Exception do
          begin
            lErrMsg := e.Message;
            exit;
          end;
        end;
      finally
        // 释放连接池
        if lZTItem <> nil then
        begin
          lZTItem.UnLockWork();
        end;
      end;
      if lErrMsg <> '' then
      begin
        Result := False;
        exit;
        ;
      end;
    end;
    Result := True;
    QOneDataResult.ResultOK := True;
  finally
    QOneDataResult.ResultMsg := lErrMsg;
    lwatchTimer.Stop;
    lRequestMilSec := lwatchTimer.ElapsedMilliseconds;
    if (self.FLog <> nil) and (self.FLog.IsSQLLog) then
    begin
      self.FLog.WriteSQLLog('账套方法[OpenDatas]:');
      self.FLog.WriteSQLLog('总共用时:[' + lRequestMilSec.ToString + ']毫秒');
      self.FLog.WriteSQLLog('错误消息:[' + lErrMsg + ']');
      for i := 0 to QOpenDatas.Count - 1 do
      begin
        lOpenData := QOpenDatas[i];
        self.FLog.WriteSQLLog('SQL语句:[' + lOpenData.OpenSQL + ']');
        for iParam := 0 to lOpenData.Params.Count - 1 do
        begin
          LOneParam := lOpenData.Params[iParam];
          self.FLog.WriteSQLLog('参数:[' + LOneParam.ParamName + ']值[' + LOneParam.ParamValue + ']');
        end;
      end;
    end;
  end;
end;

// 保存数据
function TOneZTManage.SaveDatas(QSaveDMLDatas: TList<TOneDataSaveDML>; var QOneDataResult: TOneDataResult): boolean;
var
  lDataResultItem: TOneDataResultItem;

  lZTItemList: TDictionary<string, TOneZTItem>;
  lZTItem: TOneZTItem;
  LZTQuery: TFDQuery;
  i: integer;
  lDataSaveDML: TOneDataSaveDML;
  LOneParam: TOneParam;
  lErrMsg: string;
  lTranCount: integer;

  lSaveStream: TMemoryStream;
  lArrKeys: TArray<string>;
  iKey: integer;
  iParamCount, iParam: integer;
  lFieldType: TFieldType;
  isCommit: boolean;

  lRequestMilSec: integer;
  lwatchTimer: TStopwatch;

  tempFieldName: string;
  tempMsg: string;
  lProviderFlags: TProviderFlags;
  iFlags, iField: integer;
begin
  Result := False;
  lErrMsg := '';
  isCommit := False;
  if QOneDataResult = nil then
  begin
    QOneDataResult := TOneDataResult.Create;
  end;
  // 校验
  lTranCount := 0;
  for i := 0 to QSaveDMLDatas.Count - 1 do
  begin
    lDataSaveDML := QSaveDMLDatas[i];
    if lDataSaveDML.ZTCode = '' then
      lDataSaveDML.ZTCode := self.ZTMain;
    lDataSaveDML.ZTCode := lDataSaveDML.ZTCode.ToUpper;
    if lDataSaveDML.DataSaveMode = const_DataSaveMode_SaveData then
    begin
      if lDataSaveDML.TableName = '' then
      begin
        QOneDataResult.ResultMsg := '第' + (i + 1).ToString + '个数据集保存数据集时,保存的表名不可为空';
        exit;
      end;
      if lDataSaveDML.Primarykey = '' then
      begin
        QOneDataResult.ResultMsg := '第' + (i + 1).ToString + '个数据集保存数据集时,保存的主键不可为空';
        exit;
      end;
      if lDataSaveDML.SaveData = '' then
      begin
        QOneDataResult.ResultMsg := '第' + (i + 1).ToString + '个数据集提交的数据为空,请检查';
        exit;
      end;
    end
    else if lDataSaveDML.DataSaveMode = const_DataSaveMode_SaveDML then
    begin
      if lDataSaveDML.SQL = '' then
      begin
        QOneDataResult.ResultMsg := '第' + (i + 1).ToString + '个数据集执行DML操作语句时,操作语句不可为空';
        exit;
      end;
    end
    else
    begin
      QOneDataResult.ResultMsg := '第' + (i + 1).ToString + '个数据集未知的提交模式' + lDataSaveDML.DataSaveMode;
      exit;
    end;
    // 要么事务全由客户端控制,要么全由服务端控制,不混合提交
    if lDataSaveDML.TranID <> '' then
    begin
      lTranCount := lTranCount + 1;
    end;
  end;
  if (lTranCount > 0) and (QSaveDMLDatas.Count <> lTranCount) then
  begin
    QOneDataResult.ResultMsg :=
      '事务不可混合,要么全服务端控制，要么全由客户端控制' + lDataSaveDML.DataSaveMode;
    exit;
  end;

  // 临时保存锁定的账套连接
  lwatchTimer := TStopwatch.StartNew;
  lZTItemList := TDictionary<string, TOneZTItem>.Create;
  try
    for i := 0 to QSaveDMLDatas.Count - 1 do
    begin
      lDataSaveDML := QSaveDMLDatas[i];
      if lDataSaveDML.TranID <> '' then
      begin
        if lZTItemList.TryGetValue(lDataSaveDML.TranID, lZTItem) then
        begin
          // 同个事务的连接
          continue;
        end;
      end
      else
      begin
        // 已经存在此账套连接
        if lZTItemList.TryGetValue(lDataSaveDML.ZTCode, lZTItem) then
        begin
          continue;
        end;
      end;

      lZTItem := nil;
      if lDataSaveDML.TranID <> '' then
      begin
        lZTItem := self.GetTranItem(lDataSaveDML.TranID, lErrMsg);
      end
      else
      begin
        lZTItem := self.LockZTItem(lDataSaveDML.ZTCode, lErrMsg);
      end;
      if lZTItem = nil then
      begin
        // 锁定账套连接失败
        QOneDataResult.ResultMsg := lErrMsg;
        exit;
      end;

      if lDataSaveDML.TranID <> '' then
      begin
        lZTItemList.Add(lDataSaveDML.TranID, lZTItem);
      end
      else
      begin
        lZTItemList.Add(lDataSaveDML.ZTCode, lZTItem);
      end;
    end;
    try
      try
        // 开启事务
        for lZTItem in lZTItemList.Values do
        begin
          // 自定义事务的,是由客户端自由控制
          if not lZTItem.FCustTran then
            lZTItem.ADTransaction.TranStart;
        end;
        // 循环提交的数据，处理
        for i := 0 to QSaveDMLDatas.Count - 1 do
        begin
          lDataResultItem := TOneDataResultItem.Create;
          QOneDataResult.ResultItems.Add(lDataResultItem);
          lDataResultItem.ResultDataMode := const_DataReturnMode_Empty;
          lDataSaveDML := QSaveDMLDatas[i];
          lZTItem := nil;
          if lDataSaveDML.TranID <> '' then
          begin
            lZTItemList.TryGetValue(lDataSaveDML.TranID, lZTItem);
          end
          else
          begin
            lZTItemList.TryGetValue(lDataSaveDML.ZTCode, lZTItem);
          end;

          if lZTItem = nil then
          begin
            QOneDataResult.ResultMsg := '获取账套' + lDataSaveDML.ZTCode + '连接失败';
            exit;
          end;

          if lDataSaveDML.DataSaveMode = const_DataSaveMode_SaveData then
          begin
            // 提交数据
            LZTQuery := lZTItem.ADQuery;
            LZTQuery.SQL.Text := 'select *  from ' + lDataSaveDML.TableName;
            LZTQuery.TableName := lDataSaveDML.TableName;
            LZTQuery.KeyFields := lDataSaveDML.Primarykey;
            LZTQuery.UpdateMode := TUpdateMode.upWhereKeyOnly;
            if lDataSaveDML.UpdateMode <> '' then
            begin
              LZTQuery.UpdateMode := TUpdateMode(GetEnumValue(TypeInfo(TUpdateMode), lDataSaveDML.UpdateMode));
            end;
            // 直接预创建好的变量拿来用
            lSaveStream := lZTItem.DataStream;
            OneStreamString.StreamWriteBase64Str(lSaveStream, lDataSaveDML.SaveData);
            lSaveStream.Position := 0;
            // 加载流
            LZTQuery.LoadFromStream(lSaveStream, TDataPacketFormat.dfBinary);
            if not LZTQuery.Active then
            begin
              QOneDataResult.ResultMsg :=
                '加载第' + (i + 1).ToString + '数据失败';
              exit;
            end;
            //tempMsg := LZTQuery.RecordCount.ToString;
            //字段更新标积
            if lDataSaveDML.FieldProviderFlags <> nil then
            begin
              for iField := 0 to lDataSaveDML.FieldProviderFlags.Count - 1 do
              begin
                //pfInkey,pfInUpdate,pfInWhere
                lProviderFlags := [pfInUpdate, pfInWhere];
                iFlags := lDataSaveDML.FieldProviderFlags[iField];
                if (iFlags div 100) = 1 then
                begin
                  lProviderFlags := lProviderFlags + [pfInkey];
                  iFlags := iFlags - 100;
                end
                else
                begin
                  lProviderFlags := lProviderFlags - [pfInkey];
                  iFlags := iFlags - 200;
                end;
                if (iFlags div 10) = 1 then
                begin
                  lProviderFlags := lProviderFlags + [pfInUpdate];
                  iFlags := iFlags - 10;
                end
                else
                begin
                  lProviderFlags := lProviderFlags - [pfInUpdate];
                  iFlags := iFlags - 20;
                end;
                if iFlags = 1 then
                begin
                  lProviderFlags := lProviderFlags + [pfInWhere];
                end
                else
                begin
                  lProviderFlags := lProviderFlags - [pfInWhere];
                end;
                LZTQuery.Fields[iField].ProviderFlags := lProviderFlags;
              end;
            end;
            // 主键也设定成所有
            if lDataSaveDML.Primarykey <> '' then
            begin
              lArrKeys := lDataSaveDML.Primarykey.Split([';', ','], TStringSplitOptions.ExcludeEmpty);
              for iKey := Low(lArrKeys) to High(lArrKeys) do
              begin
                // 主键参与所有
                LZTQuery.FieldByName(lArrKeys[iKey]).ProviderFlags := [pfInUpdate, pfInWhere, pfInKey];
              end;
            end;
            if lDataSaveDML.NotUpdateFields <> nil then
            begin
              for iKey := 0 to lDataSaveDML.NotUpdateFields.Count - 1 do
              begin
                tempFieldName := lDataSaveDML.NotUpdateFields[iKey];
                //这些字段不参与更新
                LZTQuery.FieldByName(tempFieldName).ProviderFlags := LZTQuery.FieldByName(tempFieldName).ProviderFlags - [pfInUpdate];
              end;
            end;
            // 其它主键也设成所有
            if lDataSaveDML.OtherKeys <> '' then
            begin
              lArrKeys := lDataSaveDML.OtherKeys.Split([';', ','], TStringSplitOptions.ExcludeEmpty);
              for iKey := Low(lArrKeys) to High(lArrKeys) do
              begin
                // 主键参与所有
                LZTQuery.FieldByName(lArrKeys[iKey]).ProviderFlags :=
                  [pfInUpdate, pfInWhere, pfInKey];
              end;
            end;
            try
              LZTQuery.ApplyUpdates(0);
              lDataResultItem.RecordCount := LZTQuery.RowsAffected;
              if lDataSaveDML.IsReturnData then
              begin
                // 返回数据集,比如有自增ID，需返回数据集
                lDataResultItem.ResultDataMode := const_DataReturnMode_Stream;
                lSaveStream := TMemoryStream.Create;
                LZTQuery.SaveToStream(lSaveStream, TDataPacketFormat.dfBinary);
                lSaveStream.Position := 0;
                lDataResultItem.SetStream(lSaveStream);
              end;
            except
              on e: Exception do
              begin
                QOneDataResult.ResultMsg := '保存出现异常:异常原因:' + e.Message;
                exit;
              end;
            end;
          end
          else if lDataSaveDML.DataSaveMode = const_DataSaveMode_SaveDML then
          begin
            // 执行DML语句
            LZTQuery := lZTItem.ADQuery;
            LZTQuery.SQL.Text := lDataSaveDML.SQL;
            iParamCount := LZTQuery.Params.Count;
            if iParamCount > 0 then
            begin
              if iParamCount > lDataSaveDML.Params.Count then
              begin
                QOneDataResult.ResultMsg :=
                  '加载第' + (i + 1).ToString + '异常参数不足';
                exit;
              end;

              for iParam := 0 to LZTQuery.Params.Count - 1 do
              begin
                LOneParam := lDataSaveDML.Params[iParam];
                lFieldType := TFieldType.ftUnknown;
                if LOneParam.ParamDataType <> '' then
                begin
                  lFieldType :=
                    TFieldType(GetEnumValue(TypeInfo(TFieldType), LOneParam.ParamDataType));
                end;
                LZTQuery.Params[iParam].DataType := lFieldType;

                if LOneParam.ParamValue = const_OneParamIsNull_Value then
                begin
                  LZTQuery.Params[iParam].Clear();
                end
                else if lFieldType = TFieldType.ftBlob then
                begin
                  //LZTQuery.Params[iParam].AsStream :=
                  //  OneStreamString.Base64ToStream(LOneParam.ParamValue);
                end
                else
                  LZTQuery.Params[iParam].Value := LOneParam.ParamValue;
              end;
            end;
            try
              LZTQuery.ExecSQL;
              lDataResultItem.RecordCount := LZTQuery.RowsAffected;
            except
              on e: Exception do
              begin
                QOneDataResult.ResultMsg := '执行DML语句异常,原因:' + e.Message;
                exit;
              end;
            end;
            if lDataSaveDML.AffectedMustCount > 0 then
            begin
              if LZTQuery.RowsAffected <> lDataSaveDML.AffectedMustCount then
              begin
                QOneDataResult.ResultMsg :=
                  '执行DML语句异常,原因:必需影响行数[' + lDataSaveDML.AffectedMustCount.ToString +
                  '],当前影响行数[' + LZTQuery.RowsAffected.ToString + ']';
                exit;
              end;
            end;
            if lDataSaveDML.AffectedMaxCount > 0 then
            begin
              if LZTQuery.RowsAffected > lDataSaveDML.AffectedMaxCount then
              begin
                QOneDataResult.ResultMsg :=
                  '执行DML语句异常,原因:最大影响行数[' + lDataSaveDML.AffectedMaxCount.ToString +
                  '],当前影响行数[' + LZTQuery.RowsAffected.ToString + ']';
                exit;
              end;
            end;
          end;
        end;
        for lZTItem in lZTItemList.Values do
        begin
          if not lZTItem.FCustTran then
            lZTItem.ADTransaction.TranCommit;
        end;
        QOneDataResult.ResultOK := True;
        isCommit := True;
      except
        on e: Exception do
        begin
          QOneDataResult.ResultOK := False;
          isCommit := False;
          QOneDataResult.ResultMsg := '保存出现异常:异常原因:' + e.Message;
          exit;
        end;
      end;
    finally
      // 事务处理
      if not isCommit then
      begin
        for lZTItem in lZTItemList.Values do
        begin
          if not lZTItem.FCustTran then
            lZTItem.ADTransaction.TranRollback;
        end;
      end;
    end;
  finally
    lwatchTimer.Stop;
    lRequestMilSec := lwatchTimer.ElapsedMilliseconds;
    for lZTItem in lZTItemList.Values do
    begin
      // 归还连接
      if not lZTItem.FCustTran then
        lZTItem.UnLockWork;
    end;
    lZTItemList.Clear;
    lZTItemList.Free;

    if (self.FLog <> nil) and (self.FLog.IsSQLLog) then
    begin
      self.FLog.WriteSQLLog('账套方法[SaveDatas]:');
      self.FLog.WriteSQLLog('总共用时:[' + lRequestMilSec.ToString + ']毫秒');
      self.FLog.WriteSQLLog('错误消息:[' + QOneDataResult.ResultMsg + ']');
      for i := 0 to QSaveDMLDatas.Count - 1 do
      begin
        lDataSaveDML := QSaveDMLDatas[i];
        self.FLog.WriteSQLLog('SQL语句:[' + lDataSaveDML.SQL + ']');
        for iParam := 0 to lDataSaveDML.Params.Count - 1 do
        begin
          LOneParam := lDataSaveDML.Params[iParam];
          self.FLog.WriteSQLLog('参数:[' + LOneParam.ParamName + ']值[' + LOneParam.ParamValue + ']');
        end;
      end;
    end;
  end;
end;

function TOneZTManage.ExecSQL(QDataSaveDML: TOneDataSaveDML; QParams: array of variant; var QErrMsg: string): integer;
var
  lZTItem: TOneZTItem;
  LZTQuery: TFDQuery;
  iParam: integer;
  isCommit: boolean;
begin
  Result := -1;
  QErrMsg := '';
  lZTItem := self.LockZTItem(QDataSaveDML.ZTCode, QErrMsg);
  if lZTItem = nil then
  begin
    if QErrMsg = '' then
      QErrMsg := '获取账套' + QDataSaveDML.ZTCode + '连接失败,原因未知';
    exit;
  end;

  isCommit := False;
  lZTItem.ADTransaction.TranStart;
  try
    // 打开数据屏B删除,插入,删除功能防止打开数据的SQL里面放有执行DML语句
    LZTQuery := lZTItem.GetQuery;
    LZTQuery.SQL.Text := QDataSaveDML.SQL;
    // 参数赋值
    if LZTQuery.Params.Count <> length(QParams) then
    begin
      QErrMsg := 'SQL最终参数个数和传进的参数个数不一至';
      exit;
    end;
    for iParam := 0 to length(QParams) - 1 do
    begin
      LZTQuery.Params[iParam].Value := QParams[iParam];
    end;

    try
      LZTQuery.ExecSQL;
      Result := LZTQuery.RowsAffected;
      if QDataSaveDML.AffectedMustCount > 0 then
      begin
        if LZTQuery.RowsAffected <> QDataSaveDML.AffectedMustCount then
        begin
          QErrMsg := '执行DML语句异常,原因:必需影响行数[' + QDataSaveDML.AffectedMustCount.ToString +
            '],当前影响行数[' + LZTQuery.RowsAffected.ToString + ']';
          exit;
        end;
      end;
      if QDataSaveDML.AffectedMaxCount > 0 then
      begin
        if LZTQuery.RowsAffected > QDataSaveDML.AffectedMaxCount then
        begin
          QErrMsg := '执行DML语句异常,原因:最大影响行数[' + QDataSaveDML.AffectedMaxCount.ToString +
            '],当前影响行数[' + LZTQuery.RowsAffected.ToString + ']';
          exit;
        end;
      end;
      lZTItem.ADTransaction.TranCommit;
      isCommit := True;
      QErrMsg := 'true';
    except
      on e: Exception do
      begin
        QErrMsg := '打开数据发生异常,原因：' + e.Message;
        exit;
      end;
    end;
  finally
    if not isCommit then
    begin
      lZTItem.ADTransaction.TranRollback;
    end;
    lZTItem.UnLockWork;
  end;
end;

procedure TOneZTManage.BuildStoredSQL(QPhyDirver: string; QOpenData: TOneDataOpen);
var
  lOneParam: TOneParam;
  lParamName: string;
  iParam, iParamCount: integer;
  lParamType: TParamType;
  lFieldType: TFieldType;
  lSizeStr: string;
  LSQL: string;
  lDeclareSQL: string;
  lDeclareSQLParams: string;
  lSetSQL: string;
  lMakeExecSQL: string;
  lMakeExecSQLParams: string;
  lExecuteSQL: string;
  lExecuteSQLDeclare: string;
  lExecuteSQLParams: string;
  lSelectParamsResult: string;
begin
  //组装
  lDeclareSQL := '';
  lSetSQL := '';
  lMakeExecSQL := '';
  lMakeExecSQLParams := '';
  lExecuteSQL := '';
  lExecuteSQLDeclare := '';
  lExecuteSQLParams := '';
  lSelectParamsResult := '';
  LSQL := '';
  if QPhyDirver.StartsWith(Driver_MSSQLServer) then
  begin
    {因为SQLDB对存储过程输入输出参数很不友好,由后台组装SQL解决参数输出问题，最后组装如下
    declare @code nvarchar(30),@name nvarchar(30), @Sqls nvarchar(max)
    set @code='11'
    set @name='flm'
    set @Sqls='exec SP_TEST @name,@code output'
    exec sp_executesql @sqls,N'@name nvarchar(30),@code nvarchar(30) output',@name,@code output
    select @code }
    if QOpenData.Params = nil then
    begin
      QOpenData.Params := TList<TOneParam>.Create;
    end;
    //定义执行的SQL变量
    lDeclareSQL := ' declare @OneZSysMakeSQL nvarchar(max) ';
    lMakeExecSQL := 'set @OneZSysMakeSQL=';
    //set @OneZSysMakeSQL=lMakeExecSQL 后面在组装
    lMakeExecSQLParams := ' exec ' + QOpenData.SPName + ' ';
    lExecuteSQL := 'exec sp_executesql @OneZSysMakeSQL ';

    iParamCount := QOpenData.Params.Count;
    for iParam := 0 to iParamCount - 1 do
    begin
      lOneParam := QOpenData.Params[iParam];
      lParamName := lOneParam.ParamName;
      if lParamName.StartsWith('@') then
      begin
        lParamName := lParamName.Substring(1);
      end;
      lParamType := TParamType(GetEnumValue(TypeInfo(TParamType), lOneParam.ParamType));
      lFieldType := TFieldType(GetEnumValue(TypeInfo(TFieldType), lOneParam.ParamDataType));
      if lOneParam.ParamSize > 0 then
        lSizeStr := lOneParam.ParamSize.ToString
      else
        lSizeStr := '255';
      //设置Set语句
      lSetSQL := lSetSQL + ' set @' + lParamName + '=:' + lParamName + ' ' + #13#10;
      //设置 MakeExecSQL语句
      if lParamType in [ptInputOutput, ptOutput] then
      begin
        lMakeExecSQLParams := lMakeExecSQLParams + ' @' + lParamName + ' output ';
        lExecuteSQLParams := lExecuteSQLParams + ' @' + lParamName + ' output ';
      end
      else
      begin
        lMakeExecSQLParams := lMakeExecSQLParams + ' @' + lParamName + ' ';
        lExecuteSQLParams := lExecuteSQLParams + ' @' + lParamName + ' ';
      end;
      if iParam < iParamCount - 1 then
      begin
        lMakeExecSQLParams := lMakeExecSQLParams + ',';
        lExecuteSQLParams := lExecuteSQLParams + ',';
      end;
      //设置Declare语句
      case lFieldType of
        ftString, ftWideString, ftFixedChar, ftFixedWideChar:
        begin
          lDeclareSQLParams :=
            lDeclareSQLParams + '@' + lParamName + ' nvarchar(' + lSizeStr + ')';
          lExecuteSQLDeclare := lDeclareSQLParams;
        end;
        ftSmallint, ftInteger:
        begin
          lDeclareSQLParams := lDeclareSQLParams + '@' + lParamName + ' int ';
          lExecuteSQLDeclare := lDeclareSQLParams;
        end;
        ftLargeint:
        begin
          lDeclareSQLParams := lDeclareSQLParams + '@' + lParamName + ' bigint ';
          lExecuteSQLDeclare := lDeclareSQLParams;
        end;
        ftWord, ftFloat, ftCurrency, ftBCD, ftFMTBcd:
        begin
          lDeclareSQLParams := lDeclareSQLParams + '@' + lParamName + ' float ';
          lExecuteSQLDeclare := lDeclareSQLParams;
        end;
        ftVariant:
        begin
          lSizeStr := '255';
          lDeclareSQLParams :=
            lDeclareSQLParams + ',@' + lParamName + ' nvarchar(' + lSizeStr + ')';
          lExecuteSQLDeclare := lDeclareSQLParams;
        end
        else
        begin
          lSizeStr := '255';
          lDeclareSQLParams :=
            lDeclareSQLParams + ',@' + lParamName + ' nvarchar(' + lSizeStr + ')';
          lExecuteSQLDeclare := lDeclareSQLParams;
        end;
      end;
      //结果输出
      lSelectParamsResult := lSelectParamsResult + ' @' + lParamName + ' as ' + lParamName;
      //增加Output
      if lParamType in [ptInputOutput, ptOutput] then
      begin
        lExecuteSQLDeclare := lExecuteSQLDeclare + ' output';
      end;
      //设置 lExecuteSQLDeclare
      if iParam < iParamCount - 1 then
      begin
        lDeclareSQLParams := lDeclareSQLParams + ' , ';
        lExecuteSQLDeclare := lExecuteSQLDeclare + ' , ';
        lSelectParamsResult := lSelectParamsResult + ' , ';
      end;
    end;
    //组装SQL
    if lDeclareSQLParams <> '' then
    begin
      lDeclareSQL := lDeclareSQL + ',' + lDeclareSQLParams;
    end;
    lMakeExecSQL := lMakeExecSQL + QuoTedStr(lMakeExecSQLParams);
    if lExecuteSQLDeclare <> '' then
    begin
      lExecuteSQL := lExecuteSQL + ',N' + QuoTedStr(lExecuteSQLDeclare);
    end;
    if lExecuteSQLParams <> '' then
    begin
      lExecuteSQL := lExecuteSQL + ',' + lExecuteSQLParams;
    end;
    if lSelectParamsResult <> '' then
    begin
      lSelectParamsResult := ' select ' + lSelectParamsResult;
    end;
    //最后SQL
    LSQL := lDeclareSQL + #13#10 + lSetSQL + #13#10 + lMakeExecSQL + #13#10 + lExecuteSQL + #13#10 + lSelectParamsResult;
  end;
  if LSQL <> '' then
  begin
    QOpenData.OpenSQL := LSQL;
  end;
end;

function TOneZTManage.ExecStored(QOpenData: TOneDataOpen; var QOneDataResult: TOneDataResult): boolean;
var
  lZTItem: TOneZTItem;
  lErrMsg: string;
  lFDStored: TFDStoredProc;
  i, iParam: integer;
  tempStr: string;
  LFDParam: TParam;
  lDictParam: TDictionary<string, TOneParam>;
  LOneParam: TOneParam;
  lStream, lParamStream: TMemoryStream;
  lDataResultItem: TOneDataResultItem;
  lPTResult: integer;
  lRequestMilSec: integer;
  lwatchTimer: TStopwatch;
  isOutParam: boolean;
  lField: TField;
  lFielName: string;
begin
  Result := False;
  lPTResult := 0;
  if QOneDataResult = nil then
  begin
    QOneDataResult := TOneDataResult.Create;
  end;
  if QOpenData.Params = nil then
  begin
    QOpenData.Params := TList<TOneParam>.Create;
  end;
  // 处理数据
  if QOpenData = nil then
  begin
    QOneDataResult.ResultMsg := '请传入要执行存储过程的信息';
    exit;
  end;
  if QOpenData.OpenSQL.Trim = '' then
  begin
    QOneDataResult.ResultMsg := '执行的存储过程SQL语句为空';
    exit;
  end;

  lZTItem := nil;
  lErrMsg := '';
  lwatchTimer := TStopwatch.StartNew;
  lDictParam := TDictionary<string, TOneParam>.Create;
  try
    for iParam := 0 to QOpenData.Params.Count - 1 do
    begin
      LOneParam := QOpenData.Params[iParam];
      lDictParam.Add(LOneParam.ParamName.ToLower, LOneParam);
    end;

    if QOpenData.ZTCode = '' then
      QOpenData.ZTCode := self.ZTMain;
    QOpenData.ZTCode := QOpenData.ZTCode.ToUpper;
    lZTItem := nil;
    // 客户端发起事务,像两层一样运作
    if QOpenData.TranID <> '' then
    begin
      lZTItem := self.GetTranItem(QOpenData.TranID, lErrMsg);
    end
    else
    begin
      lZTItem := self.LockZTItem(QOpenData.ZTCode, lErrMsg);
    end;
    if lZTItem = nil then
    begin
      if lErrMsg = '' then
        lErrMsg := '获取账套' + QOpenData.ZTCode + '连接失败,原因未知';
      exit;
    end;
    self.BuildStoredSQL(lZTItem.FZTSet.PhyDriver, QOpenData);
    lFDStored := lZTItem.ADStoredProc;
    lFDStored.SQL.Text := QOpenData.OpenSQL;
    lFDStored.PackageName := QOpenData.PackageName;
    lFDStored.StoredProcName := QOpenData.SPName;
    // 准备参数
    lFDStored.Prepare;
    if not lFDStored.Prepared then
    begin
      lErrMsg := '校验存储过程失败,请检查是否有此存储过程[' + QOpenData.SPName + ']';
      exit;
    end;
    for iParam := lFDStored.Params.Count - 1 downto 0 do
    begin
      LFDParam := lFDStored.Params[iParam];
      if LFDParam.ParamType = TParamType.ptResult then
      begin
        lPTResult := lPTResult + 1;
        continue;
      end;
      if LFDParam.Name.StartsWith('@') then
      begin
        // SQL数据库返回参数代@去掉
        LFDParam.Name := LFDParam.Name.Substring(1);
      end;
    end;

    if QOpenData.Params.Count <> lFDStored.Params.Count - lPTResult then
    begin
      tempStr := '参数个数错误->服务端参数个数[' + lFDStored.Params.Count.ToString + '],如下[';
      for iParam := lFDStored.Params.Count - 1 downto 0 do
      begin
        tempStr := tempStr + lFDStored.Params[iParam].Name;
      end;
      tempStr := tempStr + ';客户端参数个数[' + QOpenData.Params.Count.ToString + '],如下[';
      for iParam := QOpenData.Params.Count - 1 downto 0 do
      begin
        tempStr := tempStr + QOpenData.Params[iParam].ParamName;
      end;
      tempStr := tempStr + ']';
      lErrMsg := tempStr;
      exit;
    end;
    // 参数赋值
    for iParam := 0 to lFDStored.Params.Count - 1 do
    begin
      LFDParam := lFDStored.Params[iParam];
      if LFDParam.ParamType = TParamType.ptResult then
      begin
        continue;
      end;
      // 处理参数
      LOneParam := nil;
      if not lDictParam.TryGetValue(LFDParam.Name.ToLower, LOneParam) then
      begin
        lErrMsg := '参数[' + LFDParam.Name + '],找不到对应的参数,请检查传上来的参数';
        exit;
      end;
      if LFDParam.ParamType in [TParamType.ptInputOutput, TParamType.ptOutput] then
        isOutParam := True;
      if LFDParam.ParamType in [TParamType.ptInput, TParamType.ptInputOutput, TParamType.ptOutput] then
      begin
        if LFDParam.DataType = ftWideMemo then
        begin
          LFDParam.AsMemo := LOneParam.ParamValue;
        end
        else if LFDParam.DataType = ftWideString then
        begin
          LFDParam.AsWideString := LOneParam.ParamValue;
        end
        else if LFDParam.DataType in ftBlobTypes then
        begin
          //lParamStream := TMemoryStream.Create;
          //OneStreamString.StreamWriteBase64Str(lParamStream,
          //  LOneParam.ParamValue);
          //LFDParam.AsStream := lParamStream;
        end
        else
          LFDParam.Value := LOneParam.ParamValue;
      end;
    end;
    // ExecProc 执行一个存储过程返回参数
    try
      if (QOpenData.SPIsOutData) or (isOutParam) then
      begin
        lFDStored.Open;
      end
      else
      begin
        lFDStored.ExecSQL;
      end;
      //tempStr := lFDStored.Fields[0].FieldName;
      // 说明有返回数据集,添加数据集,可以返回多个数据集
      if (not isOutParam) and (not QOpenData.SPIsOutData) then
      begin
        lDataResultItem := TOneDataResultItem.Create;
        QOneDataResult.ResultItems.Add(lDataResultItem);
      end;
      begin
        //返回数据
        if not lFDStored.Active then
        begin
          lErrMsg := '设定返回数据集，但无相关数据集返回,请检查;';
          exit;
        end;
        if isOutParam and (not QOpenData.SPIsOutData) then
        begin
          lDataResultItem := TOneDataResultItem.Create;
          QOneDataResult.ResultItems.Add(lDataResultItem);
          for iParam := 0 to lFDStored.Fields.Count - 1 do
          begin
            lField := lFDStored.Fields[iParam];
            lFielName := lFDStored.Fields[iParam].FieldName;
            if lDictParam.TryGetValue(lFielName.ToLower, LOneParam) then
            begin
              LOneParam := TOneParam.Create;
              lDataResultItem.ResultParams.Add(LOneParam);
              LOneParam.ParamName := lFielName;
              LOneParam.ParamValue := VarToStr(lField.Value);
            end;
          end;
        end
        else
        begin
          lDataResultItem := TOneDataResultItem.Create;
          QOneDataResult.ResultItems.Add(lDataResultItem);
          lDataResultItem.ResultDataMode := const_DataReturnMode_Stream;
          lStream := TMemoryStream.Create;
          lFDStored.SaveToStream(lStream, TDataPacketFormat.dfBinary);
          lDataResultItem.SetStream(lStream);
          lDataResultItem := QOneDataResult.ResultItems[0];
        end;
      end;
      Result := True;
      QOneDataResult.ResultOK := True;
    except
      on e: Exception do
      begin
        lErrMsg := '执行存储过程异常:' + e.Message;
        exit;
      end;
    end;
  finally
    QOneDataResult.ResultMsg := lErrMsg;
    if lZTItem <> nil then
    begin
      lZTItem.UnLockWork;
    end;
    lDictParam.Clear;
    lDictParam.Free;
    lwatchTimer.Stop;
    lRequestMilSec := lwatchTimer.ElapsedMilliseconds;
    if (self.FLog <> nil) and (self.FLog.IsSQLLog) then
    begin
      self.FLog.WriteSQLLog('账套方法[ExecStored]:');
      self.FLog.WriteSQLLog('总共用时:[' + lRequestMilSec.ToString + ']毫秒');
      self.FLog.WriteSQLLog('错误消息:[' + lErrMsg + ']');
      self.FLog.WriteSQLLog('SQL语句:[' + QOpenData.SPName + ']');
      for iParam := 0 to QOpenData.Params.Count - 1 do
      begin
        LOneParam := QOpenData.Params[iParam];
        self.FLog.WriteSQLLog('参数:[' + LOneParam.ParamName + ']值[' + LOneParam.ParamValue + ']');
      end;
    end;
  end;

end;

procedure TOneZTManage.InitPhyDriver(QDriverName: string);
var
  lFileName: string;
  lLib: TSQLDBLibraryLoader;
begin
  if QDriverName = '' then
    exit;
  if self.FLibrarys.ContainsKey(QDriverName) then
  begin
    exit;
  end;
  if QDriverName.StartsWith(Driver_MSSQLServer) then
    exit;
  {$ifdef CPUX86}
  lFileName := 'OnePlatform\OnePhyDBDLL\'+QDriverName+'\32\'
   {$else CPUX64}
  lFileName := 'OnePlatform\OnePhyDBDLL\' + QDriverName + '\64\';
  {$endif}
  //目录不存在创建
  lFileName := OneFileHelper.CombineExeRunPath(lFileName);
  if not DirectoryExists(lFileName) then
    ForceDirectories(lFileName);
  //文件存在加载
  lLib := TSQLDBLibraryLoader.Create(nil);
  self.FLibrarys.add(QDriverName, lLib);
  lLib.ConnectionType := QDriverName;
  //if QDriverName.StartsWith(Driver_MSSQLServer) then
  //begin
  //  lLib.ConnectionType := Driver_MSSQLServer;
  //  lFileName := OneFileHelper.CombinePath(lFileName, 'dblib.dll');
  //end
  //else
  if QDriverName.StartsWith(Driver_MySQL) then
  begin
    lLib.ConnectionType := QDriverName;
    lFileName := OneFileHelper.CombinePath(lFileName, 'libmysql.dll');
  end
  else
  if QDriverName.StartsWith(Driver_Oracle) then
  begin
    lLib.ConnectionType := Driver_Oracle;
    lFileName := OneFileHelper.CombinePath(lFileName, 'oci.dll');
  end
  else
  if QDriverName.StartsWith(Driver_PostgreSQL) then
  begin
    lLib.ConnectionType := Driver_PostgreSQL;
    lFileName := OneFileHelper.CombinePath(lFileName, 'libpq.dll');
  end
  else
  if QDriverName.StartsWith(Driver_SQLite3) then
  begin
    lLib.ConnectionType := Driver_SQLite3;
    lFileName := OneFileHelper.CombinePath(lFileName, 'sqlite3.dll');
  end
  else
  if QDriverName.StartsWith(Driver_Sybase) then
  begin
    lLib.ConnectionType := Driver_Sybase;
    lFileName := OneFileHelper.CombinePath(lFileName, 'dblib.dll');
  end
  else
  if QDriverName.StartsWith(Driver_Firebird) then
  begin
    lLib.ConnectionType := Driver_Firebird;
    lFileName := OneFileHelper.CombinePath(lFileName, 'fbclient.dll');
  end;
  if fileExists(lFileName) then
  begin
    lLib.LibraryName := lFileName;
    lLib.Enabled := True;
    lLib.LoadLibrary;
  end;
end;

procedure UnInitPhyDriver;
begin
end;

procedure InitSQLInfo(var QSQLInfo: TSQLInfo);
begin
  QSQLInfo.FDriver := '';
  QSQLInfo.FDriverVersion := '';
  QSQLInfo.FPageIndex := -1;
  QSQLInfo.FPageSize := -1;
  QSQLInfo.FSQL := '';
  QSQLInfo.FOrderByLine := -1;
  QSQLInfo.FOrderSQL := '';
  QSQLInfo.FPageField := '';
  QSQLInfo.FErrMsg := '';
end;

function ClearOrderBySQL(QSQL: string): string;
var
  i: integer;
  lTempStr: string;
  lOrderIndex, lByIndex: integer;
begin
  lTempStr := '';
  lOrderIndex := -1;
  lByIndex := -1;
  for i := Low(QSQL) to High(QSQL) do
  begin
    if (QSQL[i] <> ' ') and (QSQL[i] <> char(13)) and (QSQL[i] <> char(10)) then
    begin
      lTempStr := lTempStr + QSQL[i];
    end
    else
    begin
      lTempStr := lTempStr.ToLower;
      if (lOrderIndex = -1) and (lTempStr = 'order') then
      begin
        lOrderIndex := i - 5;
      end
      else
      begin
        if (lOrderIndex > 0) then
        begin
          if lTempStr = 'by' then
          begin
            lByIndex := i - 2;
          end
          else if lTempStr = 'from' then
          begin
            if i > lOrderIndex then
            begin
              lOrderIndex := -1;
              lByIndex := -1;
            end;
          end
          else if (lByIndex = -1) and (length(lTempStr) > 0) then
          begin
            lOrderIndex := -1;
          end;
        end;
      end;
      lTempStr := '';
    end;
  end;
  if lOrderIndex > 0 then
  begin
    Result := Copy(QSQL, Low(''), lOrderIndex - 1);
  end
  else
  begin
    Result := QSQL;
  end;
end;

function SetSQLInfo(var QSQLInfo: TSQLInfo): boolean;
var
  lList: TStringList;
  i, iDriverVersion, tempIStar, tempIEnd: integer;
  tempSQL: string;
  tempOrderBySQL: string;
  lRegExpr: TRegExpr;
begin
  Result := False;
  if QSQLInfo.FPageSize <= 0 then
  begin
    //不分页取所有数据
    Result := True;
    exit;
  end;
  if QSQLInfo.FDriver = '' then
  begin
    QSQLInfo.FErrMsg := '数据库驱动为空,无法组装分页SQL';
    exit;
  end;
  if QSQLInfo.FPageIndex <= 0 then
    QSQLInfo.FPageIndex := 1;   //第一页
  lList := TStringList.Create;
  try
    lList.Text := QSQLInfo.FSQL;
    for i := 0 to lList.Count - 1 do
    begin
      tempSQL := lList[i];
      tempSQL := tempSQL.Trim();
      if tempSQL.StartsWith('order ', True) then
      begin
        tempSQL := tempSQL.Substring(5);
        tempSQL := tempSQL.Trim;
        if tempSQL.StartsWith('by ', True) then
        begin
          QSQLInfo.FOrderByLine := i;
          QSQLInfo.FOrderSQL := lList[i];
        end;
      end;
    end;

    //处理
    iDriverVersion := 0;
    tryStrToInt(QSQLInfo.FDriverVersion, iDriverVersion);
    if QSQLInfo.FDriver.StartsWith(Driver_MSSQLServer) then
    begin
      //SQLServer数据库分页
      //采用 offset 0  rows fetch next 1  rows only
      if iDriverVersion >= 2012 then
      begin
        if QSQLInfo.FOrderByLine > 0 then
        begin
          tempSQL := ' offset ' + (QSQLInfo.FPageIndex - 1).Tostring() + '  rows fetch next ' + QSQLInfo.FPageSize.ToString +
            '  rows only';
          lList.Add(tempSQL);
          QSQLInfo.FSQL := lList.Text;
        end
        else
        begin
          //最后面加上order by 及分页语句,offset必需跟在order by 后面
          lList.Add(' order by 1 ');
          tempSQL := ' offset ' + (QSQLInfo.FPageIndex - 1).Tostring() + '  rows fetch next ' + QSQLInfo.FPageSize.ToString +
            '  rows only';
          lList.Add(tempSQL);
          QSQLInfo.FSQL := lList.Text;
        end;
      end
      else
      begin
        //小于2012的用rowNumber模式分页,注意有Order by语句需处理
        tempOrderBySQL := '';
        if QSQLInfo.FOrderByLine <= 0 then
        begin
          QSQLInfo.FErrMsg :=
            'MSSQL低于2012版本数据库,分页需要有order by 相关字段 否则无法分页,且order by单独一行便于分析SQL';
          exit;
        end;
        if QSQLInfo.FOrderByLine > 0 then
        begin
          tempOrderBySQL := lList[QSQLInfo.FOrderByLine];
          lList[QSQLInfo.FOrderByLine] := '';
          //orderbySQL表名处理 表.字段，要替换成相关 SysOneT.因为over number变成外层SQL需处理
          lRegExpr := TRegExpr.Create;
          try
            lRegExpr.Expression := '([a-zA-Z]{0,}\.)';
            tempOrderBySQL := lRegExpr.Replace(tempOrderBySQL, 'SysOneT.');
          finally
            lRegExpr.Free;
          end;
        end;
        tempSQL := lList.Text;
        tempIStar := (QSQLInfo.FPageIndex - 1) * QSQLInfo.FPageSize;
        tempIEnd := tempIStar + QSQLInfo.FPageSize;
        tempSQL := ' SELECT SysOneT.* FROM ' + ' ( SELECT SysOneT.*, ROW_NUMBER() OVER( ' + tempOrderBySQL +
          ' ) AS zPage_rn FROM ' + '    ( ' + tempSQL + ' ) SysOneT ' + ' ) SysOneT WHERE SysOneT.zPage_rn > ' +
          tempIStar.ToString + ' AND  SysOneT.zPage_rn <= ' + tempIEnd.ToString;
        QSQLInfo.FSQL := tempSQL;
        QSQLInfo.FPageField := 'zPage_rn';
      end;
    end
    else
    if QSQLInfo.FDriver.StartsWith(Driver_MySQL) then
    begin
      tempIStar := (QSQLInfo.FPageIndex - 1) * QSQLInfo.FPageSize;
      tempIEnd := tempIStar + QSQLInfo.FPageSize;
      lList.Add(' limit ' + tempIStar.ToString + ', ' + tempIEnd.ToString);
      QSQLInfo.FSQL := lList.Text;
    end
    else
    begin
      QSQLInfo.FErrMsg := '未设计的数据库驱动[' + QSQLInfo.FDriver + ']请联系作者加上相关驱动写法!';
      exit;
    end;
  finally
    lList.Free;
  end;
  Result := True;
end;


initialization

finalization

end.
