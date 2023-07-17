unit OneClientDataSet;

 {$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
// 数据集控件,继承TFDMemTable
interface

uses
  SysUtils, StrUtils, Classes, DB, SQLDB, BufDataset, Generics.Collections,
  OneClientConnect, OneClientDataInfo, OneClientConst, OneAsynThread;

type
  TOneDataSet = class;
  TOneDataInfo = class;

  TOneBinaryDatapacketHandler = class(TFpcBinaryDatapacketReader)
  public
    procedure StoreRecord(ARowState: TRowState; AUpdOrder: integer = 0); override;
  end;

  TOneDataInfo = class(TPersistent)
  private
    // 设计时获取相关字段
    FIsDesignGetFields: boolean;
    // 所属数据集
    FOwnerDataSet: TOneDataSet;
    // OneServer连接
    FConnection: TOneConnection;
    // 账套代码
    FZTCode: string;
    // 控件描述
    FDescription: string;
    // 表名,保存时用到
    FTableName: string;
    // 主键,保存时用到
    FPrimaryKey: string;
    // 其它辅助主键,保存时用到
    FOtherKeys: string;
    // 数据集打开数据模式
    FOpenMode: TDataOpenMode;
    // 保存数据集模式
    FSaveMode: TDataSaveMode;
    // 服务端返回数据模式
    FDataReturnMode: TDataReturnMode;
    // 包名
    FPackageName: string;
    // 存储过程名称
    FStoredProcName: string;
    // 分页 每页大小 默认-1 不限制
    FPageSize: integer;
    // 分页 第几页
    FPageIndex: integer;
    // 分页 总共页数
    FPageCount: integer;
    // 分页 总共条数
    FPageTotal: integer;
    // 执行SQL语句，最多影响行数
    FAffectedMaxCount: integer;
    // 执行SQL语句，必需有且几条一定受影响
    FAffectedMustCount: integer;
    // 服务端返回影响行数
    FRowsAffected: integer;
    // 是否异步
    FAsynMode: boolean;
    // 是否返回数据集
    FIsReturnData: boolean;
    // 执选DML insert 时返回的自增ID
    FReturnAutoID: string;
    // 服务端返回的JSON数据
    FReturnJson: string;
    // SQL最终使用方式采用服务端SQL，客户端SQL只是为了设置方便
    // 后期扩展 ,不填默认采用客户端传SQL模式
    FSQLServerCode: string;
    // 二层模式事务ID
    FTranID: string;
    // 事务锁定时间 <=0,无限。
    FTranSpanSec: integer;
    // 错误信息
    FErrMsg: string;
  private
    // 获取连接
    function GetConnection: TOneConnection;
    procedure SetConnection(const AValue: TOneConnection);
    // 设计模式下获取相关字段
    procedure SetGetFields(Value: boolean);
  public
    constructor Create(QDataSet: TOneDataSet); overload;
    destructor Destroy; override;
  published
    /// <param name="IsDesignGetFields">设计时获取相关字段,请先设置好连接及SQL</param>
    property IsDesignGetFields: boolean read FIsDesignGetFields write SetGetFields;
    /// <param name="OwnerDataSet">所属数据集</param>
    property OwnerDataSet: TOneDataSet read FOwnerDataSet;
    /// <param name="Connection">连接OneServer服务器的连接</param>
    property Connection: TOneConnection read GetConnection write SetConnection;
    /// <param name="ZTCode">账套代码,有设置优先取此账套代码,否则取Connection公用的账套代码</param>
    property ZTCode: string read FZTCode write FZTCode;
    /// <param name="FDescription">控件描述</param>
    property Description: string read FDescription write FDescription;
    /// <param name="TableName">表名,保存时会用到</param>
    property TableName: string read FTableName write FTableName;
    /// <param name="PrimaryKey">主键,保存时用到</param>
    property PrimaryKey: string read FPrimaryKey write FPrimaryKey;
    /// <param name="OtherKeys">其它辅助主键,多个用;分开</param>
    property OtherKeys: string read FOtherKeys write FOtherKeys;
    /// <param name="OpenMode">数据集打开模式</param>
    property OpenMode: TDataOpenMode read FOpenMode write FOpenMode;
    /// <param name="SaveMode">保存数据集模式,数据集delate和DML操作语句</param>
    property SaveMode: TDataSaveMode read FSaveMode write FSaveMode;
    /// <param name="DataReturnMode">数据集返回模式</param>
    property DataReturnMode: TDataReturnMode read FDataReturnMode write FDataReturnMode;
    /// <param name="PackageName">包名</param>
    property PackageName: string read FPackageName write FPackageName;
    /// <param name="StoredProcName">存储过程名称</param>
    property StoredProcName: string read FStoredProcName write FStoredProcName;
    /// <param name="PageSize">分页 每页大小 默认-1 不限制</param>
    property PageSize: integer read FPageSize write FPageSize;
    /// <param name="PageIndex">分页 第几页</param>
    property PageIndex: integer read FPageIndex write FPageIndex;
    /// <param name="PageCount">分页 总共页数</param>
    property PageCount: integer read FPageCount write FPageCount;
    /// <param name="PageTotal">分页 总共条数</param>
    property PageTotal: integer read FPageTotal write FPageTotal;
    /// <param name="AffectedMaxCount">执行DML语句，最多影响行数，0代表不控制</param>
    property AffectedMaxCount: integer read FAffectedMaxCount write FAffectedMaxCount;
    /// <param name="AffectedMustCount">执行DML语句必需有且几条一定受影响，默认1条，0代表不控制</param>
    property AffectedMustCount: integer read FAffectedMustCount write FAffectedMustCount;
    /// <param name="RowsAffected">执行SQL语句,服务端返回影响行数</param>
    property RowsAffected: integer read FRowsAffected write FRowsAffected;
    /// <param name="AsynMode">是否异步</param>
    property AsynMode: boolean read FAsynMode write FAsynMode;
    /// <param name="IsReturnData">是否返回数据集,比如执行存储过程是否返回数据</param>
    property IsReturnData: boolean read FIsReturnData write FIsReturnData;
    /// <param name="ReturnAutoID">返回自增ID的值  执选DML insert 时返回的自增ID</param>
    property ReturnAutoID: string read FReturnAutoID write FReturnAutoID;
    /// <param name="ReturnJson">服务端返回的JSON数据</param>
    property ReturnJson: string read FReturnJson write FReturnJson;
    /// <param name="SQLServerCode">服务端SQL板模编码</param>
    property SQLServerCode: string read FSQLServerCode write FSQLServerCode;
    /// <param name="TranID">二层模式事务ID</param>
    property TranID: string read FTranID write FTranID;
    /// <param name="TranSpanSec">默认0，不超时</param>
    property TranSpanSec: integer read FTranSpanSec write FTranSpanSec;
    /// <param name="ErrMsg">错误信息</param>
    property ErrMsg: string read FErrMsg write FErrMsg;
  end;


  TOneDataSet = class(TBufDataset)
  private
    // one扩展属性
    FDataInfo: TOneDataInfo;
    // 多个数据集
    FMultiData: TList<TBufDataset>;
    // 多个数据集当前索引
    FMultiIndex: integer;
    // SQL相关
    FCommandText: TStrings;
    // 参数相关
    FParams: TParams;
    FUpdateMode: TUpdateMode;
    FOpenCallBack: EvenOKCallBackObject;
  private
    procedure SetUpdateMode(AValue: TUpdateMode);
  private
    procedure SetParams(Value: TParams);
    procedure SetCommandText(const Value: TStrings);
    procedure SQLListChanged(Sender: TObject);
    procedure SetMultiIndex(Value: integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    /// <summary>
    /// 打开数据集，返回所有数据，如果Pagesize和PageIndex设置，则返回分页数据
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function Open: boolean;
    /// <summary>
    /// 打开数据集，返回所有数据，如果Pagesize和PageIndex设置，则返回分页数据
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function OpenData: boolean;
    /// <summary>
    /// 异步打开数据集，返回所有数据，如果Pagesize和PageIndex设置，则返回分页数据
    /// </summary>
    /// <returns>成功失败多调用 QCallEven</returns>
    procedure OpenDataAsync(QProce: evenAsynProcObj);
    /// <summary>
    /// 检重复,输入SQL和参数还有原值,是否有重复的字段，依托于DataSet但不会影响本身DataSet任何东东
    /// </summary>
    /// <returns>成功失败多调用 QCallEven</returns>
    function CheckRepeat(QSQL: string; QParamValues: array of variant; QSourceValue: string): boolean;
    /// <summary>
    /// 保存数据
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function Save: boolean;
    /// <summary>
    /// 保存数据,提交数据Delta
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function SaveData: boolean;
    procedure SaveDataAsync(QProce: evenAsynProcObj);
    /// <summary>
    /// 执行DML语句,update,insert,delete语句
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function ExecDML: boolean;
    function ExecDMLs(QDMLDatas: array of TOneDataSet): boolean;
    /// <summary>
    /// 执行DML语句,update,insert,delete语句，依托于DataSet但不会影响本身DataSet任何东东
    /// QMustOneAffected:是否有一条必需受影响
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function ExecDMLSQL(QSQL: string; QParamValues: array of variant; QMustOneAffected: boolean = True): boolean;
    /// <summary>
    /// 执行存储过程，返回数据
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function OpenStored: boolean;
    /// <summary>
    /// 执行存储过程，不返回数据
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function ExecStored: boolean;
    /// <summary>
    /// 事务控制第一步:获取账套连接,标识成事务账套
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function LockTran(): boolean;
    /// <summary>
    /// 事务控制第最后步:用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function UnLockTran(): boolean;
    /// <summary>
    /// 事务控制第二步:开始事务
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function StartTran(): boolean;
    /// <summary>
    /// 事务控制第三步:提交事务
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function CommitTran(): boolean;
    /// <summary>
    /// 事务控制第三步:回滚事务
    /// </summary>
    /// <returns>失败返回False,错误信息在ErrMsg属性</returns>
    function RollbackTran(): boolean;

    procedure SaveDeltaToStream(AStream: TStream);

    function IsSameStruct(QDataset: TBufDataset): boolean;
    procedure CopyDataSetOnlyData(QDataset: TBufDataset);

    //打开多个数据集功能
    // 打开数据
    function OpenDatas(QOpenDatas: TList<TOneDataSet>): boolean; overload;
    function OpenDatas(QOpenDatas: array of TOneDataSet): boolean; overload;
    // 保存数据
    function SaveDatas(QObjectList: TList<TOneDataSet>): boolean; overload;
    function SaveDatas(QObjectArray: array of TOneDataSet): boolean; overload;
    function IsEdit(): boolean;
  published
    { Published declarations }
    /// <param name="SQL">SQL语句，您可以在这里设置您要执行的SQL语句文本，然后通过OpenData方法打开数据集</param>
    property SQL: TStrings read FCommandText write SetCommandText;
    /// <param name="DataInfo">one扩展功能的统一放在一个类</param>
    property DataInfo: TOneDataInfo read FDataInfo write FDataInfo;
    /// <param name="Params">参数设置</param>
    property Params: TParams read FParams write SetParams;
    /// <param name="MultiData">返回多个据存储的地方</param>
    property MultiData: TList<TBufDataset> read FMultiData write FMultiData;
    property MultiIndex: integer read FMultiIndex write SetMultiIndex;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereKeyOnly;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OneClient', [TOneDataSet]);
end;

function getID(IsOrder: boolean = False): string;
var
  ii: TGUID;
  lDateTime: TDateTime;
begin
  CreateGUID(ii);
  Result := LowerCase(Copy(AnsiReplaceStr(GUIDToString(ii), '-', ''), 2, 32));
end;

procedure TOneBinaryDatapacketHandler.StoreRecord(ARowState: TRowState; AUpdOrder: integer = 0);
begin
  if (ARowState = []) then
    exit;
  inherited StoreRecord(ARowState, AUpdOrder);
end;

constructor TOneDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommandText := TStringList.Create;
  FCommandText.TrailingLineBreak := False;
  TStringList(FCommandText).OnChange := SQLListChanged;
  FParams := TParams.Create;

  FDataInfo := TOneDataInfo.Create(Self);
  FUpdateMode := TUpdateMode.upWhereKeyOnly;
  FMultiData := nil;
  FMultiIndex := 0;
end;

destructor TOneDataSet.Destroy;
var
  i: integer;
begin
  FCommandText.Clear;
  FCommandText.Free;
  if FDataInfo <> nil then
  begin
    FDataInfo.Free;
    FDataInfo := nil;
  end;
  if FMultiData <> nil then
  begin
    for i := 0 to FMultiData.Count - 1 do
    begin
      FMultiData[i].Close;
      FMultiData[i].Free;
    end;
    FMultiData.Clear;
    FMultiData.Free;
  end;
  FParams.Free;
  inherited Destroy;
end;

procedure TOneDataSet.SetUpdateMode(AValue: TUpdateMode);
begin
  FUpdateMode := AValue;
end;

procedure TOneDataSet.SetParams(Value: TParams);
begin
  if Value <> FParams then
    FParams.Assign(Value);
end;

procedure TOneDataSet.SetCommandText(const Value: TStrings);
var
  SQL: string;
  List: TParams;
begin
  // 分析SQL获取相关信息
  if FCommandText <> Value then
    FCommandText.Assign(Value);
end;

// SQL改变自动改变参数
procedure TOneDataSet.SQLListChanged(Sender: TObject);
var
  i: integer;
  LNewParams: TParams;
  LOldFDParams: TParams;
  LNewFDParam, LOldFDParam: TParam;
  lOldFDPramsDict: TDictionary<string, TParam>;
  lNewFDPramsDict: TDictionary<string, TParam>;
  NewParams: TSQLDBParams;
  ConnOptions: TConnOptions;
begin
  NewParams := TSQLDBParams.Create(nil);
  ;
  try
    ConnOptions := [sqEscapeRepeat, sqEscapeSlash];
    NewParams.ParseSQL(FCommandText.Text, True, sqEscapeSlash in ConnOptions,
      sqEscapeRepeat in ConnOptions, psInterbase);
    NewParams.AssignValues(FParams);
    FParams.Assign(NewParams);
  finally
    NewParams.Free;
  end;
  //if FCommandText.Text <> '' then
  //begin
  //  // 参数
  //  lOldFDPramsDict := TDictionary<string, TParam>.Create;
  //  lNewFDPramsDict := TDictionary<string, TParam>.Create;
  //  LNewParams := TParams.Create(Self);
  //  LOldFDParams := TParams.Create;
  //  try
  //    LNewParams.ParseSQL(FCommandText.Text, True);
  //    // 值拷贝
  //    for i := 0 to FParams.Count - 1 do
  //    begin
  //      if lOldFDPramsDict.ContainsKey(FParams[i].Name.ToLower) then
  //        continue;
  //      LOldFDParam := FParams[i];
  //      LNewFDParam := TParam(LOldFDParams.Add);
  //      LNewFDParam.Name := LOldFDParam.Name;
  //      LNewFDParam.Value := LOldFDParam.Value;
  //      LNewFDParam.NumericScale := LOldFDParam.NumericScale;
  //      LNewFDParam.Precision := LOldFDParam.Precision;
  //      LNewFDParam.Size := LOldFDParam.Size;
  //      LNewFDParam.DataType := LOldFDParam.DataType;
  //      LNewFDParam.ParamType := LOldFDParam.ParamType;
  //      lOldFDPramsDict.Add(FParams[i].Name.ToLower, LNewFDParam);
  //    end;
  //    // 清除自身的
  //    FParams.Clear;
  //    for i := 0 to LNewParams.Count - 1 do
  //    begin
  //      if lNewFDPramsDict.ContainsKey(LNewParams[i].Name.ToLower) then
  //        continue;
  //      LNewFDParam := TParam(FParams.Add);
  //      LNewFDParam.Name := LNewParams[i].Name;
  //      lNewFDPramsDict.Add(LNewParams[i].Name.ToLower, LNewFDParam);
  //      if lOldFDPramsDict.TryGetValue(LNewFDParam.Name.ToLower, LOldFDParam) then
  //      begin
  //        LNewFDParam.Value := LOldFDParam.Value;
  //        LNewFDParam.NumericScale := LOldFDParam.NumericScale;
  //        LNewFDParam.Precision := LOldFDParam.Precision;
  //        LNewFDParam.Size := LOldFDParam.Size;
  //        LNewFDParam.DataType := LOldFDParam.DataType;
  //        LNewFDParam.ParamType := LOldFDParam.ParamType;
  //      end;
  //    end;
  //  finally
  //    LNewParams.Free;
  //    lOldFDPramsDict.Clear;
  //    lOldFDPramsDict.Free;
  //    LOldFDParams.Clear;
  //    LOldFDParams.Free;
  //    lNewFDPramsDict.Clear;
  //    lNewFDPramsDict.Free;
  //  end;
  //end
  //else
  //  FParams.Clear;
end;

procedure TOneDataSet.SetMultiIndex(Value: integer);
var
  iMultiCount: integer;
begin
  if Self.FMultiData = nil then
  begin
    Self.FMultiIndex := 0;
    exit;
  end;
  iMultiCount := Self.FMultiData.Count;
  if Value > Self.FMultiData.Count - 1 then
  begin
    Value := Self.FMultiData.Count - 1;
  end;
  if Value < 0 then
    Value := 0;
  Self.FMultiIndex := Value;
  if iMultiCount > 0 then
  begin
    Self.CopyFromDataset(Self.FMultiData[Self.FMultiIndex]);
  end;
end;

function TOneDataSet.Open: boolean;
begin
  Result := Self.OpenData;
end;

function TOneDataSet.OpenData: boolean;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.SQL.Text = '' then
  begin
    Self.FDataInfo.FErrMsg := '数据集DataInfo.SQL=空';
    exit;
  end;
  Result := Self.FDataInfo.FConnection.OpenData(Self);
end;

procedure TOneDataSet.OpenDataAsync(QProce: evenAsynProcObj);
var
  lThread: TOneAsynThread;
begin
  lThread := TOneAsynThread.Create(True, QProce);
  lThread.Start;
end;

function TOneDataSet.CheckRepeat(QSQL: string; QParamValues: array of variant; QSourceValue: string): boolean;
var
  lData: TOneDataSet;
  i: integer;
begin
  Result := True;
  lData := TOneDataSet.Create(nil);
  try
    lData.DataInfo.FConnection := Self.DataInfo.FConnection;
    lData.DataInfo.ZTCode := Self.DataInfo.ZTCode;
    lData.SQL.Text := QSQL;
    for i := Low(QParamValues) to High(QParamValues) do
    begin
      lData.Params[i].Value := QParamValues[i];
    end;
    if not lData.Open then
    begin
      Self.DataInfo.ErrMsg := lData.DataInfo.ErrMsg;
      exit;
    end;
    if lData.RecordCount >= 2 then
    begin
      Self.DataInfo.ErrMsg := '数据重复';
      exit;
    end;
    if lData.RecordCount = 1 then
    begin
      if lData.FieldCount > 1 then
      begin
        Self.DataInfo.ErrMsg := '有且只能判断一个字段,请纠正判断语句,只能代一个字段';
        exit;
      end;
      if lData.Fields[0].AsString <> QSourceValue then
      begin
        Self.DataInfo.ErrMsg := '数据重复';
        exit;
      end;
    end;
    Result := False;
  finally
    lData.Free;
  end;
end;

function TOneDataSet.Save: boolean;
begin
  Result := Self.SaveData;
end;


function TOneDataSet.SaveData: boolean;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  Result := Self.FDataInfo.FConnection.SaveData(Self);
end;

procedure TOneDataSet.SaveDataAsync(QProce: evenAsynProcObj);
var
  lThread: TOneAsynThread;
begin
  lThread := TOneAsynThread.Create(True, QProce);
  lThread.Start;
end;

function TOneDataSet.ExecDML: boolean;
var
  lOldSaveMode: TDataSaveMode;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  lOldSaveMode := Self.FDataInfo.SaveMode;
  try
    Self.FDataInfo.SaveMode := TDataSaveMode.saveDML;
    Result := Self.FDataInfo.FConnection.SaveData(Self);
  finally
    Self.FDataInfo.SaveMode := lOldSaveMode;
  end;
end;


function TOneDataSet.ExecDMLs(QDMLDatas: array of TOneDataSet): boolean;
var
  i: integer;
begin
  for i := 0 to High(QDMLDatas) do
  begin
    QDMLDatas[i].DataInfo.SaveMode := TDataSaveMode.saveDML;
  end;
  Result := Self.SaveDatas(QDMLDatas);
end;

function TOneDataSet.ExecDMLSQL(QSQL: string; QParamValues: array of variant; QMustOneAffected: boolean = True): boolean;
var
  lData: TOneDataSet;
  i: integer;
begin
  Result := False;
  lData := TOneDataSet.Create(nil);
  try
    lData.DataInfo.FConnection := Self.DataInfo.FConnection;
    lData.DataInfo.ZTCode := Self.DataInfo.ZTCode;
    if QMustOneAffected then
      lData.DataInfo.AffectedMustCount := 1;
    lData.SQL.Text := QSQL;
    for i := Low(QParamValues) to High(QParamValues) do
    begin
      lData.Params[i].Value := QParamValues[i];
    end;
    if not lData.ExecDML then
    begin
      Self.DataInfo.ErrMsg := lData.DataInfo.ErrMsg;
      exit;
    end;
    Result := True;
  finally
    lData.Free;
  end;
end;

function TOneDataSet.OpenStored: boolean;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.FDataInfo.FStoredProcName = '' then
  begin
    Self.FDataInfo.FErrMsg := '未设置存储过程名称';
    exit;
  end;
  //Self.FDataInfo.IsReturnData := True;
  Result := Self.FDataInfo.FConnection.ExecStored(Self);
end;

function TOneDataSet.ExecStored: boolean;
begin

  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.FDataInfo.FStoredProcName = '' then
  begin
    Self.FDataInfo.FErrMsg := '未设置存储过程名称';
    exit;
  end;
  Self.FDataInfo.IsReturnData := False;
  Result := Self.FDataInfo.FConnection.ExecStored(Self);
end;

// 1.先获取一个账套连接,标记成事务账套
function TOneDataSet.LockTran(): boolean;
var
  lOneTran: TOneTran;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.DataInfo.TranID <> '' then
  begin
    Self.FDataInfo.FErrMsg :=
      '数据集有一个事务ID，请先把上个事务ID释放';
    exit;
  end;

  lOneTran := TOneTran.Create;
  try
    lOneTran.ZTCode := Self.DataInfo.ZTCode;
    lOneTran.TranID := Self.DataInfo.TranID;
    lOneTran.MaxSpan := Self.DataInfo.TranSpanSec;
    lOneTran.Msg := '';
    Result := Self.FDataInfo.FConnection.LockTran(lOneTran);
    if not Result then
    begin
      Self.FDataInfo.FErrMsg := lOneTran.Msg;
    end
    else
    begin
      if lOneTran.TranID = '' then
      begin
        Self.FDataInfo.FErrMsg :=
          '返回成功,但事务ID为空,请检查服务端是否正确返回tranID';
      end
      else
      begin
        Self.FDataInfo.FTranID := lOneTran.TranID;
      end;
    end;

  finally
    lOneTran.Free;
  end;
end;

// 2.用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
function TOneDataSet.UnLockTran(): boolean;
var
  lOneTran: TOneTran;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.DataInfo.TranID = '' then
  begin
    Self.FDataInfo.FErrMsg := '数据集无相关事务ID，请先锁定一个事务';
    exit;
  end;
  lOneTran := TOneTran.Create;
  try
    lOneTran.ZTCode := Self.DataInfo.ZTCode;
    lOneTran.TranID := Self.DataInfo.TranID;
    lOneTran.MaxSpan := Self.DataInfo.TranSpanSec;
    lOneTran.Msg := '';
    Result := Self.FDataInfo.FConnection.UnLockTran(lOneTran);
    if not Result then
    begin
      Self.FDataInfo.FErrMsg := lOneTran.Msg;
    end
    else
    begin
      // 清空事务ID
      Self.DataInfo.TranID := '';
    end;
  finally
    lOneTran.Free;
  end;
end;

// 3.开启账套连接事务
function TOneDataSet.StartTran(): boolean;
var
  lOneTran: TOneTran;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.DataInfo.TranID = '' then
  begin
    Self.FDataInfo.FErrMsg := '数据集无相关事务ID，请先锁定一个事务';
    exit;
  end;
  lOneTran := TOneTran.Create;
  try
    lOneTran.ZTCode := Self.DataInfo.ZTCode;
    lOneTran.TranID := Self.DataInfo.TranID;
    lOneTran.MaxSpan := Self.DataInfo.TranSpanSec;
    lOneTran.Msg := '';
    Result := Self.FDataInfo.FConnection.StartTran(lOneTran);
    if not Result then
    begin
      Self.FDataInfo.FErrMsg := lOneTran.Msg;
    end;
  finally
    lOneTran.Free;
  end;
end;

// 4.提交账套连接事务
function TOneDataSet.CommitTran(): boolean;
var
  lOneTran: TOneTran;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.DataInfo.TranID = '' then
  begin
    Self.FDataInfo.FErrMsg := '数据集无相关事务ID，请先锁定一个事务';
    exit;
  end;
  lOneTran := TOneTran.Create;
  try
    lOneTran.ZTCode := Self.DataInfo.ZTCode;
    lOneTran.TranID := Self.DataInfo.TranID;
    lOneTran.MaxSpan := Self.DataInfo.TranSpanSec;
    lOneTran.Msg := '';
    Result := Self.FDataInfo.FConnection.CommitTran(lOneTran);
    if not Result then
    begin
      Self.FDataInfo.FErrMsg := lOneTran.Msg;
    end;
  finally
    lOneTran.Free;
  end;
end;

// 5.回滚账套连接事务
function TOneDataSet.RollbackTran(): boolean;
var
  lOneTran: TOneTran;
begin
  Result := False;
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if Self.DataInfo.TranID = '' then
  begin
    Self.FDataInfo.FErrMsg := '数据集无相关事务ID，请先锁定一个事务';
    exit;
  end;
  lOneTran := TOneTran.Create;
  try
    lOneTran.ZTCode := Self.DataInfo.ZTCode;
    lOneTran.TranID := Self.DataInfo.TranID;
    lOneTran.MaxSpan := Self.DataInfo.TranSpanSec;
    lOneTran.Msg := '';
    Result := Self.FDataInfo.FConnection.RollbackTran(lOneTran);
    if not Result then
    begin
      Self.FDataInfo.FErrMsg := lOneTran.Msg;
    end;
  finally
    lOneTran.Free;
  end;
end;

procedure TOneDataSet.SaveDeltaToStream(AStream: TStream);
var
  APacketReaderReg: TOneBinaryDatapacketHandler;
  APacketWriter: TDataPacketReader;
  Fmt: TDataPacketFormat;
begin
  CheckBiDirectional;
  APacketWriter := TOneBinaryDatapacketHandler.Create(Self, AStream);
  try
    GetDatasetPacket(APacketWriter);
  finally
    APacketWriter.Free;
  end;
end;

function TOneDataSet.IsSameStruct(QDataset: TBufDataset): boolean;
var
  iA, iB: integer;
  LIsFind: boolean;
begin
  Result := False;
  if self.fields.Count = 0 then
  begin
    if self.active then
      self.Close;
    if QDataset.active then
    begin
      self.CopyFromDataset(QDataset, False);
      if self.active then
        self.Close;
    end;
    Result := True;
    exit;
  end;
  if self.fields.Count <> QDataset.Fields.Count then
    exit;
  for iA := 0 to self.fields.Count - 1 do
  begin
    LIsFind := False;
    for iB := 0 to QDataset.fields.Count - 1 do
    begin
      if self.fields[iA].FieldName.ToLower = QDataset.fields[iB].FieldName.ToLower then
      begin
        LIsFind := True;
        break;
      end;
    end;
    //没找到，说明结构不相同
    if not LIsFind then
      exit;
  end;
  Result := True;
end;

procedure TOneDataSet.CopyDataSetOnlyData(QDataset: TBufDataset);
var
  lList: TList<integer>;
  iA, iB, iFieldIndex: integer;
  LIsFind: boolean;
  LHaveField: boolean;
  lF1, lF2: TField;
  lTempStream: TMemoryStream;
begin
  if self.fields.Count = 0 then
  begin
    if self.active then
      self.Close;
    self.CopyFromDataset(QDataset, True);
    exit;
  end;

  if self.active then
    self.Close;
  self.CreateDataset;
  if QDataset.RecordCount = 0 then
    exit;
  //数据复制
  LHaveField := False;
  lList := TList<integer>.Create;
  lTempStream := TMemoryStream.Create;
  try
    for iA := 0 to self.fields.Count - 1 do
    begin
      LIsFind := False;
      for iB := 0 to QDataset.fields.Count - 1 do
      begin
        if self.fields[iA].FieldName.ToLower = QDataset.fields[iB].FieldName.ToLower then
        begin
          lList.add(iB);
          LIsFind := True;
          LHaveField := True;
          break;
        end;
      end;
      //没找到，说明结构不相同
      if not LIsFind then
      begin
        lList.add(-1);
      end;
    end;
    //一个字段多没相同的退出
    if not LHaveField then
      exit;

    self.DisableControls;
    QDataset.DisableControls;
    try
      QDataset.First;
      while not QDataset.EOF do
      begin
        self.append;
        for iA := 0 to self.fields.Count - 1 do
        begin
          iFieldIndex := lList[iA];
          if iFieldIndex = -1 then
            continue;
          lF1 := self.Fields[iA];
          lF2 := QDataset.Fields[iFieldIndex];
          //复制数据
          if not lF2.IsNull then
            case lF1.DataType of
              ftFixedChar,
              ftString: lF1.AsString := lF2.AsString;
              ftFixedWideChar,
              ftWideString: lF1.AsWideString := lF2.AsWideString;
              ftBoolean: lF1.AsBoolean := lF2.AsBoolean;
              ftFloat: lF1.AsFloat := lF2.AsFloat;
              ftAutoInc,
              ftSmallInt,
              ftInteger: lF1.AsInteger := lF2.AsInteger;
              ftLargeInt: lF1.AsLargeInt := lF2.AsLargeInt;
              ftDate: lF1.AsDateTime := lF2.AsDateTime;
              ftTime: lF1.AsDateTime := lF2.AsDateTime;
              ftTimestamp,
              ftDateTime: lF1.AsDateTime := lF2.AsDateTime;
              ftCurrency: lF1.AsCurrency := lF2.AsCurrency;
              ftBCD,
              ftFmtBCD: lF1.AsBCD := lF2.AsBCD;
              else
                if (lF1.DataType in ftBlobTypes) then
                begin
                  lTempStream.Clear;
                  TBlobField(lF2).SaveToStream(lTempStream);
                  lTempStream.Position := 0;
                  TBlobField(lF1).LoadFromStream(lTempStream);
                end
                else
                  lF1.AsString := lF2.AsString;
            end;
        end;
        self.Post;
        QDataset.Next;
      end;
      QDataset.MergeChangeLog;
    finally
      self.EnableControls;
      QDataset.EnableControls;
    end;
  finally
    lList.Clear;
    lList.Free;
    lTempStream.Clear;
    lTempStream.Free;
  end;
end;

//打开多个数据集功能
// 打开数据

function TOneDataSet.OpenDatas(QOpenDatas: TList<TOneDataSet>): boolean;
var
  QList: TList<TObject>;
  i: integer;
  lErrMsg: string;
begin
  Result := False;
  lErrMsg := '';
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;

  QList := TList<TObject>.Create;
  try
    for i := 0 to QOpenDatas.Count - 1 do
    begin
      QList.Add(QOpenDatas[i]);
    end;
    Result := Self.FDataInfo.FConnection.OpenDatas(QList, lErrMsg);
    if not Result then
    begin
      Self.DataInfo.FErrMsg := lErrMsg;
    end;

  finally
    QList.Clear;
    QList.Free;
  end;
end;

function TOneDataSet.OpenDatas(QOpenDatas: array of TOneDataSet): boolean;
var
  QList: TList<TObject>;
  i: integer;
  lErrMsg: string;
begin
  Result := False;
  lErrMsg := '';
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;

  QList := TList<TObject>.Create;
  try
    for i := Low(QOpenDatas) to High(QOpenDatas) do
    begin
      QList.Add(QOpenDatas[i]);
    end;
    Result := Self.FDataInfo.FConnection.OpenDatas(QList, lErrMsg);
    if not Result then
    begin
      Self.DataInfo.FErrMsg := lErrMsg;
    end;
  finally
    QList.Clear;
    QList.Free;
  end;
end;

// 保存数据

function TOneDataSet.SaveDatas(QObjectList: TList<TOneDataSet>): boolean;
var
  QList: TList<TObject>;
  i: integer;
  lErrMsg: string;
begin
  Result := False;
  lErrMsg := '';
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;

  QList := TList<TObject>.Create;
  try
    for i := 0 to QObjectList.Count - 1 do
    begin
      QList.Add(QObjectList[i]);
    end;
    Result := Self.FDataInfo.FConnection.SaveDatas(QList, lErrMsg);
    if not Result then
    begin
      Self.FDataInfo.ErrMsg := lErrMsg;
    end;
  finally
    QList.Clear;
    QList.Free;
  end;
end;

function TOneDataSet.SaveDatas(QObjectArray: array of TOneDataSet): boolean;
var
  QList: TList<TObject>;
  i: integer;
  lErrMsg: string;
begin
  Result := False;
  lErrMsg := '';
  if Self.FDataInfo.FConnection = nil then
    Self.FDataInfo.FConnection := OneClientConnect.Unit_Connection;
  if Self.FDataInfo.FConnection = nil then
  begin
    Self.FDataInfo.FErrMsg := '数据集Connection=nil';
    exit;
  end;

  QList := TList<TObject>.Create;
  try
    for i := Low(QObjectArray) to High(QObjectArray) do
    begin
      QList.Add(QObjectArray[i]);
    end;
    Result := Self.FDataInfo.FConnection.SaveDatas(QList, lErrMsg);
    if not Result then
    begin
      Self.FDataInfo.ErrMsg := lErrMsg;
    end;
  finally
    QList.Clear;
    QList.Free;
  end;
end;

function TOneDataSet.IsEdit(): boolean;
begin
  Result := (self.State in dsEditModes) or (self.ChangeCount > 0);
end;

// **********

constructor TOneDataInfo.Create(QDataSet: TOneDataSet);
begin
  inherited Create();
  // 设计时获取相关字段
  FIsDesignGetFields := False;
  // 所属数据集
  FOwnerDataSet := QDataSet;
  FOpenMode := TDataOpenMode.OpenData;
  // 保存数据集模式
  FSaveMode := TDataSaveMode.SaveData;
  FDataReturnMode := TDataReturnMode.dataStream;
  // 分页 每页大小 默认-1 不限制
  FPageSize := -1;
  // 分页 第几页
  FPageIndex := 0;
  // 分页 总共页数
  FPageCount := 0;
  // 分页 总共条数
  FPageTotal := 0;
  // 执行SQL语句，最多影响行数
  FAffectedMaxCount := 0;
  // 执行SQL语句，必需有且几条一定受影响,默认一条
  FAffectedMustCount := 1;
  // 服务端返回影响行数
  FRowsAffected := 0;
  // 是否异步
  FAsynMode := False;
  // 是否返回数据集
  FIsReturnData := False;
  FTranID := '';
  FTranSpanSec := 0;
end;

destructor TOneDataInfo.Destroy;
begin
  inherited Destroy;
end;

function TOneDataInfo.GetConnection: TOneConnection;
begin
  Result := Self.FConnection;
end;

{ ------------------------------------------------------------------------------- }
procedure TOneDataInfo.SetConnection(const AValue: TOneConnection);
begin
  Self.FConnection := AValue;
end;

procedure TOneDataInfo.SetGetFields(Value: boolean);
var
  lStream: TMemoryStream;
  lTemp: TBufDataset;
  i: integer;
  lFieldDef: TFieldDef;
  lField: TField;
  lListField: TList<TField>;
begin
  Self.FIsDesignGetFields := False;
end;

end.
