unit DataController;

{$mode DELPHI}{$H+}
// 此单元主要对接 oneClient的OneDataSet传统交互
interface

uses OneHttpController, OneHttpRouterManage, OneHttpCtxtResult, OneDataInfo,
  Generics.Collections, OneFileHelper, StrUtils, SysUtils,
  OneControllerResult, Rtti;

type
   {$M+}
  IOneDataController = interface
    ['{6E9FE43C-F01E-466E-9A93-F557BA3D4800}']
    { 只有这种类型的参数结果才注册到RTTI,防止所有的全注册 }
    { 打开几个数据 }
    function OpenDatas(QDataOpens: TList<TOneDataOpen>): TOneDataResult;
    // 执行存储过程
    function ExecStored(QDataOpen: TOneDataOpen): TOneDataResult;
    // 保存语句或执行DML语句
    function SaveDatas(QSaveDMLDatas: TList<TOneDataSaveDML>): TOneDataResult;
    // 下载文件
    function DownLoadDataFile(fileID: string): TActionResultString;
    // 删除文件
    procedure DelDataFile(fileID: string);
    // *********二层事务自由控制***********
    // 1.先获取一个账套连接,标记成事务账套
    function LockTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 2.用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
    function UnLockTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 3.开启账套连接事务
    function StartTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 4.提交账套连接事务
    function CommitTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 5.回滚账套连接事务
    function RollbackTranItem(QTranInfo: TOneTran): TOneDataResult;
  end;

  {$M-}

  TOneDataController = class(TOneControllerBase, IOneDataController)
  public
    //各个方法实现
    function DoInvoke(QRttiMethod: TRttiMethod;
      const aArgs: array of TValue): TValue; override;
    { 只有这种类型的参数结果才注册到RTTI,防止所有的全注册 }
    { 打开几个数据 }
    function OpenDatas(QDataOpens: TList<TOneDataOpen>): TOneDataResult;
    // 执行存储过程
    function ExecStored(QDataOpen: TOneDataOpen): TOneDataResult;
    // 保存语句或执行DML语句
    function SaveDatas(QSaveDMLDatas: TList<TOneDataSaveDML>): TOneDataResult;
    // 下载文件
    function DownLoadDataFile(fileID: string): TActionResultString;
    // 删除文件
    procedure DelDataFile(fileID: string);
    // *********二层事务自由控制***********
    // 1.先获取一个账套连接,标记成事务账套
    function LockTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 2.用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
    function UnLockTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 3.开启账套连接事务
    function StartTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 4.提交账套连接事务
    function CommitTranItem(QTranInfo: TOneTran): TOneDataResult;
    // 5.回滚账套连接事务
    function RollbackTranItem(QTranInfo: TOneTran): TOneDataResult;
  end;

function CreateNewOneDataController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal, OneSQLCrypto;

function CreateNewOneDataController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TOneDataController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TOneDataController.Create;
  // 开启验证Token
  lController.FAutoCheckToken := True;
  // 开启验证签名
  lController.FAutoCheckSign := True;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TOneDataController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lIController: IOneDataController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lIController := self as IOneDataController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lIController, aArgs);
  exit;
  {$endif}
  //这种情况下是支持反射方法的,其它没办法比如uos等系统,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'OpenDatas' then
  begin
    Result := self.OpenDatas(TList<TOneDataOpen>(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'ExecStored' then
  begin
    Result := self.ExecStored(TOneDataOpen(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'SaveDatas' then
  begin
    Result := self.SaveDatas(TList<TOneDataSaveDML>(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'DownLoadDataFile' then
  begin
    Result := self.DownLoadDataFile(aArgs[0].AsString);
  end
  else
  if lMethodName = 'DelDataFile' then
  begin
    self.DelDataFile(aArgs[0].AsString);
  end
  else
  if lMethodName = 'LockTranItem' then
  begin
    Result := self.LockTranItem(TOneTran(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'UnLockTranItem' then
  begin
    Result := self.UnLockTranItem(TOneTran(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'StartTranItem' then
  begin
    Result := self.StartTranItem(TOneTran(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'CommitTranItem' then
  begin
    Result := self.CommitTranItem(TOneTran(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'RollbackTranItem' then
  begin
    Result := self.RollbackTranItem(TOneTran(aArgs[0].AsObject));
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;
end;

function TOneDataController.OpenDatas(QDataOpens: TList<TOneDataOpen>): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  i: integer;
begin
  Result := TOneDataResult.Create;
  lOneGlobal := TOneGlobal.GetInstance();

  for i := 0 to QDataOpens.Count - 1 do
  begin
    // 客户端提交的 SQL还原
    QDataOpens[i].OpenSQL := OneSQLCrypto.SwapDecodeCrypto(QDataOpens[i].OpenSQL);
  end;
  // 打开数据
  if not lOneGlobal.ZTManage.OpenDatas(QDataOpens, Result) then
  begin
    exit;
  end;
  // 解析相关数据
  if Result.ResultOK then
  begin
    Result.DoResultitems();
  end;
end;

function TOneDataController.ExecStored(QDataOpen: TOneDataOpen): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  i: integer;
begin
  Result := TOneDataResult.Create;
  QDataOpen.OpenSQL := OneSQLCrypto.SwapDecodeCrypto(QDataOpen.OpenSQL);
  lOneGlobal := TOneGlobal.GetInstance();
  // 打开数据
  if not lOneGlobal.ZTManage.ExecStored(QDataOpen, Result) then
  begin
    exit;
  end;
  // 解析相关数据
  if Result.ResultOK then
  begin
    Result.DoResultitems();
  end;
end;

function TOneDataController.SaveDatas(QSaveDMLDatas: TList<TOneDataSaveDML>):
TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  i: integer;
begin
  Result := TOneDataResult.Create;
  lOneGlobal := TOneGlobal.GetInstance();
  for i := 0 to QSaveDMLDatas.Count - 1 do
  begin
    // 客户端提交的 SQL还原
    QSaveDMLDatas[i].SQL := OneSQLCrypto.SwapDecodeCrypto(QSaveDMLDatas[i].SQL);
  end;
  // 保存数据
  if not lOneGlobal.ZTManage.SaveDatas(QSaveDMLDatas, Result) then
  begin
    exit;
  end;
  // 解析相关数据
  if Result.ResultOK then
  begin
    Result.DoResultitems();
  end;
end;

// 下载文件
function TOneDataController.DownLoadDataFile(fileID: string): TActionResultString;
var
  lFileName: string;
begin
  Result := TActionResultString.Create;
  if fileID.Trim = '' then
  begin
    Result.ResultMsg := '文件ID为空';
    exit;
  end;
  lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneDataTemp\' +
    fileID + '.zip');
  if not fileExists(lFileName) then
  begin
    Result.ResultMsg := '文件已不存在';
    exit;
  end;
  Result.ResultData := lFileName;
  // 返回的是文件
  Result.SetResultTrueFile();
end;

procedure TOneDataController.DelDataFile(fileID: string);
var
  lFileName: string;
begin
  if fileID.Trim = '' then
  begin
    exit;
  end;
  lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneDataTemp\' +
    fileID + '.data');
  if fileExists(lFileName) then
  begin
    DeleteFile(lFileName);
    exit;
  end;
end;

// 1.先获取一个账套连接,标记成事务账套
function TOneDataController.LockTranItem(QTranInfo: TOneTran): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
  lTranID: string;
begin
  Result := TOneDataResult.Create;
  lOneGlobal := TOneGlobal.GetInstance();
  lTranID := '';
  if QTranInfo.MaxSpan <= 0 then
    QTranInfo.MaxSpan := -1;
  lTranID := lOneGlobal.ZTManage.LockTranItem(QTranInfo.ZTCode,
    QTranInfo.MaxSpan, lErrMsg);
  Result.ResultMsg := lErrMsg;
  if lTranID <> '' then
  begin
    Result.ResultData := lTranID;
    Result.ResultOK := True;
  end;
end;

// 2.用完了账套连接,归还账套,如果没归还，很久后，服务端会自动处理归还
function TOneDataController.UnLockTranItem(QTranInfo: TOneTran): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if QTranInfo.TranID = '' then
  begin
    Result.ResultMsg := '事务TrandID不可为空';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  if QTranInfo.MaxSpan <= 0 then
    QTranInfo.MaxSpan := -1;
  if not lOneGlobal.ZTManage.UnLockTranItem(QTranInfo.TranID, lErrMsg) then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  Result.ResultOK := True;
end;

// 3.开启账套连接事务
function TOneDataController.StartTranItem(QTranInfo: TOneTran): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if QTranInfo.TranID = '' then
  begin
    Result.ResultMsg := '事务TrandID不可为空';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  if QTranInfo.MaxSpan <= 0 then
    QTranInfo.MaxSpan := -1;
  if not lOneGlobal.ZTManage.StartTranItem(QTranInfo.TranID, lErrMsg) then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  Result.ResultOK := True;
end;

// 4.提交账套连接事务
function TOneDataController.CommitTranItem(QTranInfo: TOneTran): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if QTranInfo.TranID = '' then
  begin
    Result.ResultMsg := '事务TrandID不可为空';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  if QTranInfo.MaxSpan <= 0 then
    QTranInfo.MaxSpan := -1;
  if not lOneGlobal.ZTManage.CommitTranItem(QTranInfo.TranID, lErrMsg) then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  Result.ResultOK := True;
end;

// 5.回滚账套连接事务
function TOneDataController.RollbackTranItem(QTranInfo: TOneTran): TOneDataResult;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
begin
  Result := TOneDataResult.Create;
  if QTranInfo.TranID = '' then
  begin
    Result.ResultMsg := '事务TrandID不可为空';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  if QTranInfo.MaxSpan <= 0 then
    QTranInfo.MaxSpan := -1;
  if not lOneGlobal.ZTManage.RollbackTranItem(QTranInfo.TranID, lErrMsg) then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  Result.ResultOK := True;
end;

initialization

  // 单例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork('OneServer/Data',
    TOneDataController, typeInfo(IOneDataController), 0, CreateNewOneDataController);

finalization

end.
