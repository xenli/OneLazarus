unit DemoZTController;

{$mode DELPHI}{$H+}
interface

uses OneHttpController, OneHttpCtxtResult, OneHttpRouterManage, SysUtils,
  Generics.Collections, Contnrs, Classes, DB, BufDataset, Rtti,
  OneDataJson, OneControllerResult;

type

  {$M+}
  IDemoZTController = interface
    ['{0EB6D0A5-59CA-478F-A7D7-54B80C509A0B}']
    function GetData(): TActionResult<TBufDataSet>;
    function SaveData(QPersons: TList<TPersonDemo>): TActionResultString;
  end;

  {$M-}

  TDemoZTController = class(TOneControllerBase, IDemoZTController)
  public
    // override标识很重要
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
      override;
    // 返回结果 [{"name":"flm0"},{"name":"flm1"},{"name":"flm2"}]
    function GetData(): TActionResult<TBufDataSet>;
    function SaveData(QPersons: TList<TPersonDemo>): TActionResultString;
  end;

function CreateNewDemoZTController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal, OneZTManage;

function CreateNewDemoZTController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TDemoZTController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TDemoZTController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TDemoZTController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lDemoController: IDemoZTController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lDemoController := self as IDemoZTController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lDemoController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'GetData' then
  begin
    Result := self.GetData();
  end
  else
  if lMethodName = 'SaveData' then
  begin
    Result := self.SaveData(TList<TPersonDemo>(aArgs[0].AsObject));
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;
end;

function TDemoZTController.GetData(): TActionResult<TBufDataSet>;
var
  i: integer;
  lOneZTMange: TOneZTManage;
  lZTItem: TOneZTItem;
  lErrMsg: string;
  lConnection: TFDConnection;
  lFDQuery: TFDQuery;
  LDataSet: TBufDataSet;
begin
  // true:TFDMemtable需要释放,true:无item LIST 这种的，无效
  Result := TActionResult<TBufDataSet>.Create(True, True);
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  // 账套为空时,默认取主账套
  lZTItem := lOneZTMange.LockZTItem('', lErrMsg);
  if lZTItem = nil then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  try
    // 从账套直接获取现成的连接
    lConnection := lZTItem.ADConnection;
    // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
    lFDQuery := lZTItem.ADQuery;
    lFDQuery.SQL.Text :=
      'select ''范联满'' as Name, 18 as age union all select ''范联满''  , 18 ';
    // 打开数据
    try
      lFDQuery.Open;
      LDataSet := TBufDataSet.Create(nil);
      LDataSet.CopyFromDataset(lFDQuery);
      Result.ResultData := LDataSet;
      Result.SetResultTrue();
    except
      on e: Exception do
      begin
        Result.ResultMsg := '打开数据发生异常:' + e.Message;
        exit;
      end;
    end;
  finally
    // 解锁,归还池
    lZTItem.UnLockWork;
  end;
end;

function TDemoZTController.SaveData(QPersons: TList<TPersonDemo>): TActionResultString;
var
  i: integer;
  lOneZTMange: TOneZTManage;
  lZTItem: TOneZTItem;
  lErrMsg: string;
  lConnection: TFDConnection;
  lTran: TFDTransaction;
  lFDQuery: TFDQuery;
  isCommit: boolean;
begin
  // true:TFDMemtable需要释放,true:无item LIST 这种的，无效
  Result := TActionResultString.Create();
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  // 账套为空时,默认取主账套
  lZTItem := lOneZTMange.LockZTItem('', lErrMsg);
  if lZTItem = nil then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  // 从账套直接获取现成的连接
  isCommit := False;
  lConnection := lZTItem.ADConnection;
  lTran := lZTItem.ADTransaction;
  lTran.TranStart;
  try
    // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
    lFDQuery := lZTItem.ADQuery;
    // 保存的表名,你要有这张表
    lFDQuery.TableName := 'person';
    // 主键直接name当主键,测试
    lFDQuery.KeyFields := 'name';
    lFDQuery.SQL.Text := 'select * from person where 1=2';
    try
      // 打开数据
      lFDQuery.Open;
      for i := 0 to QPersons.Count - 1 do
      begin
        lFDQuery.Append;
        lFDQuery.FieldByName('name').AsString := QPersons[i].Name;
        lFDQuery.FieldByName('age').AsInteger := QPersons[i].age;
        lFDQuery.Post;
      end;
      if lFDQuery.State in dsEditModes then
        lFDQuery.Post;
      lFDQuery.ApplyUpdates(0);
      Result.ResultMsg := '保存数据成功';
      Result.SetResultTrue();
      lTran.TranCommit;
      isCommit := True;
    except
      on e: Exception do
      begin
        Result.ResultMsg := '保存数据发生异常:' + e.Message;
        exit;
      end;
    end;
  finally
    // 解锁,归还池
    if not isCommit then
    begin
      lTran.TranRollback;
    end;
    lZTItem.UnLockWork;
  end;
end;

// 注册到路由
initialization

  // 注意，路由名称 不要一样，否则会判定已注册过，跳过
  // 多例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPPoolWork('DemoZT',
    TDemoZTController, TypeInfo(IDemoZTController), 0, CreateNewDemoZTController);

finalization

end.
