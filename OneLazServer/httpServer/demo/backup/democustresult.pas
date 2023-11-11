unit DemoCustResult;

{$mode DELPHI}{$H+}

interface

uses
  OneHttpController, OneHttpRouterManage, SysUtils, Classes, Rtti,
  OneControllerResult, Generics.Collections, DB, OneHttpCtxtResult, bufDataSet;

type
  //定义下泛型类，fpc3.2对这些支持不在友好.fpc3.3就没这些问题了，到时官方升级，在跟着升级
  TListTStr = TList<string>;
  TListTPerSon = TList<TPerSonDemo>;
   {$M+}
  IDemoCustResultController = interface
    ['{69983F00-5D00-43B4-9553-FC0419D665A7}']
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: "范联满"}
    function GetResultStr(): TActionResultString;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: ["范联满","范联满","范联满"]}
    function GetResultList(): TActionResultT<TListTStr>;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: [{name: "范联满", age: 10},{name: "范联满", age: 10}]}
    function GetResultListPerson(): TActionResultT<TListTPerSon>;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: [{name: "范联满", age: 10},{name: "范联满", age: 10}]}
    function GetResultData(): TActionResultT<TDataSet>;
  end;

  {$M-}

  TDemoCustResultController = class(TOneControllerBase, IDemoCustResultController)
  public
    // override标识很重要
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
      override;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: "范联满"}
    function GetResultStr(): TActionResultString;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: ["范联满","范联满","范联满"]}
    function GetResultList(): TActionResultT<TListTStr>;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: [{name: "范联满", age: 10},{name: "范联满", age: 10}]}
    function GetResultListPerson(): TActionResultT<TListTPerSon>;
    // 返回结果格式 {ResultBool: false, ResultCode: "0002", ResultMsg: "", ResultData: [{name: "范联满", age: 10},{name: "范联满", age: 10}]}
    function GetResultData(): TActionResultT<TDataSet>;
  end;

function CreateNewDemoCustResultController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal;

function CreateNewDemoCustResultController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TDemoCustResultController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TDemoCustResultController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TDemoCustResultController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lDemoController: IDemoCustResultController;
  lMethodName: string;
begin
 {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //转化成接口
  lDemoController := self as IDemoCustResultController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lDemoController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  lMethodName := QRttiMethod.Name;
  if lMethodName = 'GetResultStr' then
  begin
    Result := self.GetResultStr();
  end
  else
  if lMethodName = 'GetResultList' then
  begin
    Result := self.GetResultList();
  end
  else
  if lMethodName = 'GetResultListPerson' then
  begin
    Result := self.GetResultListPerson();
  end
  else
  if lMethodName = 'GetResultData' then
  begin
    Result := self.GetResultData();
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;
end;

function TDemoCustResultController.GetResultStr(): TActionResultString;
var
  i: integer;
begin
  // 返回的不是对象，第一个参数false无需释放,第二个参数false也没有item无需释放
  Result := TActionResultString.Create();
  Result.ResultData := '我是大傻B';
  // SetResultTrue=>   ResultBool := true; ResultCode := HTTP_ResultCode_True;
  Result.SetResultTrue();
end;

function TDemoCustResultController.GetResultList(): TActionResultT<TListTStr>;
var
  i: integer;
  lList: TList<string>;
begin

  //// 返回的对象第一个参数true需释放,第二个参数item不是对象无需释放
  Result := TActionResultT<TListTStr>.Create(True, False);
  lList := TList<string>.Create;
  for i := 0 to 9 do
  begin
    lList.Add('flm' + i.ToString);
  end;
  Result.ResultData := lList;
  //// SetResultTrue=>   ResultBool := true; ResultCode := HTTP_ResultCode_True;
  Result.SetResultTrue();
end;

function TDemoCustResultController.GetResultListPerson(): TActionResultT<TListTPerSon>;
var
  i: integer;
  lList: TList<TPerSonDemo>;
  lPerson: TPerSonDemo;
begin
  // 返回的对象第一个参数true需释放,第二个参数item是对象需释放
  // 当然如果item是全局变量,也可以设成false不释放
  Result := TActionResultT<TListTPerSon>.Create(True, True);
  lList := TList<TPerSonDemo>.Create;
  for i := 0 to 9 do
  begin
    lPerson := TPerSonDemo.Create;
    lPerson.Name := 'flm' + i.ToString;
    lPerson.age := i;
    lList.Add(lPerson);
  end;
  Result.ResultData := lList;
  Result.SetResultTrue();
end;

function TDemoCustResultController.GetResultData(): TActionResultT<TDataSet>;
var
  i: integer;
  lFDMemTable: TBufDataSet;
begin
  Result := TActionResultT<TDataSet>.Create(True, False);
  lFDMemTable := TBufDataSet.Create(nil);
  lFDMemTable.FieldDefs.Add('name', ftString, 20, False);
  lFDMemTable.FieldDefs.Add('age', ftInteger, 0, True);
  lFDMemTable.CreateDataSet();
  for i := 0 to 9 do
  begin
    lFDMemTable.Append;
    lFDMemTable.FieldByName('name').AsString := 'flm' + i.ToString();
    lFDMemTable.FieldByName('age').AsInteger := i;
    lFDMemTable.Post;
  end;
  Result.ResultData := lFDMemTable;
end;

initialization

  // 单例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork('DemoCustResult',
    TDemoCustResultController, TypeInfo(IDemoCustResultController),
    0, CreateNewDemoCustResultController);

finalization

end.
