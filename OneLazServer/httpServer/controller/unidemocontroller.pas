unit UniDemoController;

{$mode DELPHI}{$H+}


interface

uses
  StrUtils, SysUtils, Math, Classes, BufDataset, fpjson, jsonparser, RTTI,
  OneHttpController, OneHttpRouterManage, OneHttpCtxtResult, OneTokenManage,
  OneHttpConst, Generics.Collections, OneControllerResult, DB, OneGuID, OneMultipart;

type
  TLoginInfo = class
  private
    FloginCode: string; // 登陆代码
    FloginPass: string; // 登陆密码
    FloginZTCode: string; // 指定登陆账套
    FtokenID: string; // 返回去的TokenID
    FprivateKey: string; // 返回去的私钥
    FUserName: string; // 返回去的用户名称
    // 如果有其它信息自已加，本示例只是个demo
  published
    // 前后端参数及返回结果大写小请保证一至 ,不要问我为什么JSON是区分大小写的,
    property loginCode: string read FloginCode write FloginCode;
    property loginPass: string read FloginPass write FloginPass;
    property loginZTCode: string read FloginZTCode write FloginZTCode;
    property tokenID: string read FtokenID write FtokenID;
    property privateKey: string read FprivateKey write FprivateKey;
    property userName: string read FUserName write FUserName;
  end;

  TGoodsDemo = class(TOneOrmRowState)
  private
    FGoodsID_: string;
    FGoodsCode_: string;
    FGoodsName_: string;
    FGoodsPrice_: double;
    FGoodsRemark_: string;
    FGoodsImgUrl_: string;
  published
    // 前后端参数及返回结果大写小请保证一至 ,不要问我为什么JSON是区分大小写的,
    // 如果懒得数据库字段转化,请保持数据库字段也是一至的
    property FGoodsID: string read FGoodsID_ write FGoodsID_;
    property FGoodsCode: string read FGoodsCode_ write FGoodsCode_;
    property FGoodsName: string read FGoodsName_ write FGoodsName_;
    property FGoodsPrice: double read FGoodsPrice_ write FGoodsPrice_;
    property FGoodsRemark: string read FGoodsRemark_ write FGoodsRemark_;
    property FGoodsImgUrl: string read FGoodsImgUrl_ write FGoodsImgUrl_;
  end;

  TGoodsDemoList = TList<TGoodsDemo>;
  {$M+}
  IUniDemoController = interface
    ['{4AF07B61-D5A4-42FD-975A-624760A5E698}']
    function Login(QLogin: TLoginInfo): TActionResult<TLoginInfo>;
    // 登出接口
    function LoginOut(QLogin: TLoginInfo): TActionResultString;
    // 返回一个商品类表
    function GetGoodDemoList(): TActionResult<TList<TGoodsDemo>>;
    // 返回部份商品数据
    // Json上传{"pageIndex":1,"pageSize":10}
    function GetGoodDemoListPage(pageIndex: integer;
      pageSize: integer): TActionResult<TList<TGoodsDemo>>;
    // 与数据库结合
    // 返回一个商品类表,跟据 goodInfo过滤相关数据,
    function GetGoodsList(pageIndex: integer; pageSize: integer;
      goodInfo: string): TActionResult<TBufDataSet>;
    // 返回一个商品类表,跟据传上来的Json数据过滤相关数据
    function GetGoodsListByJson(QJson: TJsonObject): TActionResult<TBufDataSet>;
    // 返回一个商品信息, 上传上来的参数 {"QGoodsID":"参数值"}
    function GetGoods(QGoodsID: string): TActionResult<TGoodsDemo>;

    function SaveGoods(QGoods: TGoodsDemo): TActionResult<TGoodsDemo>;

    // 文件上传
    function PostFile(QFormData: TOneMultipartDecode): TActionResultString;
  end;

  {$M-}

  TUniDemoController = class(TOneControllerBase, IUniDemoController)
  public
    // override标识很重要
    function DoInvoke(QRttiMethod: TRttiMethod;
      const aArgs: array of TValue): TValue; override;
    // 登陆接口
    function Login(QLogin: TLoginInfo): TActionResult<TLoginInfo>;
    // 登出接口
    function LoginOut(QLogin: TLoginInfo): TActionResultString;
    // 返回一个商品类表
    function GetGoodDemoList(): TActionResult<TList<TGoodsDemo>>;
    // 返回部份商品数据
    // Json上传{"pageIndex":1,"pageSize":10}
    function GetGoodDemoListPage(pageIndex: integer;
      pageSize: integer): TActionResult<TList<TGoodsDemo>>;
    // 与数据库结合
    // 返回一个商品类表,跟据 goodInfo过滤相关数据,
    function GetGoodsList(pageIndex: integer; pageSize: integer;
      goodInfo: string): TActionResult<TBufDataSet>;
    // 返回一个商品类表,跟据传上来的Json数据过滤相关数据
    function GetGoodsListByJson(QJson: TJsonObject): TActionResult<TBufDataSet>;
    // 返回一个商品信息, 上传上来的参数 {"QGoodsID":"参数值"}
    function GetGoods(QGoodsID: string): TActionResult<TGoodsDemo>;

    function SaveGoods(QGoods: TGoodsDemo): TActionResult<TGoodsDemo>;

    // 文件上传
    function PostFile(QFormData: TOneMultipartDecode): TActionResultString;
  end;

function CreateNewUniDemoController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal, OneZTManage;

function CreateNewUniDemoController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TUniDemoController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TUniDemoController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TUniDemoController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lDemoController: IUniDemoController;
  lMethodName: string;
begin
 {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //转化成接口
  lDemoController := self as IUniDemoController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lDemoController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  lMethodName := QRttiMethod.Name;
  if lMethodName = 'Login' then
  begin
    Result := self.Login(TLoginInfo(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'LoginOut' then
  begin
    Result := self.LoginOut(TLoginInfo(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'GetGoodDemoList' then
  begin
    Result := self.GetGoodDemoList();
  end
  else
  if lMethodName = 'GetGoodDemoListPage' then
  begin
    Result := self.GetGoodDemoListPage(aArgs[0].AsInteger, aArgs[1].AsInteger);
  end
  else
  if lMethodName = 'GetGoodsList' then
  begin
    Result := self.GetGoodsList(aArgs[0].AsInteger, aArgs[1].AsInteger,
      aArgs[2].AsString);
  end
  else
  if lMethodName = 'GetGoodsListByJson' then
  begin
    Result := self.GetGoodsListByJson(TJsonObject(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'GetGoods' then
  begin
    Result := self.GetGoods(aArgs[0].AsString);
  end
  else
  if lMethodName = 'SaveGoods' then
  begin
    Result := self.SaveGoods(TGoodsDemo(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'PostFile' then
  begin
    Result := self.PostFile(TOneMultipartDecode(aArgs[0].AsObject));
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;
end;
// 前后端参数及返回结果大写小请保证一至 ,不要问我为什么JSON是区分大小写的,
function TUniDemoController.Login(QLogin: TLoginInfo): TActionResult<TLoginInfo>;
var
  lOneZTMange: TOneZTManage;
  lOneTokenManage: TOneTokenManage;
  lZTItem: TOneZTItem;
  lFDQuery: TFDQuery;
  lOneTokenItem: TOneTokenItem;
  lErrMsg: string;
  LLoginInfo: TLoginInfo;
begin
  Result := TActionResult<TLoginInfo>.Create(True, False);
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
  // 验证账号密码,比如数据库
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  // 账套为空时,默认取主账套,多账套的话可以固定一个账套代码
  lZTItem := lOneZTMange.LockZTItem(QLogin.loginZTCode, lErrMsg);
  if lZTItem = nil then
  begin
    Result.resultMsg := lErrMsg;
    exit;
  end;
  try
    // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
    lFDQuery := lZTItem.ADQuery;
    // 这边改成你的用户表
    lFDQuery.SQL.Text :=
      'select FUserID,FUserCode,FUserName,FUserPass from demo_user where FUserCode=:FUserCode';
    lFDQuery.Params[0].AsString := QLogin.loginCode;
    lFDQuery.Open;
    if lFDQuery.RecordCount = 0 then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode + ']不存在,请检查';
      exit;
    end;
    if lFDQuery.RecordCount > 1 then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode +
        ']重复,请联系管理员检查数据';
      exit;
    end;
    // 为一条时要验证密码,前端一般是MD5加密的,后端也是保存MD5加密的
    if QLogin.loginPass.ToLower <> lFDQuery.FieldByName(
      'FUserPass').AsString.ToLower then
    begin
      Result.resultMsg := '当前用户[' + QLogin.loginCode +
        ']密码不正确,请检查';
      exit;
    end;
    // 正确增加Token返回相关的toeknID及私钥
    lOneTokenManage := TOneGlobal.GetInstance().TokenManage;
    // true允许同个账号共用token,测试接口共享下防止踢来踢去
    lOneTokenItem := lOneTokenManage.AddLoginToken('uniapp', QLogin.loginCode,
      True, lErrMsg);
    if lOneTokenItem = nil then
    begin
      Result.resultMsg := lErrMsg;
      exit;
    end;
    // 为Token设置相关信息
    lOneTokenItem.LoginUserCode := QLogin.loginCode;
    lOneTokenItem.ZTCode := QLogin.loginZTCode; // 指定账套
    lOneTokenItem.SysUserID := lFDQuery.FieldByName('FUserID').AsString;
    lOneTokenItem.SysUserName := lFDQuery.FieldByName('FUserName').AsString;
    lOneTokenItem.SysUserCode := lFDQuery.FieldByName('FUserCode').AsString;
    // 返回信息设置
    LLoginInfo := TLoginInfo.Create;
    LLoginInfo.loginCode := QLogin.loginCode;
    LLoginInfo.tokenID := lOneTokenItem.tokenID;
    LLoginInfo.privateKey := lOneTokenItem.privateKey;
    LLoginInfo.userName := lFDQuery.FieldByName('FUserName').AsString;
    Result.resultData := LLoginInfo;
    Result.SetResultTrue;
  finally
    // 解锁,归还池很重要
    lZTItem.UnLockWork;
  end;
end;

function TUniDemoController.LoginOut(QLogin: TLoginInfo): TActionResultString;
var
  lOneGlobal: TOneGlobal;
begin
  Result := TActionResultString.Create();
  if QLogin.tokenID = '' then
  begin
    Result.resultMsg := 'tokenID为空请上传tokenID';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.TokenManage.RemoveToken(QLogin.tokenID);
  Result.resultData := 'Token删除成功';
  Result.SetResultTrue();
end;

// 返回一个商品类表
function TUniDemoController.GetGoodDemoList(): TActionResult<TGoodsDemoList>;
var
  lGoodDemo: TGoodsDemo;
  lList: TList<TGoodsDemo>;
  i: integer;
begin
  // TList<TGoodsDemo>即List里面的TGoodDemo用完多要设定成释放
  Result := TActionResult<TGoodsDemoList>.Create(True, True);
  lList := TList<TGoodsDemo>.Create;
  Result.resultData := lList;
  for i := 1 to 50 do
  begin
    lGoodDemo := TGoodsDemo.Create;
    lList.Add(lGoodDemo);
    lGoodDemo.FGoodsCode := 'code' + i.ToString;
    lGoodDemo.FGoodsName := 'name' + i.ToString;
    lGoodDemo.FGoodsPrice := i * 10;
    lGoodDemo.FGoodsRemark := '商品测试';
  end;
  Result.SetResultTrue();
end;

function TUniDemoController.GetGoodDemoListPage(pageIndex: integer;
  pageSize: integer): TActionResult<TList<TGoodsDemo>>;
var
  lGoodDemo: TGoodsDemo;
  lList: TList<TGoodsDemo>;
  i, iPageTotal: integer;
begin
  // TList<TGoodsDemo>即List里面的TGoodDemo用完多要设定成释放
  Result := TActionResult<TGoodsDemoList>.Create(True, True);
  // 假设总共50条
  if pageSize <= 0 then
    pageSize := 10;
  if pageSize >= 50 then
    pageSize := 50;
  if pageIndex <= 0 then
    pageIndex := 1;
  iPageTotal := ceil(50 / pageSize);
  if pageIndex > iPageTotal then
  begin
    Result.resultMsg := '最大页数[' + iPageTotal.ToString + ']数据已到底了';
    exit;
  end;
  lList := TList<TGoodsDemo>.Create;
  Result.resultData := lList;
  for i := (pageIndex - 1) * pageSize to pageIndex * pageSize do
  begin
    lGoodDemo := TGoodsDemo.Create;
    lList.Add(lGoodDemo);
    lGoodDemo.FGoodsCode := 'code' + i.ToString;
    lGoodDemo.FGoodsName := 'name' + i.ToString;
    lGoodDemo.FGoodsPrice := i * 10;
    lGoodDemo.FGoodsRemark := '商品测试';
  end;
  Result.SetResultTrue();
end;

// 从数据库中去获取相关商品资料信息 ,当然也可以直接返回TFDMemDataSet,只要前后端字段一至就行
function TUniDemoController.GetGoodsList(pageIndex: integer;
  pageSize: integer; goodInfo: string): TActionResult<TBufDataSet>;
var
  lZTItem: TOneZTItem;
  lFDQuery: TFDQuery;
  lOneTokenItem: TOneTokenItem;
  lOneZTMange: TOneZTManage;
  lOneTokenManage: TOneTokenManage;
  lGoodDemo: TGoodsDemo;
  lErrMsg: string;
  lBufDataSet: TBufDataSet;
  Lsql: string;
  LSQLInfo: TSQLInfo;
begin
  Result := TActionResult<TBufDataSet>.Create(True, False);
  // 获取用户Token信息,跟据当前线程ID,无需通过参数
  lOneTokenItem := self.GetCureentToken(lErrMsg);
  if lOneTokenItem = nil then
  begin
    Result.SetTokenFail();
    exit;
  end;
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  lZTItem := lOneZTMange.LockZTItem(lOneTokenItem.ZTCode, lErrMsg);
  if lZTItem = nil then
  begin
    Result.resultMsg := lErrMsg;
    exit;
  end;

  try
    try
      // 理论查询用orm最好的
      // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
      lFDQuery := lZTItem.ADQuery;
      // 查询所有
      if goodInfo = '' then
      begin
        Lsql :=
          'select  FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl  from demo_goods where 1=1 '
          + #13#10 + ' order by FGoodsCode ';
        //独立的一行Order by 设置分页时分析用到
      end
      else
      begin
        // 相拟查询
        Lsql :=
          'select  FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl  from demo_goods '
          + ' where FGoodsCode like :goodInfo or FGoodsName like :goodInfo ' +
          #13#10 + ' order by FGoodsCode ';
        //独立的一行Order by 设置分页时分析用到
      end;
      // 分页设置,当然你也可以自已在SQL 上面直接写分页语句
      InitSQLInfo(LSQLInfo);
      LSQLInfo.FDriver := lZTItem.ZTSet.PhyDriver;
      LSQLInfo.FDriverVersion := lZTItem.ZTSet.DBVersion;
      LSQLInfo.FPageIndex := pageIndex;
      LSQLInfo.FPageSize := pageSize;
      LSQLInfo.FSQL := Lsql;
      if not SetSQLInfo(LSQLInfo) then
      begin
        Result.resultMsg := LSQLInfo.FErrMsg;
        exit;
      end;
      //赋值SQL及参数
      lFDQuery.SQL.Text := Lsql;
      if goodInfo <> '' then
        lFDQuery.Params[0].AsString := '%' + goodInfo + '%';

      lFDQuery.Open;
      lBufDataSet := TBufDataSet.Create(nil);
      Result.resultData := lBufDataSet;
      // lFDQuery是池中的数据集，不能放出去用。要COPY下数据
      lBufDataSet.CopyFromDataset(lFDQuery);
      Result.SetResultTrue;
    except
      on e: Exception do
      begin
        Result.resultMsg := '发生异常,原因:' + e.Message;
      end;
    end;
  finally
    // 解锁,归还池很重要
    lZTItem.UnLockWork;
  end;
end;

// 返回一个商品类表,跟据传上来的Json数据过滤相关数据
function TUniDemoController.GetGoodsListByJson(QJson: TJsonObject):
TActionResult<TBufDataSet>;
var
  lZTItem: TOneZTItem;
  lFDQuery: TFDQuery;
  lOneTokenItem: TOneTokenItem;
  lOneZTMange: TOneZTManage;
  lOneTokenManage: TOneTokenManage;
  lGoodDemo: TGoodsDemo;
  lErrMsg: string;
  pageSize, pageIndex: integer;
  goodInfo: string;
  lBufDataSet: TBufDataSet;
  Lsql: string;
  LSQLInfo: TSQLInfo;
begin
  Result := TActionResult<TBufDataSet>.Create(True, False);
  // 获取用户Token信息,跟据当前线程ID,无需通过参数
  lOneTokenItem := self.GetCureentToken(lErrMsg);
  if lOneTokenItem = nil then
  begin
    Result.SetTokenFail();
    exit;
  end;
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  lZTItem := lOneZTMange.LockZTItem(lOneTokenItem.ZTCode, lErrMsg);
  if lZTItem = nil then
  begin
    Result.resultMsg := lErrMsg;
    exit;
  end;
  pageSize := QJson.Get('pageSize', 0);
  pageIndex := QJson.Get('pageIndex', 0);
  goodInfo := QJson.Get('goodInfo', '');
  try
    try
      // 理论查询用orm最好的
      // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
      lFDQuery := lZTItem.ADQuery;
      // 查询所有
      if goodInfo = '' then
      begin
        Lsql :=
          'select  FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl  from demo_goods where 1=1 '
          + #13#10 + ' order by FGoodsCode ';
        //独立的一行Order by 设置分页时分析用到
      end
      else
      begin
        // 相拟查询
        Lsql :=
          'select  FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl  from demo_goods '
          + ' where FGoodsCode like :goodInfo or FGoodsName like :goodInfo ' +
          #13#10 + ' order by FGoodsCode ';
        //独立的一行Order by 设置分页时分析用到
      end;
      // 分页设置,当然你也可以自已在SQL 上面直接写分页语句
      InitSQLInfo(LSQLInfo);
      LSQLInfo.FDriver := lZTItem.ZTSet.PhyDriver;
      LSQLInfo.FDriverVersion := lZTItem.ZTSet.DBVersion;
      LSQLInfo.FPageIndex := pageIndex;
      LSQLInfo.FPageSize := pageSize;
      LSQLInfo.FSQL := Lsql;
      if not SetSQLInfo(LSQLInfo) then
      begin
        Result.resultMsg := LSQLInfo.FErrMsg;
        exit;
      end;
      //赋值SQL及参数
      lFDQuery.SQL.Text := Lsql;
      if goodInfo <> '' then
        lFDQuery.Params[0].AsString := '%' + goodInfo + '%';

      lFDQuery.Open;
      lBufDataSet := TBufDataSet.Create(nil);
      Result.resultData := lBufDataSet;
      // lFDQuery是池中的数据集，不能放出去用。要COPY下数据
      lBufDataSet.CopyFromDataset(lFDQuery);
      Result.SetResultTrue;
    except
      on e: Exception do
      begin
        Result.resultMsg := '发生异常,原因:' + e.Message;
      end;
    end;
  finally
    // 解锁,归还池很重要
    lZTItem.UnLockWork;
  end;
end;

function TUniDemoController.GetGoods(QGoodsID: string): TActionResult<TGoodsDemo>;
var
  lZTItem: TOneZTItem;
  lFDQuery: TFDQuery;
  lOneTokenItem: TOneTokenItem;
  lOneZTMange: TOneZTManage;
  lOneTokenManage: TOneTokenManage;
  lGoodDemo: TGoodsDemo;
  lErrMsg: string;
begin
  Result := TActionResult<TGoodsDemo>.Create(True, False);
  if QGoodsID = '' then
  begin
    Result.resultMsg := '请上传参数{"QGoodsID":"值"}';
    exit;
  end;
  // 获取用户Token信息,跟据当前线程ID,无需通过参数
  lOneTokenItem := self.GetCureentToken(lErrMsg);
  if lOneTokenItem = nil then
  begin
    Result.SetTokenFail();
    exit;
  end;
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  lZTItem := lOneZTMange.LockZTItem(lOneTokenItem.ZTCode, lErrMsg);
  if lZTItem = nil then
  begin
    Result.resultMsg := lErrMsg;
    exit;
  end;

  try
    try
      // 理论查询用orm最好的
      // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
      lFDQuery := lZTItem.ADQuery;
      // 查询所有
      lFDQuery.SQL.Text :=
        'select FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl from demo_goods where FGoodsID=:FGoodsID';
      lFDQuery.Params[0].AsString := QGoodsID;
      lFDQuery.Open;
      if lFDQuery.RecordCount = 0 then
      begin
        Result.resultMsg := '不存在当前ID的商品数据';
        exit;
      end;
      lGoodDemo := TGoodsDemo.Create();
      Result.resultData := lGoodDemo;
      lGoodDemo.FGoodsID := lFDQuery.FieldByName('FGoodsID').AsString;
      lGoodDemo.FGoodsCode := lFDQuery.FieldByName('FGoodsCode').AsString;
      lGoodDemo.FGoodsName := lFDQuery.FieldByName('FGoodsName').AsString;
      lGoodDemo.FGoodsPrice := lFDQuery.FieldByName('FGoodsPrice').AsFloat;
      lGoodDemo.FGoodsRemark := lFDQuery.FieldByName('FGoodsRemark').AsString;
      lGoodDemo.FGoodsImgUrl := lFDQuery.FieldByName('FGoodsImgUrl').AsString;

      Result.SetResultTrue;
    except
      on e: Exception do
      begin
        Result.resultMsg := '发生异常,原因:' + e.Message;
      end;
    end;
  finally
    // 解锁,归还池很重要
    lZTItem.UnLockWork;
  end;
end;

function TUniDemoController.SaveGoods(QGoods: TGoodsDemo): TActionResult<TGoodsDemo>;
var
  lZTItem: TOneZTItem;
  lFDQuery: TFDQuery;
  lOneTokenItem: TOneTokenItem;
  lOneZTMange: TOneZTManage;
  lOneTokenManage: TOneTokenManage;
  lGoodDemo: TGoodsDemo;
  lErrMsg: string;
  pageSize, pageIndex: integer;
  goodInfo: string;
  isCommit: boolean;
  iCommit, iErr: integer;
begin
  // 多不释放,result.data := QGood,由参数自已释放
  Result := TActionResult<TGoodsDemo>.Create(False, False);
  // 检验数据

  if QGoods.FGoodsCode = '' then
  begin
    Result.resultMsg := '商品代码不可为空';
    exit;
  end;

  if QGoods.FGoodsName = '' then
  begin
    Result.resultMsg := '商品名称不可为空';
    exit;
  end;

  // 获取用户Token信息,跟据当前线程ID,无需通过参数
  lOneTokenItem := self.GetCureentToken(lErrMsg);
  if lOneTokenItem = nil then
  begin
    Result.SetTokenFail();
    exit;
  end;
  lOneZTMange := TOneGlobal.GetInstance().ZTManage;
  lZTItem := lOneZTMange.LockZTItem(lOneTokenItem.ZTCode, lErrMsg);
  if lZTItem = nil then
  begin
    Result.resultMsg := lErrMsg;
    exit;
  end;

  try
    isCommit := False;
    lZTItem.ADTransaction.TranStart;
    try
      // 主键有值说明是编辑,无值说明是新增
      QGoods.SetRowState(QGoods.FGoodsID);
      // 理论查询用orm最好的
      // 从账套获取现成的FDQuery,已绑定好 connetion,也无需释放
      lFDQuery := lZTItem.ADQuery;
      lFDQuery.TableName := 'demo_goods'; // 设置表名
      lFDQuery.KeyFields := 'FGoodsID'; // 设置主键

      lFDQuery.UpdateMode := TUpdateMode.upWhereKeyOnly;
      // 设置更新模式
      if QGoods.GetRowState = emRowstate.insertState then
      begin
        // 打开一个空数据集
        lFDQuery.SQL.Text :=
          'select  FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl  from demo_goods where 1=2';
      end
      else
      begin
        // 相拟查询
        lFDQuery.SQL.Text :=
          'select  FGoodsID,FGoodsCode,FGoodsName,FGoodsPrice,FGoodsRemark,FGoodsImgUrl  from demo_goods where FGoodsID=:FGoodsID';
        lFDQuery.Params[0].AsString := QGoods.FGoodsID;
      end;
      lFDQuery.Open;
      if QGoods.GetRowState = emRowstate.insertState then
      begin
        // 新增相关东东赋值下
        QGoods.FGoodsID := OneGuID.GetGUID32;
        lFDQuery.Append;
        lFDQuery.FieldByName('FGoodsID').AsString := QGoods.FGoodsID;
        lFDQuery.FieldByName('FGoodsCode').AsString := QGoods.FGoodsCode;
        lFDQuery.FieldByName('FGoodsName').AsString := QGoods.FGoodsName;
        lFDQuery.FieldByName('FGoodsPrice').AsFloat := QGoods.FGoodsPrice;
        lFDQuery.FieldByName('FGoodsRemark').AsString := QGoods.FGoodsRemark;
        lFDQuery.FieldByName('FGoodsImgUrl').AsString := QGoods.FGoodsImgUrl;
        lFDQuery.post;
      end
      else
      begin
        // 编辑相关东东
        if lFDQuery.RecordCount = 0 then
        begin
          Result.resultMsg := '数据不存在,请检查';
          exit;
        end;
        lFDQuery.edit;
        lFDQuery.FieldByName('FGoodsCode').AsString := QGoods.FGoodsCode;
        lFDQuery.FieldByName('FGoodsName').AsString := QGoods.FGoodsName;
        lFDQuery.FieldByName('FGoodsPrice').AsFloat := QGoods.FGoodsPrice;
        lFDQuery.FieldByName('FGoodsRemark').AsString := QGoods.FGoodsRemark;
        lFDQuery.FieldByName('FGoodsImgUrl').ProviderFlags := [];
        // 设定此字段不参与任何更新
        // lFDQuery.FieldByName('FGoodsImgUrl').
        lFDQuery.post;
      end;
      lFDQuery.ApplyUpdates(-1);
      iCommit := lFDQuery.RowsAffected;
      if iCommit <> 1 then
      begin
        Result.resultMsg := '更新数据有误,影响行数不为1,当前影响行数' +
          iCommit.ToString;
        exit;
      end;
      lZTItem.ADTransaction.TranCommit;
      isCommit := True;
      Result.resultData := QGoods;
      Result.SetResultTrue;
    except
      on e: Exception do
      begin
        Result.resultMsg := '发生异常,原因:' + e.Message;
      end;
    end;
  finally

    if not isCommit then
    begin
      lZTItem.ADTransaction.TranRollback;
    end;
    // 解锁,归还池很重要
    lZTItem.UnLockWork;
  end;
end;

function TUniDemoController.PostFile(QFormData: TOneMultipartDecode): TActionResultString;
var
  i: integer;
  lWebRequestFile: TOneRequestFile;
  tempStream: TCustomMemoryStream;
begin
  Result := TActionResultString.Create();
  // 接收到的文件
  for i := 0 to QFormData.Files.Count - 1 do
  begin
    lWebRequestFile := TOneRequestFile(QFormData.Files.items[i]);
    Result.resultData := Result.resultData + '当前接收到文件参数[' +
      lWebRequestFile.FieldName + ']' + '文件名称[' +
      lWebRequestFile.fileName + ']' + #10#13;
    // 文件流 ,至于要咱样是业务问题
    tempStream := TCustomMemoryStream(lWebRequestFile.Stream);
  end;
  // 接收到的参数,自已的业务自已分析
  for i := 0 to QFormData.ContentFields.Count - 1 do
  begin
    Result.resultData := Result.resultData + '当前接收到参数[' +
      QFormData.ContentFields[i] + ']' + #10#13;
  end;
  Result.SetResultTrue();
end;

initialization

  // 单例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork('UniDemo',
    TUniDemoController, TypeInfo(IUniDemoController), 0, CreateNewUniDemoController);

finalization

end.
