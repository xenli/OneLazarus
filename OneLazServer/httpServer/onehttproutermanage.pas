unit OneHttpRouterManage;

{$mode DELPHI}{$H+}

interface

uses
  StrUtils, Generics.Collections, Classes, SysUtils,
  Variants, OneHttpControllerRtti, SyncObjs, TypInfo, Rtti;

// 路由挂载的模式 unknow=未知,pool=线程池,single=单例模式,even=事件
type
  emRouterMode = (unknow, pool, single, even);

type
  TOneRouterUrlPath = class;
  TOneRouterItem = class;
  TOneRouterManage = class;
  // 创建控制层对象回调函数
  TEvenCreaNewController = function(QRouterItem: TOneRouterItem): TObject;

   TOneInterfacedObject = class(TInterfacedObject)

   end;

  TOneRouterUrlPath = class
  private
    // URL路径
    FRootName: string;
    // 方法名称
    FMethodName: string;
    // 挂载的路由
    FRouterItem: TOneRouterItem;
  public
    property RootName: string read FRootName;
    property MethodName: string read FMethodName;
    property RouterItem: TOneRouterItem read FRouterItem;
  end;

  TOneRouterItem = class(TObject)
  private
    // 多例模式下,锁
    FLockObj: TCriticalSection;
    FInterfaceTypeInfo: PTypeInfo;
    // 路由挂载的模式
    FRouterMode: emRouterMode;
    // URL路径
    FRootName: string;
    // 最大运行线程个数
    FPoolMaxCount: integer;
    // 正在运行的线程个数
    FWorkingCount: integer;
    // 挂载的控制层的类
    FPersistentClass: TClass;
    // 创建实例的方法注册
    FEvenCreateNew: TEvenCreaNewController;
    // 挂载的控制层的类的RTTI信息
    FControllerRtti: TOneControllerRtti;
    // 单例情况下，预先创建好,加快请求速度
    FSingleWorkObject: TOneInterfacedObject;
    // 也有可能是一个方法，后面在扩展
    //FEvenControllerProcedure: TEvenControllerProcedure;
  private
    // 池是否满了
    function LockPoolWorkCount(): boolean;
    // 池归还
    procedure UnLockPoolWorkCount();
    function GetClassName(): string;
  public
    constructor Create; overload;
    destructor Destroy; override;
  public
    // 锁定一个控制器出来干活
    function LockWorkItem(var QErrMsg: string): TObject;
    // 获取一个控制函数来干活
    //function LockWorkEven(var QErrMsg: string): TEvenControllerProcedure;
    // 称释放一个控制器
    procedure UnLockWorkItem(QObject: TObject);
    // 获取一个方法反射信息
    function GetRttiMethod(QMethodName: string): TOneMethodRtti;
  published
    property ControllerRtti: TOneControllerRtti read FControllerRtti;
    property RouterMode: emRouterMode read FRouterMode;
    property RootName: string read FRootName;
    property PoolMaxCount: integer read FPoolMaxCount;
    property WorkingCount: integer read FWorkingCount;
    property RootClassName: string read GetClassName;
  end;

  TOneRouterManage = class(TObject)
  private
    // 路由信息控制(路由,路由信息)
    FRouterItems: TDictionary<string, TOneRouterItem>;
    FRouterUrlPath: TDictionary<string, TOneRouterUrlPath>;
    FErrMsg: string;
  public
    constructor Create; overload;
    destructor Destroy; override;
  public
    // 跟据路由获取路由名称
    function GetRouterItem(QRootName: string; var QErrMsg: string): TOneRouterItem;
    function GetRouterItems(): TDictionary<string, TOneRouterItem>;
    // 跟据URL路径获取对应路由信息
    function GetRouterUrlPath(QUrlPath: string; var QErrMsg: string): TOneRouterUrlPath;
    // 增加池工作模式
    //QInterfaceTypeInfo接口typeInfo(接口),必传
    // QRootName URL路径, QClass控制器类型, QPoolCount池大小(<=0不控制), QEvenCreateNew 创建实例的回调函数
    // 如果QEvenCreateNew未传，那么采用  TPersistentClass创建实例
    procedure AddHTTPPoolWork(QRootName: string; QClass: TClass;
      QInterfaceTypeInfo: PTypeInfo; QPoolCount: integer;
      QEvenCreateNew: TEvenCreaNewController);
    // 增加单例模式
    //QInterfaceTypeInfo接口typeInfo(接口),必传
    // QRootName URL路径, QClass控制器类型, QWorkMaxCount最大同时调用人数(<=0不控制), QEvenCreateNew 创建实例的回调函数
    // 如果QEvenCreateNew未传，那么采用  TPersistentClass创建实例
    procedure AddHTTPSingleWork(QRootName: string; QClass: TClass;
      QInterfaceTypeInfo: PTypeInfo; QWorkMaxCount: integer;
      QEvenCreateNew: TEvenCreaNewController);



    function FormatRootName(QRootName: string): string;
  public
    property ErrMsg: string read FErrMsg;
  end;

var
  Init_RouterManage: TOneRouterManage = nil;

function GetInitRouterManage(): TOneRouterManage;

implementation


function GetInitRouterManage(): TOneRouterManage;
begin

  if Init_RouterManage = nil then
  begin
    Init_RouterManage := TOneRouterManage.Create;
  end;
  Result := Init_RouterManage;
end;

// 单个路由创建
constructor TOneRouterItem.Create;
begin
  inherited Create;
  self.FLockObj := TCriticalSection.Create;
  self.FRouterMode := emRouterMode.unknow;
  self.FPoolMaxCount := -1;
  self.FWorkingCount := 0;
  self.FPersistentClass := nil;
  self.FEvenCreateNew := nil;
  self.FControllerRtti := nil;
  self.FSingleWorkObject := nil;
  //self.FEvenControllerProcedure := nil;
end;

// 单个路由销毁
destructor TOneRouterItem.Destroy;
begin
  inherited Destroy;
  if FLockObj <> nil then
  begin
    FLockObj.Free;
  end;
  if FPersistentClass <> nil then
  begin
    FPersistentClass := nil;
  end;
  if FControllerRtti <> nil then
  begin
    FControllerRtti.Free;
    FControllerRtti := nil;
  end;
  if FSingleWorkObject <> nil then
  begin
    FSingleWorkObject._Release;
  end;
  self.FEvenCreateNew := nil;
  //self.FEvenControllerProcedure := nil;
end;

// 池模式锁定工作数量
function TOneRouterItem.LockPoolWorkCount(): boolean;
begin
  Result := False;
  FLockObj.Enter;
  try
    if self.FPoolMaxCount <= 0 then
    begin
      // 不控制池大小
      self.FWorkingCount := self.FWorkingCount + 1;
      Result := True;
      exit;
    end;
    if self.FWorkingCount >= self.FPoolMaxCount then
    begin
      Result := False;
      exit;
    end;
    self.FWorkingCount := self.FWorkingCount + 1;
    Result := True;
  finally
    FLockObj.Leave;
  end;
end;

// 池模式释放工作数量
procedure TOneRouterItem.UnLockPoolWorkCount();
begin
  FLockObj.Enter;
  try
    self.FWorkingCount := self.FWorkingCount - 1;
  finally
    FLockObj.Leave;
  end;
end;

function TOneRouterItem.GetClassName(): string;
begin
  if self.FPersistentClass <> nil then
  begin
    Result := self.FPersistentClass.QualifiedClassName;
  end
  else
  begin
    Result := '事件';
  end;
end;

// 锁定一个控制器出来干活
function TOneRouterItem.LockWorkItem(var QErrMsg: string): TObject;
var
  tempObj: TObject;
begin
  Result := nil;
  tempObj := nil;
  QErrMsg := '';
  case self.FRouterMode of
    emRouterMode.unknow:
    begin
      QErrMsg := 'URL路径->' + FRootName + ',对应的路由模式unknow未知';
      exit;
    end;
    emRouterMode.pool:
    begin
      // 池模式
      if not self.LockPoolWorkCount() then
      begin
        QErrMsg := 'URL路径->' + FRootName +
          ',对应的池已满载工作,请稍候在试!!!';
        exit;
      end;
      if (Assigned(self.FEvenCreateNew)) then
      begin
        // 跟据方法创建对象 ,最好跟据方法创建
        tempObj := self.FEvenCreateNew(self);
      end
      else
      begin
        // 跟据控制类创建对象
        if self.FPersistentClass = nil then
        begin
          QErrMsg := 'URL路径->' + FRootName +
            ',对应的路由模式控制器类型为nil无法创建实例!!!';
          exit;
        end;
        tempObj := self.FPersistentClass.Create();
      end;
      Result := tempObj;
    end;
    emRouterMode.single:
    begin
      if not self.LockPoolWorkCount() then
      begin
        QErrMsg := 'URL路径->' + FRootName +
          ',对应的工作数量已满载工作,请稍候在试!!!';
        exit;
      end;
      // 单例模式
      if self.FSingleWorkObject = nil then
      begin

        // lItem.FPersistentClass.ClassParent;
        // lItem.FPersistentClass.InheritsFrom(classaa)
        if (Assigned(self.FEvenCreateNew)) then
        begin
          // 跟据方法创建对象 ,最好跟据方法创建
          self.FSingleWorkObject := TOneInterfacedObject(self.FEvenCreateNew(self));
        end
        else
        begin
          // 跟据控制类创建对象
          if self.FPersistentClass = nil then
          begin
            QErrMsg := 'URL路径->' + FRootName +
              ',对应的路由模式控制器类型为nil无法创建实例!!!';
            exit;
          end;
          self.FSingleWorkObject := TOneInterfacedObject(self.FPersistentClass.Create());
        end;
        self.FSingleWorkObject._AddRef;
      end;
      Result := self.FSingleWorkObject;
      exit;
    end;
    emRouterMode.even:
    begin
      QErrMsg := 'URL路径->' + FRootName + ',请用获取路由方法调用事件';
      exit;
    end;
    else
    begin
      QErrMsg := 'URL路径->' + FRootName + ',对应的路由模式未设计';
      exit;
    end;
  end;
end;

// 称释放一个控制器
procedure TOneRouterItem.UnLockWorkItem(QObject: TObject);
begin
  // 路由模式判断
  case self.FRouterMode of
    emRouterMode.unknow:
    begin
      exit;
    end;
    emRouterMode.pool:
    begin
      // 池模式要进行类释放
      self.UnLockPoolWorkCount();
      // 接口自动释放
      //QObject.re;
      //QObject.Destroy;
      QObject := nil;
    end;
    emRouterMode.single:
    begin
      // 单例模式
      self.UnLockPoolWorkCount();
      exit;
    end;
    emRouterMode.even:
    begin
      self.UnLockPoolWorkCount();
      exit;
    end;
    else
    begin
      self.UnLockPoolWorkCount();
      exit;
    end;
  end;
end;

function TOneRouterItem.GetRttiMethod(QMethodName: string): TOneMethodRtti;
begin
  Result := nil;
  if self.FControllerRtti = nil then
  begin
    Result := nil;
    exit;
  end;
  Result := self.FControllerRtti.GetRttiMethod(QMethodName);
end;

// ***********************路由管理************************//
// 路由管理创建
constructor TOneRouterManage.Create;
begin
  inherited Create;
  FRouterUrlPath := TDictionary<string, TOneRouterUrlPath>.Create;
  FRouterItems := TDictionary<string, TOneRouterItem>.Create;
  FErrMsg := '';
end;

// 路由管理销毁
destructor TOneRouterManage.Destroy;
var
  lItemPath: TOneRouterUrlPath;
  lItem: TOneRouterItem;
begin

  for lItemPath in FRouterUrlPath.Values do
  begin
    lItemPath.FRouterItem := nil;
    lItemPath.Free;
  end;
  FRouterUrlPath.Clear;
  FRouterUrlPath.Free;

  for lItem in FRouterItems.Values do
  begin
    lItem.Free;
  end;
  FRouterItems.Clear;
  FRouterItems.Free;
  inherited Destroy;
end;

// 跟据路由获取路由名称
function TOneRouterManage.GetRouterItem(QRootName: string;
  var QErrMsg: string): TOneRouterItem;
var
  lItem: TOneRouterItem;
begin
  Result := nil;
  QErrMsg := '';
  if FRouterItems.TryGetValue(QRootName, lItem) then
  begin
    if (lItem = nil) then
    begin
      QErrMsg := 'URL路径->' + QRootName + ',对应的路由信息为nil';
      exit;
    end;
    Result := lItem;
  end
  else
  begin
    QErrMsg := '无效的URL路径->' + QRootName;
  end;
end;

function TOneRouterManage.GetRouterItems(): TDictionary<string, TOneRouterItem>;
begin
  Result := self.FRouterItems;
end;

function TOneRouterManage.GetRouterUrlPath(QUrlPath: string;
  var QErrMsg: string): TOneRouterUrlPath;
var
  lItem: TOneRouterUrlPath;
begin
  Result := nil;
  QErrMsg := '';
  if FRouterUrlPath.TryGetValue(QUrlPath, lItem) then
  begin
    if (lItem = nil) then
    begin
      QErrMsg := 'URL路径->' + QUrlPath + ',对应的路由信息为nil';
      exit;
    end;
    Result := lItem;
  end
  else
  begin
    QErrMsg := '无效的URL路径->' + QUrlPath;
  end;
end;

function TOneRouterManage.FormatRootName(QRootName: string): string;
var
  lRootName: string;
  iFirst: integer;
begin
  lRootName := QRootName.Trim();
  lRootName := lRootName.ToLower;
  if lRootName = '' then
  begin
    Result := '';
    exit;
  end;
  lRootName := lRootName.Replace('\', '/');
  // lRootName处理  /name1/name2 最终格式
  // 判断第一个是不是/不是话的加上
  iFirst := lRootName.IndexOf('/');
  if iFirst <> 0 then
  begin
    lRootName := '/' + lRootName;
  end;
  // 判断最后一个是不是/是的话去除
  if RightStr(lRootName, 1) = '/' then
  begin
    lRootName := Copy(lRootName, 0, lRootName.Length - 1);
  end;
  Result := lRootName;
end;

// 增加池工作模式
// QRootName URL路径, QClass控制器类型, QPoolCount池大小(<=0不控制), QEvenCreateNew 创建实例的回调函数
// 如果QEvenCreateNew未传，那么采用  TPersistentClass创建实例
procedure TOneRouterManage.AddHTTPPoolWork(QRootName: string;
  QClass: TClass; QInterfaceTypeInfo: PTypeInfo; QPoolCount: integer;
  QEvenCreateNew: TEvenCreaNewController);
var
  lItem: TOneRouterItem;
  lRootName: string;
  lObj: TObject;
  lMethodName: string;
  lPathMethod: string;
  lRouterUrlPath: TOneRouterUrlPath;
begin
  lRootName := self.FormatRootName(QRootName);
  if lRootName = '' then
    exit;
  if not FRouterItems.TryGetValue(lRootName, lItem) then
  begin
    // 创建路由信息
    lItem := TOneRouterItem.Create;
    FRouterItems.Add(lRootName, lItem);
    lItem.FRootName := lRootName;
    lItem.FPoolMaxCount := QPoolCount;
    lItem.FEvenCreateNew := QEvenCreateNew;
    lItem.FPersistentClass := QClass.ClassType;
    lItem.FRouterMode := emRouterMode.pool;
    if QClass <> nil then
    begin
      // 挂载RTTI信息
      lItem.FControllerRtti := TOneControllerRtti.Create(QInterfaceTypeInfo);
      for lMethodName in lItem.FControllerRtti.MethodList.Keys do
      begin
        lPathMethod := lRootName + '/' + lMethodName;
        if not self.FRouterUrlPath.ContainsKey(lPathMethod) then
        begin
          lRouterUrlPath := TOneRouterUrlPath.Create;
          lRouterUrlPath.FRootName := lRootName;
          lRouterUrlPath.FMethodName := lMethodName;
          lRouterUrlPath.FRouterItem := lItem;
          self.FRouterUrlPath.Add(lPathMethod, lRouterUrlPath);
        end;
      end;
    end;
  end
  else
  begin
    FErrMsg := FErrMsg + '路径[' + lRootName +
      ']已存在,请检查是否重复,当前注册类[' +
      QClass.QualifiedClassName + '],' + '已注册类[' +
      lItem.FPersistentClass.QualifiedClassName + ']' + #13#10;
  end;
end;

// 增加单例模式
// QRootName URL路径, QClass控制器类型, QWorkMaxCount最大同时调用人数(<=0不控制), QEvenCreateNew 创建实例的回调函数
// 如果QEvenCreateNew未传，那么采用  TPersistentClass创建实例
procedure TOneRouterManage.AddHTTPSingleWork(QRootName: string;
  QClass: TClass; QInterfaceTypeInfo: PTypeInfo; QWorkMaxCount: integer;
  QEvenCreateNew: TEvenCreaNewController);
var
  lItem: TOneRouterItem;
  lRootName: string;
  lObj: TObject;
  lMethodName: string;
  lPathMethod: string;
  lRouterUrlPath: TOneRouterUrlPath;
begin
  lRootName := self.FormatRootName(QRootName);
  if lRootName = '' then
    exit;
  if not FRouterItems.TryGetValue(lRootName, lItem) then
  begin
    // 创建路由信息
    lItem := TOneRouterItem.Create;
    FRouterItems.Add(lRootName, lItem);
    lItem.FRootName := lRootName;
    lItem.FPoolMaxCount := QWorkMaxCount;
    lItem.FEvenCreateNew := QEvenCreateNew;
    lItem.FPersistentClass := QClass.ClassType;
    lItem.FRouterMode := emRouterMode.single;
    if QClass <> nil then
    begin
      // 挂载RTTI信息
      lItem.FControllerRtti := TOneControllerRtti.Create(QInterfaceTypeInfo);
      for lMethodName in lItem.FControllerRtti.MethodList.Keys do
      begin
        lPathMethod := lRootName + '/' + lMethodName;
        if not self.FRouterUrlPath.ContainsKey(lPathMethod) then
        begin
          lRouterUrlPath := TOneRouterUrlPath.Create;
          lRouterUrlPath.FRootName := lRootName;
          lRouterUrlPath.FMethodName := lMethodName;
          lRouterUrlPath.FRouterItem := lItem;
          self.FRouterUrlPath.Add(lPathMethod, lRouterUrlPath);
        end;
      end;
    end;
  end
  else
  begin
    FErrMsg := FErrMsg + '路径[' + lRootName +
      ']已存在,请检查是否重复,当前注册类[' +
      QClass.QualifiedClassName + ']' + '已注册类[' +
      lItem.FPersistentClass.QualifiedClassName + ']' + #13#10;
  end;
end;

initialization

finalization

  if Init_RouterManage <> nil then
  begin
    Init_RouterManage.Free;
    Init_RouterManage := nil;
  end;

end.
