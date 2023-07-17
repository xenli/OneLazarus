unit OneTokenManage;

{$mode DELPHI}{$H+}

// token管理
// 客户端连上来后 生成一个GUID tokenID当标识身份,后续提交tokenID来确保用户身份
interface

uses
  Classes, StrUtils, SysUtils, Generics.Collections,
  DB, OneILog, DateUtils, OneThread, SyncObjs, memds, BufDataset;

type
  TOneTokenItem = class;
  TOneTokenManage = class;
  // 角色权限 noneRegister:无需注册就能访问最低级别
  // noneRole:任何人可以访问
  // userRole:用户可以访问
  // sysUserRole:系统用户
  // sysAdminRole:系统用户且为管理员
  // superRole:超级管理员
  // platformRole:平台管理员
  TOneTokenRole = (noneRegister, noneRole, userRole, sysUserRole, sysAdminRole, superRole, platformRole);

  // 如果此token没办法满足你，你自已继承 IOneTokenItem写个
  TOneTokenItem = class
  private
    // 连接ID ，长连接才有，否则这个就是假的
    FConnectionID: string;
    // 生成的TokenID唯一的
    FTokenID: string;
    // 每个token多分配一个私钥给它保证安全性
    FPrivateKey: string;
    // ***登陆信息********//
    // 登陆IP
    FLoginIP: string;
    FLoginMac: string;
    // 登陆时间
    FLoginTime: TDateTime;
    // 从哪个平台登陆 exe,web,微信，小程序
    FLoginPlatform: string;
    // 登陆代码
    FLoginUserCode: string;
    // 第三方登陆  appID
    FThirdAppID: string;
    // 第三方登陆  用户ID
    FThirdAppUserID: string;
    // 第三方登陆  用户关联ID
    FThirdAppUnionID: string;

    // 用户关联的用户代码ID
    FSysUserID: string;
    // 用户名称
    FSysUserName: string;
    FSysUserCode: string;
    // 用户类型
    FSysUserType: string;
    // 用户关联的表
    FSysUserTable: string;
    // 账套代码
    FZTCode: string;
    // 租户ID
    FPlatUserID: string;
    // ***登陆信息end********//
    // 最后交互时间
    FLastTime: TDateTime;

    FTokenRole: TOneTokenRole;
  private
    procedure SetSysUserType(Value: string);
  public
    destructor Destroy; override;
  public
    // 连接ID ，长连接才有，否则这个就是假的
    property ConnectionID: string read FConnectionID write FConnectionID;
    // 生成的TokenID唯一的
    property TokenID: string read FTokenID write FTokenID;
    // 每个token多分配一个私钥给它保证安全性
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    // ***登陆信息********//
    // 登陆IP
    property LoginIP: string read FLoginIP write FLoginIP;
    property LoginMac: string read FLoginMac write FLoginMac;
    // 登陆时间
    property LoginTime: TDateTime read FLoginTime write FLoginTime;
    // 从哪个平台登陆 exe,web,微信，小程序
    property LoginPlatform: string read FLoginPlatform write FLoginPlatform;
    // 登陆代码
    property LoginUserCode: string read FLoginUserCode write FLoginUserCode;
    // 第三方登陆  appID
    property ThirdAppID: string read FThirdAppID write FThirdAppID;
    // 第三方登陆  用户ID
    property ThirdAppUserID: string read FThirdAppUserID write FThirdAppUserID;
    // 第三方登陆  用户关联ID
    property ThirdAppUnionID: string read FThirdAppUnionID write FThirdAppUnionID;

    // 用户关联的用户代码ID
    property SysUserID: string read FSysUserID write FSysUserID;
    // 用户名称
    property SysUserName: string read FSysUserName write FSysUserName;
    property SysUserCode: string read FSysUserCode write FSysUserCode;
    // 用户类型
    property SysUserType: string read FSysUserType write SetSysUserType;
    // 用户关联的表
    property SysUserTable: string read FSysUserTable write FSysUserTable;
    // 账套代码
    property ZTCode: string read FZTCode write FZTCode;
    // 租户ID
    property PlatUserID: string read FPlatUserID write FPlatUserID;
    // ***登陆信息end********//
    // 最后交互时间
    property LastTime: TDateTime read FLastTime write FLastTime;

    property TokenRole: TOneTokenRole read FTokenRole write FTokenRole;
  end;

  TOneTokenManage = class
  private
    FLockObj: TCriticalSection;
    // token容器
    FTokenList: TDictionary<string, TOneTokenItem>;
    FOnLine: integer;
    // 多交没交互失效
    FTimeOutSec: integer;
    FLog: IOneLog;
    // 定时器,定时清理没用的Token
    FTimerThread: TOneTimerThread;
  private
    procedure onTimerWork(Sender: TObject);
  public
    constructor Create(QLog: IOneLog); overload;
    destructor Destroy; override;
  public
    // 获取一个token，交互用的,不包含任何用户信息
    function AddConnectToken(): TOneTokenItem;
    // 添加自已类型的Token
    function AddToken(QTokenItem: TOneTokenItem): boolean;
    // 获取token
    function GetToken(QTokenID: string): TOneTokenItem;
    // 增加一个token，QLoginPlatform 登陆平台，登陆用户, QIsMultiLogin:多点登陆
    function AddLoginToken(QLoginPlatform: string; QLoginCode: string; QIsMultiLogin: boolean; var QErrMsg: string): TOneTokenItem;
    // 移除一个token
    procedure RemoveToken(QTokenID: string);

    function CheckToken(QTokenID: string): boolean;
    function CheckSign(QTokenID: string; QUTCTime: string; QSign: string): boolean;

    procedure SaveToken();
    procedure LoadToken();
  public
    property TokenList: TDictionary<string, TOneTokenItem> read FTokenList;
    property OnLine: integer read FOnLine;
  end;

implementation

uses OneCrypto, OneGUID, OneFileHelper;

destructor TOneTokenItem.Destroy;
begin
  inherited Destroy;
end;

procedure TOneTokenItem.SetSysUserType(Value: string);
begin
  self.FSysUserType := Value;
  // 游客
  self.FTokenRole := TOneTokenRole.userRole;
  if self.FSysUserType = '超级管理员' then
    self.FTokenRole := TOneTokenRole.superRole
  else if self.FSysUserType = '管理员' then
    self.FTokenRole := TOneTokenRole.sysAdminRole
  else if self.FSysUserType = '操作员' then
    self.FTokenRole := TOneTokenRole.sysUserRole;
end;

// ******* TOneTokenManage **********//
constructor TOneTokenManage.Create(QLog: IOneLog);
begin
  inherited Create;
  FLog := QLog;
  FTimeOutSec := 60 * 30; // 默认30分钟失效无交互
  FLockObj := TCriticalSection.Create;
  FTokenList := TDictionary<string, TOneTokenItem>.Create;
  FTimerThread := TOneTimerThread.Create(self.onTimerWork);
  // 加载内存Token信息
  self.LoadToken;
  // 默认5分钟运作一次
  FTimerThread.IntervalSec := 60 * 5;
  FTimerThread.StartWork;

end;

destructor TOneTokenManage.Destroy;
var
  lTokenItem: TOneTokenItem;
begin
  if FTimerThread <> nil then
    FTimerThread.FreeWork;
  // for lTokenItem in FTokenList.Values do
  // begin
  // TInterfacedObject(lTokenItem).Free;
  // end;
  self.SaveToken;
  for lTokenItem in FTokenList.Values do
  begin
    lTokenItem.Free;
  end;
  FTokenList.Clear;
  FTokenList.Free;
  FLockObj.Free;
  inherited Destroy;
end;

procedure TOneTokenManage.onTimerWork(Sender: TObject);
var
  lTokenItem: TOneTokenItem;
  lNow: TDateTime;
  lSpanSec: integer;
begin
  lNow := Now;
  FLockObj.Enter;
  try
    for lTokenItem in FTokenList.Values do
    begin
      if self.FTimeOutSec > 0 then
      begin
        // 太久没交互,踢除
        if SecondsBetween(lNow, lTokenItem.LastTime) >= self.FTimeOutSec then
        begin
          FTokenList.Remove(lTokenItem.TokenID);
          lTokenItem.Free;
        end;
      end
      else
      begin
        // 无期限的, 6小时没交互也踢除
        if HoursBetween(lNow, lTokenItem.LastTime) >= 6 then
        begin
          FTokenList.Remove(lTokenItem.TokenID);
          lTokenItem.Free;
        end;
      end;
    end;
  finally
    FLockObj.Leave;
  end;
  // 保存Token
  self.SaveToken();
end;

function TOneTokenManage.AddConnectToken(): TOneTokenItem;
var
  lKey: string;
  lTokenItem: TOneTokenItem;
begin
  lKey := OneGUID.GetGUID32;
  FLockObj.Enter;
  try
    lTokenItem := TOneTokenItem.Create;
    lTokenItem.FTokenID := lKey;
    lTokenItem.FPrivateKey := OneGUID.GetGUID32;
    lTokenItem.FLoginIP := '';
    lTokenItem.FLoginTime := Now;
    lTokenItem.FLoginPlatform := '';
    lTokenItem.FLoginUserCode := '';
    lTokenItem.FSysUserID := '';
    lTokenItem.FSysUserName := '';
    lTokenItem.FSysUserType := '';
    lTokenItem.FSysUserTable := '';
    lTokenItem.FLastTime := Now;
    FTokenList.Add(lKey, lTokenItem);
    Result := lTokenItem;
  finally
    FLockObj.Leave;
  end;
end;

function TOneTokenManage.AddToken(QTokenItem: TOneTokenItem): boolean;
begin
  Result := False;
  FLockObj.Enter;
  try
    if self.FTokenList.ContainsKey(QTokenItem.TokenID) then
    begin
      exit;
    end;
    self.FTokenList.Add(QTokenItem.TokenID, QTokenItem);
    Result := True;
  finally
    FLockObj.Leave;
  end;
end;

function TOneTokenManage.GetToken(QTokenID: string): TOneTokenItem;
var
  lTokenItem: TOneTokenItem;
begin
  Result := nil;
  lTokenItem := nil;
  if self.FTokenList.TryGetValue(QTokenID, lTokenItem) then
  begin
    Result := lTokenItem;
  end;
end;

function TOneTokenManage.AddLoginToken(QLoginPlatform: string; QLoginCode: string; QIsMultiLogin: boolean; var QErrMsg: string): TOneTokenItem;
var
  lKey: string;
  lTokenItem: TOneTokenItem;
begin
  Result := nil;
  lTokenItem := nil;
  QErrMsg := '';
  lKey := QLoginPlatform + '_' + QLoginCode;
  lKey := OneCrypto.MD5Endcode(lKey);
  FLockObj.Enter;
  try
    if FTokenList.ContainsKey(lKey) then
    begin
      FTokenList.TryGetValue(lKey, lTokenItem);
      if QIsMultiLogin and (lTokenItem <> nil) then
      begin
        lTokenItem.LoginTime := Now;
        Result := lTokenItem;
        exit;
      end;
      if lTokenItem <> nil then
      begin
        lTokenItem.Free;
        lTokenItem := nil;
      end;
      FTokenList.Remove(lKey);
    end;
    lTokenItem := TOneTokenItem.Create;
    lTokenItem.TokenID := lKey;
    lTokenItem.PrivateKey := OneGUID.GetGUID32;
    lTokenItem.LoginIP := '';
    lTokenItem.LoginTime := Now;
    lTokenItem.LoginPlatform := QLoginPlatform;
    lTokenItem.LoginUserCode := QLoginCode;
    lTokenItem.SysUserID := '';
    lTokenItem.SysUserName := '';
    lTokenItem.SysUserType := '';
    lTokenItem.SysUserTable := '';
    lTokenItem.LastTime := Now;
    FTokenList.Add(lKey, lTokenItem);
    Result := lTokenItem;
  finally
    FLockObj.Leave;
  end;
end;

procedure TOneTokenManage.RemoveToken(QTokenID: string);
var
  lTokenItem: TOneTokenItem;
begin
  FLockObj.Enter;
  try
    if FTokenList.TryGetValue(QTokenID, lTokenItem) then
    begin
      if lTokenItem <> nil then
        lTokenItem.Free;
    end;
    FTokenList.Remove(QTokenID);
  finally
    FLockObj.Leave;
  end;
end;

function TOneTokenManage.CheckToken(QTokenID: string): boolean;
var
  lTokenItem: TOneTokenItem;
  lNow: TDateTime;
  lSub: double;
begin
  Result := False;
  if FTokenList.TryGetValue(QTokenID, lTokenItem) then
  begin
    lNow := Now;
    if FTimeOutSec > 0 then
    begin
      // 有设置过期时间
      lSub := SecondSpan(lNow, lTokenItem.LastTime);
      // token失效
      if lSub > FTimeOutSec then
      begin
        self.RemoveToken(lTokenItem.TokenID);
        lTokenItem.Free;
        lTokenItem := nil;
        exit;
      end;
    end;
    // 合法刷新最后交互时间,保证TOKEN有效性
    lTokenItem.LastTime := Now;
    Result := True;
  end;
end;

function TOneTokenManage.CheckSign(QTokenID: string; QUTCTime: string; QSign: string): boolean;
var
  lTokenItem: TOneTokenItem;
  lNow: TDateTime;
  tempSign: string;
begin
  Result := False;
  lNow := Now;
  if not FTokenList.TryGetValue(QTokenID, lTokenItem) then
    exit;
  tempSign := QTokenID + QUTCTime + lTokenItem.PrivateKey;
  tempSign := OneCrypto.MD5Endcode(tempSign);
  if tempSign.ToLower = QSign.ToLower then
  begin
    lTokenItem.LastTime := now;
    Result := True;
  end;
end;

procedure TOneTokenManage.SaveToken();
var
  lDataSet: TBufDataSet;
  lFileName: string;
  lTokenItem: TOneTokenItem;
begin
  lDataSet := TBufDataSet.Create(nil);
  try
    // 创建字段
    lDataSet.FieldDefs.Add('FConnectionID', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FTokenID', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FPrivateKey', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FLoginIP', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FLoginMac', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FLoginTime', ftDateTime, 0, False);
    lDataSet.FieldDefs.Add('FLoginPlatform', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FLoginUserCode', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FSysUserID', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FSysUserName', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FZTCode', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FPlatUserID', ftWidestring, 50, False);
    lDataSet.FieldDefs.Add('FLastTime', ftDateTime, 0, False);
    lDataSet.CreateDataset;
    for lTokenItem in FTokenList.Values do
    begin
      lDataSet.Append;
      lDataSet.FieldByName('FConnectionID').AsString := lTokenItem.ConnectionID;
      lDataSet.FieldByName('FTokenID').AsString := lTokenItem.TokenID;
      lDataSet.FieldByName('FPrivateKey').AsString := lTokenItem.PrivateKey;
      lDataSet.FieldByName('FLoginIP').AsString := lTokenItem.LoginIP;
      lDataSet.FieldByName('FLoginMac').AsString := lTokenItem.LoginMac;
      lDataSet.FieldByName('FLoginTime').AsDateTime := lTokenItem.LoginTime;
      lDataSet.FieldByName('FLoginPlatform').AsString := lTokenItem.LoginPlatform;
      lDataSet.FieldByName('FLoginUserCode').AsString := lTokenItem.LoginUserCode;
      lDataSet.FieldByName('FSysUserID').AsString := lTokenItem.SysUserID;
      lDataSet.FieldByName('FSysUserName').AsString := lTokenItem.SysUserName;
      lDataSet.FieldByName('FZTCode').AsString := lTokenItem.ZTCode;
      lDataSet.FieldByName('FPlatUserID').AsString := lTokenItem.PlatUserID;
      lDataSet.FieldByName('FLastTime').AsDateTime := lTokenItem.LastTime;
      lDataSet.Post;
    end;
    lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneToken');
    if not DirectoryExists(lFileName) then
      ForceDirectories(lFileName);
    lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneToken\TokenStore.data');
    lDataSet.SaveToFile(lFileName, TDataPacketFormat.dfBinary);
  finally
    lDataSet.Close;
    lDataSet.Free;
  end;
end;

procedure TOneTokenManage.LoadToken();
var
  lDataSet: TBufDataSet;
  lFileName: string;
  lTokenItem: TOneTokenItem;
begin
  lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneToken\TokenStore.data');
  if not FileExists(lFileName) then
    exit;
  lDataSet := TBufDataSet.Create(nil);
  try
    lFileName := OneFileHelper.CombineExeRunPath('OnePlatform\OneToken\TokenStore.data');
    lDataSet.LoadFromFile(lFileName, TDataPacketFormat.dfBinary);
    if not lDataSet.Active then
      exit;
    if lDataSet.RecordCount = 0 then
      exit;
    lDataSet.First;
    while not lDataSet.EOF do
    begin
      lTokenItem := TOneTokenItem.Create;
      lTokenItem.ConnectionID := lDataSet.FieldByName('FConnectionID').AsString;
      lTokenItem.TokenID := lDataSet.FieldByName('FTokenID').AsString;
      lTokenItem.PrivateKey := lDataSet.FieldByName('FPrivateKey').AsString;
      lTokenItem.LoginIP := lDataSet.FieldByName('FLoginIP').AsString;
      lTokenItem.LoginMac := lDataSet.FieldByName('FLoginMac').AsString;
      lTokenItem.LoginTime := lDataSet.FieldByName('FLoginTime').AsDateTime;
      lTokenItem.LoginPlatform := lDataSet.FieldByName('FLoginPlatform').AsString;
      lTokenItem.LoginUserCode := lDataSet.FieldByName('FLoginUserCode').AsString;
      lTokenItem.SysUserID := lDataSet.FieldByName('FSysUserID').AsString;
      lTokenItem.SysUserName := lDataSet.FieldByName('FSysUserName').AsString;
      lTokenItem.ZTCode := lDataSet.FieldByName('FZTCode').AsString;
      lTokenItem.PlatUserID := lDataSet.FieldByName('FPlatUserID').AsString;
      lTokenItem.LastTime := lDataSet.FieldByName('FLastTime').AsDateTime;
      if not self.TokenList.ContainsKey(lTokenItem.TokenID) then
      begin
        self.TokenList.Add(lTokenItem.TokenID, lTokenItem);
      end;
      lDataSet.Next;
    end;
  finally
    lDataSet.Close;
    lDataSet.Free;
  end;
end;

end.
