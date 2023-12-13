unit AppManage;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Generics.Collections, public_type,
  Client_UserInfo, Client_SetInfo, OneLog, OneFileHelper, OneSerialization, Dialogs,
  OneAsynThread, ExtCtrls;

var
  const_OnePlatform: string = 'OnePlatform';
  const_OneSet: string = 'OneSet';

type
  TAppManage = class(TObject)
  private
    // Exe名称
    FExeName: string;
    // Exe目录
    FExePath: string;
    // 表格布局保存所在目录
    FGridLayoutPath: string;
    // 报表所在目录
    FReportPath: string;
    // 日记:
    FLog: TOneLog;
    // 客户端配置
    FMyClientSet: TClientSetInfo;
    // 客户用户信息
    FMyClientInfo: TClientUserInfo;


    {*******管控窗体*******}
    FIsFreeAllForm: boolean;
    FIndexForm: TForm;
    //管控所有窗体
    FFormList: TList<TForm>;
    //MDI窗体新建关闭通知主窗体事件挂载
    FOnMDIToMainFormCallBack: TMDIFormCallBackEven;
  private
    procedure InitFile();
    procedure LoadMyClientSet();
  public
    constructor Create;
    destructor Destroy; override;
  public
    function SaveMyClientSet(QErrMsg: string): boolean;
    function GetGridLayoutInit(QFileName: string): string;
    function GetReportFileName(QFileName: string): string;
    function GetZTStringList(var QErrMsg: string): TStringList;
    {*******管控窗体*******}
    //当窗体新建时要通知主窗体新建Tabs标签
    procedure MDIFormCreateToMainForm(Sender: TForm);
    //当窗体关闭时要通知主窗体关闭Tabs标签
    procedure MDIFormCloseToMainForm(Sender: TForm);
    //当窗体改变标签同时改变主窗体Tabs标签
    procedure MDIFormCaptionToMainForm(Sender: TForm);
    //当窗体要展示切换Tabs标签
    procedure MDIFormBringToMainForm(Sender: TForm);
    //释放所有窗体,当主窗体关闭时
    procedure FreeAllForm();
  published
    property MyClientSet: TClientSetInfo read FMyClientSet;
    property MyClientInfo: TClientUserInfo read FMyClientInfo;
    property Log: TOneLog read FLog;
    property OnMDIToMainFormCallBack: TMDIFormCallBackEven read FOnMDIToMainFormCallBack write FOnMDIToMainFormCallBack;
  end;

function UnitAppManage(): TAppManage;

implementation

uses frm_BaseUI, impl_form;

var
  unit_AppManage: TAppManage = nil;

function UnitAppManage(): TAppManage;
begin
  Result := nil;
  if unit_AppManage = nil then
  begin
    unit_AppManage := TAppManage.Create;
  end;
  Result := unit_AppManage;
end;

constructor TAppManage.Create;
begin
  inherited Create;
  self.InitFile;
  FOnMDIToMainFormCallBack := nil;
  FIsFreeAllForm := False;
  FFormList := TList<TForm>.Create;

  self.FLog := TOneLog.Create(nil);
  self.FMyClientSet := TClientSetInfo.Create;
  self.FMyClientInfo := TClientUserInfo.Create;
  self.LoadMyClientSet();
  // 加载默认配置
  self.FMyClientInfo.MyLoginCode := self.FMyClientSet.LoginCode;
  self.FMyClientInfo.MYLoginPass := self.FMyClientSet.LoginPass;
  self.FMyClientInfo.HostCaption := self.FMyClientSet.IndexServer.HostCaption;
  self.FMyClientInfo.hostName := self.FMyClientSet.IndexServer.hostName;
  self.FMyClientInfo.HostPort := self.FMyClientSet.IndexServer.HostPort;
  self.FMyClientInfo.HostZTCode := self.FMyClientSet.IndexServer.HostZTCode;
  self.FMyClientInfo.PlatUserCode := self.FMyClientSet.IndexServer.PlatUserCode;
  self.FMyClientInfo.HostKey := self.FMyClientSet.IndexServer.HostKey;
end;

destructor TAppManage.Destroy;
begin
  FFormList.Clear;
  FFormList.Free;
  self.FMyClientSet.Free;
  self.FMyClientInfo.Free;
  if FLog <> nil then
  begin
    FLog.StopWork;
    FLog.Free;
  end;
  inherited Destroy;
end;

procedure TAppManage.InitFile();
var
  tempPath: string;
  lListPath: TList<string>;
  i: integer;
begin
  self.FExeName := OneFileHelper.GetExeName;
  self.FExePath := OneFileHelper.GetExeRunPath;
  tempPath := OneFileHelper.CombinePath(self.FExePath, const_OnePlatform);
  if not DirectoryExists(tempPath) then
    ForceDirectories(tempPath);
  lListPath := TList<string>.Create;
  try
    lListPath.Add(const_OneSet);
    // 日记默认目录
    lListPath.Add('OneLogs');
    // 临时存储数据的
    lListPath.Add('OneTemp');
    lListPath.Add('OneDataTemp');
    lListPath.Add('OneGridLayout');
    lListPath.Add('OneReport');
    // 生成文件
    for i := 0 to lListPath.Count - 1 do
    begin
      tempPath := OneFileHelper.CombinePathC(self.FExePath, const_OnePlatform, lListPath[i]);
      if not DirectoryExists(tempPath) then
        ForceDirectories(tempPath);
      if lListPath[i] = 'OneGridLayout' then
        self.FGridLayoutPath := tempPath
      else if lListPath[i] = 'OneReport' then
        self.FReportPath := tempPath;
    end;
  finally
    lListPath.Clear;
    lListPath.Free;
  end;
end;

procedure TAppManage.LoadMyClientSet();
var
  lServerSetFileName, lErrMsg: string;
begin
  lServerSetFileName := OneFileHelper.CombinePathD(self.FExePath, const_OnePlatform, const_OneSet, 'OneClientSet.JSON');
  // 加载配置
  if not OneSerialization.JSONToObjectFormFile(self.FMyClientSet, lServerSetFileName, lErrMsg) then
  begin
    ShowMessage(lErrMsg);
  end
  else
  begin
    // 序列化
    OneSerialization.ObjectToJsonFile(self.FMyClientSet, lServerSetFileName, lErrMsg);
  end;
end;

function TAppManage.SaveMyClientSet(QErrMsg: string): boolean;
var
  lServerSetFileName: string;
begin
  lServerSetFileName := OneFileHelper.CombinePathD(self.FExePath, const_OnePlatform, const_OneSet, 'OneClientSet.JSON');
  Result := OneSerialization.ObjectToJsonFile(self.FMyClientSet, lServerSetFileName, QErrMsg);
end;

function TAppManage.GetGridLayoutInit(QFileName: string): string;
begin
  Result := OneFileHelper.CombinePath(self.FGridLayoutPath, QFileName);
end;

function TAppManage.GetReportFileName(QFileName: string): string;
begin
  Result := OneFileHelper.CombinePath(self.FReportPath, QFileName);
end;

function TAppManage.GetZTStringList(var QErrMsg: string): TStringList;
begin
  Result := nil;
end;

procedure TAppManage.FreeAllForm();
var
  i: integer;
begin
  //置空回调
  self.FOnMDIToMainFormCallBack := nil;
  FIsFreeAllForm := True;
  for i := 0 to FFormList.Count - 1 do
  begin
    FFormList[i].Free;
  end;
end;

//当窗体新建时要通知主窗体新建Tabs标签
procedure TAppManage.MDIFormCreateToMainForm(Sender: TForm);
begin
  if Assigned(FOnMDIToMainFormCallBack) then
  begin
    self.FFormList.add(Sender);
    FOnMDIToMainFormCallBack(Sender, TMDIFormCallBackState.StateCreate);
  end;
end;

//当窗体关闭时要通知主窗体关闭Tabs标签
procedure TAppManage.MDIFormCloseToMainForm(Sender: TForm);
var
  i: integer;
  lForm: TfrmBaseUI;
begin
  if FIsFreeAllForm then
  begin
    exit;
  end;
  if not (Sender is TfrmBaseUI) then
  begin
    exit;
  end;
  for i := self.FFormList.Count - 1 downto 0 do
  begin
    if self.FFormList[i] is TfrmBaseUI then
    begin
      lForm := TfrmBaseUI(self.FFormList[i]);
      if lForm.CreateID = TfrmBaseUI(Sender).CreateID then
      begin
        self.FFormList.Delete(i);
        break;
      end;
    end;
  end;
  if Assigned(FOnMDIToMainFormCallBack) then
  begin
    try
      FOnMDIToMainFormCallBack(Sender, TMDIFormCallBackState.StateClose);
    finally
    end;
  end;
end;

procedure TAppManage.MDIFormCaptionToMainForm(Sender: TForm);
begin
  if Assigned(FOnMDIToMainFormCallBack) then
  begin
    FOnMDIToMainFormCallBack(Sender, TMDIFormCallBackState.StateCaption);
  end;
end;

procedure TAppManage.MDIFormBringToMainForm(Sender: TForm);
var
  i: integer;
  lForm: TfrmBaseUI;
  lPanle: TPanel;
begin
  if Assigned(FOnMDIToMainFormCallBack) then
  begin
    FOnMDIToMainFormCallBack(Sender, TMDIFormCallBackState.StateCaption);
  end;
end;

initialization

finalization
  if unit_AppManage <> nil then
  begin
    unit_AppManage.Free;
  end;
end.
