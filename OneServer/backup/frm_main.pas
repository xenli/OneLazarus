unit frm_main;

{$IFDEF FPC}
  {$MODE Delphi}

{$ENDIF}

interface

uses
  SysUtils, Variants, Classes, Graphics, Generics.Collections, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DB, BufDataset, SQLDB, SQLDBLib, memds,
  IBConnection, SQLite3Conn, PQConnection, oracleconnection, odbcconn,
  mysql40conn, mysql41conn, mysql50conn, mysql51conn, mysql55conn, mysql56conn,
  mysql57conn, mysql80conn, MSSQLConn, OneLog, DBCtrls, Menus, OneFileHelper,
  OneHttpRouterManage, Buttons, ExtCtrls, DBGrids, process, LCLIntf;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BufDataset1: TBufDataset;
    edSuperPass: TEdit;
    editDbType: TDBComboBox;
    editDbDBName1: TDBEdit;
    editDbVersion: TDBEdit;
    editDbHost: TDBEdit;
    editDbPort: TDBEdit;
    editDbDBName: TDBEdit;
    editDbDBUser: TDBEdit;
    editDbDBPass: TDBEdit;
    editDbOtherParams: TDBEdit;
    editDbKeeConnect: TDBCheckBox;
    edLogPath: TEdit;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    lbConnectSecretkey1: TLabel;
    MemDataset1: TMemDataset;
    qryTokenFConnectionID: TWideStringField;
    qryTokenFLoginMac: TWideStringField;
    qryZTSet: TBufDataset;
    qryZTPool: TBufDataset;
    qryVirtual: TBufDataset;
    qryToken: TBufDataset;
    qryRouter: TBufDataset;
    ImageList1: TImageList;
    pageMain: TPageControl;
    qryZTSetDBCharSet: TWideStringField;
    qryZTSetDBHostName: TWideStringField;
    qryZTSetDBHostPort: TLongintField;
    qryZTSetDBKeepConnect: TBooleanField;
    qryZTSetDBName: TWideStringField;
    qryZTSetDBOtherParams: TWideStringField;
    qryZTSetDBType: TWideStringField;
    qryZTSetDBUserName: TWideStringField;
    qryZTSetDBUserPass: TWideStringField;
    qryZTSetDBVersion: TWideStringField;
    SQLConnector: TSQLConnector;
    SQLDBLibraryLoader1: TSQLDBLibraryLoader;
    SQLQuery1: TSQLQuery;
    tabServerReamk: TTabSheet;
    tabZTManage: TTabSheet;
    Memo1: TMemo;
    dsZTSet: TDataSource;
    tabLog: TTabSheet;
    tabToken: TTabSheet;
    qryZTSetFZTCode: TWideStringField;
    qryZTSetFZTCaption: TWideStringField;
    qryZTSetFInitPoolCount: TLongintField;
    qryZTSetFMaxPoolCount: TLongintField;
    qryZTSetFIsEnable: TBooleanField;
    qryZTSetFConnectionStr: TWideStringField;
    qryZTSetFPhyDriver: TWideStringField;
    qryZTSetFIsMain: TBooleanField;
    tabHTTPServer: TTabSheet;
    plTop: TPanel;
    Image1: TImage;

    groupHTTP: TGroupBox;
    tbStart: TButton;
    tbStop: TButton;
    tbRequest: TButton;
    Label1: TLabel;
    edHTTPPort: TEdit;
    Label2: TLabel;
    edHTTPPool: TEdit;
    Label3: TLabel;
    edHTTPQueue: TEdit;
    edHTTPAutoStart: TCheckBox;
    groupWebSocket: TGroupBox;
    tbSaveHTTPSet: TButton;
    lbConnectSecretkey: TLabel;
    edConnectSecretkey: TEdit;
    tbBuildConnectSecretkey: TButton;
    lbServerHint: TLabel;
    GroupBox3: TGroupBox;
    Panel1: TPanel;
    tbZTAdd: TButton;
    tbZTDel: TButton;
    edZTAutoStart: TCheckBox;
    tbZTSave: TButton;
    tbZTOpen: TButton;
    dbGridZTSet: TDBGrid;
    plZTSet: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    dbZTCode: TDBEdit;
    dbZTCaption: TDBEdit;
    dbInitPoolCount: TDBEdit;
    dbMaxPoolCount: TDBEdit;
    dbConnectionStr: TDBEdit;
    dbIsEnable: TDBCheckBox;
    tbZTSetOK: TButton;
    tbZTPing: TButton;
    dbIsMain: TDBCheckBox;
    GroupBox4: TGroupBox;
    pnZTPool: TPanel;
    tbGetZTPool: TButton;
    tbZTMangeStarWork: TButton;
    dsZTPool: TDataSource;
    qryZTPoolFZTCode: TWideStringField;
    qryZTPoolFInitPoolCount: TLongintField;
    qryZTPoolFMaxPoolCount: TLongintField;
    qryZTPoolFPoolCreateCount: TLongintField;
    qryZTPoolFPoolWorkCount: TLongintField;
    qryZTPoolFStop: TBooleanField;
    dbGridZTPool: TDBGrid;
    tbZTNotStop: TButton;
    tbZTStop: TButton;
    pnLogSet: TPanel;
    Label11: TLabel;
    edLogSec: TEdit;
    edHTTPLog: TCheckBox;
    edSQLLog: TCheckBox;
    tbSaveLogSet: TButton;
    GroupBox5: TGroupBox;
    Panel2: TPanel;
    tbOutLogStart: TButton;
    tbOutLogClear: TButton;
    edLog: TMemo;
    tbOpenLogFile: TButton;
    TrayIcon1: TTrayIcon;
    HookMenu: TPopupMenu;
    pmiShowMain: TMenuItem;
    HookCloseMain: TMenuItem;
    GroupBox1: TGroupBox;
    tabVirtualFile: TTabSheet;
    plVirtualFile: TPanel;
    tbFileAdd: TButton;
    tbFileDel: TButton;
    dsVirtual: TDataSource;
    grdVirtualFile: TDBGrid;
    qryVirtualFVirtualCode: TWideStringField;
    qryVirtualFVirtualCaption: TWideStringField;
    qryVirtualFPhyPath: TWideStringField;
    qryVirtualFIsEnable: TBooleanField;
    qryVirtualFIsWeb: TBooleanField;
    tbVirtualSave: TButton;
    tbVirtualStarWork: TButton;
    TabSheet1: TTabSheet;
    GroupBox2: TGroupBox;
    dbGridRouter: TDBGrid;
    Panel3: TPanel;
    tbRouterSelect: TButton;
    GroupBox6: TGroupBox;
    edRouterErrMsg: TMemo;
    dsRouter: TDataSource;
    qryRouterFOrderNumber: TLongintField;
    qryRouterFRootName: TWideStringField;
    qryRouterFClassName: TWideStringField;
    qryRouterFPoolMaxCount: TLongintField;
    qryRouterFWorkingCount: TLongintField;
    Panel4: TPanel;
    tbTokenSelect: TButton;
    tbTokenDelete: TButton;
    dsToken: TDataSource;
    gridToken: TDBGrid;
    qryTokenFTokenID: TWideStringField;
    qryTokenFPrivateKey: TWideStringField;
    qryTokenFLoginIP: TWideStringField;
    qryTokenFLoginTime: TDateTimeField;
    qryTokenFLoginPlatform: TWideStringField;
    qryTokenFLoginUserCode: TWideStringField;
    qryTokenFSysUserID: TWideStringField;
    qryTokenFSysUserName: TWideStringField;
    qryTokenFZTCode: TWideStringField;
    qryTokenFPlatUserID: TWideStringField;
    qryTokenFLastTime: TDateTimeField;
    tbTokenSave: TButton;
    BtnRes: TBitBtn;
    btnClose: TBitBtn;
    chWinTaskStart: TCheckBox;
    chWinRegisterStart: TCheckBox;
    Label12: TLabel;
    edTokenOutSec: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure tbStartClick(Sender: TObject);
    procedure tbStopClick(Sender: TObject);
    procedure tbRequestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbSaveHTTPSetClick(Sender: TObject);
    procedure tbZTAddClick(Sender: TObject);
    procedure tbZTDelClick(Sender: TObject);
    procedure tbZTNotStopClick(Sender: TObject);
    procedure tbZTStopClick(Sender: TObject);
    procedure tbZTSaveClick(Sender: TObject);
    procedure tbZTOpenClick(Sender: TObject);
    procedure tbZTSetOKClick(Sender: TObject);
    procedure dbGridZTSetDblClick(Sender: TObject);
    procedure tbZTPingClick(Sender: TObject);
    procedure tbZTMangeStarWorkClick(Sender: TObject);
    procedure tbBuildConnectSecretkeyClick(Sender: TObject);
    procedure tbGetZTPoolClick(Sender: TObject);
    procedure tbSaveLogSetClick(Sender: TObject);
    procedure tbOutLogClearClick(Sender: TObject);
    procedure tbOutLogStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrayIcon1Click(Sender: TObject);
    procedure pmiShowMainClick(Sender: TObject);
    procedure HookCloseMainClick(Sender: TObject);
    procedure tbOpenLogFileClick(Sender: TObject);
    procedure tbFileAddClick(Sender: TObject);
    procedure tbFileDelClick(Sender: TObject);
    procedure tbVirtualSaveClick(Sender: TObject);
    procedure tbVirtualStarWorkClick(Sender: TObject);
    procedure tbRouterSelectClick(Sender: TObject);
    procedure tbTokenSelectClick(Sender: TObject);
    procedure tbTokenDeleteClick(Sender: TObject);
    procedure tbTokenSaveClick(Sender: TObject);
    procedure BtnResClick(Sender: TObject);
    procedure chWinTaskStartClick(Sender: TObject);
    procedure chWinRegisterStartClick(Sender: TObject);
  private
    { Private declarations }
    FIsClose: boolean;
    function YesNoMsg(QTitel: string; QMsg: string): boolean;
    procedure MsgCallBack(const QMsg: string);
    procedure OpenZTMange();
    procedure OpenVirtualMange();
    procedure OpenZTPool();
  end;

var
  frmMain: TfrmMain;
  // 加一个重启标志
  Restart_Flag: boolean = False;

implementation

{$R *.lfm}


uses OneGlobal, OneZTManage, OneGUID, OneVirtualFile, OneTokenManage;

procedure TfrmMain.tbRouterSelectClick(Sender: TObject);
var
  lRouterManage: TOneRouterManage;
  lRouterItems: TDictionary<string, TOneRouterItem>;
  lRouterItem: TOneRouterItem;
begin
  if qryRouter.Active then
    qryRouter.Close;
  qryRouter.CreateDataSet;
  lRouterManage := OneHttpRouterManage.GetInitRouterManage();
  lRouterItems := lRouterManage.GetRouterItems();
  for lRouterItem in lRouterItems.values do
  begin
    qryRouter.Append;
    qryRouterFOrderNumber.AsInteger := qryRouter.RecNo + 2;
    qryRouterFRootName.AsString := lRouterItem.RootName;
    qryRouterFClassName.AsString := lRouterItem.RootClassName;
    qryRouterFPoolMaxCount.AsInteger := lRouterItem.PoolMaxCount;
    qryRouterFWorkingCount.AsInteger := lRouterItem.WorkingCount;
    qryRouter.Post;
  end;

end;

procedure TfrmMain.tbTokenDeleteClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
begin
  if qryToken.IsEmpty then
  begin
    ShowMessage('请先选中一条Token数据');
    exit;
  end;
  if not YesNoMsg('删除提醒', '确定删除当前TokenID' + qryTokenFTokenID.AsString + '的记录') then
    exit;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.TokenManage.RemoveToken(qryTokenFTokenID.AsString);
  qryToken.Delete;
end;

procedure TfrmMain.BtnResClick(Sender: TObject);
var
  aProcess: TProcess; //TProcess is crossplatform is best way
begin
  if not YesNoMsg('重启提醒', '确定重启服务') then
    exit;
  aProcess := TProcess.Create(nil);
  aProcess.CommandLine := '"' + Application.ExeName + '"';
  aProcess.Execute;
  aProcess.Free;
  Application.Terminate;
end;

procedure TfrmMain.chWinRegisterStartClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
begin
  //lOneGlobal := TOneGlobal.GetInstance();
  //OneWinReg.WinAutoStart(OneFileHelper.GetExeName(), Application.ExeName,
  //  chWinRegisterStart.Checked);
  //lOneGlobal.ServerSet.WinRegisterStart := chWinRegisterStart.Checked;
  //if not lOneGlobal.SaveServerSet(lErrMsg) then
  //begin
  //  ShowMessage(lErrMsg);
  //end;
end;

procedure TfrmMain.chWinTaskStartClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
begin
  //lOneGlobal := TOneGlobal.GetInstance();
  //OneWinReg.WinTaskStart(OneFileHelper.GetExeName(), Application.ExeName,
  //  chWinTaskStart.Checked);
  //lOneGlobal.ServerSet.WinTaskStart := chWinTaskStart.Checked;
  //if not lOneGlobal.SaveServerSet(lErrMsg) then
  //begin
  //  ShowMessage(lErrMsg);
  //end;
end;

procedure TfrmMain.dbGridZTSetDblClick(Sender: TObject);
begin
  if qryZTSet.IsEmpty then
    exit;
  plZTSet.Visible := True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not FIsClose) and (not Restart_Flag) then
  begin
    Action := TCloseAction.caNone;
    self.Hide;
    TrayIcon1.BalloonTitle := 'OneDelphi服务';
    TrayIcon1.BalloonHint := '最小化至托盘';
    TrayIcon1.ShowBalloonHint;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
begin
  FIsClose := False;
  self.pageMain.ActivePageIndex := 0;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.StarWork(lErrMsg);
  // 服务配置加载
  edHTTPPort.Text := lOneGlobal.ServerSet.HTTPPort.ToString;
  edHTTPPool.Text := lOneGlobal.ServerSet.HTTPPool.ToString;
  edHTTPQueue.Text := lOneGlobal.ServerSet.HTTPQueue.ToString;
  edHTTPAutoStart.Checked := lOneGlobal.ServerSet.HTTPAutoWork;
  edConnectSecretkey.Text := lOneGlobal.ServerSet.ConnectSecretkey;
  edTokenOutSec.Text := lOneGlobal.ServerSet.TokenIntervalSec.ToString;
  chWinTaskStart.Checked := lOneGlobal.ServerSet.WinTaskStart;
  chWinRegisterStart.Checked := lOneGlobal.ServerSet.WinRegisterStart;
  edSuperPass.Text := lOneGlobal.ServerSet.SuperAdminPass;
  // 账套自动工作
  edZTAutoStart.Checked := lOneGlobal.ZTMangeSet.AutoWork;
  // 日记配置加载
  edLogPath.Text := lOneGlobal.LogSet.LogPath;
  edLogSec.Text := lOneGlobal.LogSet.LogSec.ToString;
  edHTTPLog.Checked := lOneGlobal.LogSet.HTTPLog;
  edSQLLog.Checked := lOneGlobal.LogSet.SQLLog;
  // 账套配置加载,创建缓存数据
  self.OpenZTMange();
  // 虚拟文件加载
  self.OpenVirtualMange();

  self.OpenZTPool();
  if lOneGlobal.HttpServer.Started then
    lbServerHint.Caption := 'HTTP运行状态:端口[' + lOneGlobal.HttpServer.Port.ToString + '],状态[启动]'
  else
    lbServerHint.Caption := 'HTTP运行状态:端口[' + lOneGlobal.HttpServer.Port.ToString + '],状态[未启动]';
  if OneHttpRouterManage.GetInitRouterManage().ErrMsg <> '' then
  begin
    lbServerHint.Caption := lbServerHint.Caption + ';路由注册状态[错误]';
    edRouterErrMsg.Lines.Text := OneHttpRouterManage.GetInitRouterManage().ErrMsg;
  end;

end;


procedure TfrmMain.OpenZTMange();
var
  lOneGlobal: TOneGlobal;
  lZTSet: TOneZTSet;
  i: integer;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  if qryZTSet.Active then
    qryZTSet.Close;
  qryZTSet.CreateDataSet;
  for i := 0 to lOneGlobal.ZTMangeSet.ZTSetList.Count - 1 do
  begin
    lZTSet := lOneGlobal.ZTMangeSet.ZTSetList[i];
    qryZTSet.Append;
    qryZTSetFZTCode.AsString := lZTSet.ZTCode;
    qryZTSetFZTCaption.AsString := lZTSet.ZTCaption;
    qryZTSetFInitPoolCount.AsInteger := lZTSet.InitPoolCount;
    qryZTSetFMaxPoolCount.AsInteger := lZTSet.MaxPoolCount;
    qryZTSetFPhyDriver.AsString := lZTSet.PhyDriver;
    qryZTSetFConnectionStr.AsString := lZTSet.ConnectionStr;
    qryZTSetFIsEnable.AsBoolean := lZTSet.IsEnable;
    qryZTSetFIsMain.AsBoolean := lZTSet.IsMain;
    qryZTSetDBType.AsString := lZTSet.DBType;
    qryZTSetDBVersion.AsString := lZTSet.DBVersion;
    qryZTSetDBHostName.AsString := lZTSet.DBHostName;
    qryZTSetDBCharSet.AsString := lZTSet.DBCharSet;
    qryZTSetDBHostPort.AsInteger := lZTSet.DBHostPort;
    qryZTSetDBName.AsString := lZTSet.DBName;
    qryZTSetDBUserName.AsString := lZTSet.DBUserName;
    qryZTSetDBUserPass.AsString := lZTSet.DBUserPass;
    qryZTSetDBKeepConnect.AsBoolean := lZTSet.DBKeepConnect;
    qryZTSetDBOtherParams.AsString := lZTSet.DBOtherParams;
    qryZTSet.Post;
  end;
  qryZTSet.MergeChangeLog;
end;

procedure TfrmMain.OpenVirtualMange();
var
  lOneGlobal: TOneGlobal;
  lVirtualSet: TOneVirtualItem;
  i: integer;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  if qryVirtual.Active then
    qryVirtual.Close;
  qryVirtual.CreateDataSet;
  for i := 0 to lOneGlobal.VirtualSet.VirtualSetList.Count - 1 do
  begin
    lVirtualSet := lOneGlobal.VirtualSet.VirtualSetList[i];
    qryVirtual.Append;
    qryVirtualFVirtualCode.AsString := lVirtualSet.VirtualCode;
    qryVirtualFVirtualCaption.AsString := lVirtualSet.VirtualCaption;
    qryVirtualFPhyPath.AsString := lVirtualSet.phyPath;
    qryVirtualFIsEnable.AsBoolean := lVirtualSet.IsEnable;
    qryVirtualFIsWeb.AsBoolean := lVirtualSet.IsWeb;
    qryVirtual.Post;
  end;
  qryVirtual.MergeChangeLog;
end;

procedure TfrmMain.OpenZTPool();
var
  lOneGlobal: TOneGlobal;
  lZTPool: TOneZTPool;
begin

  if qryZTPool.Active then
    qryZTPool.Close;
  qryZTPool.CreateDataSet;
  lOneGlobal := TOneGlobal.GetInstance();
  for lZTPool in lOneGlobal.ZTManage.ZTPools.values do
  begin
    qryZTPool.Append;
    qryZTPoolFZTCode.AsString := lZTPool.ZTCode;
    qryZTPoolFInitPoolCount.AsInteger := lZTPool.InitPoolCount;
    qryZTPoolFMaxPoolCount.AsInteger := lZTPool.MaxPoolCount;
    qryZTPoolFPoolCreateCount.AsInteger := lZTPool.PoolCreateCount;
    qryZTPoolFPoolWorkCount.AsInteger := lZTPool.PoolWorkCount;
    qryZTPoolFStop.AsBoolean := lZTPool.Stop;
    qryZTPool.Post;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  TOneGlobal.GetInstance().Free;
end;

procedure TfrmMain.HookCloseMainClick(Sender: TObject);
begin
  if YesNoMsg('退出提醒', '是否真的要退出服务端程序？') then
  begin
    FIsClose := True;
    self.Close;
  end;
end;

procedure TfrmMain.tbBuildConnectSecretkeyClick(Sender: TObject);
begin
  edConnectSecretkey.Text := OneGUID.GetGUID32;
end;

procedure TfrmMain.tbFileAddClick(Sender: TObject);
begin

  qryVirtual.Append;
  qryVirtualFVirtualCaption.AsString := '新增虚拟目录';
  qryVirtual.Post;
end;

procedure TfrmMain.tbFileDelClick(Sender: TObject);
begin
  if qryVirtual.IsEmpty then
  begin
    exit;
  end;
  if not YesNoMsg('删除目录提醒', '确定删除虚拟目录') then
    exit;
  qryVirtual.Delete;
end;

procedure TfrmMain.tbGetZTPoolClick(Sender: TObject);
begin

  self.OpenZTPool();
end;

procedure TfrmMain.tbOpenLogFileClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  lFilePath: string;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  lFilePath := (lOneGlobal.Log as TOneLog).LogPath;
  OpenDocument(lFilePath);
end;

procedure TfrmMain.tbOutLogClearClick(Sender: TObject);
begin

  edLog.Lines.Clear;
end;

procedure TfrmMain.tbOutLogStartClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  lOneLog: TOneLog;
begin

  lOneGlobal := TOneGlobal.GetInstance();
  lOneLog := lOneGlobal.Log as TOneLog;
  if not lOneLog.IsOut then
  begin
    lOneLog.CallBack := self.MsgCallBack;
    lOneLog.IsOut := True;
    tbOutLogStart.Caption := '关闭即时日记';
  end
  else
  begin
    lOneLog.IsOut := False;
    lOneLog.CallBack := nil;
    tbOutLogStart.Caption := '开启即时日记';
  end;
end;

procedure TfrmMain.MsgCallBack(const QMsg: string);
var
  lTimeString: string;
  i: integer;
begin
  {$IFDEF FPC}
   {$ELSE}
  TThread.Synchronize(nil,
    procedure ()
    begin
      lTimeString := FormatDateTime('yyyy-mm-dd hh:mm:ss', Now);
      edLog.Lines.BeginUpdate;
      try
        i := edLog.Lines.Count;
        if i > 3000 then
          edLog.Lines.Clear;
        edLog.Lines.Add(lTimeString + '->' + QMsg);
      finally
        edLog.Lines.EndUpdate;
      end;
    end);
  {$ENDIF}
end;

procedure TfrmMain.pmiShowMainClick(Sender: TObject);
begin
  if not self.Visible then
    self.Visible := True;
end;

procedure TfrmMain.tbRequestClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  if not self.YesNoMsg('HTTP服务', '当前禁止HTTP服务端口' + lOneGlobal.HttpServer.Port.ToString()) then
  begin
    exit;
  end;
  if lOneGlobal.HttpServer.ServerStopRequest() then
  begin
    ShowMessage('禁止服务请求成功');
  end
  else
  begin
    ShowMessage(lOneGlobal.HttpServer.ErrMsg);
  end;
end;

procedure TfrmMain.tbSaveHTTPSetClick(Sender: TObject);
var
  tempStr: string;
  lErrMsg: string;
  lPort, lPool, lQueue, lTokenSec: integer;
  lOneGlobal: TOneGlobal;
begin

  tempStr := edHTTPPort.Text;
  if not tryStrToInt(tempStr, lPort) then
  begin
    ShowMessage('请输入正确的端口');
    exit;
  end;
  tempStr := edHTTPPool.Text;
  if not tryStrToInt(tempStr, lPool) then
  begin
    ShowMessage('请输入正确的HTTP工作线程数');
    exit;
  end;
  tempStr := edHTTPQueue.Text;
  if not tryStrToInt(tempStr, lQueue) then
  begin
    ShowMessage('请输入正确的队列大小');
    exit;
  end;
  if lPool <= 0 then
    lPool := 100;
  if lQueue <= 0 then
    lQueue := 1000;

  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.ServerSet.HTTPPort := lPort;
  lOneGlobal.ServerSet.HTTPPool := lPool;
  lOneGlobal.ServerSet.HTTPQueue := lQueue;
  lOneGlobal.ServerSet.HTTPAutoWork := edHTTPAutoStart.Checked;
  lOneGlobal.ServerSet.ConnectSecretkey := edConnectSecretkey.Text;

  lOneGlobal.ServerSet.WinTaskStart := chWinTaskStart.Checked;
  lOneGlobal.ServerSet.WinRegisterStart := chWinRegisterStart.Checked;

  lOneGlobal.ServerSet.SuperAdminPass := edSuperPass.Text;

  tempStr := edTokenOutSec.Text;
  if not tryStrToInt(tempStr, lTokenSec) then
  begin
    lTokenSec := 0;
  end;
  lOneGlobal.ServerSet.TokenIntervalSec := lTokenSec;

  if lOneGlobal.SaveServerSet(lErrMsg) then
  begin
    ShowMessage('保存服务端配置成功');
  end
  else
  begin
    ShowMessage(lErrMsg);
  end;
end;

procedure TfrmMain.tbSaveLogSetClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  tempStr, lErrMsg: string;
  tempI: integer;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.LogSet.LogPath := edLogPath.Text;
  tempStr := edLogSec.Text;
  if tryStrToInt(tempStr, tempI) then
  begin
    if tempI <= 0 then
      tempI := 5;
    lOneGlobal.LogSet.LogSec := tempI;
  end;
  lOneGlobal.LogSet.HTTPLog := edHTTPLog.Checked;
  lOneGlobal.LogSet.SQLLog := edSQLLog.Checked;
  if lOneGlobal.SaveLogSet(lErrMsg) then
  begin
    ShowMessage('保存日志配置成功');
  end
  else
  begin
    ShowMessage(lErrMsg);
  end;
end;

procedure TfrmMain.tbStartClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  if not self.YesNoMsg('HTTP服务', '当前启动HTTP服务端口' + lOneGlobal.ServerSet.HTTPPort.ToString()) then
  begin
    exit;
  end;
  try
    if lOneGlobal.HTTPServerStart(lErrMsg) then
    begin
      ShowMessage('启动服务成功,当前启动HTTP服务端口:' + lOneGlobal.HttpServer.Port.ToString);
    end
    else
    begin
      ShowMessage(lErrMsg);
    end;
  finally
    if lOneGlobal.HttpServer.Started then
      lbServerHint.Caption := 'HTTP运行状态:端口[' + lOneGlobal.HttpServer.Port.ToString + '],状态[启动]'
    else
      lbServerHint.Caption := 'HTTP运行状态:端口[' + lOneGlobal.HttpServer.Port.ToString + '],状态[未启动]';
  end;
end;

procedure TfrmMain.tbStopClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  if not self.YesNoMsg('HTTP服务', '当前停止HTTP服务端口' + lOneGlobal.HttpServer.Port.ToString()) then
  begin
    exit;
  end;
  if lOneGlobal.HttpServer.ServerStop() then
  begin
    ShowMessage('停止服务成功');
  end
  else
  begin
    ShowMessage(lOneGlobal.HttpServer.ErrMsg);
  end;
end;

procedure TfrmMain.tbTokenSaveClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  lTokenItem: TOneTokenItem;
begin
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.TokenManage.SaveToken;
  ShowMessage('保存Token信息成功');
end;

procedure TfrmMain.tbTokenSelectClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  lTokenItem: TOneTokenItem;
begin
  qryToken.DisableControls;
  try
    if qryToken.Active then
      qryToken.Close;
    qryToken.CreateDataSet;

    lOneGlobal := TOneGlobal.GetInstance();
    for lTokenItem in lOneGlobal.TokenManage.TokenList.values do
    begin
      qryToken.Append;
      qryTokenFConnectionID.AsString := lTokenItem.ConnectionID;
      qryTokenFTokenID.AsString := lTokenItem.TokenID;
      qryTokenFPrivateKey.AsString := lTokenItem.PrivateKey;
      qryTokenFLoginIP.AsString := lTokenItem.LoginIP;
      qryTokenFLoginMac.AsString := lTokenItem.LoginMac;
      qryTokenFLoginTime.AsDateTime := lTokenItem.LoginTime;
      qryTokenFLoginPlatform.AsString := lTokenItem.LoginPlatform;
      qryTokenFLoginUserCode.AsString := lTokenItem.LoginUserCode;
      qryTokenFSysUserID.AsString := lTokenItem.SysUserID;
      qryTokenFSysUserName.AsString := lTokenItem.SysUserName;
      qryTokenFZTCode.AsString := lTokenItem.ZTCode;
      qryTokenFPlatUserID.AsString := lTokenItem.PlatUserID;
      qryTokenFLastTime.AsDateTime := lTokenItem.LastTime;
      qryToken.Post;
    end;
  finally
    qryToken.EnableControls;
  end;
  ShowMessage('查看Token信息成功');
end;

procedure TfrmMain.tbVirtualSaveClick(Sender: TObject);
var
  lOneGlobal: TOneGlobal;
  lVirtualItem: TOneVirtualItem;
  lErrMsg: string;
  i: integer;
begin

  if qryVirtual.State in dsEditModes then
    qryVirtual.Post;
  lOneGlobal := TOneGlobal.GetInstance();
  for i := 0 to lOneGlobal.VirtualSet.VirtualSetList.Count - 1 do
  begin
    lVirtualItem := lOneGlobal.VirtualSet.VirtualSetList[i];
    lVirtualItem.Free;
  end;
  lOneGlobal.VirtualSet.VirtualSetList.Clear;
  try
    qryVirtual.DisableControls;
    qryVirtual.First;
    while not qryVirtual.EOF do
    begin
      lVirtualItem := TOneVirtualItem.Create;
      lOneGlobal.VirtualSet.VirtualSetList.Add(lVirtualItem);
      lVirtualItem.VirtualCode := qryVirtualFVirtualCode.AsString;
      lVirtualItem.VirtualCaption := qryVirtualFVirtualCaption.AsString;
      lVirtualItem.phyPath := qryVirtualFPhyPath.AsString;
      lVirtualItem.IsEnable := qryVirtualFIsEnable.AsBoolean;
      lVirtualItem.IsWeb := qryVirtualFIsWeb.AsBoolean;
      qryVirtual.Next;
    end;
  finally
    qryVirtual.EnableControls;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  if lOneGlobal.SaveVirtualSet(lErrMsg) then
  begin
    ShowMessage('保存成功,如要要立即生效,记得重新加载按钮扭下');
  end;
end;

procedure TfrmMain.tbVirtualStarWorkClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
begin
  lErrMsg := '确定已重新加载虚拟文件配置';
  if not self.YesNoMsg('', lErrMsg) then
  begin
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.VirtualManage.StarWork(lOneGlobal.VirtualSet.VirtualSetList);
  ShowMessage('重新加载虚拟文件配置成功');
end;

procedure TfrmMain.tbZTAddClick(Sender: TObject);
begin
  qryZTSet.Append;
  qryZTSetFInitPoolCount.AsInteger := 10;
  qryZTSetFMaxPoolCount.AsInteger := 10;
  qryZTSet.Post;
end;

function TfrmMain.YesNoMsg(QTitel: string; QMsg: string): boolean;
begin
  Result := False;
  if QTitel = '' then
    QTitel := '信息提示';
  if MessageDlg(QTitel, QMsg, mtInformation, [mbOK, mbCancel], 0) = mrOk then
    Result := True;
end;

procedure TfrmMain.tbZTDelClick(Sender: TObject);
var
  lErrMsg: string;
begin
  if qryZTSet.IsEmpty then
  begin
    ShowMessage('数据为空,无需删除');
    exit;
  end;
  lErrMsg := '确定删除当前账套[' + qryZTSetFZTCode.AsString + ']?';
  if not self.YesNoMsg('', lErrMsg) then
  begin
    exit;
  end;
  qryZTSet.Delete;
end;

procedure TfrmMain.tbZTMangeStarWorkClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
begin
  lErrMsg := '确定已保存好账套并且重新加载所有账套';
  if not self.YesNoMsg('', lErrMsg) then
  begin
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  if not lOneGlobal.ZTManage.StarWork(lOneGlobal.ZTMangeSet.ZTSetList, lErrMsg) then
  begin
    ShowMessage('重载账套管理失败,原因:' + lErrMsg);
  end
  else
  begin
    ShowMessage('重载账套管理成功');
  end;
end;

procedure TfrmMain.tbZTNotStopClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
  lZTCode: string;
begin
  if qryZTPool.IsEmpty then
  begin
    ShowMessage('无相关运行账套，请先查看运行账套');
    exit;
  end;
  lZTCode := qryZTPoolFZTCode.AsString;
  lZTCode := lZTCode.Trim;
  if lZTCode.Trim = '' then
  begin
    ShowMessage('账套代码为空,无法操作数据');
    exit;
  end;
  lErrMsg := '确定开始运行当前账套[' + lZTCode + '],请确保账套管理已加载此账套?';
  if not self.YesNoMsg('', lErrMsg) then
  begin
    exit;
  end;
  // 找到账套，打上标识stop
  lOneGlobal := TOneGlobal.GetInstance();
  if lOneGlobal.ZTManage.StopZT(lZTCode, False, lErrMsg) then
  begin
    qryZTPool.Edit;
    qryZTPoolFStop.AsBoolean := False;
    qryZTPool.Post;
    ShowMessage('账套代码[' + lZTCode + ']开始运行');
  end
  else
  begin
    ShowMessage('账套代码[' + lZTCode + ']' + lErrMsg);
  end;
end;

procedure TfrmMain.tbZTOpenClick(Sender: TObject);
begin

end;

procedure TfrmMain.tbZTPingClick(Sender: TObject);
var
  lList: TStringList;
  i: integer;
  lDBType: string;
begin
  lDBType := qryZTSetDBType.AsString;
  if lDBType.StartsWith('MySQL') then
  begin
    if qryZTSetDBVersion.AsString = '' then
    begin
      ShowMessage('当驱动为[MySQL],请填写MySQL版本号,参考下面红色提示!!!');
      exit;
    end;
    lDBType := lDBType + ' ' + qryZTSetDBVersion.AsString;
  end;
  if lDBType.StartsWith('MSSQLServer') then
  begin
    if qryZTSetDBVersion.AsString = '' then
    begin
      ShowMessage('当驱动为[MSSQLServer],请填写MSSQLServer版本号,参考下面红色提示!!!');
      exit;
    end;
  end;
  qryZTSet.Edit;
  qryZTSetFPhyDriver.AsString := lDBType;
  qryZTSetFConnectionStr.AsString :=
    'HostName=' + qryZTSetDBHostName.AsString + ';UserName=' + qryZTSetDBUserName.AsString;
  qryZTSet.Post;

  if qryZTSetFConnectionStr.AsString.Trim = '' then
  begin
    ShowMessage('当前账套' + qryZTSetFZTCode.AsString + '连接字符串为空');
    exit;
  end;
  if SQLConnector.Connected then
    SQLConnector.Close();
  lList := TStringList.Create;
  try
    lList.LineBreak := ';';
    try
      SQLConnector.ConnectorType := qryZTSetFPhyDriver.AsString;
      SQLConnector.HostName := qryZTSetDBHostName.AsString;
      SQLConnector.DatabaseName := qryZTSetDBName.AsString;
      SQLConnector.CharSet := qryZTSetDBCharSet.AsString;
      if (qryZTSetDBHostPort.AsInteger > 0) then
      begin
        SQLConnector.params.Values['Port'] := qryZTSetDBHostPort.AsString;
      end;
      SQLConnector.UserName := qryZTSetDBUserName.AsString;
      SQLConnector.Password := qryZTSetDBUserPass.AsString;
      lList.Text := qryZTSetDBOtherParams.AsString;
      for i := 0 to lList.Count - 1 do
      begin
        SQLConnector.Params.Add(lList[i]);
      end;
      SQLConnector.Connected := True;
      if SQLConnector.Connected then
      begin
        ShowMessage('连接成功');
      end;
    except
      on e: Exception do
      begin
        ShowMessage('连接失败,返回原因:' + e.Message);
      end;
    end;
  finally
    lList.Free;
  end;

end;

procedure TfrmMain.tbZTSaveClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
  i: integer;
  lZTSet: TOneZTSet;
  IsMain: boolean;
begin

  if qryZTSet.State in dsEditModes then
    qryZTSet.Post;
  lOneGlobal := TOneGlobal.GetInstance();
  lOneGlobal.ZTMangeSet.AutoWork := edZTAutoStart.Checked;
  for i := 0 to lOneGlobal.ZTMangeSet.ZTSetList.Count - 1 do
  begin
    lZTSet := lOneGlobal.ZTMangeSet.ZTSetList[i];
    lZTSet.Free;
  end;
  lOneGlobal.ZTMangeSet.ZTSetList.Clear;
  try
    qryZTSet.DisableControls;
    qryZTSet.First;
    IsMain := False;
    while not qryZTSet.EOF do
    begin
      if qryZTSetFIsMain.AsBoolean then
      begin
        if IsMain then
        begin
          ShowMessage('主账套有且只能有一个,请检查');
          exit;
        end;
        IsMain := qryZTSetFIsMain.AsBoolean;
      end;
      qryZTSet.Next;
    end;
    qryZTSet.First;
    while not qryZTSet.EOF do
    begin
      lZTSet := TOneZTSet.Create;
      lOneGlobal.ZTMangeSet.ZTSetList.Add(lZTSet);
      lZTSet.ZTCode := qryZTSetFZTCode.AsString;
      lZTSet.ZTCode := lZTSet.ZTCode.Trim;
      lZTSet.ZTCaption := qryZTSetFZTCaption.AsString;
      lZTSet.InitPoolCount := qryZTSetFInitPoolCount.AsInteger;
      lZTSet.MaxPoolCount := qryZTSetFMaxPoolCount.AsInteger;
      lZTSet.PhyDriver := qryZTSetFPhyDriver.AsString;
      lZTSet.ConnectionStr := qryZTSetFConnectionStr.AsString;
      lZTSet.IsEnable := qryZTSetFIsEnable.AsBoolean;
      lZTSet.IsMain := qryZTSetFIsMain.AsBoolean;
      lZTSet.DBType := qryZTSetDBType.AsString;
      lZTSet.DBVersion := qryZTSetDBVersion.AsString;
      lZTSet.DBHostName := qryZTSetDBHostName.AsString;
      lZTSet.DBCharSet := qryZTSetDBCharSet.AsString;
      lZTSet.DBHostPort := qryZTSetDBHostPort.AsInteger;
      lZTSet.DBName := qryZTSetDBName.AsString;
      lZTSet.DBUserName := qryZTSetDBUserName.AsString;
      lZTSet.DBUserPass := qryZTSetDBUserPass.AsString;
      lZTSet.DBKeepConnect := qryZTSetDBKeepConnect.AsBoolean;
      lZTSet.DBOtherParams := qryZTSetDBOtherParams.AsString;
      qryZTSet.Next;
    end;
  finally
    qryZTSet.EnableControls;
  end;
  if lOneGlobal.SaveZTMangeSet(lErrMsg) then
  begin
    ShowMessage('保存账套配置成功!!!');
  end
  else
  begin
    ShowMessage(lErrMsg);
  end;
end;

procedure TfrmMain.tbZTSetOKClick(Sender: TObject);
var
  lDBType: string;
begin
  lDBType := qryZTSetDBType.AsString;
  if lDBType.StartsWith('MySQL') then
  begin
    if qryZTSetDBVersion.AsString = '' then
    begin
      ShowMessage('当驱动为[MySQL],请填写MySQL版本号,参考下面红色提示!!!');
      exit;
    end;
    lDBType := lDBType + ' ' + qryZTSetDBVersion.AsString;
  end;
  if lDBType.StartsWith('MSSQLServer') then
  begin
    if qryZTSetDBVersion.AsString = '' then
    begin
      ShowMessage('当驱动为[MSSQLServer],请填写MSSQLServer版本号,参考下面红色提示!!!');
      exit;
    end;
  end;
  qryZTSet.Edit;
  qryZTSetFPhyDriver.AsString := lDBType;
  qryZTSetFConnectionStr.AsString :=
    'HostName=' + qryZTSetDBHostName.AsString + ';UserName=' + qryZTSetDBUserName.AsString;
  qryZTSet.Post;
  plZTSet.Visible := False;
end;

procedure TfrmMain.tbZTStopClick(Sender: TObject);
var
  lErrMsg: string;
  lOneGlobal: TOneGlobal;
  lZTCode: string;
begin
  if qryZTPool.IsEmpty then
  begin
    ShowMessage('无相关运行账套，请先查看运行账套');
    exit;
  end;
  lZTCode := qryZTPoolFZTCode.AsString;
  lZTCode := lZTCode.Trim;
  if lZTCode.Trim = '' then
  begin
    ShowMessage('账套代码为空,无法操作数据');
    exit;
  end;
  lErrMsg := '确定停止运行当前账套[' + lZTCode + ']?';
  if not self.YesNoMsg('', lErrMsg) then
  begin
    exit;
  end;
  // 找到账套，打上标识stop
  lOneGlobal := TOneGlobal.GetInstance();
  if lOneGlobal.ZTManage.StopZT(lZTCode, True, lErrMsg) then
  begin
    qryZTPool.Edit;
    qryZTPoolFStop.AsBoolean := True;
    qryZTPool.Post;
    ShowMessage('账套代码[' + lZTCode + ']停止运行');
  end
  else
  begin
    ShowMessage('账套代码[' + lZTCode + ']' + lErrMsg);
  end;
end;

procedure TfrmMain.TrayIcon1Click(Sender: TObject);
begin
  self.Visible := True;
  self.WindowState := wsNormal;
  self.FormStyle := fsStayOnTop;
end;

finalization

end.
