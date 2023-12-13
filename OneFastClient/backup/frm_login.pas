unit frm_login;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  frm_BaseUI;

type

  { TfrmLogin }

  TfrmLogin = class(TfrmBaseUI)
    StatusBar: TStatusBar;
    tbLogin: TButton;
    edUser: TEdit;
    edPass: TEdit;
    Image1: TImage;
    tbSet: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure tbLoginClick(Sender: TObject);
    procedure tbSetClick(Sender: TObject);
  private

  public

  end;

{ 对外公布的接口 }
function DoLogin(): boolean;

var
  frmLogin: TfrmLogin;

implementation

{$R *.lfm}
uses func_public, impl_form, AppManage, dm_ClientServer, client_userInfo, FastLoginController;

function DoLogin(): boolean;
begin
  try
    Result := False;
    frmLogin := TfrmLogin.Create(Application);
    frmLogin.Visible := False;
    with frmLogin do
    begin
      if ShowModal = mrOk then
      begin
        Result := True;
      end;
    end;
  finally
    FreeAndNil(frmLogin);
  end;
end;

{ TfrmLogin }

procedure TfrmLogin.tbLoginClick(Sender: TObject);
var
  vUserCode, vUserPass: string;
  lMyClientUserInfo: TClientUserInfo;
  lErrMsg: string;
  lFastLogin: TFastLogin;
begin
  // ModalResult := mrok;
  // exit;
  vUserCode := edUser.Text;
  vUserPass := edPass.Text;
  if Trim(vUserCode) = '' then
  begin
    func_public.OkMsg('用户账号为空');
    exit;
  end;
  if Trim(vUserPass) = '' then
  begin
    func_public.OkMsg('密码为空');
    exit;
  end;
  //连接服务器
  lMyClientUserInfo := AppManage.UnitAppManage().MyClientInfo;
  if (lMyClientUserInfo.HostCaption = '') or (lMyClientUserInfo.HostName = '') then
  begin
    func_public.OkMsg('未选中任何服务器,请先设置');
    exit;
  end;
  lErrMsg := '';
  if not dm_ClientServer.dmClientServer.ConnectServer(lErrMsg) then
  begin
    func_public.OkMsg(lErrMsg);
    exit;
  end;
  //接口式登陆，更安全
  //当然你也可以用 DatSet请求，把逻辑写在本地判断
  lFastLogin := TFastLogin.Create;
  try
    lFastLogin.loginCode := vUserCode;
    lFastLogin.loginPass := vUserPass;
    lFastLogin.secretkey := lMyClientUserInfo.HostKey;
    if not FastLoginController.Login(lFastLogin) then
    begin
      func_public.OkMsg(lFastLogin.errMsg);
      exit;
    end;
    lMyClientUserInfo.TokenID := lFastLogin.TokenID;
    lMyClientUserInfo.UserID := lFastLogin.adminID;
    lMyClientUserInfo.UserCode := lFastLogin.adminCode;
    lMyClientUserInfo.UserName := lFastLogin.adminName;
    if lMyClientUserInfo.UserCode = 'SuperAdmin' then
    begin
      lMyClientUserInfo.bSuperAdmin := True;
    end;
  finally
    lFastLogin.Free;
  end;

  self.ModalResult := mrOk;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
var
  lHostCaption: string;
begin
  inherited;
  lHostCaption := AppManage.UnitAppManage().MyClientSet.IndexServer.HostCaption;
  StatusBar.Panels[0].Text := '服务器:' + lHostCaption;
end;

procedure TfrmLogin.tbSetClick(Sender: TObject);
var
  lHostCaption: string;
begin
  if impl_form.ImplForm().ShowLoginSet then
  begin
    lHostCaption := AppManage.UnitAppManage().MyClientSet.IndexServer.HostCaption;
    StatusBar.Panels[0].Text := '服务器:' + lHostCaption;
  end;
end;

end.
