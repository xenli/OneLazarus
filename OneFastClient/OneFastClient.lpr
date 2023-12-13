program OneFastClient;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
                           {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
                           {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols,
  memdslaz,
  datetimectrls,
  frm_Main,
  dm_img,
  frm_BaseUI,
  frm_BaseStand,
  AppManage,
  func_public,
  frm_basestandmdi,
  frm_login,
  frm_YesNoMsg,
  impl_form,
  frm_UserCenter,
  frm_LoginSet,
  Client_UserInfo,
  client_SetInfo,
  public_type,
  frm_AdminCenter,
  dm_ClientServer,
  frm_AdminUserManage,
  frm_GroupManage,
  FastLoginController,
  frm_RoleManage,
  frm_AdminUserEdit,
  frm_Wait,
  frm_GroupPick,
  admin_GroupInfo, frm_RolePick;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TdmImg, dmImg);
  Application.CreateForm(TdmClientServer, dmClientServer);
  //全局类的东东初始化
  AppManage.UnitAppManage();
  if frm_login.DoLogin() then
  begin
    Application.CreateForm(TfrmMain, frmMain);
    Application.Run;
  end
  else
  begin
    exit;
  end;
end.
