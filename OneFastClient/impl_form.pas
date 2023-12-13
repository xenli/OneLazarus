unit impl_form;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Admin_GroupInfo;

type
  TImplForm = class
  public
    {用户登陆界面}
    function DoLogin(): boolean;
    {用户设置登陆服务器配置}
    function ShowLoginSet: boolean;
    {用户中心}
    procedure ShowUserCenter();
    {管理员中心}
    procedure ShowAdminCenter();
    {组织架构管理}
    procedure ShowGroupManage();
    function ShowGroupPick(QGroupData: PGroupTreeData): boolean;
    {角色管理}
    procedure ShowRoleManage();
    function ShowRolePick(QRoleData: PRoleTreeData): boolean;
    {操作员管理}
    procedure ShowAdminUserMange();
    {操作员用户编辑}
    procedure ShowAdminUserEdit(QAdminID: string);
    { 提醒界面-消息 }
    procedure ShowMsg(aMsg: string; aTitle: string = '');
    { 提醒界面 -确认 }
    function ShowYesNoMsg(aMsg: string; aTitle: string = ''): boolean;

    procedure ShowWaitForm();
    procedure CloseWaitForm();
  end;

function ImplForm(): TImplForm;

implementation

uses frm_Login, frm_LoginSet, frm_YesNoMsg, frm_UserCenter, frm_AdminCenter,
  frm_GroupManage, frm_Wait, frm_GroupPick, frm_RolePick,
  frm_RoleManage, frm_AdminUserManage, frm_AdminUserEdit;

var
  unit_impForm: TImplForm = nil;

function ImplForm(): TImplForm;
begin
  if unit_impForm = nil then
    unit_impForm := TImplForm.Create;
  Result := unit_impForm;
end;


// **********用户**************
function TImplForm.DoLogin(): boolean;
begin
  Result := frm_Login.DoLogin();
end;

function TImplForm.ShowLoginSet: boolean;
begin
  Result := frm_LoginSet.ShowLoginSet();
end;

procedure TImplForm.ShowUserCenter();
begin
  frm_UserCenter.ShowUserCenter();
end;

{管理员中心}
procedure TImplForm.ShowAdminCenter();
begin
  frm_AdminCenter.ShowAdminCenter();
end;

procedure TImplForm.ShowGroupManage();
begin
  frm_GroupManage.ShowGroupManage();
end;

function TImplForm.ShowGroupPick(QGroupData: PGroupTreeData): boolean;
begin
  Result := frm_GroupPick.ShowGroupPick(QGroupData);
end;

procedure TImplForm.ShowRoleManage();
begin
  frm_RoleManage.ShowRoleManage();
end;

function TImplForm.ShowRolePick(QRoleData: PRoleTreeData): boolean;
begin
  Result := frm_RolePick.ShowRolePick(QRoleData);
end;

procedure TImplForm.ShowAdminUserMange();
begin
  frm_AdminUserManage.ShowAdminUserMange();
end;

procedure TImplForm.ShowAdminUserEdit(QAdminID: string);
begin
  frm_AdminUserEdit.ShowAdminUserEdit(QAdminID);
end;

procedure TImplForm.ShowMsg(aMsg: string; aTitle: string = '');
begin
  frm_YesNoMsg.ShowMsg(aMsg, aTitle);
end;

function TImplForm.ShowYesNoMsg(aMsg: string; aTitle: string = ''): boolean;
begin
  Result := frm_YesNoMsg.ShowYesNoMsg(aMsg, aTitle);
end;

procedure TImplForm.ShowWaitForm();
begin
  frm_Wait.ShowWaitForm();
end;

procedure TImplForm.CloseWaitForm();
begin
  frm_Wait.CloseWaitForm();
end;

initialization

finalization
  if unit_impForm <> nil then
  begin
    unit_impForm.Free;
  end;
end.
