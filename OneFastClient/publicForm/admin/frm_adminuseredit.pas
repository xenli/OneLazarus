unit frm_AdminUserEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  DBExtCtrls, Buttons, EditBtn, DBDateTimePicker, frm_BaseStand,
  OneClientDataSet, OneCrypto;

type

  { TfrmAdminUserEdit }

  TfrmAdminUserEdit = class(TfrmBaseStand)
    tbPickGroup: TBitBtn;
    tbPickRole: TBitBtn;
    dbFLimtStartTime: TDBDateTimePicker;
    dbFLimtEndTime: TDBDateTimePicker;
    tbPassCancle: TBitBtn;
    tbPassChange: TBitBtn;
    dbFIsLimit: TDBCheckBox;
    dbFIsEnable: TDBCheckBox;
    dbFIsMultiLogin: TDBCheckBox;
    dbFAdminType: TDBComboBox;
    dbFAdminCode: TDBEdit;
    dbFAdminName: TDBEdit;
    dbFAdminPass: TDBEdit;
    dbFAdminTel: TDBEdit;
    dbFGroupCaption: TDBEdit;
    dbFRoleCaption: TDBEdit;
    dbFRemark: TDBEdit;
    dsAdmin: TDataSource;
    edOldPass: TEdit;
    edNewPass: TEdit;
    plPass: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    qryAdmin: TOneDataSet;
    qryAdminFAdminCode: TStringField;
    qryAdminFAdminID: TStringField;
    qryAdminFAdminName: TStringField;
    qryAdminFAdminPass: TStringField;
    qryAdminFAdminTel: TStringField;
    qryAdminFAdminType: TStringField;
    qryAdminFCreateTime: TDateTimeField;
    qryAdminFGroupCaption: TStringField;
    qryAdminFGroupID: TStringField;
    qryAdminFGroupTreeCode: TStringField;
    qryAdminFIsEnable: TBooleanField;
    qryAdminFIsLimit: TBooleanField;
    qryAdminFIsMultiLogin: TBooleanField;
    qryAdminFLimtEndTime: TDateTimeField;
    qryAdminFLimtStartTime: TDateTimeField;
    qryAdminFRemark: TStringField;
    qryAdminFRoleCaption: TStringField;
    qryAdminFRoleID: TStringField;
    tbAdminPassChange: TBitBtn;
    procedure dsAdminDataChange(Sender: TObject; Field: TField);
    procedure dsAdminStateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure qryAdminNewRecord(DataSet: TDataSet);
    procedure tbCloseClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbPassCancleClick(Sender: TObject);
    procedure tbPassChangeClick(Sender: TObject);
    procedure tbPickGroupClick(Sender: TObject);
    procedure tbPickRoleClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
  private

  public
    procedure SetCurrID(Value: string); override;
    procedure SetButtonEnabled(); override;
  end;

procedure ShowAdminUserEdit(QAdminID: string);

var
  frmAdminUserEdit: TfrmAdminUserEdit;

implementation

{$R *.lfm}
uses func_public, Admin_GroupInfo, impl_form;

{ TfrmAdminUserEdit }


procedure ShowAdminUserEdit(QAdminID: string);
begin
  if Assigned(frmAdminUserEdit) then
  begin
    if not frmAdminUserEdit.Visible then
      frmAdminUserEdit.Visible := True;
    frmAdminUserEdit.BringToFront;
  end
  else
  begin
    frmAdminUserEdit := TfrmAdminUserEdit.Create(nil);
    frmAdminUserEdit.Show;
    frmAdminUserEdit.CurrID := QAdminID;
  end;
end;

procedure TfrmAdminUserEdit.tbCloseClick(Sender: TObject);
begin
  inherited;
end;

procedure TfrmAdminUserEdit.tbNewClick(Sender: TObject);
begin
  self.CurrID := '';
end;

procedure TfrmAdminUserEdit.tbPassCancleClick(Sender: TObject);
begin
  plPass.Visible := False;
end;

procedure TfrmAdminUserEdit.tbPassChangeClick(Sender: TObject);
var
  lOldPass, lNewPass: string;
  lSQL: string;
begin
  inherited;

  lOldPass := edOldPass.Text;
  lOldPass := lOldPass.Trim;
  lNewPass := edNewPass.Text;
  lNewPass := lNewPass.Trim;
  if lOldPass = '' then
  begin
    func_public.OkMsg('旧密码不可为空!');
    exit;
  end;
  if lOldPass = '' then
  begin
    func_public.OkMsg('旧密码不可为空!');
    exit;
  end;

  if lNewPass = '' then
  begin
    func_public.OkMsg('新密码不可为空!');
    exit;
  end;
  if not func_public.YesNoMsg('确定更改当前用户密码?') then
  begin
    exit;
  end;
  lOldPass := OneCrypto.MD5Endcode(lOldPass);
  lNewPass := OneCrypto.MD5Endcode(lNewPass);
  if lOldPass.ToLower <> qryAdminFAdminPass.AsString.ToLower then
  begin
    func_public.OkMsg('旧密码错误,无法修改密码!');
    exit;
  end;
  lSQL := 'update onefast_admin set FAdminPass=:NewPass where FAdminID=:FAdminID and FAdminPass=:OldPass ';
  if not qryAdmin.ExecDMLSQL(lSQL, [lNewPass, qryAdminFAdminID.AsString, lOldPass], True) then
  begin
    func_public.OkMsg(qryAdmin.DataInfo.ErrMsg);
    exit;
  end;
  plPass.Visible := False;
  func_public.OkMsg('密码修改成功!');
  self.CurrID := FCurrID;
end;

procedure TfrmAdminUserEdit.tbPickGroupClick(Sender: TObject);
var
  lGroupData: PGroupTreeData;
begin
  new(lGroupData);
  try
    if impl_form.ImplForm().ShowGroupPick(lGroupData) then
    begin
      qryAdmin.Edit;
      qryAdminFGroupID.AsString := lGroupData^.FGroupID;
      qryAdminFGroupCaption.AsString := lGroupData^.FGroupCaption;
      qryAdmin.Post;
    end;
  finally
    dispose(lGroupData)
  end;

end;

procedure TfrmAdminUserEdit.tbPickRoleClick(Sender: TObject);
var
  lRoleData: PRoleTreeData;
begin
  new(lRoleData);
  try
    if impl_form.ImplForm().ShowRolePick(lRoleData) then
    begin
      qryAdmin.Edit;
      qryAdminFRoleID.AsString := lRoleData^.FRoleID;
      qryAdminFRoleCaption.AsString := lRoleData^.FRoleCaption;
      qryAdmin.Post;
    end;
  finally
    dispose(lRoleData)
  end;

end;

procedure TfrmAdminUserEdit.tbRefreshClick(Sender: TObject);
begin
  if tbRefresh.Caption = '刷新' then
  begin
    self.CurrID := self.FCurrID;
  end
  else
  begin
    if func_public.YesNoMsg('确定取消数据编辑?') then
    begin
      self.CurrID := self.FCurrID;
    end;
  end;

end;

procedure TfrmAdminUserEdit.tbSaveClick(Sender: TObject);
var
  lCheckSQL: string;
begin
  inherited;
  if qryAdmin.State in dsEditModes then
    qryAdmin.post;
  if qryAdminFAdminCode.AsString = '' then
  begin
    func_public.OkMsg('登陆代码不可为空');
    exit;
  end;
  if FCurrID = '' then
  begin
    if not func_public.YesNoMsg('确定登陆代码为[' + qryAdminFAdminCode.AsString + ']一旦保存不可修改') then
      exit;
  end;
  // 检重复
  lCheckSQL := 'select FAdminID from onefast_admin where FAdminCode=:FAdminCode';
  if qryAdmin.CheckRepeat(lCheckSQL, [qryAdminFAdminCode.AsString], qryAdminFAdminID.AsString) then
  begin
    func_public.OkMsg(qryAdmin.DataInfo.ErrMsg);
    exit;
  end;
  if not qryAdmin.SaveData() then
  begin
    func_public.OkMsg(qryAdmin.DataInfo.ErrMsg);
    exit;
  end
  else
  begin
    func_public.OkMsg('保存数据成功');
  end;
  // 保存成功重新打开数据
  self.CurrID := qryAdminFAdminID.AsString;
end;

procedure TfrmAdminUserEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
  if frmAdminUserEdit <> nil then
    frmAdminUserEdit := nil;
end;

procedure TfrmAdminUserEdit.dsAdminStateChange(Sender: TObject);
begin
  self.SetButtonEnabled();
end;

procedure TfrmAdminUserEdit.dsAdminDataChange(Sender: TObject; Field: TField);
begin

end;

procedure TfrmAdminUserEdit.qryAdminNewRecord(DataSet: TDataSet);
begin
  qryAdminFAdminID.AsString := func_public.GetGUIDStr();
  qryAdminFAdminPass.AsString := OneCrypto.MD5Endcode('123456');
end;

procedure TfrmAdminUserEdit.SetCurrID(Value: string);
begin
  self.FCurrID := Value;
  if qryAdmin.Active then
    qryAdmin.Close;
  qryAdmin.Params[0].AsString := self.FCurrID;
  if not qryAdmin.Open then
  begin
    func_public.OkMsg(qryAdmin.DataInfo.ErrMsg);
    exit;
  end;
  // 新建一条记录
  if self.FCurrID = '' then
    qryAdmin.Append;
end;

procedure TfrmAdminUserEdit.SetButtonEnabled();
var
  isEdit: boolean;
begin
  inherited;

  isEdit := qryAdmin.IsEdit();
  tbNew.Enabled := not isEdit;
  tbAdminPassChange.Enabled := not isEdit;
  tbSave.Enabled := isEdit;
  if isEdit then
  begin
    tbRefresh.Caption := '取消';
  end
  else
  begin
    tbRefresh.Caption := '刷新';
  end;
  dbFAdminCode.Enabled := (FCurrID = '');
end;

end.
