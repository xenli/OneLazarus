unit frm_AdminCenter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  frm_basestandmdi, Generics.Collections;

type

  { TfrmAdminCenter }

  TfrmAdminCenter = class(TfrmStandMDI)
    tbGroupMange: TBitBtn;
    tbRoleManage: TBitBtn;
    tbUserManage: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure tbGroupMangeClick(Sender: TObject);
    procedure tbRoleManageClick(Sender: TObject);
    procedure tbUserManageClick(Sender: TObject);
  private

  public

  end;

procedure ShowAdminCenter();

var
  frmAdminCenter: TfrmAdminCenter;

implementation

{$R *.lfm}
uses impl_form;

{ TfrmAdminCenter }


procedure ShowAdminCenter();
begin
  if Assigned(frmAdminCenter) then
  begin
    if not frmAdminCenter.Visible then
      frmAdminCenter.Visible := True;
    frmAdminCenter.BringToFront;
    frmAdminCenter.ChangeMainFormTabs();
  end
  else
  begin
    frmAdminCenter := TfrmAdminCenter.Create(nil);
    frmAdminCenter.Show;
  end;
end;

procedure TfrmAdminCenter.tbGroupMangeClick(Sender: TObject);
begin
  impl_form.ImplForm().ShowGroupManage();
end;

procedure TfrmAdminCenter.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  if frmAdminCenter <> nil then
    frmAdminCenter := nil;
end;

procedure TfrmAdminCenter.tbRoleManageClick(Sender: TObject);
begin
  impl_form.ImplForm().ShowRoleManage();
end;

procedure TfrmAdminCenter.tbUserManageClick(Sender: TObject);
begin
  impl_form.ImplForm().ShowAdminUserMange();
end;

end.
