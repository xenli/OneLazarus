unit frm_basestandmdi;

{$mode ObjFPC}{$H+}
{这个其实还是正常窗体,只是和主窗体PageControl交互,充当MDI窗体}
{因为真正的MDI窗体会闪烁，切换也会闪烁}
{MDI子窗体基类,所有的MDI子窗体多继承此窗体,处理与主窗体标签问题}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, frm_BaseStand;

type

  { TfrmStandMDI }

  TfrmStandMDI = class(TfrmBaseStand)
    plDoubleBuffered: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    procedure SetMDICaption(QCaption: string; QChangeTabCaption: boolean = True);
    procedure ChangeMainFormTabs();
  end;

var
  frmStandMDI: TfrmStandMDI;

implementation

{$R *.lfm}
uses AppManage;

procedure TfrmStandMDI.FormCreate(Sender: TObject);
begin
  try
    inherited FormCreate(Sender);
    self.BorderStyle := bsNone;
    self.FormStyle := fsNormal;
    self.WindowState := wsMaximized;
  finally
    AppManage.UnitAppManage().MDIFormCreateToMainForm(self);
  end;
end;

procedure TfrmStandMDI.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := cafree;
end;

procedure TfrmStandMDI.FormDestroy(Sender: TObject);
begin
  //窗体关闭移除主页Tabs标签
  AppManage.UnitAppManage().MDIFormCloseToMainForm(self);
end;


procedure TfrmStandMDI.SetMDICaption(QCaption: string; QChangeTabCaption: boolean = True);
begin
  self.Caption := QCaption;
  //窗体设置标签更改主页Tabs标签
  if QChangeTabCaption then
    AppManage.UnitAppManage().MDIFormCaptionToMainForm(self);
end;

procedure TfrmStandMDI.ChangeMainFormTabs();
begin
  AppManage.UnitAppManage().MDIFormBringToMainForm(Self);
end;

end.
