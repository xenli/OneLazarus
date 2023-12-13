unit frm_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, frm_BaseUI, public_type;

type

  { TfrmMain }

  TfrmMain = class(TfrmBaseUI)
    Label1: TLabel;
    tabMenu: TPageControl;
    plHeadMenu: TPanel;
    plHead: TPanel;
    ScrollBox1: TScrollBox;
    tbClose: TBitBtn;
    Image1: TImage;
    plMainTop: TPanel;
    plMainLeft: TPanel;
    tbClose1: TBitBtn;
    tbUserCenter: TBitBtn;
    tbAdmin: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tabMenuChange(Sender: TObject);
    procedure tbAdminClick(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
    procedure tbUserCenterClick(Sender: TObject);
  private
    //MDI窗体回调事件
    procedure MDIFormCallBack(Sender: TForm; QFormState: TMDIFormCallBackState);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }
uses frm_baseStandMdi, func_public, AppManage, impl_form;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //释放所有窗体
  AppManage.UnitAppManage().FreeAllForm();

  CloseAction := TCloseAction.caFree;
  if frmMain <> nil then
  begin
    frmMain := nil;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not func_public.YesNoMsg('是否退出程序') then
    CanClose := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  self.WindowState := wsMaximized;
  AppManage.UnitAppManage().OnMDIToMainFormCallBack := @self.MDIFormCallBack;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmMain.MDIFormCallBack(Sender: TForm; QFormState: TMDIFormCallBackState);
var
  lTabSheet: TTabSheet;
  lfrmBaseUI: TfrmBaseUI;
  i: integer;
begin

  if not (Sender is TfrmBaseUI) then
  begin
    ShowMessage('窗体最初基类必需继承[frm_BaseUI.TfrmBaseUI]');
    exit;
  end;
  lfrmBaseUI := TfrmBaseUI(Sender);
  if QFormState = StateCreate then
  begin
    lTabSheet := self.tabMenu.AddTabSheet;
    lTabSheet.Caption := Sender.Caption;
    lTabSheet.Hint := lfrmBaseUI.CreateID;
    Sender.Parent := lTabSheet;
    Sender.Align := TAlign.alClient;
    self.tabMenu.ActivePage := lTabSheet;
  end
  else if QFormState = StateClose then
  begin
    for i := self.tabMenu.PageCount - 1 downto 0 do
    begin
      if self.tabMenu.Pages[i].Hint = lfrmBaseUI.CreateID then
      begin
        self.tabMenu.Pages[i].Free;
        break;
      end;
    end;
  end
  else if QFormState = StateShow then
  begin
    for i := self.tabMenu.PageCount - 1 downto 0 do
    begin
      if self.tabMenu.Pages[i].Hint = lfrmBaseUI.CreateID then
      begin
        self.tabMenu.ActivePageIndex := i;
        break;
      end;
    end;
  end
  else if QFormState = StateCaption then
  begin
    for i := self.tabMenu.PageCount - 1 downto 0 do
    begin
      if self.tabMenu.Pages[i].Hint = lfrmBaseUI.CreateID then
      begin
        self.tabMenu.Pages[i].Caption := lfrmBaseUI.Caption;
        break;
      end;
    end;
  end;

end;

procedure TfrmMain.tabMenuChange(Sender: TObject);
var
  lCreateID: string;
begin
  //lCreateID := self.tabMenu.ActivePage.Hint;
  //切换MDI窗体
  //AppManage.UnitAppManage().MDIFormBringToFront(lCreateID);
  //self.tabMenu.Refresh;
  // self.Refresh;
end;



procedure TfrmMain.tbAdminClick(Sender: TObject);
begin
  impl_form.ImplForm().ShowAdminCenter();
end;


procedure TfrmMain.tbCloseClick(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmMain.tbUserCenterClick(Sender: TObject);
begin
  impl_form.ImplForm().ShowUserCenter();
end;

end.
