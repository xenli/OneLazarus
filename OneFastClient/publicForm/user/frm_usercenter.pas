unit frm_UserCenter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DBCtrls, StdCtrls, frm_basestandmdi, OneClientDataSet;

type

  { TfrmUserCenter }

  TfrmUserCenter = class(TfrmStandMDI)
    BitBtn1: TBitBtn;
    tbShowPassChange: TBitBtn;
    BitBtn3: TBitBtn;
    tbChangePass: TBitBtn;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBCheckBox3: TDBCheckBox;
    DBEdit1: TDBEdit;
    DBEdit10: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    DBEdit7: TDBEdit;
    DBEdit8: TDBEdit;
    DBEdit9: TDBEdit;
    dsAdmin: TDataSource;
    edOldPass: TEdit;
    edOldPass1: TEdit;
    groupBoxPassChange: TGroupBox;
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
    plUser: TPanel;
    tbChangePass1: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure tbClose1Click(Sender: TObject);
    procedure tbShowPassChangeClick(Sender: TObject);
  private

  public

  end;

procedure ShowUserCenter();

var
  frmUserCenter: TfrmUserCenter;

implementation

{$R *.lfm}

procedure ShowUserCenter();
begin
  if Assigned(frmUserCenter) then
  begin
    frmUserCenter.Visible := True;
    frmUserCenter.BringToFront;
    frmUserCenter.ChangeMainFormTabs();
  end
  else
  begin
    frmUserCenter := TfrmUserCenter.Create(nil);
    frmUserCenter.Show;
    frmUserCenter.CurrID := '';
  end;
end;

{ TfrmUserCenter }

procedure TfrmUserCenter.tbClose1Click(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmUserCenter.tbShowPassChangeClick(Sender: TObject);
begin
  groupBoxPassChange.Visible := not groupBoxPassChange.Visible;
end;

procedure TfrmUserCenter.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  frmUserCenter := nil;
end;

end.
