unit frm_BaseStand;

{$mode ObjFPC}{$H+}
{公共标准窗体基类,主要处理窗体共用的方法以及事件}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, frm_BaseUI;

type

  { TfrmBaseStand }

  TfrmBaseStand = class(TfrmBaseUI)
    plTool: TPanel;
    tbClose: TBitBtn;
    tbNew: TBitBtn;
    tbEdit: TBitBtn;
    tbDel: TBitBtn;
    tbSave: TBitBtn;
    tbRefresh: TBitBtn;
    tbClose5: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
  private

  protected
    FCurrID: string;
  public
    procedure SetCurrID(Value: string); virtual;
    procedure SetButtonEnabled(); virtual;
  published
    property CurrID: string read FCurrID write SetCurrID;
  end;

var
  frmBaseStand: TfrmBaseStand;

implementation

{$R *.lfm}

{ TfrmBaseStand }

procedure TfrmBaseStand.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
end;


procedure TfrmBaseStand.tbCloseClick(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmBaseStand.SetCurrID(Value: string);
begin
  self.FCurrID := Value;
end;

procedure TfrmBaseStand.SetButtonEnabled();
begin

end;

end.
