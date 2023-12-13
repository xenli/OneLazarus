unit frm_YesNoMsg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, frm_BaseUI;

type

  { TfrmYesNoMsg }

  TfrmYesNoMsg = class(TfrmBaseUI)
    tbYes: TBitBtn;
    tbCancel: TBitBtn;
    Image1: TImage;
    edRemark: TMemo;
    Panel1: TPanel;
    procedure tbYesClick(Sender: TObject);
    procedure tbCancelClick(Sender: TObject);
  private

  public

  end;

function ShowYesNoMsg(aMsg: string; aTitle: string = ''): boolean;
procedure ShowMsg(aMsg: string; aTitle: string = '');

var
  frmYesNoMsg: TfrmYesNoMsg;

implementation

{$R *.lfm}
function ShowYesNoMsg(aMsg: string; aTitle: string = ''): boolean;
var
  lForm: TfrmYesNoMsg;
begin
  Result := False;
  if aTitle = '' then
    aTitle := '询问';
  lForm := TfrmYesNoMsg.Create(nil);
  try
    lForm.Caption := aTitle;
    lForm.edRemark.Lines.Text := aMsg;
    lForm.ShowModal;
    if lForm.ModalResult = mrOk then
      Result := True;
  finally
    lForm.Free;
  end;
end;

procedure ShowMsg(aMsg: string; aTitle: string = '');
var
  lForm: TfrmYesNoMsg;
begin
  if aTitle = '' then
    aTitle := '消息提醒';
  lForm := TfrmYesNoMsg.Create(nil);
  try
    lForm.Caption := aTitle;
    lForm.tbYes.Visible := False;
    lForm.tbCancel.Caption := '确定';
    lForm.edRemark.Lines.Text := aMsg;
    lForm.ShowModal;
  finally
    lForm.Free;
  end;
end;

{ TfrmYesNoMsg }

procedure TfrmYesNoMsg.tbYesClick(Sender: TObject);
begin
  self.ModalResult := mrOk;
end;

procedure TfrmYesNoMsg.tbCancelClick(Sender: TObject);
begin
  self.ModalResult := mrCancel;
end;

end.
