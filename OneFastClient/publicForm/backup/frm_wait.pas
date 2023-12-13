unit frm_Wait;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, frm_BaseUI;

type

  { TfrmWait }

  TfrmWait = class(TfrmBaseUI)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

procedure ShowWaitForm();
procedure CloseWaitForm();

var
  frmWait: TfrmWait;

implementation

{$R *.lfm}
procedure ShowWaitForm();
begin
  if Assigned(frmWait) then
  begin
    if not frmWait.Visible then
      frmWait.Visible := True;
    frmWait.BringToFront;
  end
  else
  begin
    frmWait := TfrmWait.Create(nil);
    frmWait.Show;
  end;
end;

procedure CloseWaitForm();
begin
  if Assigned(frmWait) then
  begin
    frmWait.Close;
    frmWait := nil;
  end;
end;

{ TfrmWait }

procedure TfrmWait.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;

end;

end.
