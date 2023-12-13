unit frm_BaseUI;

{$mode ObjFPC}{$H+}
{窗体基类,所有的窗体多继承此窗体，控制基本信息比如字体大小这类型的，样式，风格}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TfrmBaseUI }

  TfrmBaseUI = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    //每个窗体唯一标识
    FCreateID: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CreateID: string read FCreateID write FCreateID;
  end;

var
  frmBaseUI: TfrmBaseUI;

implementation

{$R *.lfm}
uses func_public;

procedure TfrmBaseUI.FormCreate(Sender: TObject);
begin

end;

constructor TfrmBaseUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreateID := func_public.GetGUIDStr();
end;

end.
