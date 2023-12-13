unit public_type;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  //StateCreate创建MDI窗体，StateClose关闭MDI窗体，StateCaption改变窗体标签名称，StateBring显示MDI窗体
  TMDIFormCallBackState = (StateCreate, StateClose, StateCaption, StateShow);
  TMDIFormCallBackEven = procedure(Sender: TForm; QFormState: TMDIFormCallBackState) of object;

implementation

end.
