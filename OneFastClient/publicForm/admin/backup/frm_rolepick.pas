unit frm_RolePick;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, laz.VirtualTrees,
  frm_BaseStand, OneClientDataSet, Admin_GroupInfo, Generics.Collections;

type

  { TfrmRolePick }

  TfrmRolePick = class(TfrmBaseStand)
    dsRole: TDataSource;
    qryRole: TOneDataSet;
    qryRoleFPRoleID: TStringField;
    qryRoleFRemark: TStringField;
    qryRoleFRoleCaption: TStringField;
    qryRoleFRoleCode: TStringField;
    qryRoleFRoleID: TStringField;
    qryRoleFRoleTreeCode: TStringField;
    TreeRole: TLazVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure TreeRoleGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeRoleNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    FRoleData: PRoleTreeData;
  private
    procedure InitDataTree();
  public
    procedure SetCurrID(Value: string); override;
  end;

function ShowRolePick(QRoleData: PRoleTreeData): boolean;

var
  frmRolePick: TfrmRolePick;

implementation

{$R *.lfm}
uses func_public;

function ShowRolePick(QRoleData: PRoleTreeData): boolean;
begin
  try
    Result := False;
    frmRolePick := TfrmRolePick.Create(Application);
    frmRolePick.FRoleData := QRoleData;
    frmRolePick.CurrID := '';
    if frmRolePick.ShowModal = mrOk then
    begin
      Result := True;
    end;
  finally
    FreeAndNil(frmRolePick);
  end;
end;

{ TfrmRolePick }

procedure TfrmRolePick.FormCreate(Sender: TObject);
begin
  inherited;
  TreeRole.NodeDataSize := SizeOf(TRoleTreeData);
end;

procedure TfrmRolePick.tbNewClick(Sender: TObject);
var
  lNode: PVirtualNode;
  lNodeData: PRoleTreeData;
begin
  if qryRole.IsEmpty then
    exit;

  lNode := TreeRole.FocusedNode;
  if lNode = nil then
    exit;
  lNodeData := TreeRole.GetNodeData(lNode);
  if not Assigned(lNodeData) then
  begin
    exit;
  end;
  Self.FRoleData^.FRoleID := lNodeData^.FRoleID;
  Self.FRoleData^.FRoleCaption := lNodeData^.FRoleCaption;
  ModalResult := mrOk;
end;

procedure TfrmRolePick.tbRefreshClick(Sender: TObject);
begin
  self.CurrID := '';
end;

procedure TfrmRolePick.TreeRoleGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  lRoleTreeData: PRoleTreeData;
begin
  lRoleTreeData := Sender.GetNodeData(Node);
  if lRoleTreeData <> nil then
  begin
    if Column = 0 then
    begin
      CellText := lRoleTreeData^.FRoleTreeCode;
    end
    else
    if Column = 1 then
    begin
      CellText := lRoleTreeData^.FRoleCaption;
    end;
  end;
end;

procedure TfrmRolePick.TreeRoleNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  tbNewClick(tbNew);
end;

procedure TfrmRolePick.InitDataTree;
var
  lPNode, lNode: PVirtualNode;
  lRoleTreeData: PRoleTreeData;
  lNodeDict: specialize TDictionary<string, PVirtualNode>;
begin
  treeRole.Clear;
  if qryRole.IsEmpty then
  begin
    exit;
  end;
  //把DataSet转化成树形
  lNodeDict := specialize TDictionary<string, PVirtualNode>.Create;
  try
    qryRole.First;
    while not qryRole.EOF do
    begin
      lPNode := nil;
      //寻找是否有父节点
      lNodeDict.TryGetValue(qryRoleFPRoleID.AsString, lPNode);
      //添加节点
      lNode := TreeRole.AddChild(lPNode);
      //获取节点数据,会自动创建一个指针大小和PGroupTreeData一至
      lRoleTreeData := TreeRole.GetNodeData(lNode);
      lRoleTreeData^.FRoleID := qryRoleFRoleID.AsString;
      lRoleTreeData^.FRoleTreeCode := qryRoleFRoleTreeCode.AsString;
      lRoleTreeData^.FRoleCaption := qryRoleFRoleCaption.AsString;
      lNodeDict.Add(lRoleTreeData^.FRoleID, lNode);
      qryRole.Next;
    end;
    treeRole.FullExpand();
  finally
    lNodeDict.Clear;
    lNodeDict.Free;
  end;
end;

procedure TfrmRolePick.SetCurrID(Value: string);
begin
  self.FCurrID := Value;
  if qryRole.Active then
  begin
    qryRole.Close;
  end;
  if not qryRole.OpenData then
  begin
    func_public.OkMsg(qryRole.DataInfo.ErrMsg);
    exit;
  end;
  self.InitDataTree();
end;

end.
