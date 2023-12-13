unit frm_GroupPick;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, laz.VirtualTrees,
  frm_BaseStand, OneClientDataSet, Generics.Collections, Admin_GroupInfo;

type

  { TfrmGroupPick }

  TfrmGroupPick = class(TfrmBaseStand)
    dsGroup: TDataSource;
    qryGroup: TOneDataSet;
    qryGroupFGroupCaption: TStringField;
    qryGroupFGroupCode: TStringField;
    qryGroupFGroupID: TStringField;
    qryGroupFGroupTreeCode: TStringField;
    qryGroupFGroupType: TStringField;
    qryGroupFPGroupID: TStringField;
    qryGroupFRemark: TStringField;
    TreeGroup: TLazVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure TreeGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FGroupData: PGroupTreeData;
  private
    procedure InitDataTree();
  public
    procedure SetCurrID(Value: string); override;
  end;



function ShowGroupPick(QGroupData: PGroupTreeData): boolean;

var
  frmGroupPick: TfrmGroupPick;

implementation

{$R *.lfm}
uses func_public;

function ShowGroupPick(QGroupData: PGroupTreeData): boolean;
begin
  try
    Result := False;
    frmGroupPick := TfrmGroupPick.Create(Application);
    frmGroupPick.FGroupData := QGroupData;
    frmGroupPick.CurrID := '';
    if frmGroupPick.ShowModal = mrOk then
    begin
      Result := True;
    end;
  finally
    FreeAndNil(frmGroupPick);
  end;
end;

{ TfrmGroupPick }

procedure TfrmGroupPick.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //CloseAction := TCloseAction.caFree;
end;

procedure TfrmGroupPick.FormCreate(Sender: TObject);
begin
  inherited;
  TreeGroup.NodeDataSize := SizeOf(TGroupTreeData);
end;

procedure TfrmGroupPick.tbNewClick(Sender: TObject);
var
  lNode: PVirtualNode;
  lNodeData: PGroupTreeData;
begin
  if qryGroup.IsEmpty then
    exit;

  lNode := TreeGroup.FocusedNode;
  if lNode = nil then
    exit;
  lNodeData := TreeGroup.GetNodeData(lNode);
  if not Assigned(lNodeData) then
  begin
    exit;
  end;
  Self.FGroupData^.FGroupID := lNodeData^.FGroupID;
  Self.FGroupData^.FGroupCaption := lNodeData^.FGroupCaption;
  ModalResult := mrOk;
end;

procedure TfrmGroupPick.tbRefreshClick(Sender: TObject);
begin
  self.CurrID := '';
end;

procedure TfrmGroupPick.TreeGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  lGroupTreeData: PGroupTreeData;
begin
  lGroupTreeData := Sender.GetNodeData(Node);
  if lGroupTreeData <> nil then
  begin
    if Column = 0 then
    begin
      CellText := lGroupTreeData^.FGroupTreeCode;
    end
    else
    if Column = 1 then
    begin
      CellText := lGroupTreeData^.FGroupCaption;
    end;
  end;
end;

procedure TfrmGroupPick.InitDataTree;
var
  lPNode, lNode: PVirtualNode;
  lGroupTreeData: PGroupTreeData;
  lNodeDict: specialize TDictionary<string, PVirtualNode>;
begin
  treeGroup.Clear;
  if qryGroup.IsEmpty then
  begin
    exit;
  end;
  //把DataSet转化成树形
  lNodeDict := specialize TDictionary<string, PVirtualNode>.Create;
  try
    qryGroup.First;
    while not qryGroup.EOF do
    begin
      lPNode := nil;
      //寻找是否有父节点
      lNodeDict.TryGetValue(qryGroupFPGroupID.AsString, lPNode);
      //添加节点
      lNode := TreeGroup.AddChild(lPNode);
      //获取节点数据,会自动创建一个指针大小和PGroupTreeData一至
      lGroupTreeData := TreeGroup.GetNodeData(lNode);
      lGroupTreeData^.FGroupID := qryGroupFGroupID.AsString;
      lGroupTreeData^.FGroupTreeCode := qryGroupFGroupTreeCode.AsString;
      lGroupTreeData^.FGroupCaption := qryGroupFGroupCaption.AsString;
      lNodeDict.Add(lGroupTreeData^.FGroupID, lNode);
      qryGroup.Next;
    end;
    treeGroup.FullExpand();
  finally
    lNodeDict.Clear;
    lNodeDict.Free;
  end;
end;

procedure TfrmGroupPick.SetCurrID(Value: string);
begin
  self.FCurrID := Value;
  if qryGroup.Active then
  begin
    qryGroup.Close;
  end;
  if not qryGroup.OpenData then
  begin
    func_public.OkMsg(qryGroup.DataInfo.ErrMsg);
    exit;
  end;
  self.InitDataTree();
end;

end.
