unit frm_GroupManage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, DBGrids, StdCtrls, DBCtrls,
  laz.VirtualTrees, frm_basestandmdi, OneClientDataSet, DB, Generics.Collections, Admin_GroupInfo;

type

  { TfrmGroupManage }

  TfrmGroupManage = class(TfrmStandMDI)
    dbFGroupType: TDBComboBox;
    dbFGroupCode: TDBEdit;
    dbFGroupCaption: TDBEdit;
    dbFGroupTreeCode: TDBEdit;
    dbFRemark: TDBEdit;
    dsGroup: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    qryGroup: TOneDataSet;
    qryGroupFGroupCaption: TStringField;
    qryGroupFGroupCode: TStringField;
    qryGroupFGroupID: TStringField;
    qryGroupFGroupTreeCode: TStringField;
    qryGroupFGroupType: TStringField;
    qryGroupFPGroupID: TStringField;
    qryGroupFRemark: TStringField;
    TreeGroup: TLazVirtualStringTree;
    procedure dsGroupStateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure qryGroupNewRecord(DataSet: TDataSet);
    procedure tbEditClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure TreeGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeGroupNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    FNodeDict: specialize TDictionary<string, PVirtualNode>;
  private
    procedure InitDataTree();
    procedure SaveTreeCode();
  public
    procedure SetCurrID(Value: string); override;
    procedure SetButtonEnabled(); override;
  end;

procedure ShowGroupManage();

var
  frmGroupManage: TfrmGroupManage;

implementation

{$R *.lfm}
uses func_public;

{ TfrmGroupManage }


procedure ShowGroupManage();
begin
  if Assigned(frmGroupManage) then
  begin
    if not frmGroupManage.Visible then
      frmGroupManage.Visible := True;
    frmGroupManage.BringToFront;
    frmGroupManage.ChangeMainFormTabs();
  end
  else
  begin
    frmGroupManage := TfrmGroupManage.Create(Application);
    frmGroupManage.Show;
    frmGroupManage.CurrID := '';
  end;
end;


procedure TfrmGroupManage.FormCreate(Sender: TObject);
begin
  inherited;
  FNodeDict := specialize TDictionary<string, PVirtualNode>.Create;
  TreeGroup.NodeDataSize := SizeOf(TGroupTreeData);
end;

procedure TfrmGroupManage.qryGroupNewRecord(DataSet: TDataSet);
begin
  qryGroupFGroupID.AsString := func_public.GetGUIDStr();
  qryGroupFPGroupID.AsString := '-1';
end;

procedure TfrmGroupManage.tbEditClick(Sender: TObject);
var
  lParentNode, lNode: PVirtualNode;
  lGroupTreeData: PGroupTreeData;
begin
  inherited;
  if not qryGroup.Active then
  begin
    func_public.OkMsg('数据集未打开,请先打开数据');
    exit;
  end;
  lParentNode := nil;
  lParentNode := TreeGroup.FocusedNode;
  lNode := TreeGroup.AddChild(lParentNode);
  qryGroup.Append;
  qryGroupFGroupCaption.AsString := '新的节点';
  //赋值父节点ID
  if lParentNode <> nil then
  begin
    lGroupTreeData := TreeGroup.GetNodeData(lParentNode);
    if Assigned(lGroupTreeData) then
    begin
      qryGroupFPGroupID.AsString := lGroupTreeData^.FGroupID;
    end;
  end;
  qryGroup.Post;
  //为新增节点赋值相关数据
  lGroupTreeData := TreeGroup.GetNodeData(lNode);
  lGroupTreeData^.FGroupID := qryGroupFGroupID.AsString;
  lGroupTreeData^.FGroupTreeCode := '';
  lGroupTreeData^.FGroupCaption := qryGroupFGroupCaption.AsString;
  self.FNodeDict.add(lGroupTreeData^.FGroupID, lNode);
end;

procedure TfrmGroupManage.tbNewClick(Sender: TObject);
var
  lFocusedNode, lParentNode, lNode: PVirtualNode;
  lGroupTreeData: PGroupTreeData;
begin
  inherited;
  if not qryGroup.Active then
  begin
    func_public.OkMsg('数据集未打开,请先打开数据');
    exit;
  end;
  lParentNode := nil;
  lFocusedNode := TreeGroup.FocusedNode;
  if lFocusedNode <> nil then
  begin
    //选中节点，建立选中节点同级节点
    lParentNode := lFocusedNode^.Parent;
  end;
  lNode := TreeGroup.AddChild(lParentNode);
  qryGroup.Append;
  qryGroupFGroupCaption.AsString := '新的节点';
  //赋值父节点ID
  if lParentNode <> nil then
  begin
    lGroupTreeData := TreeGroup.GetNodeData(lParentNode);
    if Assigned(lGroupTreeData) then
    begin
      qryGroupFPGroupID.AsString := lGroupTreeData^.FGroupID;
    end;
  end;
  qryGroup.Post;
  //为新增节点赋值相关数据
  lGroupTreeData := TreeGroup.GetNodeData(lNode);
  lGroupTreeData^.FGroupID := qryGroupFGroupID.AsString;
  lGroupTreeData^.FGroupTreeCode := '';
  lGroupTreeData^.FGroupCaption := qryGroupFGroupCaption.AsString;
  self.FNodeDict.add(lGroupTreeData^.FGroupID, lNode);
end;

procedure TfrmGroupManage.tbRefreshClick(Sender: TObject);
begin
  self.CurrID := self.FCurrID;
end;

procedure TfrmGroupManage.tbSaveClick(Sender: TObject);
begin
  if qryGroup.State in dsEditModes then
    qryGroup.Post;
  SaveTreeCode;
  if qryGroup.SaveData then
  begin
    func_public.OkMsg('保存数据成功');
    CurrID := FCurrID;
  end
  else
  begin
    func_public.OkMsg(qryGroup.DataInfo.ErrMsg);
  end;
end;

procedure TfrmGroupManage.TreeGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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

procedure TfrmGroupManage.TreeGroupNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  lNode: PVirtualNode;
  lNodeData: PGroupTreeData;
begin
  lNode := Sender.FocusedNode;
  if not Assigned(lNode) then
  begin
    exit;
  end;
  lNodeData := Sender.GetNodeData(lNode);
  if not Assigned(lNodeData) then
  begin
    exit;
  end;
  qryGroup.Locate('FGroupID', lNodeData^.FGroupID, []);
end;

procedure TfrmGroupManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  if frmGroupManage <> nil then
    frmGroupManage := nil;
  FNodeDict.Clear;
  FNodeDict.Free;
end;

procedure TfrmGroupManage.dsGroupStateChange(Sender: TObject);
begin
  self.SetButtonEnabled();
end;

procedure TfrmGroupManage.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TfrmGroupManage.SetCurrID(Value: string);
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
  qryGroup.First;
end;

procedure TfrmGroupManage.SetButtonEnabled();
var
  isEdit: boolean;
begin
  isEdit := qryGroup.IsEdit();
  tbSave.Enabled := isEdit;
  if isEdit then
  begin
    tbRefresh.Caption := '取消';
  end
  else
  begin
    tbRefresh.Caption := '刷新';
  end;
end;

procedure TfrmGroupManage.InitDataTree();
var
  lPNode, lNode: PVirtualNode;
  lGroupTreeData: PGroupTreeData;
begin
  treeGroup.Clear;
  if qryGroup.IsEmpty then
  begin
    exit;
  end;
  //把DataSet转化成树形
  FNodeDict.Clear;
  qryGroup.First;
  while not qryGroup.EOF do
  begin
    lPNode := nil;
    //寻找是否有父节点
    FNodeDict.TryGetValue(qryGroupFPGroupID.AsString, lPNode);
    //添加节点
    lNode := TreeGroup.AddChild(lPNode);
    //获取节点数据,会自动创建一个指针大小和PGroupTreeData一至
    lGroupTreeData := TreeGroup.GetNodeData(lNode);
    lGroupTreeData^.FGroupID := qryGroupFGroupID.AsString;
    lGroupTreeData^.FGroupTreeCode := qryGroupFGroupTreeCode.AsString;
    lGroupTreeData^.FGroupCaption := qryGroupFGroupCaption.AsString;
    FNodeDict.Add(lGroupTreeData^.FGroupID, lNode);
    qryGroup.Next;
  end;
  treeGroup.FullExpand();
end;

procedure TfrmGroupManage.SaveTreeCode();
var
  iNode: integer;
  lRootNode: PVirtualNode;

  procedure DataNodeEdit(QNode: PVirtualNode; QIndexNode: integer; QPNode: PVirtualNode);
  var
    vTreeCode: string;
    lTreeData, lPTreeData: PGroupTreeData;
  begin
    lTreeData := TreeGroup.GetNodeData(QNode);
    if lTreeData <> nil then
    begin
      vTreeCode := (QIndexNode + 1).toString;
      if vTreeCode.Length < 2 then
        vTreeCode := '0' + vTreeCode;
      if QPNode <> nil then
      begin
        lPTreeData := TreeGroup.GetNodeData(QPNode);
        if Assigned(lPTreeData) then
        begin
          if qryGroup.Locate('FGroupID', lPTreeData^.FGroupID, []) then
          begin
            vTreeCode := qryGroupFGroupTreeCode.AsString + '.' + vTreeCode;
          end;
        end;
      end;
      if qryGroup.Locate('FGroupID', lTreeData^.FGroupID, []) then
      begin
        qryGroup.Edit;
        qryGroupFGroupTreeCode.AsString := vTreeCode;
        qryGroup.Post;
      end;
    end;
  end;

  procedure DataSaveNodeChild(QPNode: PVirtualNode);
  var
    iTemp: integer;
    lChildNode: PVirtualNode;
  begin
    lChildNode := TreeGroup.GetFirstChild(QPNode);
    iTemp := 0;
    while Assigned(lChildNode) do
    begin
      DataNodeEdit(lChildNode, iTemp, QPNode);
      { 进行递归 }
      DataSaveNodeChild(lChildNode);
      iTemp := iTemp + 1;
      lChildNode := TreeGroup.GetNextSibling(lChildNode);
    end;
  end;

begin
  if not qryGroup.Active then
    exit;

  qryGroup.DisableControls;
  try
    lRootNode := TreeGroup.RootNode;
    DataSaveNodeChild(lRootNode);
  finally
    qryGroup.EnableControls;
  end;
end;

end.
