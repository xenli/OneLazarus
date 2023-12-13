unit frm_RoleManage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  ExtCtrls, laz.VirtualTrees, Generics.Collections, frm_basestandmdi,
  OneClientDataSet, Admin_GroupInfo;

type

  { TfrmRoleManage }

  TfrmRoleManage = class(TfrmStandMDI)
    dbFRemark: TDBEdit;
    dbFRoleCaption: TDBEdit;
    dbFRoleCode: TDBEdit;
    dbFRoleTreeCode: TDBEdit;
    dsRole: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    qryRole: TOneDataSet;
    qryRoleFPRoleID: TStringField;
    qryRoleFRemark: TStringField;
    qryRoleFRoleCaption: TStringField;
    qryRoleFRoleCode: TStringField;
    qryRoleFRoleID: TStringField;
    qryRoleFRoleTreeCode: TStringField;
    TreeRole: TLazVirtualStringTree;
    procedure dsRoleDataChange(Sender: TObject; Field: TField);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure qryRoleNewRecord(DataSet: TDataSet);
    procedure tbEditClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure TreeRoleGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeRoleNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    FNodeDict: specialize TDictionary<string, PVirtualNode>;
  private
    procedure InitDataTree();
    procedure SaveTreeCode();
  public
    procedure SetCurrID(Value: string); override;
    procedure SetButtonEnabled(); override;
  end;


procedure ShowRoleManage();

var
  frmRoleManage: TfrmRoleManage;

implementation

{$R *.lfm}
uses func_public;

{ TfrmRoleManage }


procedure ShowRoleManage();
begin
  if Assigned(frmRoleManage) then
  begin
    if not frmRoleManage.Visible then
      frmRoleManage.Visible := True;
    frmRoleManage.BringToFront;
    frmRoleManage.ChangeMainFormTabs();
  end
  else
  begin
    frmRoleManage := TfrmRoleManage.Create(nil);
    frmRoleManage.Show;
    frmRoleManage.CurrID := '';
  end;
end;

procedure TfrmRoleManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  if frmRoleManage <> nil then
    frmRoleManage := nil;
  FNodeDict.Clear;
  FNodeDict.Free;
end;

procedure TfrmRoleManage.FormCreate(Sender: TObject);
begin
  inherited;
  FNodeDict := specialize TDictionary<string, PVirtualNode>.Create;
  TreeRole.NodeDataSize := SizeOf(TRoleTreeData);
end;

procedure TfrmRoleManage.qryRoleNewRecord(DataSet: TDataSet);
begin
  qryRoleFRoleID.AsString := func_public.GetGUIDStr();
  qryRoleFPRoleID.AsString := '-1';
end;

procedure TfrmRoleManage.dsRoleDataChange(Sender: TObject; Field: TField);
begin
  self.SetButtonEnabled();
end;

procedure TfrmRoleManage.tbEditClick(Sender: TObject);
var
  lParentNode, lNode: PVirtualNode;
  lGroupTreeData: PRoleTreeData;
begin
  inherited;
  if not qryRole.Active then
  begin
    func_public.OkMsg('数据集未打开,请先打开数据');
    exit;
  end;
  lParentNode := nil;
  lParentNode := treeRole.FocusedNode;
  lNode := treeRole.AddChild(lParentNode);
  qryRole.Append;
  qryRoleFRoleCaption.AsString := '新的节点';
  //赋值父节点ID
  if lParentNode <> nil then
  begin
    lGroupTreeData := treeRole.GetNodeData(lParentNode);
    if Assigned(lGroupTreeData) then
    begin
      qryRoleFPRoleID.AsString := lGroupTreeData^.FRoleID;
    end;
  end;
  qryRole.Post;
  //为新增节点赋值相关数据
  lGroupTreeData := treeRole.GetNodeData(lNode);
  lGroupTreeData^.FRoleID := qryRoleFRoleID.AsString;
  lGroupTreeData^.FRoleTreeCode := '';
  lGroupTreeData^.FRoleCaption := qryRoleFRoleCaption.AsString;
  self.FNodeDict.add(lGroupTreeData^.FRoleID, lNode);
end;

procedure TfrmRoleManage.tbNewClick(Sender: TObject);
var
  lFocusedNode, lParentNode, lNode: PVirtualNode;
  lGroupTreeData: PRoleTreeData;
begin
  inherited;
  if not qryRole.Active then
  begin
    func_public.OkMsg('数据集未打开,请先打开数据');
    exit;
  end;
  lParentNode := nil;
  lFocusedNode := TreeRole.FocusedNode;
  if lFocusedNode <> nil then
  begin
    //选中节点，建立选中节点同级节点
    lParentNode := lFocusedNode^.Parent;
  end;
  lNode := TreeRole.AddChild(lParentNode);
  qryRole.Append;
  qryRoleFRoleCaption.AsString := '新的节点';
  //赋值父节点ID
  if lParentNode <> nil then
  begin
    lGroupTreeData := TreeRole.GetNodeData(lParentNode);
    if Assigned(lGroupTreeData) then
    begin
      qryRoleFPRoleID.AsString := lGroupTreeData^.FRoleID;
    end;
  end;
  qryRole.Post;
  //为新增节点赋值相关数据
  lGroupTreeData := TreeRole.GetNodeData(lNode);
  lGroupTreeData^.FRoleID := qryRoleFRoleID.AsString;
  lGroupTreeData^.FRoleTreeCode := '';
  lGroupTreeData^.FRoleCaption := qryRoleFRoleCaption.AsString;
  self.FNodeDict.add(lGroupTreeData^.FRoleID, lNode);
end;

procedure TfrmRoleManage.tbRefreshClick(Sender: TObject);
begin
  self.CurrID := self.FCurrID;
end;

procedure TfrmRoleManage.tbSaveClick(Sender: TObject);
begin
  if qryRole.State in dsEditModes then
    qryRole.Post;
  SaveTreeCode;
  if qryRole.SaveData then
  begin
    CurrID := FCurrID;
  end
  else
  begin
    func_public.OkMsg(qryRole.DataInfo.ErrMsg);
  end;
end;

procedure TfrmRoleManage.TreeRoleGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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

procedure TfrmRoleManage.TreeRoleNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  lNode: PVirtualNode;
  lNodeData: PRoleTreeData;
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
  qryRole.Locate('FRoleID', lNodeData^.FRoleID, []);
end;

procedure TfrmRoleManage.InitDataTree;
var
  lPNode, lNode: PVirtualNode;
  lGroupTreeData: PRoleTreeData;
begin
  treeRole.Clear;
  if qryRole.IsEmpty then
  begin
    exit;
  end;
  //把DataSet转化成树形
  FNodeDict.Clear;
  qryRole.First;
  while not qryRole.EOF do
  begin
    lPNode := nil;
    //寻找是否有父节点
    FNodeDict.TryGetValue(qryRoleFPRoleID.AsString, lPNode);
    //添加节点
    lNode := treeRole.AddChild(lPNode);
    //获取节点数据,会自动创建一个指针大小和PRoleTreeData一至
    lGroupTreeData := treeRole.GetNodeData(lNode);
    lGroupTreeData^.FRoleID := qryRoleFRoleID.AsString;
    lGroupTreeData^.FRoleTreeCode := qryRoleFRoleTreeCode.AsString;
    lGroupTreeData^.FRoleCaption := qryRoleFRoleCaption.AsString;
    FNodeDict.Add(lGroupTreeData^.FRoleID, lNode);
    qryRole.Next;
  end;
  treeRole.FullExpand();
end;

procedure TfrmRoleManage.SaveTreeCode;
var
  lRootNode: PVirtualNode;

  procedure DataNodeEdit(QNode: PVirtualNode; QIndexNode: integer; QPNode: PVirtualNode);
  var
    vTreeCode: string;
    lTreeData, lPTreeData: PRoleTreeData;
  begin
    lTreeData := treeRole.GetNodeData(QNode);
    if lTreeData <> nil then
    begin
      vTreeCode := (QIndexNode + 1).toString;
      if vTreeCode.Length < 2 then
        vTreeCode := '0' + vTreeCode;
      if QPNode <> nil then
      begin
        lPTreeData := treeRole.GetNodeData(QPNode);
        if Assigned(lPTreeData) then
        begin
          if qryRole.Locate('FRoleID', lPTreeData^.FRoleID, []) then
          begin
            vTreeCode := qryRoleFRoleTreeCode.AsString + '.' + vTreeCode;
          end;
        end;
      end;
      if qryRole.Locate('FRoleID', lTreeData^.FRoleID, []) then
      begin
        qryRole.Edit;
        qryRoleFRoleTreeCode.AsString := vTreeCode;
        qryRole.Post;
      end;
    end;
  end;

  procedure DataSaveNodeChild(QPNode: PVirtualNode);
  var
    iTemp: integer;
    lChildNode: PVirtualNode;
  begin
    lChildNode := treeRole.GetFirstChild(QPNode);
    iTemp := 0;
    while Assigned(lChildNode) do
    begin
      DataNodeEdit(lChildNode, iTemp, QPNode);
      { 进行递归 }
      DataSaveNodeChild(lChildNode);
      iTemp := iTemp + 1;
      lChildNode := treeRole.GetNextSibling(lChildNode);
    end;
  end;

begin
  if not qryRole.Active then
    exit;

  qryRole.DisableControls;
  try
    lRootNode := treeRole.RootNode;
    DataSaveNodeChild(lRootNode);
  finally
    qryRole.EnableControls;
  end;
end;

procedure TfrmRoleManage.SetCurrID(Value: string);
begin
  inherited SetCurrID(Value);
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

procedure TfrmRoleManage.SetButtonEnabled;
var
  isEdit: boolean;
begin
  isEdit := qryRole.IsEdit();
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

end.
