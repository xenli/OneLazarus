unit frm_AdminUserManage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, memds, Forms, Controls, Graphics, Dialogs, Generics.Collections,
  ComCtrls, DBGrids, laz.VirtualTrees, frm_basestandmdi, OneClientDataSet, Admin_GroupInfo;

type

  { TfrmAdminUserManage }

  TfrmAdminUserManage = class(TfrmStandMDI)
    DBGrid1: TDBGrid;
    dsAdmin: TDataSource;
    dsGroup: TDataSource;
    qryAdmin: TOneDataSet;
    qryAdminFAdminCode: TStringField;
    qryAdminFAdminID: TStringField;
    qryAdminFAdminName: TStringField;
    qryAdminFAdminTel: TStringField;
    qryAdminFAdminType: TStringField;
    qryAdminFCreateTime: TDateTimeField;
    qryAdminFGroupCaption: TStringField;
    qryAdminFGroupID: TStringField;
    qryAdminFGroupTreeCode: TStringField;
    qryAdminFIsEnable: TBooleanField;
    qryAdminFIsLimit: TBooleanField;
    qryAdminFIsMultiLogin: TBooleanField;
    qryAdminFLimtEndTime: TDateTimeField;
    qryAdminFLimtStartTime: TDateTimeField;
    qryAdminFRemark: TStringField;
    qryAdminFRoleCaption: TStringField;
    qryAdminFRoleID: TStringField;
    qryGroup: TOneDataSet;
    qryGroupFGroupCaption: TStringField;
    qryGroupFGroupCode: TStringField;
    qryGroupFGroupID: TStringField;
    qryGroupFGroupTreeCode: TStringField;
    qryGroupFGroupType: TStringField;
    qryGroupFPGroupID: TStringField;
    qryGroupFRemark: TStringField;
    TreeGroup: TLazVirtualStringTree;
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure TreeGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure TreeGroupNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  private
    procedure InitDataTree();
  public
    procedure SetCurrID(Value: string); override;
  end;



procedure ShowAdminUserMange();

var
  frmAdminUserManage: TfrmAdminUserManage;

implementation

{$R *.lfm}
uses func_public, impl_form;

procedure ShowAdminUserMange();
begin
  if Assigned(frmAdminUserManage) then
  begin
    if not frmAdminUserManage.Visible then
      frmAdminUserManage.Visible := True;
    frmAdminUserManage.BringToFront;
    //切换Tags标签
    frmAdminUserManage.ChangeMainFormTabs();
  end
  else
  begin
    frmAdminUserManage := TfrmAdminUserManage.Create(nil);
    frmAdminUserManage.Show;
    frmAdminUserManage.CurrID := '';
  end;
end;

{ TfrmAdminUserManage }

procedure TfrmAdminUserManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  if frmAdminUserManage <> nil then
    frmAdminUserManage := nil;
end;

procedure TfrmAdminUserManage.DBGrid1DblClick(Sender: TObject);
begin
  if qryAdmin.IsEmpty then
    exit;
  impl_form.ImplForm().ShowAdminUserEdit(qryAdminFAdminID.AsString);
end;

procedure TfrmAdminUserManage.FormCreate(Sender: TObject);
begin
  inherited;
  TreeGroup.NodeDataSize := SizeOf(TGroupTreeData);
end;

procedure TfrmAdminUserManage.tbEditClick(Sender: TObject);
begin
  if qryAdmin.IsEmpty then
  begin
    func_public.OkMsg('请先选择一条操作员数据');
    exit;
  end;
  impl_form.ImplForm().ShowAdminUserEdit(qryAdminFAdminID.AsString);
end;

procedure TfrmAdminUserManage.tbNewClick(Sender: TObject);
begin
  impl_form.ImplForm().ShowAdminUserEdit('');
end;

procedure TfrmAdminUserManage.tbRefreshClick(Sender: TObject);
begin
  self.CurrID := '';
end;

procedure TfrmAdminUserManage.InitDataTree();
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

procedure TfrmAdminUserManage.SetCurrID(Value: string);
begin
  if not qryGroup.OpenDatas([qryGroup, qryAdmin]) then
  begin
    func_public.OkMsg(qryGroup.DataInfo.ErrMsg);
    exit;
  end;
  //qryGroup.Filtered := False;
  //try
  //  qryGroup.Filter := '';
  //finally
  //  qryGroup.Filtered := True;
  //end;
  self.InitDataTree();
end;

procedure TfrmAdminUserManage.TreeGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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

procedure TfrmAdminUserManage.TreeGroupNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  lTreeCode: string;
  lNode: PVirtualNode;
  lNodeData: PGroupTreeData;
begin
  inherited;
  if qryGroup.isEmpty then
    exit;

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
  if qryGroup.Locate('FGroupID', lNodeData^.FGroupID, []) then
  begin
    lTreeCode := qryGroupFGroupTreeCode.AsString;
    qryAdmin.filtered := False;
    //like是用*号
    qryAdmin.filter := 'FGroupTreeCode = ' + (lTreeCode + '*').QuotedString;
    qryAdmin.filtered := True;
  end;
end;

end.
