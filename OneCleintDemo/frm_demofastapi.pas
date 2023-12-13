unit frm_demoFastApi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  DBGrids, ExtCtrls, DBCtrls, OneClientConnect, OneClientDataSet;

type

  { TForm1 }

  TForm1 = class(TForm)
    dbFFilterDefaultValue: TDBComboBox;
    dbFFieldDefaultValueType: TDBComboBox;
    dbFDataJsonType: TDBComboBox;
    dbFbOutParam: TDBCheckBox;
    dbFDataOpenMode: TDBComboBox;
    dbFFieldDefaultValue: TDBComboBox;
    dbFFilterDefaultType: TDBComboBox;
    dbFFilterbMust: TDBCheckBox;
    dbFFilterbValue: TDBCheckBox;
    dbFOutParamTag: TDBEdit;
    dbFFilterExpression: TDBComboBox;
    dbFFilterFormat: TDBEdit;
    dbFFilterFieldMode: TDBComboBox;
    dbFFieldIsMustValue: TDBCheckBox;
    dbFFilterField: TDBEdit;
    dbFFilterDataType: TDBComboBox;
    dbFFilterName: TDBEdit;
    dbFFieldProvidFlagKey: TDBCheckBox;
    dbFFieldIsMust: TDBCheckBox;
    dbFFieldProvidFlagUpdate: TDBCheckBox;
    dbFFieldProvidFlagWhere: TDBCheckBox;
    dbFFilterCaption: TDBEdit;
    dbFFilterJsonName: TDBEdit;
    dbFOrderNumber: TDBEdit;
    dbFDataPageSize: TDBEdit;
    dbFMinAffected: TDBEdit;
    dbFDataJsonName: TDBEdit;
    dbFDataName: TDBEdit;
    dbFDataCaption: TDBEdit;
    dbFDataUpdateMode: TDBEdit;
    dbFDataZTCode: TDBEdit;
    dbFDataTable: TDBEdit;
    dbFDataPrimaryKey: TDBEdit;
    dbFDataStoreName: TDBEdit;
    dbFIsEnabled: TDBCheckBox;
    dbFApiCode: TDBEdit;
    dbFApiCaption: TDBEdit;
    dbFMaxAffected: TDBEdit;
    dbFFieldName: TDBEdit;
    dbFFieldCaption: TDBEdit;
    dbFilterFOrderNumber: TDBEdit;
    dbFOrderNumber3: TDBEdit;
    dbFFieldKind: TDBEdit;
    dbFFieldDataType: TDBEdit;
    dbFFieldSize: TDBEdit;
    dbFFieldPrecision: TDBEdit;
    dbFDataSQL: TDBMemo;
    dsApi: TDataSource;
    grdList: TDBGrid;
    grdList1: TDBGrid;
    grdFilter: TDBGrid;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbHint: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    qryDataFApiID: TStringField;
    qryDataFDataCaption: TStringField;
    qryDataFDataID: TStringField;
    qryDataFDataJsonName: TStringField;
    qryDataFDataJsonType: TStringField;
    qryDataFDataName: TStringField;
    qryDataFDataOpenMode: TStringField;
    qryDataFDataPageSize: TLongintField;
    qryDataFDataPrimaryKey: TStringField;
    qryDataFDataSQL: TMemoField;
    qryDataFDataStoreName: TStringField;
    qryDataFDataTable: TStringField;
    qryDataFDataUpdateMode: TStringField;
    qryDataFDataZTCode: TStringField;
    qryDataFMaxAffected: TLongintField;
    qryDataFMinAffected: TLongintField;
    qryDataFPDataID: TStringField;
    qryDataFTreeCode: TStringField;
    qryApi: TOneDataSet;
    qryFieldFApiID: TStringField;
    qryFieldFDataID: TStringField;
    qryFieldFFieldCaption: TStringField;
    qryFieldFFieldCheckEmpty: TBooleanField;
    qryFieldFFieldDataType: TStringField;
    qryFieldFFieldDefaultValue: TStringField;
    qryFieldFFieldDefaultValueType: TStringField;
    qryFieldFFieldFormat: TStringField;
    qryFieldFFieldID: TStringField;
    qryFieldFFieldIsMust: TBooleanField;
    qryFieldFFieldIsMustValue: TBooleanField;
    qryFieldFFieldJsonName: TStringField;
    qryFieldFFieldKind: TStringField;
    qryFieldFFieldName: TStringField;
    qryFieldFFieldPrecision: TLongintField;
    qryFieldFFieldProvidFlagKey: TBooleanField;
    qryFieldFFieldProvidFlagUpdate: TBooleanField;
    qryFieldFFieldProvidFlagWhere: TBooleanField;
    qryFieldFFieldShowPass: TBooleanField;
    qryFieldFFieldSize: TLongintField;
    qryFieldFOrderNumber: TLongintField;
    qryFilterFApiID: TStringField;
    qryFilterFbOutParam: TBooleanField;
    qryFilterFDataID: TStringField;
    qryFilterFFilterbMust: TBooleanField;
    qryFilterFFilterbValue: TBooleanField;
    qryFilterFFilterCaption: TStringField;
    qryFilterFFilterDataType: TStringField;
    qryFilterFFilterDefaultType: TStringField;
    qryFilterFFilterDefaultValue: TStringField;
    qryFilterFFilterExpression: TStringField;
    qryFilterFFilterField: TStringField;
    qryFilterFFilterFieldItems: TStringField;
    qryFilterFFilterFieldMode: TStringField;
    qryFilterFFilterFormat: TStringField;
    qryFilterFFilterID: TStringField;
    qryFilterFFilterJsonName: TStringField;
    qryFilterFFilterName: TStringField;
    qryFilterFOrderNumber: TLongintField;
    qryFilterFOutParamTag: TStringField;
    qryFilterFPFilterField: TStringField;
    qryListFApiAuthor: TStringField;
    qryApiFApiAuthor: TStringField;
    qryListFApiCaption: TStringField;
    qryApiFApiCaption: TStringField;
    qryListFApiCode: TStringField;
    qryApiFApiCode: TStringField;
    qryListFApiID: TStringField;
    qryApiFApiID: TStringField;
    qryListFApiRole: TStringField;
    qryApiFApiRole: TStringField;
    qryListFIsEnabled: TBooleanField;
    qryApiFIsEnabled: TBooleanField;
    qryListFIsMenu: TBooleanField;
    qryApiFIsMenu: TBooleanField;
    qryListFOrderNumber: TLongintField;
    qryApiFOrderNumber: TLongintField;
    qryListFPApiID: TStringField;
    qryApiFPApiID: TStringField;
    TabSheet7: TTabSheet;
    tbOpenList: TButton;
    tbFilterDel: TButton;
    tbEdit: TButton;
    tbNew: TButton;
    tbSave: TButton;
    tbDataAdd: TButton;
    tbDataDel: TButton;
    tbFieldAdd: TButton;
    tbFieldDel: TButton;
    tbFilterAdd: TButton;
    DBGrid1: TDBGrid;
    gridData: TDBGrid;
    dsData: TDataSource;
    dsField: TDataSource;
    dsList: TDataSource;
    dsFilter: TDataSource;
    edServerHost: TEdit;
    edServerKey: TEdit;
    edServerPort: TEdit;
    edServerZTCode: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    OneConnection: TOneConnection;
    pgMain: TPageControl;
    pageApi: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    qryData: TOneDataSet;
    qryField: TOneDataSet;
    qryList: TOneDataSet;
    qryFilter: TOneDataSet;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    tbConnect: TButton;
    tbConnectClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure qryDataAfterInsert(DataSet: TDataSet);
    procedure qryDataAfterScroll(DataSet: TDataSet);
    procedure qryFieldAfterInsert(DataSet: TDataSet);
    procedure qryFilterAfterInsert(DataSet: TDataSet);
    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbDataAddClick(Sender: TObject);
    procedure tbDataDelClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure tbFieldAddClick(Sender: TObject);
    procedure tbFieldDelClick(Sender: TObject);
    procedure tbFilterAddClick(Sender: TObject);
    procedure tbFilterDelClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbOpenListClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
  private
    FCurrID: string;
  private
    procedure openApiInfo(qApiID: string);
    function GetGuid(): string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.GetGuid(): string;
var
  ii: TGUID;
begin
  CreateGUID(ii);
  Result := Copy(GUIDToString(ii).replace('-', ''), 2, 32);
end;

procedure TForm1.tbNewClick(Sender: TObject);
begin
  self.openApiInfo('-1');
end;

procedure TForm1.tbOpenListClick(Sender: TObject);
begin
  if not qryList.OpenData then
  begin
    ShowMessage(qryList.DataInfo.ErrMsg);
    exit;
  end;
  ShowMessage('打开Api列表数据成功');
end;

procedure TForm1.tbSaveClick(Sender: TObject);
begin
  if not qryApi.SaveDatas([qryApi, qryData, qryField, qryFilter]) then
  begin
    ShowMessage(qryApi.DataInfo.ErrMsg);
    exit;
  end;
  ShowMessage('保存Api信息成功,如需立即生效请刷服务端接口信息');
end;

procedure TForm1.tbConnectClick(Sender: TObject);
begin
  if OneConnection.Connected then
  begin
    ShowMessage('已连接无需在连接');
    exit;
  end;
  OneConnection.HTTPHost := edServerHost.Text;
  OneConnection.HTTPPort := StrToInt(edServerPort.Text);
  OneConnection.ConnectSecretkey := edServerKey.Text;
  OneConnection.ZTCode := edServerZTCode.Text;
  Unit_Connection := OneConnection;
  if OneConnection.DoConnect() then
  begin
    ShowMessage('接接成功');
  end
  else
  begin
    ShowMessage(OneConnection.ErrMsg);
  end;
end;

procedure TForm1.qryFilterAfterInsert(DataSet: TDataSet);
begin
  qryFilterFFilterID.AsString := self.GetGuid();
  qryFilterFDataID.AsString := qryDataFDataID.AsString;
  qryFilterFApiID.AsString := qryApiFApiID.AsString;
  qryFilterFOrderNumber.AsInteger := qryFilter.RecordCount + 1;
end;

procedure TForm1.qryFieldAfterInsert(DataSet: TDataSet);
begin
  qryFieldFFieldID.AsString := self.GetGuid();
  qryFieldFDataID.AsString := qryDataFDataID.AsString;
  qryFieldFApiID.AsString := qryApiFApiID.AsString;
  qryFieldFOrderNumber.AsInteger := qryField.RecordCount + 1;
end;

procedure TForm1.qryDataAfterInsert(DataSet: TDataSet);
begin
  qryDataFDataID.AsString := self.GetGuid();
  qryDataFApiID.AsString := qryApiFApiID.AsString;
  qryDataFTreeCode.AsString := '0' + (qryField.RecordCount + 1).ToString;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pgMain.ActivePageIndex := 0;
  pageApi.ActivePageIndex := 0;
end;

procedure TForm1.qryDataAfterScroll(DataSet: TDataSet);
begin
  qryField.DisableControls;
  qryFilter.DisableControls;
  qryField.Filtered := False;
  qryFilter.Filtered := False;
  try
    qryField.Filter := 'FDataID=' + QuoTedStr(qryDataFDataID.AsString);
    qryFilter.Filter := 'FDataID=' + QuoTedStr(qryDataFDataID.AsString);
  finally
    qryField.Filtered := True;
    qryFilter.Filtered := True;
    qryField.EnableControls;
    qryFilter.EnableControls;
  end;
end;

procedure TForm1.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TForm1.tbDataAddClick(Sender: TObject);
begin
  if not qryData.Active then
  begin
    ShowMessage('请先打开数据集');
    exit;
  end;
  qryData.Append;
  qryData.Post;
end;

procedure TForm1.tbDataDelClick(Sender: TObject);
begin
  if not qryData.Active then
  begin
    ShowMessage('请先打开数据集');
    exit;
  end;
  if not qryData.IsEmpty then
  begin
    qryField.First;
    while not qryField.EOF do
    begin
      qryField.Delete;
    end;
    qryFilter.First;
    while not qryFilter.EOF do
    begin
      qryFilter.Delete;
    end;
    qryData.Delete;
  end;
end;

procedure TForm1.tbEditClick(Sender: TObject);
begin
  if qryList.IsEmpty then
  begin
    ShowMessage('请先选中一条数据进行编辑');
    exit;
  end;
  lbHint.Caption := '正在编辑接口:' + qryListFApiCode.AsString;
  self.openApiInfo(qryListFApiID.AsString);
end;

procedure TForm1.tbFieldAddClick(Sender: TObject);
begin
  if not qryField.Active then
  begin
    ShowMessage('请先打开数据集');
    exit;
  end;
  if qryData.IsEmpty then
  begin
    ShowMessage('请先选中一条数据集信息');
    exit;
  end;
  qryField.Append;
  qryField.Post;
end;

procedure TForm1.tbFieldDelClick(Sender: TObject);
begin
  if not qryField.Active then
  begin
    ShowMessage('请先打开数据集');
    exit;
  end;
  if not qryField.IsEmpty then
    qryField.Delete;
end;

procedure TForm1.tbFilterAddClick(Sender: TObject);
begin
  if not qryFilter.Active then
  begin
    ShowMessage('请先打开数据集');
    exit;
  end;
  if qryData.IsEmpty then
  begin
    ShowMessage('请先选中一条数据集信息');
    exit;
  end;
  qryFilter.Append;
  qryFilter.Post;
end;

procedure TForm1.tbFilterDelClick(Sender: TObject);
begin
  if not qryFilter.Active then
  begin
    ShowMessage('请先打开数据集');
    exit;
  end;
  if not qryFilter.IsEmpty then
    qryFilter.Delete;
end;

procedure TForm1.openApiInfo(qApiID: string);
begin

  qryApi.Params[0].AsString := qApiID;
  qryData.Params[0].AsString := qApiID;
  qryField.Params[0].AsString := qApiID;
  qryFilter.Params[0].AsString := qApiID;

  if not qryApi.OpenDatas([qryApi, qryData, qryField, qryFilter]) then
  begin
    ShowMessage(qryApi.DataInfo.ErrMsg);
    exit;
  end;
  if (qApiID = '-1') then
  begin
    //新建一条数据
    qryApi.Append;
    qryApiFApiID.AsString := self.GetGuid();
    qryApi.Post;
  end
  else if (qryApi.IsEmpty) then
  begin
    ShowMessage('此Api信息已不存在,请刷新列表');
    exit;
  end;
  FCurrID := qryApiFApiID.AsString;
  ShowMessage('打开Api信息数据成功');
end;

end.
