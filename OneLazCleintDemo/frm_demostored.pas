unit frm_demoStored;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, OneClientConnect, OneClientDataSet, TypInfo;

type

  { TfrmDemoStored }

  TfrmDemoStored = class(TForm)
    dsData: TDataSource;
    qryParamsFParamResultValue: TWideStringField;
    tbGetParams: TButton;
    gridParams: TDBGrid;
    qryParams: TBufDataset;
    dsParams: TDataSource;
    Label7: TLabel;
    OneConnection: TOneConnection;
    qryData: TOneDataSet;
    qryParamsFParamDataType: TWideStringField;
    qryParamsFParamName: TWideStringField;
    qryParamsFParamSize: TLongintField;
    qryParamsFParamType: TWideStringField;
    qryParamsFParamValue: TWideStringField;
    tbDoStored: TButton;
    edIsData: TCheckBox;
    DBGrid1: TDBGrid;
    edServerHost: TEdit;
    edServerKey: TEdit;
    edServerPort: TEdit;
    edServerZTCode: TEdit;
    edStoredName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edSQL: TMemo;
    tbConnect: TButton;
    tbConnectClose: TButton;
    procedure dsParamsDataChange(Sender: TObject; Field: TField);
    procedure tbGetParamsClick(Sender: TObject);
    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbDoStoredClick(Sender: TObject);
  private

  public

  end;

var
  frmDemoStored: TfrmDemoStored;

implementation

{$R *.lfm}

{ TfrmDemoStored }

procedure TfrmDemoStored.tbConnectClick(Sender: TObject);
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
  if OneConnection.DoConnect() then
  begin
    ShowMessage('接接成功');
  end
  else
  begin
    ShowMessage(OneConnection.ErrMsg);
  end;
end;

procedure TfrmDemoStored.tbGetParamsClick(Sender: TObject);
var
  iParam: integer;
  lTempData: TBufDataSet;
  lParamName, lParamNameB: string;
begin
  if not qryParams.Active then
    qryParams.CreateDataset;
  qryData.SQL.Text := edSQL.Lines.Text;
  lTempData := TBufDataSet.Create(nil);
  try
    lTempData.CopyFromDataset(qryParams);
    qryParams.Close;
    qryParams.CreateDataset;
    for iParam := 0 to qryData.Params.Count - 1 do
    begin
      lParamName := qryData.Params[iParam].Name;
      qryParams.Append;
      qryParamsFParamName.AsString := lParamName;
      lTempData.First;
      while not lTempData.EOF do
      begin
        lParamNameB := lTempData.FieldByName('FParamName').AsString;
        if lParamName.ToLower = lParamNameB.ToLower then
        begin
          qryParamsFParamValue.AsString := lTempData.FieldByName('FParamValue').AsString;
          if lTempData.FieldByName('FParamType').AsString <> '' then
            qryParamsFParamType.AsString := lTempData.FieldByName('FParamType').AsString;
          if lTempData.FieldByName('FParamDataType').AsString <> '' then
            qryParamsFParamDataType.AsString := lTempData.FieldByName('FParamDataType').AsString;
          qryParamsFParamSize.AsInteger := lTempData.FieldByName('FParamSize').AsInteger;
          break;
        end;
        lTempData.Next;
      end;
      qryParams.Post;
    end;
  finally
    lTempData.Free;
  end;
end;

procedure TfrmDemoStored.dsParamsDataChange(Sender: TObject; Field: TField);
begin

end;

procedure TfrmDemoStored.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TfrmDemoStored.tbDoStoredClick(Sender: TObject);
var
  iParam, i: integer;
  lParamName, lParamNameB: string;
  lBool: boolean;
  lColumn: TColumn;
begin
  if qryParams.Active then
  begin
    qryParams.First;
    while not qryParams.EOF do
    begin
      qryParams.Edit;
      qryParamsFParamResultValue.AsString := '';
      qryParams.Post;
      qryParams.Next;
    end;
  end;
  qryData.DataInfo.StoredProcName := edStoredName.Text;
  qryData.DataInfo.IsReturnData := edIsData.Checked;
  qryData.SQL.Text := edSQL.Text;
  for iParam := 0 to qryData.Params.Count - 1 do
  begin
    lParamName := qryData.Params[iParam].Name;
    if qryParams.Active then
    begin
      qryParams.First;
      while not qryParams.EOF do
      begin
        lParamNameB := qryParamsFParamName.AsString;
        if lParamName.ToLower = lParamNameB.ToLower then
        begin
          qryData.Params[iParam].Value := qryParamsFParamValue.AsString;
          qryData.Params[iParam].ParamType := TParamType(GetEnumValue(TypeInfo(TParamType), qryParamsFParamType.AsString));
          qryData.Params[iParam].DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), qryParamsFParamDataType.AsString));
          qryData.Params[iParam].Size := qryParamsFParamSize.AsInteger;
          break;
        end;
        qryParams.Next;
      end;
    end;
  end;
  if not qryData.OpenStored then
  begin
    ShowMessage(qryData.DataInfo.ErrMsg);
    exit;
  end;
  for iParam := 0 to qryData.Params.Count - 1 do
  begin
    lParamName := qryData.Params[iParam].Name;
    if qryParams.Active then
    begin
      qryParams.First;
      while not qryParams.EOF do
      begin
        lParamNameB := qryParamsFParamName.AsString;
        if lParamName.ToLower = lParamNameB.ToLower then
        begin
          qryParams.Edit;
          qryParamsFParamResultValue.Value := qryData.Params[iParam].Value;
          qryParams.Post;
          break;
        end;
        qryParams.Next;
      end;
    end;
  end;
  if edIsData.Checked then
  begin
    DBGrid1.Columns.Clear;
    for i := 0 to qryData.Fields.Count - 1 do
    begin
      //tempMsg := qryData.Fields[i].FieldName;
      lColumn := DBGrid1.Columns.Add;
      lColumn.FieldName := qryData.Fields[i].FieldName;
      lColumn.Title.Caption := qryData.Fields[i].FieldName;
      lColumn.Width := 100;
    end;
    ShowMessage('打开数据成功');
  end;
end;

end.
