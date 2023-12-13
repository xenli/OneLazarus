unit frm_demoDatas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids,
  OneClientConnect, OneClientDataSet,OneClientHelper;

type

  { TfrmDemoDatas }

  TfrmDemoDatas = class(TForm)
    DBGridA: TDBGrid;
    DBGridB: TDBGrid;
    dsDataA: TDataSource;
    dsDataB: TDataSource;
    edPrimaryKeyA: TEdit;
    edPrimaryKeyB: TEdit;
    edServerHost: TEdit;
    edServerKey: TEdit;
    edServerPort: TEdit;
    edServerZTCode: TEdit;
    edSQLA: TMemo;
    edSQLB: TMemo;
    edSQLAParams: TMemo;
    edSQLBParams: TMemo;
    edTableNameA: TEdit;
    edTableNameB: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OneConnection: TOneConnection;
    qryDataA: TOneDataSet;
    qryDataB: TOneDataSet;
    tbConnect: TButton;
    tbConnectClose: TButton;
    tbOpenData: TButton;
    tbSaveData: TButton;
    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbOpenDataClick(Sender: TObject);
    procedure tbSaveDataClick(Sender: TObject);
  private

  public

  end;

var
  frmDemoDatas: TfrmDemoDatas;

implementation

{$R *.lfm}

{ TfrmDemoDatas }

procedure TfrmDemoDatas.tbConnectClick(Sender: TObject);
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

procedure TfrmDemoDatas.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TfrmDemoDatas.tbOpenDataClick(Sender: TObject);
var
  i: integer;
  lColumn: TColumn;
begin
  if not OneConnection.Connected then
  begin
    ShowMessage('未连接请先连接');
    exit;
  end;
  // 或者在控件UI直接设置 Connection
  qryDataA.DataInfo.Connection := OneConnection;
  qryDataA.SQL.Text := edSQLA.Lines.Text;
  for i := 0 to qryDataA.Params.Count - 1 do
  begin
    if edSQLAParams.Lines.Count >= i + 1 then
    begin
      qryDataA.Params[i].Value := edSQLAParams.Lines[i];
    end;
  end;
  //B数据集赋值
  qryDataB.DataInfo.Connection := OneConnection;
  qryDataB.SQL.Text := edSQLB.Lines.Text;
  for i := 0 to qryDataB.Params.Count - 1 do
  begin
    if edSQLBParams.Lines.Count >= i + 1 then
    begin
      qryDataB.Params[i].Value := edSQLBParams.Lines[i];
    end;
  end;
  //关闭数据
  if qryDataA.Active then
  begin
    qryDataA.Close;
    qryDataA.Fields.Clear;
  end;
  if qryDataB.Active then
  begin
    qryDataB.Close;
    qryDataB.Fields.Clear;
  end;
  //打开数据 或者用  OneConnection也有打开多个数据集的功能
  if not qryDataA.OpenDatas([qryDataA,qryDataB]) then
  begin
    ShowMessage(qryDataA.DataInfo.ErrMsg);
    exit;
  end;

  DBGridA.Columns.Clear;
  for i := 0 to qryDataA.Fields.Count - 1 do
  begin
    lColumn := DBGridA.Columns.Add;
    lColumn.FieldName := qryDataA.Fields[i].FieldName;
    lColumn.Title.Caption := qryDataA.Fields[i].FieldName;
    lColumn.Width := 100;
  end;

   DBGridB.Columns.Clear;
  for i := 0 to qryDataB.Fields.Count - 1 do
  begin
    lColumn := DBGridB.Columns.Add;
    lColumn.FieldName := qryDataB.Fields[i].FieldName;
    lColumn.Title.Caption := qryDataB.Fields[i].FieldName;
    lColumn.Width := 100;
  end;
  ShowMessage('打开数据成功');
end;

procedure TfrmDemoDatas.tbSaveDataClick(Sender: TObject);
begin
  qryDataA.DataInfo.Connection := OneConnection;
  qryDataA.DataInfo.TableName := edTableNameA.Text;
  qryDataA.DataInfo.PrimaryKey := edPrimaryKeyA.Text;

  qryDataB.DataInfo.Connection := OneConnection;
  qryDataB.DataInfo.TableName := edTableNameB.Text;
  qryDataB.DataInfo.PrimaryKey := edPrimaryKeyB.Text;

  if qryDataA.State in dsEditModes then
    qryDataA.Post;
  if qryDataB.State in dsEditModes then
    qryDataB.Post;
  //数据控件有多个保存数据集方法 OneConnection也有
  if qryDataA.SaveDatas([qryDataA,qryDataB]) then
  begin
    ShowMessage('保存数据成功');
  end
  else
  begin
    ShowMessage(qryDataA.DataInfo.ErrMsg);
  end;
end;

end.
