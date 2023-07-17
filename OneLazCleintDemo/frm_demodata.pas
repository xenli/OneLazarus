unit frm_DemoData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, OneClientConnect, OneClientDataSet, OneClientVirtualFile;

type

  { TForm1 }

  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    edPageCount: TEdit;
    edPageTotal: TEdit;
    edTableName: TEdit;
    edPrimaryKey: TEdit;
    edServerPort: TEdit;
    edPageNow: TEdit;
    edPageSize: TEdit;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edSQL: TMemo;
    edSQLParams: TMemo;
    tbConnect: TButton;
    tbOpenData: TButton;
    tbSaveData: TButton;
    tbConnectClose: TButton;
    dsData: TDataSource;
    edServerHost: TEdit;
    edServerKey: TEdit;
    edServerZTCode: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OneConnection: TOneConnection;
    qryData: TOneDataSet;
    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbOpenDataClick(Sender: TObject);
    procedure tbSaveDataClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

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
  if OneConnection.DoConnect() then
  begin
    ShowMessage('接接成功');
  end
  else
  begin
    ShowMessage(OneConnection.ErrMsg);
  end;
end;

procedure TForm1.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TForm1.tbOpenDataClick(Sender: TObject);
var
  i: integer;
  lColumn: TColumn;
  tempMsg:string;
begin
  if not OneConnection.Connected then
  begin
    ShowMessage('未连接请先连接');
    exit;
  end;
  // 或者在控件UI直接设置 Connection
  qryData.DataInfo.Connection := OneConnection;
  qryData.SQL.Text := edSQL.Lines.Text;
  for i := 0 to qryData.Params.Count - 1 do
  begin

    if edSQLParams.Lines.Count >= i + 1 then
    begin
      qryData.Params[i].Value := edSQLParams.Lines[i];
    end;
  end;

  if trystrToInt(edPageSize.Text, i) then
  begin
    if i <= 0 then
      i := -1;
    qryData.DataInfo.PageSize := i;
  end;
  if trystrToInt(edPageNow.Text, i) then
  begin
    if i <= 0 then
      i := 1;
    qryData.DataInfo.PageIndex := i;
  end;
  if qryData.Active then
  begin
    qryData.Close;
    qryData.Fields.Clear;
  end;
  if not qryData.OpenData then
  begin
    ShowMessage(qryData.DataInfo.ErrMsg);
    exit;
  end;

  edPageCount.Text := qryData.DataInfo.PageCount.ToString;
  edPageTotal.Text := qryData.DataInfo.PageTotal.ToString;

  DBGrid1.Columns.Clear;
  for i := 0 to qryData.Fields.Count - 1 do
  begin
    tempMsg :=  qryData.Fields[i].FieldName;
    lColumn := DBGrid1.Columns.Add;
    lColumn.FieldName := qryData.Fields[i].FieldName;
    lColumn.Title.Caption := qryData.Fields[i].FieldName;
    lColumn.Width := 100;
  end;
  ShowMessage('打开数据成功');
end;

procedure TForm1.tbSaveDataClick(Sender: TObject);
begin
  qryData.DataInfo.Connection := OneConnection;
  qryData.DataInfo.TableName := edTableName.Text;
  qryData.DataInfo.PrimaryKey := edPrimaryKey.Text;
  if qryData.State in dsEditModes then
    qryData.Post;
  // 两个方法多行，最终调用同一个方法
  // if qryOpenData.save then
  if qryData.SaveData then
  begin
    ShowMessage('保存数据成功');
  end
  else
  begin
    ShowMessage(qryData.DataInfo.ErrMsg);
  end;
end;

end.
