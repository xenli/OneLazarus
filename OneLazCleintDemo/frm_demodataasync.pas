unit frm_demoDataAsync;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBGrids, OneClientConnect, OneClientDataSet, OneAsynThread;

type
  { TfrmDemoDataAsync }

  TfrmDemoDataAsync = class(TForm)
    tbOpenDataAsynB: TButton;
    DBGrid1: TDBGrid;
    dsData: TDataSource;
    edPageCount: TEdit;
    edPageNow: TEdit;
    edPageSize: TEdit;
    edPageTotal: TEdit;
    edPrimaryKey: TEdit;
    edServerHost: TEdit;
    edServerKey: TEdit;
    edServerPort: TEdit;
    edServerZTCode: TEdit;
    edSQL: TMemo;
    edSQLParams: TMemo;
    edTableName: TEdit;
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
    qryData: TOneDataSet;
    tbConnect: TButton;
    tbConnectClose: TButton;
    tbOpenData: TButton;
    tbOpenDataAsync: TButton;
    tbSaveAsync: TButton;
    tbSaveAsyncB: TButton;
    procedure Button1Click(Sender: TObject);
    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbOpenDataAsynBClick(Sender: TObject);
    procedure tbOpenDataAsyncClick(Sender: TObject);
    procedure tbOpenDataClick(Sender: TObject);
    procedure tbSaveAsyncBClick(Sender: TObject);
    procedure tbSaveAsyncClick(Sender: TObject);
  private
    procedure DoOpenData();
    procedure DoSaveData();
  public

  end;

var
  frmDemoDataAsync: TfrmDemoDataAsync;

implementation

{$R *.lfm}

procedure TfrmDemoDataAsync.tbConnectClick(Sender: TObject);
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

procedure TfrmDemoDataAsync.Button1Click(Sender: TObject);
begin
  self.DoSaveData();
end;


procedure TfrmDemoDataAsync.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TfrmDemoDataAsync.tbOpenDataAsynBClick(Sender: TObject);
var
  lThread: TOneAsynThread;
begin
  lThread := TOneAsynThread.Create(True, self.DoOpenData);
  lThread.Start;
end;

procedure TfrmDemoDataAsync.tbOpenDataAsyncClick(Sender: TObject);
begin
  qryData.OpenDataAsync(self.DoOpenData);
  //下面的代码不会等 OpenDataAsync执行完成，直接先执行
  showMessage('异步打开数据,我不会等待OpenData执行完成在执行');
end;

procedure TfrmDemoDataAsync.tbOpenDataClick(Sender: TObject);
var
  i: integer;
  lColumn: TColumn;
begin
  self.DoOpenData();
end;

procedure TfrmDemoDataAsync.tbSaveAsyncBClick(Sender: TObject);
var
  lThread: TOneAsynThread;
begin
  lThread := TOneAsynThread.Create(True, self.DoSaveData);
  lThread.Start;
end;

procedure TfrmDemoDataAsync.tbSaveAsyncClick(Sender: TObject);
begin
  qryData.SaveDataAsync(self.DoSaveData);
end;

procedure TfrmDemoDataAsync.DoOpenData();
var
  i: integer;
  iCol: integer;
  lColumn: TColumn;
begin
  if not OneConnection.Connected then
  begin
    ShowMessage('未连接请先连接');
    exit;
  end;
  {或者在控件UI直接设置 Connection}
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

  for iCol := 0 to qryData.Fields.Count - 1 do
  begin
    lColumn := DBGrid1.Columns.Add;
    lColumn.FieldName := qryData.Fields[iCol].FieldName;
    lColumn.Title.Caption := qryData.Fields[iCol].FieldName;
    lColumn.Width := 100;
  end;
  ShowMessage('打开数据成功');
end;

procedure  TfrmDemoDataAsync.DoSaveData();
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
