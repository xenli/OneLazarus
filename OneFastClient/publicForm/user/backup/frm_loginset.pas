unit frm_LoginSet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, DBCtrls, frm_BaseStand, OneClientDataSet;

type

  { TfrmLoginSet }

  TfrmLoginSet = class(TfrmBaseStand)
    dbFHostCaption: TDBEdit;
    dbFHostName: TDBEdit;
    dbFHostKey: TDBEdit;
    dbFHostZTCode: TDBEdit;
    dbFHostZTCode1: TDBEdit;
    DBGrid1: TDBGrid;
    dsMain: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    qryMain: TOneDataSet;
    qryMainFHostCaption: TStringField;
    qryMainFHostKey: TStringField;
    qryMainFHostName: TStringField;
    qryMainFHostPort: TLongintField;
    qryMainFHostZTCode: TStringField;
    qryMainFPlatUserCode: TStringField;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure tbDelClick(Sender: TObject);
    procedure tbEditClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
  private

  public
    procedure SetCurrID(Value: string); override;
  end;

function ShowLoginSet: boolean;

var
  frmLoginSet: TfrmLoginSet;

implementation

{$R *.lfm}
uses AppManage, client_SetInfo, func_public;

{ TfrmLoginSet }


function ShowLoginSet: boolean;
begin
  try
    Result := False;
    frmLoginSet := TfrmLoginSet.Create(Application);
    if frmLoginSet.ShowModal = mrOk then
    begin
      Result := True;
    end;
  finally
    FreeAndNil(frmLoginSet);
  end;
end;

procedure TfrmLoginSet.FormCreate(Sender: TObject);
begin
  inherited;
  self.CurrID := '';
end;

procedure TfrmLoginSet.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TfrmLoginSet.tbDelClick(Sender: TObject);
begin
  inherited;
  if qryMain.IsEmpty then
  begin
    func_public.OkMsg('未选中任何服务器配置,无法执行删除动作');
    exit;
  end;
  qryMain.Delete;
end;

procedure TfrmLoginSet.tbEditClick(Sender: TObject);
var
  lAppManage: TAppManage;
  lErrMsg: string;
begin
  lErrMsg := '';
  if qryMain.IsEmpty then
  begin
    func_public.OkMsg('未选中任何服务器配置');
    exit;
  end;
  lAppManage := AppManage.UnitAppManage();
  if not func_public.YesNoMsg('确定要选中【' + qryMainFHostCaption.AsString + '】为当前服务器') then
    exit;
  // 设定当前服务器信息
  lAppManage.MyClientSet.IndexServer.HostCaption := qryMainFHostCaption.AsString;
  lAppManage.MyClientSet.IndexServer.HostName := qryMainFHostName.AsString;
  lAppManage.MyClientSet.IndexServer.HostPort := qryMainFHostPort.AsInteger;
  lAppManage.MyClientSet.IndexServer.HostZTCode := qryMainFHostZTCode.AsString;
  lAppManage.MyClientSet.IndexServer.HostKey := qryMainFHostKey.AsString;
  lAppManage.MyClientSet.IndexServer.PlatUserCode := qryMainFPlatUserCode.AsString;
  // 同步用户信息
  lAppManage.MyClientInfo.HostCaption := qryMainFHostCaption.AsString;
  lAppManage.MyClientInfo.HostName := qryMainFHostName.AsString;
  lAppManage.MyClientInfo.HostPort := qryMainFHostPort.AsInteger;
  lAppManage.MyClientInfo.HostZTCode := qryMainFHostZTCode.AsString;
  lAppManage.MyClientInfo.PlatUserCode := qryMainFPlatUserCode.AsString;
  lAppManage.MyClientInfo.HostKey := qryMainFHostKey.AsString;
  if not lAppManage.SaveMyClientSet(lErrMsg) then
  begin
    ShowMessage(lErrMsg);
  end;
  ModalResult := mrOk;
  exit;
end;

procedure TfrmLoginSet.tbNewClick(Sender: TObject);
begin
  inherited;
  qryMain.Append;
  qryMainFHostCaption.AsString := '新建服务';
  qryMainFHostName.AsString := 'http://127.0.0.1:9090';
  qryMain.Post;
end;

procedure TfrmLoginSet.tbRefreshClick(Sender: TObject);
begin
  inherited;
  self.CurrID := '';
end;

procedure TfrmLoginSet.tbSaveClick(Sender: TObject);
var
  lAppManage: TAppManage;
  lServerSet: TServerSet;
  lErrMsg: string;
begin
  inherited;
  if qryMain.State in dsEditModes then
    qryMain.Post;
  lAppManage := AppManage.UnitAppManage();
  lAppManage.MyClientSet.ClearServerList;
  qryMain.DisableControls;
  try
    qryMain.First;
    while not qryMain.EOF do
    begin
      lServerSet := TServerSet.Create;
      lAppManage.MyClientSet.ServerList.add(lServerSet);
      lServerSet.HostCaption := qryMainFHostCaption.AsString;
      lServerSet.HostName := qryMainFHostName.AsString;
      lServerSet.HostPort := qryMainFHostPort.AsInteger;
      lServerSet.HostZTCode := qryMainFHostZTCode.AsString;
      lServerSet.HostKey := qryMainFHostKey.AsString;
      lServerSet.PlatUserCode := qryMainFPlatUserCode.AsString;
      qryMain.Next;
    end;
  finally
    qryMain.EnableControls;
  end;
  lErrMsg := '';
  if not lAppManage.SaveMyClientSet(lErrMsg) then
  begin
    ShowMessage(lErrMsg);
  end
  else
  begin
    func_public.OkMsg('保存信息成功');
  end;
end;

procedure TfrmLoginSet.SetCurrID(Value: string);
var
  lAppManage: TAppManage;
  i: integer;
  lServerSet: TServerSet;
begin
  if qryMain.Active then
    qryMain.Close;
  qryMain.CreateDataSet;
  lAppManage := AppManage.UnitAppManage();
  for i := 0 to lAppManage.MyClientSet.ServerList.Count - 1 do
  begin
    lServerSet := lAppManage.MyClientSet.ServerList[i];
    qryMain.Append;
    qryMainFHostCaption.AsString := lServerSet.HostCaption;
    qryMainFHostName.AsString := lServerSet.HostName;
    qryMainFHostPort.AsInteger := lServerSet.HostPort;
    qryMainFHostKey.AsString := lServerSet.HostKey;
    qryMainFHostZTCode.AsString := lServerSet.HostZTCode;
    qryMainFPlatUserCode.AsString := lServerSet.PlatUserCode;
    qryMain.Post;
  end;
  qryMain.MergeChangeLog;
end;

end.
