unit frm_demoVirtualFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  OneClientConnect, OneClientVirtualFile;

type

  { TfrmDemoVirtualFile }

  TfrmDemoVirtualFile = class(TForm)
    edLocalFileA: TEdit;
    edLocalFileB: TEdit;
    edNewFileName: TEdit;
    edRemark: TMemo;
    edRemoteFileA: TEdit;
    edRemoteFileB: TEdit;
    edServerHost: TEdit;
    edServerKey: TEdit;
    edServerPort: TEdit;
    edServerZTCode: TEdit;
    edVirtualCodeA: TEdit;
    edVirtualCodeB: TEdit;
    GroupBox1: TGroupBox;
    groupDown: TGroupBox;
    groupUpload: TGroupBox;
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
    OneVirtualFile: TOneVirtualFile;
    tbConnect: TButton;
    tbConnectClose: TButton;
    tbDownLoad: TButton;
    tbUpLoad: TButton;
    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbDownLoadClick(Sender: TObject);
    procedure tbUpLoadClick(Sender: TObject);
  private

  public

  end;

var
  frmDemoVirtualFile: TfrmDemoVirtualFile;

implementation

{$R *.lfm}

{ TfrmDemoVirtualFile }

procedure TfrmDemoVirtualFile.tbConnectClick(Sender: TObject);
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

procedure TfrmDemoVirtualFile.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TfrmDemoVirtualFile.tbDownLoadClick(Sender: TObject);
begin
  OneVirtualFile.VirtualCode := edVirtualCodeB.Text;
  OneVirtualFile.RemoteFile := edRemoteFileB.Text;
  OneVirtualFile.LocalFile := edLocalFileB.Text;
  if OneVirtualFile.DownloadFile() then
  begin
    ShowMessage('下载文件成功');
  end
  else
  begin
    ShowMessage(OneVirtualFile.ErrMsg);
  end;
end;

procedure TfrmDemoVirtualFile.tbUpLoadClick(Sender: TObject);
begin
  OneVirtualFile.VirtualCode := edVirtualCodeA.Text;
  OneVirtualFile.RemoteFile := edRemoteFileA.Text;
  OneVirtualFile.LocalFile := edLocalFileA.Text;
  if OneVirtualFile.UploadFile() then
  begin
    edNewFileName.Text := OneVirtualFile.ReturnFileName;
    ShowMessage('上传文件成功,最终保存文件名称[' + OneVirtualFile.ReturnFileName + ']');
  end
  else
  begin
    ShowMessage(OneVirtualFile.ErrMsg);
  end;
end;

end.
