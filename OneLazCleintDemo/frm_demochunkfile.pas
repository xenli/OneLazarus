unit frm_DemoChunkFile;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  OneClientConnect, OneClientVirtualFile, OneClientConst;

type

  { TfrmDemoChunkFile }

  TfrmDemoChunkFile = class(TForm)
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
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edLocalFileAList: TMemo;
    OneConnection: TOneConnection;
    OneVirtualFile: TOneVirtualFile;
    ProgressFile: TProgressBar;
    ProgressList: TProgressBar;
    tbConnect: TButton;
    tbConnectClose: TButton;
    tbDownLoad: TButton;
    tbUpLoad: TButton;
    tbUpLoadBatch: TButton;

    procedure tbConnectClick(Sender: TObject);
    procedure tbConnectCloseClick(Sender: TObject);
    procedure tbDownLoadClick(Sender: TObject);
    procedure tbUpLoadBatchClick(Sender: TObject);
    procedure tbUpLoadClick(Sender: TObject);
  private
    procedure doFileUpDownChunkCallBack(QUpDownMode: emUpDownMode; QStatus: emUpDownChunkStatus; QTotalSize: int64; QPosition: int64; QErrmsg: string);
  public

  end;

var
  frmDemoChunkFile: TfrmDemoChunkFile;

implementation

{$R *.lfm}

{ TfrmDemoChunkFile }

procedure TfrmDemoChunkFile.tbConnectClick(Sender: TObject);
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

procedure TfrmDemoChunkFile.doFileUpDownChunkCallBack(QUpDownMode: emUpDownMode; QStatus: emUpDownChunkStatus; QTotalSize: int64; QPosition: int64; QErrmsg: string);
begin
  application.ProcessMessages;
  case QStatus of
    emUpDownChunkStatus.upDownStart:
    begin
      //文件开始上传进度
      ProgressFile.Visible := True;
      ProgressFile.Max := QTotalSize;
      ProgressFile.Position := 0;
    end;
    emUpDownChunkStatus.upDownProcess:
    begin
      //文件下载进度
      ProgressFile.Visible := True;
      ProgressFile.Position := QPosition;
    end;
    emUpDownChunkStatus.upDownEnd:
    begin
      ProgressFile.Position := QPosition;
      ProgressFile.Visible := False;
      if not OneVirtualFile.IsBatch then
      begin
        if QUpDownMode=emUpDownMode.UpLoad then
          ShowMessage('文件上传成功')
        else
          ShowMessage('文件下载成功');
      end;
    end;
    emUpDownChunkStatus.upDownListStar:
    begin
      //多个文件上传开始
      ProgressList.Visible := True;
      ProgressList.Max := QTotalSize;
      ProgressList.Position := 0;
    end;
    emUpDownChunkStatus.upDownListProcess:
    begin
      ProgressList.Visible := True;
      ProgressList.Position := QPosition;
    end;
    emUpDownChunkStatus.upDownListEnd:
    begin
      ProgressList.Visible := False;
      ProgressList.Position := QPosition;
      if OneVirtualFile.IsBatch then
      begin
        if QUpDownMode=emUpDownMode.UpLoad then
          ShowMessage('文件列表上传成功')
        else
          ShowMessage('文件列表下载成功');
      end;
    end;
    emUpDownChunkStatus.upDownErr:
    begin
      //上传错误提示
      ShowMessage(QErrmsg);
    end;
  end;
end;

procedure TfrmDemoChunkFile.tbConnectCloseClick(Sender: TObject);
begin
  OneConnection.DisConnect;
  ShowMessage('断开连接');
end;

procedure TfrmDemoChunkFile.tbDownLoadClick(Sender: TObject);
begin
  OneVirtualFile.VirtualCode := edVirtualCodeB.Text;
  OneVirtualFile.RemoteFile := edRemoteFileB.Text;
  OneVirtualFile.LocalFile := edLocalFileB.Text;
  //OneVirtualFile.DownloadFileAsync(self.doFileUpDownChunkCallBack);
end;

procedure TfrmDemoChunkFile.tbUpLoadBatchClick(Sender: TObject);
var
  i: integer;
begin
  OneVirtualFile.VirtualCode := edVirtualCodeA.Text;
  OneVirtualFile.RemoteFile := edRemoteFileA.Text;
  OneVirtualFile.LocalFile := edLocalFileA.Text;
  OneVirtualFile.FileList.Clear;
  for i := 0 to edLocalFileAList.Lines.Count - 1 do
  begin
    OneVirtualFile.FileList.Add(edLocalFileAList.Lines[i]);
  end;
  OneVirtualFile.UploadFileListAsync(self.doFileUpDownChunkCallBack);
end;

procedure TfrmDemoChunkFile.tbUpLoadClick(Sender: TObject);
begin
  OneVirtualFile.VirtualCode := edVirtualCodeA.Text;
  OneVirtualFile.RemoteFile := edRemoteFileA.Text;
  OneVirtualFile.LocalFile := edLocalFileA.Text;
  //OneVirtualFile.UploadFileAsync(self.doFileUpDownChunkCallBack);
end;

end.
