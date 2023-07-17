unit OneClientVirtualFile;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

interface

uses OneClientConst, Classes, SysUtils, OneClientConnect, OneStreamString, Generics.Collections;

type
  evenAsynProcFile = procedure(QCallBack: EvenUpDownChunkCallBack) of object;

  TOneAsynFileThread = class(TThread)
  private
    FMyProce: evenAsynProcFile;
    FCallBack: EvenUpDownChunkCallBack;
  private
    procedure doProce();
  public
    class function CreateAnonymousThread(QProce: evenAsynProcFile; QCallBack: EvenUpDownChunkCallBack): TOneAsynFileThread; static;
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean; QProce: evenAsynProcFile; QCallBack: EvenUpDownChunkCallBack); overload;
  end;

  TOneVirtualFile = class(TComponent)
  private
    FConnection: TOneConnection;
    FVirtualCode: string;
    FRemoteFile: string;
    FLocalFile: string;
    FReturnFileName: string;
    FChunkBlock: integer;
    FUpDownChunkCallBack: EvenUpDownChunkCallBack;
    FErrMsg: string;
    {批量上传或下载文件名称}
    FIsBatch: boolean;
    FFileList: TList<string>;
  private
    function GetConnection: TOneConnection;
    procedure SetConnection(const AValue: TOneConnection);
    {分块上传}
    procedure DoUploadChunkFile(QCallBack: EvenUpDownChunkCallBack);
    procedure DoDownloadChunkFile(QCallBack: EvenUpDownChunkCallBack);
  public
    {小文件上传下载}
    function UploadFile(): boolean;
    function DownloadFile(): boolean;
    {异步分块上传下载}
    procedure UploadChunkFileAsync(QEven: EvenUpDownChunkCallBack);
    procedure DownloadChunkFileAsync(QEven: EvenUpDownChunkCallBack);
    {批量上传文件}
    procedure UploadFileListAsync(QEven: EvenUpDownChunkCallBack);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property FileList: TList<string> read FFileList write FFileList;
    property IsBatch: boolean read FIsBatch;
  published
    property Connection: TOneConnection read GetConnection write SetConnection;
    property VirtualCode: string read FVirtualCode write FVirtualCode;
    property RemoteFile: string read FRemoteFile write FRemoteFile;
    property LocalFile: string read FLocalFile write FLocalFile;
    property ReturnFileName: string read FReturnFileName write FReturnFileName;
    property ChunkBlock: integer read FChunkBlock write FChunkBlock;
    property ErrMsg: string read FErrMsg write FErrMsg;
    property onUpDownChunkCallBack: EvenUpDownChunkCallBack read FUpDownChunkCallBack write FUpDownChunkCallBack;
  end;

procedure Register;

implementation

uses OneFileHelper;

procedure Register;
begin
  RegisterComponents('OneClient', [TOneVirtualFile]);
end;


{使用方法: TOneAsynThread.CreateAnonymousThread(QProce).start;}
class function TOneAsynFileThread.CreateAnonymousThread(QProce: evenAsynProcFile; QCallBack: EvenUpDownChunkCallBack): TOneAsynFileThread;
begin
  Result := TOneAsynFileThread.Create(True, QProce, QCallBack);
end;

constructor TOneAsynFileThread.Create(CreateSuspended: boolean; QProce: evenAsynProcFile; QCallBack: EvenUpDownChunkCallBack);
begin
  inherited Create(CreateSuspended);
  FMyProce := QProce;
  FCallBack := QCallBack;
end;

procedure TOneAsynFileThread.doProce();
begin
  FMyProce(FCallBack);
end;

procedure TOneAsynFileThread.Execute;
begin
  if Assigned(FMyProce) then
  begin
    TThread.Synchronize(nil, doProce);
  end;
end;

{TOneVirtualFile}
constructor TOneVirtualFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsBatch := False;
  FFileList := TList<string>.Create;
end;

destructor TOneVirtualFile.Destroy;
begin
  FFileList.Clear;
  FFileList.Free;
  inherited Destroy;
end;

function TOneVirtualFile.GetConnection: TOneConnection;
begin
  Result := self.FConnection;
end;

procedure TOneVirtualFile.SetConnection(const AValue: TOneConnection);
begin
  self.FConnection := AValue;
end;

function TOneVirtualFile.UploadFile(): boolean;
var
  lVirtualInfo: TVirtualInfo;
begin
  Result := False;
  if self.FConnection = nil then
    self.FConnection := OneClientConnect.Unit_Connection;
  if self.FConnection = nil then
  begin
    self.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if not self.FConnection.Connected then
  begin
    self.ErrMsg := '服务器未连接';
    exit;
  end;
  lVirtualInfo := TVirtualInfo.Create;
  try
    lVirtualInfo.VirtualCode := self.VirtualCode;
    lVirtualInfo.RemoteFile := self.RemoteFile;
    lVirtualInfo.LocalFile := self.LocalFile;
    Result := self.FConnection.UploadFile(lVirtualInfo);
    self.ErrMsg := lVirtualInfo.ErrMsg;
    self.ReturnFileName := lVirtualInfo.RemoteFileName;
  finally
    lVirtualInfo.Free;
  end;
end;

function TOneVirtualFile.DownloadFile(): boolean;
var
  lVirtualInfo: TVirtualInfo;
begin
  Result := False;
  if self.FConnection = nil then
    self.FConnection := OneClientConnect.Unit_Connection;
  if self.FConnection = nil then
  begin
    self.FErrMsg := '数据集Connection=nil';
    exit;
  end;
  if not self.FConnection.Connected then
  begin
    self.ErrMsg := '服务器未连接';
    exit;
  end;
  lVirtualInfo := TVirtualInfo.Create;
  try
    lVirtualInfo.VirtualCode := self.VirtualCode;
    lVirtualInfo.RemoteFile := self.RemoteFile;
    lVirtualInfo.LocalFile := self.LocalFile;
    Result := self.FConnection.DownloadFile(lVirtualInfo);
    self.ErrMsg := lVirtualInfo.ErrMsg;
  finally
    lVirtualInfo.Free;
  end;
end;

procedure TOneVirtualFile.UploadChunkFileAsync(QEven: EvenUpDownChunkCallBack);
begin
  if not Assigned(QEven) then
  begin
    QEven := self.FUpDownChunkCallBack;
  end;
  self.FIsBatch := False;
  self.FFileList.Clear;
  TOneAsynFileThread.CreateAnonymousThread(self.DoUploadChunkFile, QEven).Start;
end;

procedure TOneVirtualFile.UploadFileListAsync(QEven: EvenUpDownChunkCallBack);
begin
  if not Assigned(QEven) then
  begin
    QEven := self.FUpDownChunkCallBack;
  end;
  self.FIsBatch := True;
  TOneAsynFileThread.CreateAnonymousThread(self.DoUploadChunkFile, QEven).Start;
end;

procedure TOneVirtualFile.DownloadChunkFileAsync(QEven: EvenUpDownChunkCallBack);
begin
  if not Assigned(QEven) then
  begin
    QEven := self.FUpDownChunkCallBack;
  end;
  self.FIsBatch := False;
  self.FFileList.Clear;
  TOneAsynFileThread.CreateAnonymousThread(self.DoDownloadChunkFile, QEven).Start;
end;

procedure TOneVirtualFile.DoUploadChunkFile(QCallBack: EvenUpDownChunkCallBack);
var
  lTask: TVirtualTask;
  lLoclFile: string;
  iFile, iCount: integer;
begin
  if self.FConnection = nil then
    self.FConnection := OneClientConnect.Unit_Connection;
  if self.FConnection = nil then
  begin
    self.FErrMsg := '文件上传控件服务器连接Connection=nil';
    if Assigned(QCallBack) then
    begin
      QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownErr, 0, 0, self.FErrMsg);
    end;
    exit;
  end;
  if not self.FConnection.Connected then
  begin
    self.ErrMsg := '服务器未连接';
    QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownErr, 0, 0, self.FErrMsg);
    exit;
  end;
  {单个文件上传时，把文件名称加到批量列表}
  if not self.FIsBatch then
  begin
    self.FFileList.Clear;
    self.FFileList.Add(self.LocalFile);
  end;
  iCount := self.FFileList.Count;
  {批量上传文件}
  if Assigned(QCallBack) and (self.FIsBatch) then
  begin
    QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownListStar, iCount, 0, '');
  end;
  for iFile := 0 to iCount - 1 do
  begin
    lLoclFile := self.FFileList[iFile];
    if lLoclFile.Trim() = '' then
    begin
      continue;
    end;
    lTask := TVirtualTask.Create;
    try
      lTask.VirtualCode := self.VirtualCode;
      lTask.RemoteFile := self.RemoteFile;
      lTask.LocalFile := lLoclFile;
      if self.FIsBatch then
      begin
        lTask.RemoteFile := ExtractFilePath(lTask.RemoteFile);
        lTask.RemoteFile := lTask.RemoteFile + ExtractFileName(lTask.LocalFile);
      end;
      if not self.FConnection.UploadChunkFile(lTask, QCallBack) then
      begin
        self.ErrMsg := lTask.ErrMsg;
      end;
      self.ReturnFileName := lTask.NewFileName;
    finally
      lTask.Free;
    end;
    if Assigned(QCallBack) and (self.FIsBatch) then
    begin
      if iFile = iCount - 1 then
        QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownListEnd, iCount, iFile, '')
      else
        QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownListProcess, iCount, iFile, '');
    end;
  end;
end;

procedure TOneVirtualFile.DoDownloadChunkFile(QCallBack: EvenUpDownChunkCallBack);
var
  lTask: TVirtualTask;
begin
  if self.FConnection = nil then
    self.FConnection := OneClientConnect.Unit_Connection;
  if self.FConnection = nil then
  begin
    self.FErrMsg := '文件上传控件服务器连接Connection=nil';
    if Assigned(QCallBack) then
    begin
      QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownErr, 0, 0, self.FErrMsg);
    end;
    exit;
  end;
  if not self.FConnection.Connected then
  begin
    self.ErrMsg := '服务器未连接';
    QCallBack(emUpDownMode.UpLoad, emUpDownChunkStatus.upDownErr, 0, 0, self.FErrMsg);
    exit;
  end;

  lTask := TVirtualTask.Create;
  try
    lTask.VirtualCode := self.VirtualCode;
    lTask.RemoteFile := self.RemoteFile;
    lTask.LocalFile := self.LocalFile;
    self.FConnection.DownloadChunkFile(lTask, QCallBack);
    self.ErrMsg := lTask.ErrMsg;
  finally
    lTask.Free;
  end;
end;

end.
