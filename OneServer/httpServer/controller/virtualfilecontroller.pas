unit VirtualFileController;

{$mode DELPHI}{$H+}
// 此单元主要对接 oneClient文件上传下载
interface

uses OneHttpController, OneHttpCtxtResult, OneHttpRouterManage, SysUtils, rtti,
  Generics.Collections, Contnrs, Classes, DB, OneControllerResult, OneVirtualFile;

type
  {$M+}
  IOneVirtualFileController = interface
    ['{6E51FF99-E2D3-44EC-8FA7-B1F8D83EADB7}']
    function UploadFile(QVirtualInfo: TVirtualInfo): TActionResultString;
    function DownloadFile(QVirtualInfo: TVirtualInfo): TActionResultString;
    function GetTaskID(QVirtualTask: TVirtualTask): TActionResult<TVirtualTask>;
    function UploadChunkFile(QVirtualTask: TVirtualTask): TActionResultString;
    function DownloadChunkFile(QVirtualTask: TVirtualTask): TActionResultString;
  end;

 {$M-}


  TOneVirtualFileController = class(TOneControllerBase, IOneVirtualFileController)
  public
    //各个方法实现
    function DoInvoke(QRttiMethod: TRttiMethod; const aArgs: array of TValue): TValue;
      override;
    function UploadFile(QVirtualInfo: TVirtualInfo): TActionResultString;
    function DownloadFile(QVirtualInfo: TVirtualInfo): TActionResultString;
    function GetTaskID(QVirtualTask: TVirtualTask): TActionResult<TVirtualTask>;
    function UploadChunkFile(QVirtualTask: TVirtualTask): TActionResultString;
    function DownloadChunkFile(QVirtualTask: TVirtualTask): TActionResultString;
  end;

function CreateNewVirtualFileController(QRouterItem: TOneRouterItem): TObject;

implementation

uses OneGlobal, OneFileHelper, OneStreamString;

function CreateNewVirtualFileController(QRouterItem: TOneRouterItem): TObject;
var
  lController: TOneVirtualFileController;
begin
  // 自定义创建控制器类，否则会按 TPersistentclass.create
  // 最好自定义一个好
  lController := TOneVirtualFileController.Create;
  // 挂载RTTI信息
  lController.RouterItem := QRouterItem;
  Result := lController;
end;

function TOneVirtualFileController.DoInvoke(QRttiMethod: TRttiMethod;
  const aArgs: array of TValue): TValue;
var
  lIController: IOneVirtualFileController;
  lMethodName: string;
begin
  Result := nil;
  lMethodName := QRttiMethod.Name;
  {$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
  //只有这几种CPU硬件支持 反射方法
  //转化成接口
  lIController := self as IOneVirtualFileController;
  //进行接口RTTI代理
  Result := QRttiMethod.Invoke(lIController, aArgs);
  exit;
  {$endif}
  //其它CPU硬件不支持,那么只能手写一个一个判断
  //希望后面加强其它CPU的反射,那么就不会这么麻烦了
  if lMethodName = 'UploadFile' then
  begin
    Result := self.UploadFile(TVirtualInfo(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'DownloadFile' then
  begin
    Result := self.DownloadFile(TVirtualInfo(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'GetTaskID' then
  begin
    Result := self.GetTaskID(TVirtualTask(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'UploadChunkFile' then
  begin
    Result := self.UploadChunkFile(TVirtualTask(aArgs[0].AsObject));
  end
  else
  if lMethodName = 'DownloadChunkFile' then
  begin
    Result := self.DownloadChunkFile(TVirtualTask(aArgs[0].AsObject));
  end
  else
  begin
    raise Exception.Create(lMethodName + '未关联方法,请注意方法大小写');
  end;
end;

function TOneVirtualFileController.UploadFile(QVirtualInfo:
  TVirtualInfo): TActionResultString;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
  lFileName, lExten, lPath: string;
  lVirtualItem: TOneVirtualItem;
  lInStream: TMemoryStream;
begin
  Result := TActionResultString.Create;
  if QVirtualInfo = nil then
  begin
    Result.ResultMsg := '传进的参数，无法转化成 TVirtualInfo类';
    exit;
  end;
  if QVirtualInfo.VirtualCode = '' then
  begin
    Result.ResultMsg := '虚拟路径代码[VirtualCode]为空,请检查';
    exit;
  end;
  if QVirtualInfo.RemoteFile = '' then
  begin
    Result.ResultMsg := '路径文件[RemoteFile]为空,请检查';
    exit;
  end;
  if OneFileHelper.DriveExists(QVirtualInfo.RemoteFile) then
  begin
    Result.ResultMsg :=
      '路径文件[RemoteFile]不可包含驱动，只能是路径/xxx/xxxx/xx';
    exit;
  end;
  if not OneFileHelper.HasExtension(QVirtualInfo.RemoteFile) then
  begin
    Result.ResultMsg :=
      '路径文件[RemoteFile]无文件扩展名,无法确这是路径,还是文件完整路径';
    exit;
  end;
  if QVirtualInfo.StreamBase64 = '' then
  begin
    Result.ResultMsg := '文件内容[StreamBase64]为空,请检查';
    exit;
  end;
  lFileName := ExtractFileName(QVirtualInfo.RemoteFile);
  if lFileName = '' then
  begin
    Result.ResultMsg := '要保存的的文件名秒为空,请检查文件路径';
    exit;
  end;

  lOneGlobal := TOneGlobal.GetInstance();
  lVirtualItem := lOneGlobal.VirtualManage.GetVirtual(QVirtualInfo.VirtualCode, lErrMsg);
  if lErrMsg <> '' then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  // 组装文件路径
  QVirtualInfo.RemoteFile := OneFileHelper.CombinePath(lVirtualItem.PhyPath,
    QVirtualInfo.RemoteFile);
  lPath := ExtractFilePath(QVirtualInfo.RemoteFile);
  if not DirectoryExists(lPath) then
  begin
    ForceDirectories(lPath);
    ;
  end;
  if FileExists(QVirtualInfo.RemoteFile) then
  begin
    // 变个文件名
    lFileName := ExtractFileName(QVirtualInfo.RemoteFile);
    lExten := ExtractFileExt(QVirtualInfo.RemoteFile);
    lFileName := lFileName + '_' + lOneGlobal.VirtualManage.GetFileLsh() + lExten;
    QVirtualInfo.RemoteFile := OneFileHelper.CombinePath(lPath, lFileName);
  end;
  // 拼接文件
  lInStream := TMemoryStream.Create;
  try
    // 反写base64到流中
    OneStreamString.StreamWriteBase64Str(lInStream, QVirtualInfo.StreamBase64);
    // 解压流
    lInStream.Position := 0;
    lInStream.SaveToFile(QVirtualInfo.RemoteFile);
  finally
    lInStream.Clear;
    lInStream.Free;
  end;
  // 返回新的文件名给前端
  Result.ResultData := lFileName;
  // 返回的是文件
  Result.ResultMsg := '保存文件成功';
  Result.SetResultTrue();
end;

function TOneVirtualFileController.DownloadFile(QVirtualInfo:
  TVirtualInfo): TActionResultString;
var
  lOneGlobal: TOneGlobal;
  lErrMsg: string;
  lFileName: string;
  lVirtualItem: TOneVirtualItem;
  lFileStream: TFileStream;
begin
  Result := TActionResultString.Create;
  if QVirtualInfo = nil then
  begin
    Result.ResultMsg := '传进的参数，无法转化成 TVirtualInfo类';
    exit;
  end;
  if QVirtualInfo.VirtualCode = '' then
  begin
    Result.ResultMsg := '虚拟路径代码[VirtualCode]为空,请检查';
    exit;
  end;
  if QVirtualInfo.RemoteFile = '' then
  begin
    Result.ResultMsg := '路径文件[RemoteFile]为空,请检查';
    exit;
  end;
  if not OneFileHelper.HasExtension(QVirtualInfo.RemoteFile) then
  begin
    Result.ResultMsg :=
      '路径文件[RemoteFile]无文件扩展名称无法确定是路径还是文件';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lVirtualItem := lOneGlobal.VirtualManage.GetVirtual(QVirtualInfo.VirtualCode, lErrMsg);
  if lErrMsg <> '' then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  lFileName := ExtractFileName(QVirtualInfo.RemoteFile);
  if lFileName = '' then
  begin
    Result.ResultMsg := '文件名称为空,请检查[' + QVirtualInfo.RemoteFile + ']';
    exit;
  end;
  // 拼接文件
  lFileName := OneFileHelper.CombinePath(lVirtualItem.PhyPath, QVirtualInfo.RemoteFile);
  if not FileExists(lFileName) then
  begin
    Result.ResultMsg := '服务端文件不存在请检查';
    exit;
  end;
  lFileStream := TFileStream.Create(lFileName, fmopenRead and fmShareDenyWrite);
  try
    // 大于10M文件,请用分块下载
    if lFileStream.Size > 1024 * 1024 * 10 then
    begin
      Result.ResultMsg := '文件过大超过10M,请用分块下载';
      //Result.ResultData := 'bigfile';
      exit;
    end;
    lFileStream.Position := 0;
    Result.ResultData := OneStreamString.StreamToBase64Str(lFileStream);
    Result.SetResultTrue();
  finally
    lFileStream.Free;
  end;
end;

function TOneVirtualFileController.GetTaskID(QVirtualTask: TVirtualTask): TActionResult<
TVirtualTask>;
var
  lServerTask, lResultTask: TVirtualTask;
  lOneGlobal: TOneGlobal;
  lVirtualItem: TOneVirtualItem;
  lPath, lFileName, lExten, lNewFileName: string;
  lErrMsg: string;
  lFileStream: TFileStream;
begin
  Result := TActionResult<TVirtualTask>.Create(True, False);
  if (QVirtualTask.UpDownMode <> '上传') and (QVirtualTask.UpDownMode <> '下载') then
  begin
    Result.ResultMsg := '文件模式[UpDownMode]只能是上传或下载';
    exit;
  end;
  if QVirtualTask.VirtualCode = '' then
  begin
    Result.ResultMsg := '虚拟路径代码[VirtualCode]为空,请检查';
    exit;
  end;
  if QVirtualTask.RemoteFile = '' then
  begin
    Result.ResultMsg := '路径文件[RemoteFile]为空,请检查';
    exit;
  end;
  if OneFileHelper.DriveExists(QVirtualTask.RemoteFile) then
  begin
    Result.ResultMsg :=
      '路径文件[RemoteFile]不可包含驱动，只能是路径/xxx/xxxx/xx';
    exit;
  end;
  if not OneFileHelper.HasExtension(QVirtualTask.RemoteFile) then
  begin
    Result.ResultMsg :=
      '路径文件[RemoteFile]无文件扩展名,无法确这是路径,还是文件完整路径';
    exit;
  end;
  lFileName := ExtractFileName(QVirtualTask.RemoteFile);
  if lFileName = '' then
  begin
    Result.ResultMsg := '路径文件[RemoteFile]无法取到相关文件名';
    exit;
  end;
  // 组装真实路径
  lOneGlobal := TOneGlobal.GetInstance();
  lVirtualItem := lOneGlobal.VirtualManage.GetVirtual(QVirtualTask.VirtualCode, lErrMsg);
  if lErrMsg <> '' then
  begin
    Result.ResultMsg := lErrMsg;
    exit;
  end;
  QVirtualTask.RemoteFile := OneFileHelper.CombinePath(lVirtualItem.PhyPath,
    QVirtualTask.RemoteFile);
  // 上传特有
  if (QVirtualTask.UpDownMode = '上传') then
  begin

    lPath := ExtractFilePath(QVirtualTask.RemoteFile);
    if not DirectoryExists(lPath) then
    begin
      ForceDirectories(lPath);
    end;
    if FileExists(QVirtualTask.RemoteFile) then
    begin
      // 变个文件名
      lNewFileName := ExtractFileName(QVirtualTask.RemoteFile);
      lExten := ExtractFileExt(QVirtualTask.RemoteFile);
      lNewFileName := lNewFileName + '_' +
        lOneGlobal.VirtualManage.GetFileLsh() + lExten;
      QVirtualTask.RemoteFile := OneFileHelper.CombinePath(lPath, lNewFileName);
    end;
  end
  else if (QVirtualTask.UpDownMode = '下载') then
  begin
    if not FileExists(QVirtualTask.RemoteFile) then
    begin
      Result.ResultMsg := '服务端文件不存在请检查';
      exit;
    end;
    lFileStream := TFileStream.Create(QVirtualTask.RemoteFile, fmShareDenyNone);
    try
      QVirtualTask.FileTotalSize := lFileStream.Size;
    finally
      lFileStream.Free;
    end;
  end;
  if lNewFileName = '' then
    lNewFileName := lFileName;
  lOneGlobal := TOneGlobal.GetInstance();
  lServerTask := lOneGlobal.VirtualManage.CreateNewTask();
  // 下载有这个值
  lServerTask.FileTotalSize := QVirtualTask.FileTotalSize;
  lServerTask.FilePosition := 0;
  lServerTask.VirtualCode := QVirtualTask.VirtualCode;
  lServerTask.RemoteFile := QVirtualTask.RemoteFile;
  lServerTask.StreamBase64 := '';
  lServerTask.UpDownMode := QVirtualTask.UpDownMode;
  lServerTask.FileName := lFileName;
  lServerTask.NewFileName := lNewFileName;
  lServerTask.LocalFile := QVirtualTask.LocalFile;
  // 返回这三个信息就够了
  lResultTask := TVirtualTask.Create;
  lResultTask.TaskID := lServerTask.TaskID;
  lResultTask.FileTotalSize := QVirtualTask.FileTotalSize;
  lResultTask.NewFileName := lNewFileName;
  Result.ResultDataT := lResultTask;
  Result.SetResultTrue();
end;

function TOneVirtualFileController.UploadChunkFile(QVirtualTask:
  TVirtualTask): TActionResultString;
var
  lTask: TVirtualTask;
  lOneGlobal: TOneGlobal;
  lFileStream: TFileStream;
  lInStream: TMemoryStream;
begin
  Result := TActionResultString.Create;
  if QVirtualTask.TaskID = '' then
  begin
    Result.ResultMsg := '上传任务taskID为空,请先申请一个taskID';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lTask := lOneGlobal.VirtualManage.GetTask(QVirtualTask.TaskID);
  if lTask = nil then
  begin
    Result.ResultMsg := '任务已不存在,请检查任务ID';
    exit;
  end;
  // 第一次需要上传相关信息
  if QVirtualTask.StreamBase64 = '' then
  begin
    Result.ResultMsg := '文件内容[StreamBase64]为空,请检查';
    exit;
  end;

  lInStream := TMemoryStream.Create;
  if FileExists(lTask.RemoteFile) then
  begin
    lFileStream := TFileStream.Create(lTask.RemoteFile, fmOpenReadWrite);
  end
  else
  begin
    lFileStream := TFileStream.Create(lTask.RemoteFile, fmcreate);
  end;
  try
    OneStreamString.StreamWriteBase64Str(lInStream, QVirtualTask.StreamBase64);
    lInStream.Position := 0;
    lFileStream.Position := QVirtualTask.FilePosition;
    lFileStream.CopyFrom(lInStream, lInStream.Size);
    lTask.FilePosition := QVirtualTask.FilePosition;
    if lTask.FilePosition + lInStream.Size >= lTask.FileTotalSize then
    begin
      lTask.FilePosition := lTask.FilePosition + lInStream.Size;
      lTask.IsEnd := True;
    end;
    Result.SetResultTrue;
  finally
    lFileStream.Free;
    lInStream.Free;
  end;
end;

function TOneVirtualFileController.DownloadChunkFile(QVirtualTask:
  TVirtualTask): TActionResultString;
var
  lTask: TVirtualTask;
  lOneGlobal: TOneGlobal;
  lFileStream: TFileStream;
  tempStream: TMemoryStream;
begin
  Result := TActionResultString.Create();
  if QVirtualTask.TaskID = '' then
  begin
    Result.ResultMsg := '上传任务taskID为空,请先申请一个taskID';
    exit;
  end;
  lOneGlobal := TOneGlobal.GetInstance();
  lTask := lOneGlobal.VirtualManage.GetTask(QVirtualTask.TaskID);
  if lTask = nil then
  begin
    Result.ResultMsg := '任务已不存在,请检查任务ID';
    exit;
  end;
  if QVirtualTask.FileChunSize <= 0 then
    QVirtualTask.FileChunSize := 1024 * 1024 * 1;
  if QVirtualTask.FilePosition >= lTask.FileTotalSize - 1 then
  begin
    Result.ResultMsg := '已下载完成';
    //Result.ResultData := '';
    Result.SetResultTrue;
    exit;
  end;
  if QVirtualTask.FilePosition + QVirtualTask.FileChunSize > lTask.FileTotalSize then
  begin
    QVirtualTask.FileChunSize := lTask.FileTotalSize - QVirtualTask.FilePosition;
  end;

  lFileStream := TFileStream.Create(lTask.RemoteFile, fmopenRead);
  tempStream := TMemoryStream.Create;
  try
    lFileStream.Position := QVirtualTask.FilePosition;
    tempStream.CopyFrom(lFileStream, QVirtualTask.FileChunSize);
    Result.ResultData := OneStreamString.StreamToBase64Str(tempStream);
    Result.SetResultTrue;
  finally
    lFileStream.Free;
  end;
end;

// 注册到路由
initialization

  // 注意，路由名称 不要一样，否则会判定已注册过，跳过
  // 多例模式注册
  OneHttpRouterManage.GetInitRouterManage().AddHTTPSingleWork(
    'OneServer/VirtualFile', TOneVirtualFileController,
    TypeInfo(IOneVirtualFileController),
    0, CreateNewVirtualFileController);

finalization

end.
