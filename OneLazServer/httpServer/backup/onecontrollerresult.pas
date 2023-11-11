unit OneControllerResult;

{$mode DELPHI}{$H+}

interface

uses  Classes, SysUtils, Generics.Collections, Rtti,
  OneHttpConst, OneSerialization;

type
  emOneResultMode = (resultString, OneGet, OnePost, OneForm, OneUpload,
    OneDownload);

type
  IActionResult = interface;

  IActionResult = interface
    ['{FBE2DEB1-9319-4DD4-93EC-17FF6E68A2C3}']
  end;

  TActionResultBase = class
  private
    FResultSuccess: boolean;
    FResultCode: string;
    FResultMsg: string;
    FIsFile: boolean;
  public
    procedure SetResultTrue();
    // 文件相关
    procedure SetResultTrueFile();
    function IsResultFile(): boolean;
    procedure SetTokenFail();
  public
    property ResultSuccess: boolean read FResultSuccess write FResultSuccess;
    property ResultCode: string read FResultCode write FResultCode;
    property ResultMsg: string read FResultMsg write FResultMsg;
  end;

  TActionResultString = class(TActionResultBase)
  private
    FResultData: string;
  published
    property ResultSuccess;
    property ResultCode;
    property ResultMsg;
    property ResultData: string read FResultData write FResultData;
  end;

  TActionResultObject = class(TActionResultBase)
  private
    FResultData: TObject;
    FFreeResultData: boolean;
    FFreeListItem: boolean;
  public
    constructor Create(QFreeResultData: boolean; QFreeListItem: boolean); overload;
    destructor Destroy; override;
  published
    property ResultSuccess;
    property ResultCode;
    property ResultMsg;
    property ResultData: TObject read FResultData write FResultData;
  end;

  TActionResult<T> = class(TActionResultObject)
  private
    FResultDataT: T;
  private
    procedure SetDataT(QValue: T);
  public
    property ResultDataT: T read FResultDataT write SetDataT;
    constructor Create(QFreeResultData: boolean; QFreeListItem: boolean); overload;
    //destructor Destroy;override;
  end;

  TActionResultInteger = class(TActionResultBase)
  private
    FResultData: integer;
  published
    property ResultSuccess;
    property ResultCode;
    property ResultMsg;
    property ResultData: integer read FResultData write FResultData;
  end;

  TActionResultBoolean = class(TActionResultBase)
  private
    FResultData: boolean;
  published
    property ResultSuccess;
    property ResultCode;
    property ResultMsg;
    property ResultData: boolean read FResultData write FResultData;
  end;

  TActionResultInt64 = class(TActionResultBase)
  private
    FResultData: int64;
  published
    property ResultSuccess;
    property ResultCode;
    property ResultMsg;
    property ResultData: int64 read FResultData write FResultData;
  end;


implementation

constructor TActionResultObject.Create(QFreeResultData: boolean; QFreeListItem: boolean);
begin
  inherited Create;
  self.FFreeResultData := QFreeResultData;
  self.FFreeListItem := QFreeListItem;
  self.FResultSuccess := False;
  self.ResultCode := HTTP_ResultCode_Fail;
  self.FIsFile := False;
end;

destructor TActionResultObject.Destroy;
var
  lTValue: TValue;
begin
  if self.ResultData = nil then
    exit;
  if FFreeResultData then
  begin
    // 判断是不是对象
    // 要自动释放类的，需要释放
    TValue.Make(@self.ResultData, system.TypeInfo(self.ResultData), lTValue);
    OneSerialization.FreeTValue(lTValue, self.FFreeListItem);
  end;
  inherited Destroy;
end;

procedure TActionResult<T>.SetDataT(QValue: T);
var
  lValuT: TValue;
begin
  self.FResultDataT := QValue;
  TValue.Make(QValue, typeInfo(T), lValuT);
  if lValuT.IsObject then
    self.FResultData := TObject(QValue)
  else
    raise Exception.Create('此结果只支持类');
end;
constructor TActionResult<T>.Create(QFreeResultData: boolean; QFreeListItem: boolean);
begin
  inherited create(QFreeResultData,QFreeListItem);
end;

procedure TActionResultBase.SetResultTrue();
begin
  self.ResultSuccess := True;
  self.ResultCode := HTTP_ResultCode_True;
end;

procedure TActionResultBase.SetResultTrueFile();
begin
  self.FIsFile := True;
  self.ResultSuccess := True;
  self.ResultCode := HTTP_ResultCode_True;
end;

function TActionResultBase.IsResultFile(): boolean;
begin
  Result := self.FIsFile;
end;
procedure TActionResultBase.SetTokenFail();
begin
  self.FResultCode := HTTP_ResultCode_TokenFail;
  self.FResultMsg := 'Token验证失败,请重新登陆';
end;

end.
