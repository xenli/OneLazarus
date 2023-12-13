unit OneClientResult;

 {$mode DELPHI}{$H+}
 {$modeswitch PROPERTIES}

// 终端的Data需要自已解析释放
interface

uses Classes, SysUtils, Generics.Collections;

type
  TClientConnect = class
  private
    FConnectSecretkey: string;
    FClientIP: string; // 客户端传上来的客户端IP
    FClientMac: string; // 客户端传上来的客户端Mac地址
    FTokenID: string; // 服务端返回的TokenID
    FPrivateKey: string; // 服务端返回的私钥
  published
    property ConnectSecretkey: string read FConnectSecretkey write FConnectSecretkey;
    property ClientIP: string read FClientIP write FClientIP;
    property ClientMac: string read FClientMac write FClientMac;
    property TokenID: string read FTokenID write FTokenID;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
  end;

  TActionResultBase = class
  private
    FResultSuccess: boolean;
    FResultCode: string;
    FResultMsg: string;
    FIsFile: boolean;
  public
    constructor Create();
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
  published
    property ResultSuccess;
    property ResultCode;
    property ResultMsg;
    property ResultData: TObject read FResultData write FResultData;
  end;

  TActionResult<T> = class(TActionResultObject)
  private
    FResultDataT: T;
    procedure SetDataT(QValue: T);
  public
    property ResultDataT: T read FResultDataT write SetDataT;
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

constructor TActionResultBase.Create();
begin
  inherited Create;
  self.FResultSuccess := False;
end;

procedure TActionResult<T>.SetDataT(QValue: T);
begin
  self.FResultDataT := QValue;
  self.FResultData := TObject(QValue);
end;

end.
