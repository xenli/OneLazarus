unit Client_UserInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TClientUserInfo = class(TObject)
  private
    { 服务器名称 }
    FHostCaption: string;
    { 服务器IP }
    FHostName: string;
    { 服务器端口 }
    FHostPort: integer;
    {租户代码}
    FPlatUserCode: string;
    { 账套代码 }
    FHostZTCode: string;
    { 服务器秘钥 }
    FHostKey: string;
    { 登陆代码 }
    FMyLoginCode: string;
    FMYLoginPass: string;
    FbAdmin: boolean;
    FbSuperAdmin: boolean;
    { 登陆成功服务端返回的用户信息 }
    FTokenID: string;
    FUserID: string;
    FUserCode: string;
    FUserName: string;
    // **********一般常用的,预留的可以用的 *******//
    FSassID: string; // SassID和MerchantID类似
    FMerchantID: string; // 所属商家,也可以理廨一个数据库N个共用,此用户是哪个商家的
    FMerchantCode: string;
    FMerchantName: string;

    FBranchID: string;
    FBranchCode: string; // 所属分公司
    FBranchName: string;
    FShopID: string;
    FShopCode: string; // 所属门店
    FShopName: string;
    FDepartID: string; // 所属部门
    FDepartCode: string;
    FDepartName: string;
    // ********************************************//
  public
    constructor Create; overload;
    destructor Destroy; override;
    function GetOtherInfo(QNode: string): string;
  published
    property HostCaption: string read FHostCaption write FHostCaption;
    property HostName: string read FHostName write FHostName;
    property HostPort: integer read FHostPort write FHostPort;
    property PlatUserCode: string read FPlatUserCode write FPlatUserCode;
    property HostZTCode: string read FHostZTCode write FHostZTCode;
    property HostKey: string read FHostKey write FHostKey;
    property MyLoginCode: string read FMyLoginCode write FMyLoginCode;
    property MYLoginPass: string read FMYLoginPass write FMYLoginPass;
    property UserID: string read FUserID write FUserID;
    property TokenID: string read FTokenID write FTokenID;
    property UserCode: string read FUserCode write FUserCode;
    property UserName: string read FUserName write FUserName;

    property SassID: string read FSassID write FSassID;
    property MerchantID: string read FMerchantID write FMerchantID;
    property MerchantCode: string read FMerchantCode write FMerchantCode;
    property MerchantName: string read FMerchantName write FMerchantName;
    property BranchID: string read FBranchID write FBranchID;
    property BranchCode: string read FBranchCode write FBranchCode;
    property BranchName: string read FBranchName write FBranchName;
    property ShopID: string read FShopID write FShopID;
    property ShopCode: string read FShopCode write FShopCode;
    property ShopName: string read FShopName write FShopName;
    property DepartID: string read FDepartID write FDepartID;
    property DepartCode: string read FDepartCode write FDepartCode;
    property DepartName: string read FDepartName write FDepartName;


    property bAdmin: boolean read FbAdmin write FbAdmin;
    property bSuperAdmin: boolean read FbSuperAdmin write FbSuperAdmin;
  end;

implementation

constructor TClientUserInfo.Create;
begin
  inherited Create;
  HostCaption := '';
  HostName := '';
  HostPort := -1;
  HostZTCode := '';
  bSuperAdmin := False;
  bAdmin := False;
end;

destructor TClientUserInfo.Destroy;
begin
  inherited Destroy;
end;

function TClientUserInfo.GetOtherInfo(QNode: string): string;
var
  TempList: TStringList;
  i: integer;
begin
  TempList := TStringList.Create;
  try
    Result := TempList.Values[QNode];
  finally
    TempList.Clear;
    TempList.Free;
  end;
end;

end.
