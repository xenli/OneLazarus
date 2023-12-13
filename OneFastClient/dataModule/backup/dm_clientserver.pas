unit dm_ClientServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, MSSQLConn, OneClientConnect;

type

  { TdmClientServer }

  TdmClientServer = class(TDataModule)
    OneConnection: TOneConnection;
    SQLConnector1: TSQLConnector;
    SQLQuery1: TSQLQuery;
    SQLQuery1FAdminCode: TStringField;
    SQLQuery1FAdminID: TStringField;
    SQLQuery1FAdminName: TStringField;
    SQLQuery1FAdminPass: TStringField;
    SQLQuery1FAdminTel: TStringField;
    SQLQuery1FAdminType: TStringField;
    SQLQuery1FCreateTime: TDateTimeField;
    SQLQuery1FGroupCaption: TStringField;
    SQLQuery1FGroupID: TStringField;
    SQLQuery1FGroupTreeCode: TStringField;
    SQLQuery1FIsEnable: TBooleanField;
    SQLQuery1FIsLimit: TBooleanField;
    SQLQuery1FIsMultiLogin: TBooleanField;
    SQLQuery1FLimtEndTime: TDateTimeField;
    SQLQuery1FLimtStartTime: TDateTimeField;
    SQLQuery1FRemark: TStringField;
    SQLQuery1FRoleCaption: TStringField;
    SQLQuery1FRoleID: TStringField;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure MSSQLConnection1AfterConnect(Sender: TObject);
  private

  public
    function ConnectServer(var QErrMsg: string): boolean;
  end;

var
  dmClientServer: TdmClientServer;

implementation

{$R *.lfm}
uses AppManage, Client_UserInfo;

{ TdmClientServer }

procedure TdmClientServer.DataModuleCreate(Sender: TObject);
begin
  // 全局Connection绑定,不需要一个一个界面去赋值Connection
  OneClientConnect.Unit_Connection := OneConnection;
end;

procedure TdmClientServer.MSSQLConnection1AfterConnect(Sender: TObject);
begin

end;

// 连接服务器
function TdmClientServer.ConnectServer(var QErrMsg: string): boolean;
var
  lClientUserInfo: TClientUserInfo;
begin
  Result := False;
  QErrMsg := '';
  lClientUserInfo := AppManage.UnitAppManage().MyClientInfo;
  OneConnection.HTTPHost := lClientUserInfo.HostName;
  OneConnection.HTTPPort := lClientUserInfo.HostPort;
  OneConnection.ConnectSecretkey := lClientUserInfo.HostKey;
  OneConnection.ZTCode := lClientUserInfo.HostZTCode;
  // 这边不产生Token,由登陆去完成,这边只是探测服务器可用不可用
  Result := OneConnection.DoConnectPing();
  if Result then
  begin
  end
  else
  begin
    QErrMsg := OneConnection.ErrMsg;
  end;
end;

end.
