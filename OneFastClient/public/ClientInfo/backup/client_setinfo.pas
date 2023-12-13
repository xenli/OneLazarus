unit client_SetInfo;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, OneSerialization;

type
  // 连接服务端配置
  TServerSet = class
  private
    FHostCaption: string;
    FHostName: string;
    FHostPort: integer;
    FHostZTCode: string;
    FHostKey: string;
    FPlatUserCode: string;
  published
    property HostCaption: string read FHostCaption write FHostCaption;
    property HostName: string read FHostName write FHostName;
    property HostPort: integer read FHostPort write FHostPort;
    property HostZTCode: string read FHostZTCode write FHostZTCode;
    property HostKey: string read FHostKey write FHostKey;
    property PlatUserCode: string read FPlatUserCode write FPlatUserCode;
  end;

  TClientSetInfo = class(TObject)
  private
    // 登陆账号
    FLoginCode: string;
    // 密码
    FLoginPass: string;
    // 记住密码
    FIsPass: boolean;
    // 当前配置
    FIndexServer: TServerSet;
    // 服务端配置列表
    FServerList: TList<TServerSet>;

    FSkinName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearServerList;
  published
    property LoginCode: string read FLoginCode write FLoginCode;
    property LoginPass: string read FLoginPass write FLoginPass;
    property IsPass: boolean read FIsPass write FIsPass;
    property IndexServer: TServerSet read FIndexServer write FIndexServer;
    property ServerList: TList<TServerSet> read FServerList write FServerList;
    property SkinName: string read FSkinName write FSkinName;
  end;

implementation

constructor TClientSetInfo.Create;
begin
  FIndexServer := TServerSet.Create;
  FServerList := specialize TList<TServerSet>.Create;
end;

destructor TClientSetInfo.Destroy;
var
  i: integer;
begin
  FIndexServer.Free;
  for i := 0 to FServerList.Count - 1 do
  begin
    FServerList[i].Free;
  end;
  FServerList.Clear;
  FServerList.Free;
  inherited Destroy;
end;

procedure TClientSetInfo.ClearServerList;
var
  i: integer;
begin
  for i := 0 to self.FServerList.Count - 1 do
  begin
    self.FServerList[i].Free;
  end;
  self.FServerList.Clear;
end;

function CreateTServerSet(): TObject;
begin
  Result := TServerSet.Create;
end;

initialization
  //泛型注册
  OneSerialization.AddListClass(TList<TServerSet>, TServerSet, CreateTServerSet);
end.
