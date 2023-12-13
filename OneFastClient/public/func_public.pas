unit func_public;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

function GetGUIDStr(): string;
function YesNoMsg(aMsg: string; aTitle: string = ''): boolean;
{ 显示提示信息 }
procedure OkMsg(aMsg: string; aTitle: string = '');

implementation

uses frm_YesNoMsg;

function GetGUIDStr(): string;
var
  lGuid: TGUID;
begin
  Result := '';
  // 生成一个新的GUID
  CreateGUID(lGuid);
  // 将GUID转换为字符串
  Result := LowerCase(Copy(AnsiReplaceStr(GUIDToString(lGuid), '-', ''), 2, 32));
end;

function YesNoMsg(aMsg: string; aTitle: string = ''): boolean;
begin
  Result := frm_YesNoMsg.ShowYesNoMsg(aMsg, aTitle);
end;

procedure OkMsg(aMsg: string; aTitle: string = '');
begin
  frm_YesNoMsg.ShowMsg(aMsg, aTitle);
end;

end.
