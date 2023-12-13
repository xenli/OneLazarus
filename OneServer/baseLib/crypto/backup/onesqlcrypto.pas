unit OneSQLCrypto;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, Classes, base64;

// 交换打乱加密
function SwapCrypto(QStr: string): string;
// 交换打乱解密
function SwapDecodeCrypto(QStr: string): string;
// 判断是不是base64字符串
function IsBase64Str(QStr: string): boolean;

implementation

function IsBase64Str(QStr: string): boolean;
  // 每隔76个字符，就强制回车换行
const
  Base64Chars: set of ansichar =
    ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '+', '/', '=', #10, #13];
var
  i: integer;
begin
  Result := True;
  //for i := Low('') to High(QStr) do
  //begin
  //  if not (QStr[i] in Base64Chars) then
  //  begin
  //    Result := False;
  //    Break;
  //  end;
  //end;
end;

function SwapCrypto(QStr: string): string;
var
  iLow,iHight,iMid: integer;
  i: integer;
  tempStr: char;
begin
  if QStr = '' then
  begin
    Result := '';
    exit;
  end;
  QStr := EncodeStringBase64(QStr);
  iLow := Low(QStr);
  iHight := high(QStr);
  iMid := trunc(iHight/2);
  for i := iLow to iMid do
  begin
    tempStr := QStr[i];
    QStr[i] := QStr[iMid + i];
    QStr[iMid + i] :=  tempStr;
  end;
  Result := QStr;
end;

function SwapDecodeCrypto(QStr: string): string;
var
  iLow,iHight,iMid: integer;
  i: integer;
  tempStr: char;
begin
  if QStr = '' then
  begin
    Result := '';
    exit;
  end;
  iLow := Low(QStr);
  iHight := high(QStr);
  iMid := trunc(iHight/2);
  for i := iLow to iMid do
  begin
    tempStr := QStr[i];
    QStr[i] := QStr[iMid + i];
    QStr[iMid + i] := tempStr;
  end;
  Result := DecodeStringBase64(QStr);
end;

end.
