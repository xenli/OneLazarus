unit OneCrypto;

{$mode ObjFPC}{$H+}

interface

uses md5, Types, StrUtils, SysUtils, Classes;

function MD5Endcode(QStr: string): string; overload;
function MD5Endcode(QStr: string; QbUpperCase: boolean): string; overload;
// 如果不是utf-8需要用codepage转码后在来
function MD5Endcode(QStr: string; QCodePage: integer): string; overload;

{AES}

function GetFileHashMD5(FileName: string): string;
function GetStrHashSHA1(Str: string): string;
function GetStrHashSHA224(Str: string): string;
function GetStrHashSHA256(Str: string): string;
function GetStrHashSHA384(Str: string): string;
function GetStrHashSHA512(Str: string): string;
function GetStrHashSHA512_224(Str: string): string;
function GetStrHashSHA512_256(Str: string): string;
function GetStrHashBobJenkins(Str: string): string;

implementation

function MD5Endcode(QStr: string): string;
begin
  Result := MD5Print(MD5String(QStr));
end;

function MD5Endcode(QStr: string; QbUpperCase: boolean): string;
begin
  Result := MD5Print(MD5String(QStr));
  if QbUpperCase then
    Result := Result.ToUpper
  else
    Result := Result.ToLower;
end;

function MD5Endcode(QStr: string; QCodePage: integer): string;
var
  ss: TStringStream;
  md5: string;
begin
  // gbk 936
  ss := TStringStream.Create(QStr, QCodePage);
  try
    // md5 := THashMD5.GetHashString(ss);
    Result := md5;
  finally
    ss.Free;
  end;
end;

function GetStrHashSHA1(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashSHA224(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashSHA256(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashSHA384(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashSHA512(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashSHA512_224(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashSHA512_256(Str: string): string;
begin
  Result := Str;
end;

function GetStrHashBobJenkins(Str: string): string;
begin
  Result := Str;
end;

function GetFileHashMD5(FileName: string): string;
begin
  Result := MD5Print(MD5File(FileName));
end;

function GetFileHashSHA1(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashSHA224(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashSHA256(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashSHA384(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashSHA512(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashSHA512_224(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashSHA512_256(FileName: string): string;
begin
  Result := '';
end;

function GetFileHashBobJenkins(FileName: string): string;
begin
  Result := '';
end;

end.
