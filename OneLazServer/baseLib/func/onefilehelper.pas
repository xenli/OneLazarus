unit OneFileHelper;

{$mode DELPHI}{$H+}

interface

uses SysUtils, StrUtils;
// 返回Exe运行目录
function GetExeRunPath(): string;
// 返回Exe运行名称
function GetExeName(): string;
// 格式化路径分割符,比如\写成/
function FormatPath(QPath: string): string;
// 拼接A,B
function CombinePath(QPathA: string; QPathB: string): string;
function CombineExeRunPath(QPath: string): string;
function CombineExeRunPathB(QPathA: string; QPathB: string): string;
function CombinePathC(QPathA: string; QPathB: string; QPathC: string): string;
function CombinePathD(QPathA: string; QPathB: string; QPathC: string; QPathD: string): string;

function HasExtension(QPath: string): boolean;
function DriveExists(QPath: string): boolean;

implementation

function GetExeRunPath(): string;
var
  lFullExeName: string;
begin
  lFullExeName := ParamStr(0);
  Result := ExtractFilePath(lFullExeName);
end;

function GetExeName(): string;
var
  lFullExeName: string;
begin
  lFullExeName := ParamStr(0);
  Result := ExtractFileName(lFullExeName);
end;

function FormatPath(QPath: string): string;
var
  lSeparatorChar: string;
  lReplaceChar: string;
begin
  lSeparatorChar := system.DirectorySeparator;
  lReplaceChar := '/';
  if lSeparatorChar = '/' then
  begin
    lReplaceChar := '\';
  end;
  QPath := QPath.Replace(lReplaceChar, lSeparatorChar);
  Result := QPath;
end;

function CombinePath(QPathA: string; QPathB: string): string;
var
  lSeparatorChar: string;
  lReplaceChar: string;
begin
  lSeparatorChar := system.DirectorySeparator;
  lReplaceChar := '/';
  if lSeparatorChar = '/' then
  begin
    lReplaceChar := '\';
  end;
  QPathA := QPathA.Replace(lReplaceChar, lSeparatorChar);
  QPathB := QPathB.Replace(lReplaceChar, lSeparatorChar);
  if QPathB.Length > 0 then
  begin
    if QPathA.Length > 0 then
    begin
      // 判断第一个是不是 lSeparatorChar,是的话去除
      if leftStr(QPathB, 1) = lSeparatorChar then
      begin
        QPathB := QPathB.Substring(1, QPathB.Length - 1);
      end;
    end;
  end;
  Result := ConcatPaths([QPathA, QPathB]);
end;

function CombineExeRunPath(QPath: string): string;
var
  lExeRunPath: string;
begin
  lExeRunPath := GetExeRunPath();
  Result := CombinePath(lExeRunPath, QPath);
end;

function CombineExeRunPathB(QPathA: string; QPathB: string): string;
var
  lExeRunPath: string;
begin
  lExeRunPath := GetExeRunPath();
  Result := CombinePathC(lExeRunPath, QPathA, QPathB);
end;

function CombinePathC(QPathA: string; QPathB: string; QPathC: string): string;
var
  lPathA, lPathB: string;
begin
  lPathA := CombinePath(QPathA, QPathB);
  lPathB := QPathC;
  Result := CombinePath(lPathA, lPathB);
end;

function CombinePathD(QPathA: string; QPathB: string; QPathC: string; QPathD: string): string;
var
  lPathA, lPathB: string;
begin
  lPathA := CombinePath(QPathA, QPathB);
  lPathB := QPathC;
  lPathA := CombinePath(lPathA, lPathB);
  lPathB := QPathD;
  Result := CombinePath(lPathA, lPathB);
end;

function HasExtension(QPath: string): boolean;
begin
  Result := (ExtractFileExt(QPath) <> '');
end;

function DriveExists(QPath: string): boolean;
begin
  Result := (ExtractFileDrive(QPath) <> '');
end;

end.
