unit OneUUID;

// 版权声明引用了开源的，非作者所有
// https://github.com/yitter/IdGenerator/tree/master/Delphi
interface

uses
  Classes, StrUtils, SysUtils;

function GetUUID(): int64;
function GetUUIDStr(): string;

implementation



function GetUUID(): int64;
begin
  //Result := GetYitIdHelper().NextId();
end;

function GetUUIDStr(): string;
begin
  Result := GetUUID().ToString;
end;

initialization


finalization

end.
