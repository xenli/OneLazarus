unit OneGUID;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function GetGUID32(): string;

implementation

function GetGUID32(): string;
var
  ii: TGUID;
begin
  CreateGUID(ii);
  Result := Copy(GUIDToString(ii).replace('-', ''), 2, 32);
end;

end.
