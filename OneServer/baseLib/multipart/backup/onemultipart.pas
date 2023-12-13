unit OneMultipart;

{$mode DELPHI}{$H+}


interface

uses SysUtils, Classes, HTTPDefs;

type
  TOneMultipartDecode = class(TRequest)
  end;

  TOneRequestFile = class(TWebRequestFile)

  end;


function IsMultipartForm(QContentType: string): boolean;
function MultiPartFormDataDecode(QContentType: string;
  QContent: rawbytestring): TOneMultipartDecode;

implementation

const
  sMultiPartFormData = 'multipart/form-data';

function IsMultipartForm(QContentType: string): boolean;
begin
  Result := StrLIComp(PChar(QContentType), PChar(sMultiPartFormData),
    Length(sMultiPartFormData)) = 0;
end;


function MultiPartFormDataDecode(QContentType: string;
  QContent: rawbytestring): TOneMultipartDecode;
var
  lMultipartDecode: TOneMultipartDecode;
begin
  Result := nil;
  if not IsMultipartForm(QContentType) then
    exit;
  lMultipartDecode := TOneMultipartDecode.Create;
  lMultipartDecode.Content := QContent;
  lMultipartDecode.ContentType := QContentType;
  lMultipartDecode.ContentLength := Length(QContent);
  lMultipartDecode.InitPostVars;
  //lMultipartDecode.Files  FormData文件在这
  //lMultipartDecode.Fields FormData参数放在这
  //lMultipartDecode.FieldCount FormData的Fields参数个数
  Result := lMultipartDecode;
end;

end.
