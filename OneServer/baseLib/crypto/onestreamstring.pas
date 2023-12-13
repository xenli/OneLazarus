unit OneStreamString;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, base64;

// 流转base64字符串
function StreamToBase64Str(aStream: TStream): string;
// 流写入base64字符串
function StreamWriteBase64Str(aStream: TStream; QRawByteString: string): boolean;
// basd64字符串转成流
function Base64ToStream(QRawByteString: string): TStream;
// 字符串转base64
function StringToBase64(QRawByteString: string): string;
// 字节转base64
function BytesToBase64(QBytes: TBytes): string;
// 流转字节
function StreamToBytes(aStream: TStream): TBytes;
// 流写入字节
function StreamWriteBytes(aStream: TStream; QDataBytes: TBytes): boolean;
// 流转字符串
function StreamToString(var QResult: rawbytestring; aStream: TStream): boolean;
  overload;
// 流转字行串
function StreamToString(aStream: TStream): string; overload;
// 流写入字符串
function StreamToWriteString(QData: rawbytestring; aStream: TStream): boolean;
function StreamGetFileType(aStream: TStream): string;
function iLow: integer;

implementation

function iLow: integer;
begin
  Result := Low('');
end;

function StreamToBase64Str(aStream: TStream): string;
var
  OutputStream: TStringStream;
  Encoder: TBase64EncodingStream;
begin
  Result := '';
  OutputStream := TStringStream.Create('');
  Encoder := TBase64EncodingStream.Create(OutputStream);
  try
    aStream.Position := 0;
    Encoder.CopyFrom(aStream, aStream.Size);
    Encoder.Flush;
    OutputStream.Position := 0;
    Result := OutputStream.DataString;
  finally
    Encoder.Free;
    OutputStream.Free;
  end;
end;

function StreamWriteBase64Str(aStream: TStream; QRawByteString: string): boolean;
var
  InStream: TStringStream;
  Decoder: TBase64DecodingStream;
begin
  Result := False;
  InStream := TStringStream.Create(QRawByteString);
  try
    Decoder := TBase64DecodingStream.Create(InStream, bdmStrict);
  try
    aStream.CopyFrom(Decoder, Decoder.Size);
    aStream.Position := 0;
    Result := True;
  finally
    Decoder.Free;
  end;
  finally
    InStream.Free;
  end;
end;

function Base64ToStream(QRawByteString: string): TStream;
begin
  Result := TMemoryStream.Create;
  StreamWriteBase64Str(Result, QRawByteString);
  Result.Position := 0;
end;

function StringToBase64(QRawByteString: string): string;
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    StreamToWriteString(QRawByteString, lStream);
    Result := StreamToBase64Str(lStream);
  finally
    lStream.Free;
  end;
end;

function BytesToBase64(QBytes: TBytes): string;
var
  lStream: TBytesStream;
begin
  lStream := TBytesStream.Create(QBytes);
  try
    Result := StreamToBase64Str(lStream);
  finally
    lStream.Clear;
    lStream.Free;
  end;
end;

function StreamToBytes(aStream: TStream): TBytes;
begin
  aStream.Position := 0;
  SetLength(Result, aStream.Size);
  aStream.Read(Result[0], aStream.Size);
end;

function StreamWriteBytes(aStream: TStream; QDataBytes: TBytes): boolean;
var
  iSize: integer;
begin
  Result := False;
  iSize := Length(QDataBytes);
  aStream.Position := 0;
  aStream.Write(QDataBytes[0], iSize);
  aStream.Position := 0;
  Result := True;
end;

function StreamToString(var QResult: rawbytestring; aStream: TStream): boolean;
begin
  Result := False;
  aStream.Position := 0;
  SetLength(QResult, aStream.Size);
  aStream.Read(QResult[iLow], aStream.Size);
  Result := True;
end;

function StreamToString(aStream: TStream): string;
begin
  Result := '';
  aStream.Position := 0;
  SetLength(Result, aStream.Size);
  aStream.Read(Result[iLow], aStream.Size);
end;

function StreamToWriteString(QData: rawbytestring; aStream: TStream): boolean;
var
  iSize: integer;
begin
  Result := False;
  iSize := Length(QData);
  if iSize > 0 then
  begin
    aStream.Position := 0;
    aStream.Write(QData[iLow], iSize);
    aStream.Position := 0;
    Result := True;
  end;
end;

function StreamGetFileType(aStream: TStream): string;
var
  Buffer: word;
begin
  Result := '';
  aStream.Position := 0;
  if aStream.Size = 0 then
    exit;
  aStream.ReadBuffer(Buffer, 2);
  case Buffer of
    $4D42:
      Result := 'bmp';
    $D8FF:
      Result := 'jpeg';
    $4947:
      Result := 'gifp';
    $050A:
      Result := 'pcx';
    $5089:
      Result := 'png';
    $4238:
      Result := 'psd';
    $A659:
      Result := 'ras';
    $DA01:
      Result := 'sgi';
    $4949:
      Result := 'tiff';
  end;
end;

end.
