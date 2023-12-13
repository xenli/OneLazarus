unit OneDataJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, fpjson, jsonparser, OneStreamString;

function DataSetToJson(const QDataSet: TDataSet): TJsonData;
function JsonToDataSet(const QDataSet: TDataSet): boolean;

implementation

function DataSetToJson(const QDataSet: TDataSet): TJsonData;
var
  lField: TField;
  i: integer;
  lJsonObj: TJsonObject;
  tempStream: TMemoryStream;
  tempStr: string;
begin
  Result := TJsonArray.Create;
  if not QDataSet.Active then
  begin
    exit;
  end;
  if (QDataSet.RecordCount = 0) or (QDataSet.Fields.Count = 0) then
  begin
    exit;
  end;
  //遍历生成数据
  tempStream := TMemoryStream.Create;
  try
    QDataSet.First;
    while not QDataSet.EOF do
    begin
      lJsonObj := TJsonObject.Create;
      TJsonArray(Result).Add(lJsonObj);
      for i := 0 to QDataSet.Fields.Count - 1 do
      begin
        {for循环字段开始}
        lField := QDataSet.Fields[i];
        case lField.DataType of
          ftFixedChar, ftString:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsString);
          end;
          ftFixedWideChar, ftWideString:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsWideString);
          end;
          ftBoolean:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsBoolean);
          end;
          ftFloat:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsFloat);
          end;
          ftAutoInc, ftSmallInt, ftInteger:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsInteger);
          end;
          ftLargeInt:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsLargeInt);
          end;
          ftDate:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsDateTime);
          end;
          ftTime:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsDateTime);
          end;
          ftTimestamp, ftDateTime:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsDateTime);
          end;
          ftCurrency:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsCurrency);
          end;
          ftBCD, ftFmtBCD:
          begin
            lJsonObj.Add(lField.FieldName, lField.AsCurrency);
          end
          else
          begin
            if (lField.DataType in ftBlobTypes) then
            begin
              tempStream.Clear;
              TBlobField(lField).SaveToStream(tempStream);
              tempStr := OneStreamString.StreamToBase64Str(tempStream);
              lJsonObj.Add(lField.FieldName, tempStr);
            end
            else
              lJsonObj.Add(lField.FieldName, lField.AsString);
          end;
        end;
        {for循环字段结束}
      end;
      QDataSet.Next;
    end;
  finally
    tempStream.Clear;
    tempStream.Free;
  end;
end;

function JsonToDataSet(const QDataSet: TDataSet): boolean;
begin

end;

end.
