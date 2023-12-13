unit OneAttribute;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  // 类表名注解
  TOneTableAttribute = class
  strict protected
    FTableName: string;
  end;
  //TCustomAttribute  高版本才有等以后升级在升级
  TOneDBAttribute = class(TObject)
  strict protected
    /// <summary>Internal use only</summary>
    FFieldName: string;
    FFormat: string;
  public
    constructor Create(QFieldName: string; QFormat: string = '');
    /// <summary>Alignment in bytes</summary>
    property FieldName: string read FFieldName;
    property Format: string read FFormat;
  end;

  TOneJsonAttribute = class(TObject)
  strict protected
    /// <summary>Internal use only</summary>
    FJsonName: string;
    FFormat: string;
  public
    constructor Create(QJsonName: string; QFormat: string);
    /// <summary>Alignment in bytes</summary>
    property JsonName: string read FJsonName;
    property Format: string read FFormat;
  end;


  TOneWebApiAttribute = class(TCustomAttribute)
  strict protected
    /// <summary>Internal use only</summary>
    FUrlPath: string;
    FMethod: string;
  public
    property UrlPath: string read FUrlPath;
    property Method: string read FMethod;
  end;

implementation

constructor TOneDBAttribute.Create(QFieldName: string; QFormat: string = '');
begin
  inherited Create;
  self.FFieldName := QFieldName;
  self.FFormat := QFormat;
end;

constructor TOneJsonAttribute.Create(QJsonName: string; QFormat: string);
begin
  inherited Create;
  self.FJsonName := QJsonName;
  self.FFormat := QFormat;
end;

end.
