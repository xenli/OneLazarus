unit OneClientDataInfo;

{$mode DELPHI}{$H+}

interface

uses Classes, Generics.Collections, SysUtils, Variants, OneSerialization;

const
  const_TranID = 'TranID';
  // 返回数据模式
  const_DataReturnMode_Stream = 'dataStream';
  const_DataReturnMode_File = 'dataFile';
  const_DataReturnMode_JSON = 'dataJson';
  const_DataReturnMode_Empty = 'dataEmpty';
  // 参数相关设置
  const_OneParamIsNull_Value = 'ONE_ISNULL_ONE';
  // 保存数据模式
  const_DataSaveMode_SaveData = 'saveData';
  const_DataSaveMode_SaveDML = 'saveDML';

type
  TDataOpenMode = (openData, openStored);
  TDataReturnMode = (dataStream, dataFile, dataJson, dataEmpty);
  TDataSaveMode = (saveData, saveDML);

type
  TOneParam = class;
  TOneDataOpen = class;
  TOneDataResultItem = class;
  TOneDataResult = class;
  TOneTran = class;

  TOneTran = class
  private
    FZTCode: string;
    FTranID: string;
    FMaxSpan: integer;
    FMsg: string;
  published
    property ZTCode: string read FZTCode write FZTCode;
    property TranID: string read FTranID write FTranID;
    property MaxSpan: integer read FMaxSpan write FMaxSpan;
    property Msg: string read FMsg write FMsg;
  end;

  // 参数
  TOneParam = class
  private
    FParamName: string; // 参数名称
    FParamValue: string; // 参数值
    FParamType: string; // 参数是否输出参数
    FParamDataType: string; // 参数数据类型
    FParamSize: integer;
  published
    property ParamName: string read FParamName write FParamName;
    property ParamValue: string read FParamValue write FParamValue;
    property ParamType: string read FParamType write FParamType;
    property ParamDataType: string read FParamDataType write FParamDataType;
    property ParamSize: integer read FParamSize write FParamSize;
  end;

  // 打开数据信息
  TOneDataOpen = class
  private
    FTranID: string;
    FZTCode: string; // 取哪个账套的数据
    FSQLCode: string;
    FOpenSQL: string; // 打开的SQL语句
    FPackageName: string; // 存储过程所在在包
    FSPName: string; // 执行存储过程打开数据
    FSPIsOutData: boolean; // 存储过程返回数据
    FParams: TList<TOneParam>; // 参数及存储过程返回的参数(原本输出)
    FPageSize: integer;
    FPageIndex: integer;
    FPageRefresh: boolean; // 有分页情况,每次多返回总数据
    FDataReturnMode: string;

  public
    constructor Create; overload;
    destructor Destroy; override;
  published
    property TranID: string read FTranID write FTranID;
    property ZTCode: string read FZTCode write FZTCode;
    property SQLCode: string read FSQLCode write FSQLCode;
    property OpenSQL: string read FOpenSQL write FOpenSQL;
    property PackageName: string read FPackageName write FPackageName;
    property SPName: string read FSPName write FSPName;
    property SPIsOutData: boolean read FSPIsOutData write FSPIsOutData;
    property Params: TList<TOneParam> read FParams write FParams;
    property PageSize: integer read FPageSize write FPageSize;
    property PageIndex: integer read FPageIndex write FPageIndex;
    property PageRefresh: boolean read FPageRefresh write FPageRefresh;
    property DataReturnMode: string read FDataReturnMode write FDataReturnMode;
  end;

  // 保存和执行DML
  TOneDataSaveDML = class
  private
    FTranID: string;
    FZTCode: string; // 取哪个账套的数据
    FDataSaveMode: string;
    FSQL: string; // 执行DML语句
    FTableName: string; // 保存表的表名
    FPrimarykey: string; // 保存表的主键
    FOtherKeys: string; // 其它键当主键更新
    FSaveData: string; // 保存的数据
    FUpdateMode: string; // 更新模式 主键更新,还是全字段更新
    FAffectedMaxCount: integer; // 最大影晌行数
    FAffectedMustCount: integer; // 有且要有几条受影响
    // 这三个比较少用到
    FSaveDataInsertSQL: string; // 自定义SQL插入
    FSaveDataUpdateSQL: string; // 自定义SQL更新
    FSaveDataDelSQL: string; // 自定义SQL删除
    FIsReturnData: boolean; // 保存完后是否返回数据集

    FIsAutoID: boolean;

    FParams: TList<TOneParam>;
    FNotUpdateFields: TList<string>;
    FFieldProviderFlags: TList<integer>;
  public
    constructor Create; overload;
    destructor Destroy; override;
  published
    property TranID: string read FTranID write FTranID;
    property ZTCode: string read FZTCode write FZTCode;
    property DataSaveMode: string read FDataSaveMode write FDataSaveMode;
    property SQL: string read FSQL write FSQL;
    property TableName: string read FTableName write FTableName;
    property Primarykey: string read FPrimarykey write FPrimarykey;
    property OtherKeys: string read FOtherKeys write FOtherKeys;
    property saveData: string read FSaveData write FSaveData;
    property UpdateMode: string read FUpdateMode write FUpdateMode;
    property AffectedMaxCount: integer read FAffectedMaxCount write FAffectedMaxCount;
    property AffectedMustCount: integer read FAffectedMustCount write FAffectedMustCount;
    property SaveDataInsertSQL: string read FSaveDataInsertSQL write FSaveDataInsertSQL;
    property SaveDataUpdateSQL: string read FSaveDataUpdateSQL write FSaveDataUpdateSQL;
    property SaveDataDelSQL: string read FSaveDataDelSQL write FSaveDataDelSQL;
    property IsReturnData: boolean read FIsReturnData write FIsReturnData;
    property IsAutoID: boolean read FIsAutoID write FIsAutoID;
    property Params: TList<TOneParam> read FParams write FParams;
    property NotUpdateFields: TList<string> read FNotUpdateFields write FNotUpdateFields;
    property FieldProviderFlags: TList<integer> read FFieldProviderFlags write FFieldProviderFlags;
  end;


  TOneDataResultItem = class
  private
    FResultPage: boolean; // 分页
    FResultDataCount: integer; // 多个数据集一个SQL语句
    FResultTotal: integer; // 第一个数据集总记录数,分页下取总条数
    FRecordCount: integer; // 第一个数据集记录数
    FResultDataMode: string; // 输出模式
    FResultContext: string;
    // 输出结果,一确转换成 字符串,流也是转换成base64字符串
    FResultParams: TList<TOneParam>; // 返回的参数
    FTempStream: TMemoryStream; // 临时存储流数据的地方
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure SetStream(QStream: TMemoryStream);
  published
    property ResultPage: boolean read FResultPage write FResultPage;
    property ResultDataCount: integer read FResultDataCount write FResultDataCount;
    property ResultTotal: integer read FResultTotal write FResultTotal;
    property RecordCount: integer read FRecordCount write FRecordCount;
    property ResultDataMode: string read FResultDataMode write FResultDataMode;
    property ResultContext: string read FResultContext write FResultContext;
    property ResultParams: TList<TOneParam> read FResultParams write FResultParams;
  end;

  // 返回结果
  TOneDataResult = class
  private
    FResultOK: boolean;
    FResultMsg: string;
    FResultData: string;
    FResultCount: integer;
    FResultItems: TList<TOneDataResultItem>;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function DoResultitems(): boolean;
  published
    property ResultOK: boolean read FResultOK write FResultOK;
    property ResultMsg: string read FResultMsg write FResultMsg;
    property ResultData: string read FResultData write FResultData;
    property ResultCount: integer read FResultCount write FResultCount;
    property ResultItems: TList<TOneDataResultItem> read FResultItems write FResultItems;
  end;

implementation

uses OneStreamString;

constructor TOneDataOpen.Create;
begin
  inherited Create;
  FParams := TList<TOneParam>.Create;
end;

destructor TOneDataOpen.Destroy;
var
  i: integer;
begin
  if FParams <> nil then
  begin
    for i := 0 to FParams.Count - 1 do
    begin
      FParams[i].Free;
    end;
    FParams.Clear;
    FParams.Free;
  end;
  inherited Destroy;
end;

constructor TOneDataSaveDML.Create;
begin
  inherited Create;
  FParams := TList<TOneParam>.Create;
  FNotUpdateFields := TList<string>.Create;
  FFieldProviderFlags := TList<integer>.Create;
  FAffectedMaxCount := -1;
  FAffectedMustCount := -1;
end;

destructor TOneDataSaveDML.Destroy;
var
  i: integer;
begin
  for i := 0 to FParams.Count - 1 do
  begin
    FParams[i].Free;
  end;
  FParams.Clear;
  FParams.Free;
  FNotUpdateFields.Clear;
  FNotUpdateFields.Free;
  FFieldProviderFlags.Clear;
  FFieldProviderFlags.Free;
  inherited Destroy;
end;

constructor TOneDataResultItem.Create;
begin
  inherited Create;
  FResultPage := False;
  FResultDataCount := 0; // 多个数据集一个SQL语句
  FResultTotal := 0; // 第一个数据集总记录数,分页下取总条数
  FRecordCount := 0; // 第一个数据集记录数
  FResultParams := TList<TOneParam>.Create; // 返回的参数
  FTempStream := nil; // 临时存储流数据的地方
end;

destructor TOneDataResultItem.Destroy;
var
  i: integer;
begin
  if FResultParams <> nil then
  begin
    for i := 0 to FResultParams.Count - 1 do
    begin
      FResultParams[i].Free;
    end;
    FResultParams.Clear;
    FResultParams.Free;
  end;
  if FTempStream <> nil then
  begin
    FTempStream.Clear;
    FTempStream.Free;
  end;
  inherited Destroy;
end;

procedure TOneDataResultItem.SetStream(QStream: TMemoryStream);
begin
  FTempStream := QStream;
end;

constructor TOneDataResult.Create;
begin
  inherited Create;
  FResultOK := False;
  FResultMsg := '';
  FResultCount := 0;
  FResultItems := TList<TOneDataResultItem>.Create;
end;

destructor TOneDataResult.Destroy;
var
  i: integer;
begin
  if FResultItems <> nil then
  begin
    for i := 0 to FResultItems.Count - 1 do
    begin
      FResultItems[i].Free;
    end;
    FResultItems.Clear;
    FResultItems.Free;
  end;
  inherited Destroy;
end;

function TOneDataResult.DoResultitems(): boolean;
var
  i: integer;
  lDataResultItem: TOneDataResultItem;
begin
  for i := 0 to self.ResultItems.Count - 1 do
  begin
    lDataResultItem := self.ResultItems[i];
    if lDataResultItem.ResultDataMode = const_DataReturnMode_Stream then
    begin
      if lDataResultItem.FTempStream <> nil then
      begin
        lDataResultItem.ResultContext :=
          OneStreamString.StreamToBase64Str(lDataResultItem.FTempStream);
        // 即时释放内存
        lDataResultItem.FTempStream.Clear;
        lDataResultItem.FTempStream.Free;
        lDataResultItem.FTempStream := nil;
      end;
    end;
  end;
end;

function CreateTOneDataOpen(): TObject;
begin
  Result := TOneDataOpen.Create;
end;

function CreateTOneDataSaveDML(): TObject;
begin
  Result := TOneDataSaveDML.Create;
end;

function CreateTOneDataResultItem(): TObject;
begin
  Result := TOneDataResultItem.Create;
end;

function CreateTOneParam(): TObject;
begin
  Result := TOneParam.Create;
end;

initialization
  //泛型注册
  OneSerialization.AddListClass(TList<TOneDataOpen>, TOneDataOpen, CreateTOneDataOpen);
  OneSerialization.AddListClass(TList<TOneDataSaveDML>, TOneDataSaveDML,
    CreateTOneDataSaveDML);
  OneSerialization.AddListClass(TList<TOneDataResultItem>, TOneDataResultItem,
    CreateTOneDataResultItem);
  OneSerialization.AddListClass(TList<TOneParam>, TOneParam, CreateTOneParam);
end.
