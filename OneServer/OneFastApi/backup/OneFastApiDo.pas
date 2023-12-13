﻿unit OneFastApiDo;

{$mode DELPHI}{$H+}
interface

uses
  Generics.Collections, Variants, TypInfo,
  DB, Classes, SysUtils, StrUtils, fpjson, jsonparser,
  DateUtils,
  OneTokenManage, OneZTManage, OneFastApiManage;

type
  TApiAll = class;

  TApiFieldTemp = class
  private
    FFieldName: string;
    FFormat: string;
    FJsonName: string;
    FField: TField;
  end;

  TApiStepResult = class
  private
    FPFilterField: string;
    FFilterField: string;
    FPStepResult: TApiStepResult;
    FApiDataID: string;
    FDataSet: TDataSet;
    FResultJsonObj: TJsonObject;
    FResultDataJsonArr: TJsonArray;
    FResultDataJsonArrIsFree: boolean;
    FResultAffected: integer;
    FBuildParams: TParams;
    FBuildFilterSQL: string;
    FApiData: TFastApiData;
    FChilds: TList<TApiStepResult>;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property DataSet: TDataSet read FDataSet;
    property ApiData: TFastApiData read FApiData;
    property Childs: TList<TApiStepResult> read FChilds;
  end;

  TApiAll = class
  private
    UnionID: string;
    // 传进来的参数
    token: TOneTokenItem;
    postDataJson: TJsonObject;

    dataJson: TJSONData;
    pageJson: TJsonObject;

    ClientZT: TOneZTItem;
    ZTDict: TDictionary<string, TOneZTItem>;


    isDoOK: boolean;
    resultCode: string;

  public
    paramJson: TJsonObject;

    StepResultList: TList<TApiStepResult>;
    errMsg: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetZTItem(QZTCode: string): TOneZTItem;
    function IsErr(): boolean;
  end;

function DoFastApiResultApiAll(QToken: TOneTokenItem; QPostDataJson: TJsonObject; var QApiInfo: TFastApiInfo): TApiAll;
function DoFastApi(QToken: TOneTokenItem; QPostDataJson: TJsonObject): TJSONData;
function DoCheckApiAuthor(QToken: TOneTokenItem; QApiInfo: TFastApiInfo; QApiAll: TApiAll): boolean;

function DoFastApiStep(QApiDatas: TList<TFastApiData>; QApiAll: TApiAll; QPStepResult: TApiStepResult): boolean;
function DoFastResultJson(QApiInfo: TFastApiInfo; QApiAll: TApiAll): TJSONData;
// openData打开数据
function DoOpenData(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
function DoDataStore(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
function DoDml(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
function DoAppendDatas(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;

function BuildDataSQLAndParams(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
function BuildFilterSQLAndParams(QApiData: TFastApiData; QApiFilter: TFastApiFilter; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
function BuildFilterParamValue(QApiAll: TApiAll; QApiFilter: TFastApiFilter; QParam: TParam; QValue: string): boolean;

function BuildDataToJsonArray(QApiData: TFastApiData; Query: TDataSet; QApiAll: TApiAll): TJsonArray;
function BlobFieldToBase64(ABlobField: TBlobField): string;

function BuildAppendDataSet(Query: TDataSet; QApiData: TFastApiData; QApiAll: TApiAll): boolean;
function SetFieldValue(QDataField: TField; QApiField: TFastApiField; QDataJson: TJsonObject; QApiAll: TApiAll): boolean;

implementation

uses OneGlobal, OneUUID, OneGUID, OneStreamString;

constructor TApiStepResult.Create;
begin
  FPStepResult := nil;
  FDataSet := nil;
  FApiData := nil;
  FResultJsonObj := nil;
  FResultDataJsonArr := nil;
  FResultDataJsonArrIsFree := False;
  FBuildParams := TParams.Create(nil, TParam);
  FChilds := TList<TApiStepResult>.Create;
end;

destructor TApiStepResult.Destroy;
var
  iChild: integer;
  iArr: integer;
begin
  FBuildParams.Clear;
  FBuildParams.Free;
  if FDataSet <> nil then
    FDataSet.Free;
  for iChild := 0 to FChilds.Count - 1 do
  begin
    FChilds[iChild].Free;
  end;
  FChilds.Clear;
  FChilds.Free;
  if FResultDataJsonArrIsFree and (FResultDataJsonArr <> nil) then
  begin
    for iArr := FResultDataJsonArr.Count - 1 downto 0 do
    begin
      FResultDataJsonArr.Delete(iArr);
    end;
    FResultDataJsonArr.Free;
  end;
  inherited Destroy;
end;

constructor TApiAll.Create;
var
  ii: TGUID;
begin
  CreateGUID(ii);
  // 本次请求全局维一ID
  self.UnionID := Copy(AnsiReplaceStr(GUIDToString(ii), '-', ''), 2, 32);
  self.token := nil;
  self.postDataJson := nil;
  self.paramJson := nil;
  self.pageJson := nil;
  self.dataJson := nil;
  self.ZTDict := nil;
  self.ClientZT := nil;
  self.StepResultList := TList<TApiStepResult>.Create;

  self.resultCode := 'FastApiFail';
  self.isDoOK := False;
  self.errMsg := '';
end;

destructor TApiAll.Destroy;
var
  i: integer;
begin
  for i := 0 to self.StepResultList.Count - 1 do
  begin
    self.StepResultList[i].Free;
  end;
  self.StepResultList.Clear;
  self.StepResultList.Free;

  token := nil;
  postDataJson := nil;
  dataJson := nil;
  paramJson := nil;
  pageJson := nil;
  self.ZTDict := nil;
  self.ClientZT := nil;
  inherited;
end;

function TApiAll.GetZTItem(QZTCode: string): TOneZTItem;
var
  lZTItem: TOneZTItem;
begin
  Result := nil;
  if self.ClientZT <> nil then
  begin
    Result := self.ClientZT;
    exit;
  end;
  lZTItem := nil;
  self.ZTDict.TryGetValue(QZTCode, lZTItem);
  Result := lZTItem;
end;

function TApiAll.IsErr(): boolean;
begin
  Result := not self.isDoOK;
end;

function DoFastApiResultApiAll(QToken: TOneTokenItem; QPostDataJson: TJsonObject; var QApiInfo: TFastApiInfo): TApiAll;
var
  lFastApiManage: TOneFastApiManage;
  lApiInfo: TFastApiInfo;
  lApiData: TFastApiData;
  iData: integer;
  isHaveTran: boolean;

  lZTItemDict: TDictionary<string, TOneZTItem>;
  lZTCode: string;
  lZTManage: TOneZTManage;
  lZTItem: TOneZTItem;
  lZTItems: TList<TOneZTItem>;
  iZT: integer;

  lClientZTCode: string;
  lApiCode: string;
  lApiDataJson, lApiParamJson, lApiPageJson: TJsonData;
  lApiAll: TApiAll;
  iComitStep: integer;
  isStepOK: boolean;
begin
  Result := nil;
  isStepOK := False;
  lApiDataJson := nil;
  lApiParamJson := nil;
  lApiPageJson := nil;
  isHaveTran := False;
  lApiInfo := nil;
  lApiAll := TApiAll.Create;
  try
    lApiCode := QPostDataJson.Get('apiCode', '');
    if lApiCode = '' then
    begin
      lApiAll.errMsg := '提交的Json数据,接口代码不存在键值【apiCode】,且为字符串';
      exit;
    end;
    if not QPostDataJson.Find('apiData', lApiDataJson) then
    begin
      lApiAll.errMsg := '提交的Json数据,接口数据不存在键值【apiData】,且为JSON数据';
      exit;
    end;
    if not QPostDataJson.Find('apiParam', lApiParamJson) then
    begin
      lApiAll.errMsg := '提交的Json数据,接口数据不存在键值【apiParam】,且为JSON对象数据';
      exit;
    end;
    if QPostDataJson.Find('apiPage', lApiPageJson) then
    begin
      // 非必需的,如果有传必需传json对象
      if not (lApiPageJson is TJsonObject) then
      begin
        lApiAll.errMsg := '提交的Json数据,接口数据 【apiPage】不合法,应为JSON对象{"pageIndex":1,"pageSize":20}';
        exit;
      end;
    end;
    lClientZTCode := QPostDataJson.Get('apiZTCode', '');
    if not ((lApiDataJson is TJsonObject) or (lApiDataJson is TJsonArray)) then
    begin
      lApiAll.errMsg := '提交的Json数据,接口数据 【apiData】不合法,应为JSON对象{"a":"b"....}或JSON数组对象[{},{}]';
      exit;
    end;
    if not (lApiParamJson is TJsonObject) then
    begin
      lApiAll.errMsg := '提交的Json数据,接口数据 【apiParam】不合法,应为JSON对象{"a":"b"....}';
      exit;
    end;
    lFastApiManage := UnitFastApiManage();
    lApiInfo := lFastApiManage.GetApiInfo(lApiCode, lApiAll.errMsg);
    if lApiInfo = nil then
      exit;
    QApiInfo := lApiInfo;
    // 开始分析
    if not lApiInfo.CheckApiInfo() then
    begin
      lApiAll.errMsg := lApiInfo.errMsg;
      exit;
    end;
    // 权限把控
    if not DoCheckApiAuthor(QToken, lApiInfo, lApiAll) then
    begin
      exit;
    end;

    lZTItemDict := TDictionary<string, TOneZTItem>.Create;
    try
      lApiAll.token := QToken;
      lApiAll.postDataJson := QPostDataJson;
      lApiAll.dataJson := lApiDataJson;
      lApiAll.paramJson := TJsonObject(lApiParamJson);
      lApiAll.pageJson := TJsonObject(lApiPageJson);
      lApiAll.ZTDict := lZTItemDict;
      // 处理有事务的先执行
      for iData := 0 to lApiInfo.fastDatas.Count - 1 do
      begin
        lApiData := lApiInfo.fastDatas[iData];
        if not lZTItemDict.ContainsKey(lApiData.FDataZTCode) then
        begin
          lZTItemDict.Add(lApiData.FDataZTCode, nil);
        end;
        case lApiData.DataOpenMode() of
          openData, openDataStore, doStore:
          begin
          end;
          doDMLSQL, appendDatas:
          begin
            isHaveTran := True;
          end;
          else
          begin
            lApiAll.errMsg := '未归类的设计模式[' + lApiData.FDataOpenMode + ']';
            exit;
          end;
        end;
      end;
      if lClientZTCode <> '' then
      begin
        if lZTItemDict.Count > 1 then
        begin
          // 如果一个模板,多个数据集,取的是不同的账套,那么此时无法切换账套
          lClientZTCode := '前台账套【' + lClientZTCode + '】，后台配置账套';
          for lZTCode in lZTItemDict.keys do
          begin
            lClientZTCode := lClientZTCode + '[' + lZTCode + ']';
          end;
          lApiAll.errMsg := '一个模板配置账套为多个账套，无法切换账套';
          exit;
        end
        else
        begin
          // 如果一个模板配置是单账套,跟据前台切换账套
          lZTItemDict.Clear;
          lZTItemDict.Add(lClientZTCode, nil);
        end;
      end;

      iComitStep := 0;
      lZTItems := TList<TOneZTItem>.Create;
      try
        // 有事务的开启事务并行执行
        lZTManage := TOneGlobal.GetInstance().ZTManage;
        for lZTCode in lZTItemDict.keys do
        begin
          lZTItem := lZTManage.LockZTItem(lZTCode, lApiAll.errMsg);
          if lZTItem = nil then
          begin
            exit;
          end;
          lZTItemDict.Items[lZTCode] := lZTItem;
          lZTItems.Add(lZTItem);
          if (lClientZTCode <> '') and (lZTItemDict.Count = 1) then
            lApiAll.ClientZT := lZTItem;
        end;

        if isHaveTran then
        begin
          for iZT := 0 to lZTItems.Count - 1 do
          begin
            lZTItem := lZTItems[iZT];
            lZTItem.ADTransaction.StartTransaction;
          end;
          iComitStep := 1;
        end;
        if not DoFastApiStep(lApiInfo.fastDatas, lApiAll, nil) then
        begin
          exit;
        end;
        if isHaveTran then
        begin
          for iZT := 0 to lZTItems.Count - 1 do
          begin
            lZTItem := lZTItems[iZT];
            lZTItem.ADTransaction.Commit;
          end;
          iComitStep := 0;
        end;
      finally
        if iComitStep = 1 then
        begin
          for iZT := 0 to lZTItems.Count - 1 do
          begin
            lZTItem := lZTItems[iZT];
            lZTItem.ADTransaction.Rollback;
          end;
        end;
        for iZT := 0 to lZTItems.Count - 1 do
        begin
          lZTItem := lZTItems[iZT];
          lZTItem.UnLockWork;
        end;
        lZTItems.Clear;
        lZTItems.Free;
      end;
      // 结果输出
      lApiAll.isDoOK := True;
      if lApiAll.resultCode = 'FastApiFail' then
      begin
        lApiAll.resultCode := 'FastApiSuccess';
      end;
    finally
      lZTItemDict.Clear;
      lZTItemDict.Free;
    end;
  finally
    // 错误返回
    Result := lApiAll;
  end;
end;

function DoFastApi(QToken: TOneTokenItem; QPostDataJson: TJsonObject): TJsonData;
var
  lApiAll: TApiAll;
  lApiInfo: TFastApiInfo;
begin
  Result := nil;
  lApiInfo := nil;
  lApiAll := nil;
  try
    lApiAll := DoFastApiResultApiAll(QToken, QPostDataJson, lApiInfo);
  finally
    // 错误返回
    if lApiAll <> nil then
    begin
      Result := DoFastResultJson(lApiInfo, lApiAll);
      lApiAll.Free;
    end;
  end;
end;

function DoCheckApiAuthor(QToken: TOneTokenItem; QApiInfo: TFastApiInfo; QApiAll: TApiAll): boolean;
begin
  Result := False;
  if QApiInfo.fastApi.FApiAuthor = '' then
  begin
    // 不需要权限
    Result := True;
    exit;
  end;
  if QApiInfo.fastApi.FApiAuthor = '公开' then
  begin
    Result := True;
    exit;
  end
  else if QApiInfo.fastApi.FApiAuthor = 'Token验证' then
  begin
    if QToken = nil then
    begin
      QApiAll.errMsg := '请先登陆,此接口需要Token验证';
      exit;
    end;

    if QApiInfo.fastApi.FApiRole <> '' then
    begin
      if QApiInfo.fastApi.FApiRole = '超级管理员' then
      begin
        if QToken.TokenRole < TOneTokenRole.superRole then
        begin
          QApiAll.errMsg := '此接口只有超级管理员可以访问';
          exit;
        end
        else
        begin
          Result := True;
        end;
      end
      else if QApiInfo.fastApi.FApiRole = '管理员' then
      begin
        if QToken.TokenRole < TOneTokenRole.sysAdminRole then
        begin
          QApiAll.errMsg := '此接口只有管理员以上级别可以访问';
          exit;
        end
        else
        begin
          Result := True;
        end;
      end
      else if QApiInfo.fastApi.FApiRole = '系统用户' then
      begin
        if QToken.TokenRole < TOneTokenRole.sysUserRole then
        begin
          QApiAll.errMsg := '此接口只有系统用户以上级别可以访问';
          exit;
        end
        else
        begin
          Result := True;
        end;
      end
      else
      begin
        QApiAll.errMsg := '未设计的角色权限[' + QApiInfo.fastApi.FApiRole + ']';
        exit;
      end;
    end
    else
    begin
      // 不需角色权限 返回true
      Result := True;
    end;
  end
  else if QApiInfo.fastApi.FApiAuthor = 'AppID验证' then
  begin

    QApiAll.errMsg := '未设计的验证方式[' + QApiInfo.fastApi.FApiAuthor + ']';
    exit;
  end
  else
  begin

    QApiAll.errMsg := '未设计的验证方式[' + QApiInfo.fastApi.FApiAuthor + ']';
    exit;
  end;
end;

function DoFastResultJson(QApiInfo: TFastApiInfo; QApiAll: TApiAll): TJsonData;
var
  lJsonObj, lJsonResultData, lStepJson, lParamJson: TJsonObject;
  lJsonArr: TJsonArray;
  iStep, iParam: integer;
  lParam: TParam;
  lStepResult: TApiStepResult;

  procedure DoChildStepResult(QSetpChildList: TList<TApiStepResult>);
  var
    iChild: integer;
    tempStepResult, tempPStepResult: TApiStepResult;
    tempJsonArr: TJsonArray;
    tempData, tempPData: TDataSet;
    tempIArr: integer;
    tempIAdd: integer;
  begin
    if (QSetpChildList = nil) or (QSetpChildList.Count = 0) then
      exit;
    for iChild := 0 to QSetpChildList.Count - 1 do
    begin
      tempStepResult := QSetpChildList[iChild];
      if tempStepResult.FDataSet <> nil then
      begin
        if (tempStepResult.FPFilterField <> '') and (tempStepResult.FFilterField <> '') then
        begin
          tempData := tempStepResult.FDataSet;
          tempPStepResult := tempStepResult.FPStepResult;
          tempPData := tempPStepResult.FDataSet;
          tempPData.First;
          tempIArr := 0;
          while not tempPData.EOF do
          begin
            tempData.Filtered := False;
            tempData.Filter := tempStepResult.FFilterField + '=' + QuoTedStr(tempPData.FieldByName(tempStepResult.FPFilterField).AsString);
            tempData.Filtered := True;
            tempJsonArr := BuildDataToJsonArray(tempStepResult.FApiData, tempData, QApiAll);
            if tempStepResult.FResultDataJsonArr = nil then
            begin
              tempStepResult.FResultDataJsonArr := TJsonArray.Create;
              tempStepResult.FResultDataJsonArrIsFree := True; // 需要释放自已
            end
            else
            begin
              for tempIAdd := 0 to tempJsonArr.Count - 1 do
              begin
                tempStepResult.FResultDataJsonArr.Add(tempJsonArr.Items[tempIAdd]);
              end;
            end;
            TJsonObject(tempPStepResult.FResultDataJsonArr.Items[tempIArr]).Add(tempStepResult.FApiData.FDataJsonName, tempJsonArr);
            tempIArr := tempIArr + 1;
            tempPData.Next;
          end;
        end
        else
        begin
          tempPStepResult := tempStepResult.FPStepResult;
          if tempPStepResult.FResultJsonObj <> nil then
          begin
            tempJsonArr := BuildDataToJsonArray(tempStepResult.FApiData, tempStepResult.FDataSet, QApiAll);
            tempPStepResult.FResultJsonObj.Add(tempStepResult.FApiData.FDataJsonName, tempJsonArr);
          end;
        end;
      end;
      if lStepResult.FApiData.DataOpenMode = doDMLSQL then
      begin
        lStepJson.Add('AffectedCount', lStepResult.FResultAffected);
      end;
      DoChildStepResult(tempStepResult.FChilds);
    end;
  end;

begin
  lJsonObj := TJsonObject.Create();
  Result := lJsonObj;
  lJsonObj.Add('ResultSuccess', QApiAll.isDoOK);
  lJsonObj.Add('ResultCode', QApiAll.resultCode);
  lJsonObj.Add('ResultMsg', QApiAll.errMsg);
  if not QApiAll.isDoOK then
  begin
    exit;
  end;
  try
    lJsonResultData := TJsonObject.Create;
    lJsonObj.Add('ResultData', lJsonResultData);
    for iStep := 0 to QApiAll.StepResultList.Count - 1 do
    begin
      lStepJson := TJsonObject.Create();
      lStepResult := QApiAll.StepResultList[iStep];
      lStepResult.FResultJsonObj := lStepJson;
      lJsonResultData.Add(lStepResult.FApiData.FDataJsonName, lStepJson);
      if lStepResult.FDataSet <> nil then
      begin
        lJsonArr := BuildDataToJsonArray(lStepResult.FApiData, lStepResult.FDataSet, QApiAll);
        if lStepResult.FApiData.FDataJsonType = 'JsonObject' then
        begin
          if lJsonArr.Count = 0 then
          begin
            lStepJson.Add('Data', TJsonNull.Create);
          end
          else
          begin
            lStepJson.Add('Data', TJsonData(lJsonArr.Items[0].clone));
          end;
          lJsonArr.Free;
        end
        else
        begin
          lStepJson.Add('Data', lJsonArr);
          lStepResult.FResultDataJsonArr := lJsonArr;
        end;
        // 参数增加
        if lStepResult.FApiData.DataOpenMode in [openDataStore, doStore] then
        begin
          lParamJson := TJsonObject.Create;
          lStepJson.Add('Params', lParamJson);
          for iParam := 0 to lStepResult.FBuildParams.Count - 1 do
          begin
            lParam := lStepResult.FBuildParams[iParam];
            if lParam.ParamType in [ptOutput, ptInputOutput] then
            begin
              // 输出参数
              lParamJson.Add(lParam.Name, lParam.AsString);
            end;
          end;
        end;

        lStepJson.Add('DataCount', lStepResult.FDataSet.RecordCount);
      end
      else
      begin
        if lStepResult.FApiData.DataOpenMode in [openDataStore, doStore] then
        begin
          // 添加输出参数,如果没有返回数据集
          lParamJson := TJsonObject.Create;
          lStepJson.Add('Params', lParamJson);
          for iParam := 0 to lStepResult.FBuildParams.Count - 1 do
          begin
            lParam := lStepResult.FBuildParams[iParam];
            if lParam.ParamType in [ptOutput, ptInputOutput] then
            begin
              // 输出参数
              lParamJson.Add(lParam.Name, lParam.AsString);
            end;
          end;
        end;
      end;
      if lStepResult.FApiData.DataOpenMode = doDMLSQL then
      begin
        lStepJson.Add('AffectedCount', lStepResult.FResultAffected);
      end;
      DoChildStepResult(lStepResult.FChilds);
    end;

  except
    on e: Exception do
    begin
      lJsonObj.Delete('ResultSuccess');
      lJsonObj.Delete('ResultMsg');
      lJsonObj.add('ResultSuccess', False);
      lJsonObj.add('ResultMsg', e.Message);
    end;
  end;
end;

function DoFastApiStep(QApiDatas: TList<TFastApiData>; QApiAll: TApiAll; QPStepResult: TApiStepResult): boolean;
var
  iData: integer;
  lApiData: TFastApiData;
  lDataSet: TDataSet;
  lStepResult: TApiStepResult;
begin
  Result := False;
  for iData := 0 to QApiDatas.Count - 1 do
  begin
    lApiData := QApiDatas[iData];
    lStepResult := TApiStepResult.Create;
    lStepResult.FApiDataID := lApiData.FDataID;
    lStepResult.FApiData := lApiData;
    lStepResult.FPStepResult := QPStepResult;
    if QPStepResult <> nil then
      QPStepResult.FChilds.Add(lStepResult)
    else
      QApiAll.StepResultList.Add(lStepResult);

    case lApiData.DataOpenMode() of
      openData:
      begin
        // 跟据SQL打开数据
        if not DoOpenData(lApiData, QApiAll, lStepResult) then
        begin
          exit;
        end;
      end;
      openDataStore:
      begin
        // 执行存储过程,有返回数据集
        if not DoDataStore(lApiData, QApiAll, lStepResult) then
          exit;
      end;
      doStore:
      begin
        // 执行存储过程,无返回数据集
        if not DoDataStore(lApiData, QApiAll, lStepResult) then
          exit;
      end;
      doDMLSQL:
      begin
        if not DoDml(lApiData, QApiAll, lStepResult) then
          exit;
      end;
      appendDatas:
      begin
        // 批量增加数据
        if not DoAppendDatas(lApiData, QApiAll, lStepResult) then
          exit;
      end;
    end;
    if (lApiData.ChildDatas <> nil) and (lApiData.ChildDatas.Count > 0) then
    begin
      // 查询儿子关联数据
      if not DoFastApiStep(lApiData.ChildDatas, QApiAll, lStepResult) then
      begin
        exit;
      end;
    end;
  end;
  Result := True;
end;

function DoOpenData(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
var
  iData, iParam: integer;
  lApiData: TFastApiData;
  lPageJson: TJsonObject;
  iPageIndex, iPageSize, tempI: integer;
  // {"pageIndex":1,"pageSize":20}
  lStringList: TStringList;
  lZTItem: TOneZTItem;
  lQuery: TFDQuery;
  lFDParam: TParam;
  lParam: TParam;
  lJNumer: TJSONNumber;

  LSQLInfo: TSQLInfo;
begin
  Result := False;
  iPageIndex := -1;
  iPageSize := -1;

  lPageJson := QApiAll.pageJson;
  if lPageJson <> nil then
  begin
    if lPageJson.Find('pageIndex', lJNumer) then
    begin
      iPageIndex := lJNumer.AsInteger;
    end;
    if lPageJson.Find('pageSize', lJNumer) then
    begin
      iPageSize := lJNumer.AsInteger;
    end;
  end;
  if iPageSize <= 0 then
  begin
    iPageSize := QApiData.FDataPageSize;
  end;
  if iPageSize <= 0 then
    iPageSize := -1;
  // 解析参数 以及组装出SQL
  if not BuildDataSQLAndParams(QApiData, QApiAll, QStepResult) then
    exit;
  lQuery := nil;
  try
    lStringList := TStringList.Create;
    try
      lStringList.Text := QApiData.FDataSQL;
      if QApiData.FilterLine >= 0 then
      begin
        lStringList[QApiData.FilterLine] := QStepResult.FBuildFilterSQL;
      end;
      // 获取账套
      lZTItem := QApiAll.GetZTItem(QApiData.FDataZTCode);
      // 打开数据
      lQuery := lZTItem.CreateNewQuery(True);
      InitSQLInfo(LSQLInfo);
      LSQLInfo.FDriver := lZTItem.ZTSet.PhyDriver;
      LSQLInfo.FDriverVersion := lZTItem.ZTSet.DBVersion;
      LSQLInfo.FPageIndex := iPageIndex;
      LSQLInfo.FPageSize := iPageSize;
      LSQLInfo.FSQL := lStringList.Text;
      if not SetSQLInfo(LSQLInfo) then
      begin
        QApiAll.errMsg := LSQLInfo.FErrMsg;
        exit;
      end;
      lQuery.SQL.Text := LSQLInfo.FSQL;
      // 分页处理
      //if (iPageSize > 0) and (iPageIndex > 0) then
      //begin
      //  lQuery.FetchOptions.RecsSkip := (iPageIndex - 1) * iPageSize;
      //  lQuery.FetchOptions.RecsMax := iPageSize;
      //end
      //else
      //begin
      //  lQuery.FetchOptions.RecsSkip := -1;
      //  if iPageSize > 0 then
      //    lQuery.FetchOptions.RecsMax := iPageSize
      //  else
      //    lQuery.FetchOptions.RecsMax := -1;
      //end;
      // 参数处理
      for iParam := 0 to lQuery.params.Count - 1 do
      begin
        lFDParam := lQuery.params[iParam];
        lParam := QStepResult.FBuildParams.FindParam(lFDParam.Name);
        if lParam = nil then
        begin
          QApiAll.errMsg := '参数[' + lFDParam.Name + ']找不到,请开发人员检查组装的参数代码';
          exit;
        end;
        lFDParam.DataType := lParam.DataType;
        lFDParam.ParamType := lParam.ParamType;
        lFDParam.Value := lParam.Value;
      end;

      lQuery.Open;
      if not lQuery.Active then
      begin
        QApiAll.errMsg := '打开数据失败';
        exit;
      end;
      if QApiData.FDataJsonType = 'JsonObject' then
      begin
        if lQuery.RecordCount > 1 then
        begin
          QApiAll.errMsg := '数据集[' + QApiData.FDataName + ']设定是返回Json对象,实际结果数据量大于1';
          exit;
        end;
      end;
      QStepResult.FDataSet := lQuery;
    finally
      lStringList.Free;
    end;
    Result := True;
  finally
    if not Result then
    begin
      if lQuery <> nil then
      begin
        lQuery.Free;
      end;
      QStepResult.FDataSet := nil;
    end;
  end;
end;

function DoDataStore(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
var
  iData, iParam: integer;
  lApiData: TFastApiData;
  tempI: integer;
  // {"pageIndex":1,"pageSize":20}
  lZTItem: TOneZTItem;
  lFDStored: TFDStoredProc;
  lFDParam: TParam;
  lParam: TParam;

  lPTResult: integer;
begin
  Result := False;
  // 解析参数 以及组装出SQL
  if not BuildDataSQLAndParams(QApiData, QApiAll, QStepResult) then
    exit;
  // 获取账套
  lZTItem := QApiAll.GetZTItem(QApiData.FDataZTCode);
  // 打开数据
  lFDStored := lZTItem.ADStoredProc;
  lFDStored.StoredProcName := QApiData.FDataStoreName;
  lFDStored.Prepare;
  if not lFDStored.Prepared then
  begin
    QApiAll.errMsg := '校验存储过程失败,请检查是否有此存储过程[' + QApiData.FDataStoreName + ']';
    exit;
  end;
  lPTResult := 0;
  // 参数处理
  for iParam := lFDStored.params.Count - 1 downto 0 do
  begin
    lFDParam := lFDStored.params[iParam];
    if lFDParam.ParamType = TParamType.ptResult then
    begin
      lPTResult := lPTResult + 1;
      continue;
    end;
    if lFDParam.Name.StartsWith('@') then
    begin
      // SQL数据库返回参数代@去掉
      lFDParam.Name := lFDParam.Name.Substring(1);
    end;
  end;

  if QStepResult.FBuildParams.Count <> lFDStored.params.Count - lPTResult then
  begin
    QApiAll.errMsg := '参数个数错误->服务端参数个数[' + lFDStored.params.Count.ToString + '],如下[';
    for iParam := lFDStored.params.Count - 1 downto 0 do
    begin
      QApiAll.errMsg := QApiAll.errMsg + lFDStored.params[iParam].Name;
    end;
    QApiAll.errMsg := QApiAll.errMsg + ';客户端参数个数[' + QStepResult.FBuildParams.Count.ToString + '],如下[';
    for iParam := QStepResult.FBuildParams.Count - 1 downto 0 do
    begin
      QApiAll.errMsg := QApiAll.errMsg + QStepResult.FBuildParams[iParam].Name;
    end;
    QApiAll.errMsg := QApiAll.errMsg + ']';
    exit;
  end;

  // 参数赋值
  for iParam := 0 to lFDStored.params.Count - 1 do
  begin
    lFDParam := lFDStored.params[iParam];
    if lFDParam.ParamType = TParamType.ptResult then
    begin
      continue;
    end;
    // 处理参数
    lParam := QStepResult.FBuildParams.FindParam(lFDParam.Name);
    if lParam = nil then
    begin
      QApiAll.errMsg := '存储过程参数[' + lFDParam.Name + ']找不到相关的参数配置';
      exit;
    end;
    // lFDParam.DataType := lParam.DataType;
    lParam.ParamType := lFDParam.ParamType;
    if not lParam.IsNull then
      lFDParam.Value := lParam.Value;
  end;

  try
    if QApiData.DataOpenMode = emDataOpenMode.openDataStore then
    begin

      //if not lFDStored.OpenOrExecute then
      //begin
      //  QApiAll.errMsg := lZTItem.FDException.FErrmsg;
      //  exit;
      //end;
    end
    else
    begin

      //lFDStored.ExecProc;
    end;


  except
    on e: Exception do
    begin
      QApiAll.errMsg := '执行存储过程异常:' + e.Message;
      exit;
    end;
  end;
  Result := True;
end;

function DoDml(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
var
  iData, iParam: integer;
  lApiData: TFastApiData;
  tempI: integer;
  // {"pageIndex":1,"pageSize":20}
  lStringList: TStringList;
  lZTItem: TOneZTItem;
  LZTQuery: TFDQuery;
  lFDParam: TParam;
  lParam: TParam;
  iRowsAffected: integer;
begin
  Result := False;
  // 解析参数 以及组装出SQL
  if not BuildDataSQLAndParams(QApiData, QApiAll, QStepResult) then
    exit;

  lStringList := TStringList.Create;
  try
    lStringList.Text := QApiData.FDataSQL;
    if QApiData.FilterLine >= 0 then
    begin
      lStringList[QApiData.FilterLine] := QStepResult.FBuildFilterSQL;
    end;
    // 获取账套
    lZTItem := QApiAll.GetZTItem(QApiData.FDataZTCode);
    // 打开数据
    LZTQuery := lZTItem.ADQuery;
    LZTQuery.SQL.Text := lStringList.Text;
    // 参数处理
    for iParam := 0 to LZTQuery.params.Count - 1 do
    begin
      lFDParam := LZTQuery.params[iParam];
      lParam := QStepResult.FBuildParams.FindParam(lFDParam.Name);
      if lParam = nil then
      begin
        QApiAll.errMsg := '参数[' + lFDParam.Name + ']找不到,请开发人员检查组装的参数代码';
        exit;
      end;
      lFDParam.DataType := lParam.DataType;
      lFDParam.ParamType := lParam.ParamType;
      lFDParam.Value := lParam.Value;
    end;

    try
      LZTQuery.ExecSQL;
      iRowsAffected := LZTQuery.RowsAffected;
      QStepResult.FResultAffected := iRowsAffected;
      if QApiData.FMinAffected > 0 then
      begin
        if iRowsAffected < QApiData.FMinAffected then
        begin
          QApiAll.errMsg := '执行失败:数据集[' + QApiData.FDataName + ']当前影响行数[' + iRowsAffected.ToString +
            ']小于设置最小影响行数[' + QApiData.FMinAffected.ToString + ']';
          exit;
        end;
      end;
      if QApiData.FMaxAffected > 0 then
      begin
        if iRowsAffected > QApiData.FMaxAffected then
        begin
          QApiAll.errMsg := '执行失败:数据集[' + QApiData.FDataName + ']当前影响行数[' + iRowsAffected.ToString +
            ']大于设置最大影响行数[' + QApiData.FMaxAffected.ToString + ']';
          exit;
        end;
      end;
    except
      on e: Exception do
      begin
        QApiAll.errMsg := '执行DML语句异常,原因:' + e.Message;
        exit;
      end;
    end;
    Result := True;
  finally
    lStringList.Free;
  end;
end;

// 批量增加数据
function DoAppendDatas(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
var
  lZTItem: TOneZTItem;
  LZTQuery: TFDQuery;
begin
  Result := False;
  if QApiAll.dataJson = nil then
  begin
    QApiAll.errMsg := '提交的数据[apiData]为空!';
    exit;
  end;
  // 获取账套
  lZTItem := QApiAll.GetZTItem(QApiData.FDataZTCode);
  // 打开一个空数据
  LZTQuery := lZTItem.ADQuery;
  LZTQuery.TableName := QApiData.FDataTable;
  LZTQuery.KeyFields := QApiData.FDataPrimaryKey;
  LZTQuery.SQL.Text := 'select * from ' + QApiData.FDataTable + ' where 1=2 ';
  LZTQuery.Open();

  // 开始打添加JSON数据
  if not BuildAppendDataSet(LZTQuery, QApiData, QApiAll) then
  begin
    exit;
  end;
  // 保存数据有错,退出
  LZTQuery.ApplyUpdates(0);
  Result := True;
end;

function BuildDataSQLAndParams(QApiData: TFastApiData; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
var
  iFilter: integer;
  lApiFilter: TFastApiFilter;
  lParams: TParams;
begin
  Result := False;
  for iFilter := 0 to QApiData.ChildFilters.Count - 1 do
  begin
    lApiFilter := QApiData.ChildFilters[iFilter];
    if not BuildFilterSQLAndParams(QApiData, lApiFilter, QApiAll, QStepResult) then
      exit;
  end;
  Result := True;
end;

function BuildFilterSQLAndParams(QApiData: TFastApiData; QApiFilter: TFastApiFilter; QApiAll: TApiAll; QStepResult: TApiStepResult): boolean;
var
  lParam: TParam;
  tempValue: string;
  tempSQL: string;
  lJSONPair: TJsonData;
  isGetDefault: boolean;

  lFieldNames: TArray<string>;
  iField: integer;
  lFieldName, lFieldParamName: string;

  lParamNames: string;
  iValue: integer;
  lArrValues: TArray<string>;

  lData: TDataSet;
begin
  Result := False;
  tempValue := '';
  isGetDefault := True;
  tempSQL := '';
  // fromJsonData,fromJsonParam,fromConst,
  if QApiFilter.FJsonIsEmptyGetDefault then
  begin
    // 优先取Json数据
    // 取Post上来的Json参数
    if QApiAll.paramJson = nil then
    begin
      QApiAll.errMsg := 'QParamJson参数为nil';
      exit;
    end;
    if QApiAll.paramJson.Find(QApiFilter.FFilterJsonName, lJSONPair) then
    begin
      tempValue := lJSONPair.Value;
    end;
    if tempValue <> '' then
      isGetDefault := False;
  end;

  if isGetDefault then
  begin
    if QApiFilter.FFilterFieldMode = '父级关联' then
    begin
      tempValue := '';
      if QStepResult.FPStepResult = nil then
      begin
        QApiAll.errMsg := '数据集配置[' + QApiData.FDataName + ']无父级数据集,不可配置参数来源父数据集';
        exit;
      end;
      if (QStepResult.FPStepResult.FDataSet = nil) or (QStepResult.FPStepResult.FDataSet.RecordCount = 0) then
      begin
        QApiAll.errMsg := '数据集配置[' + QApiData.FDataName + ']父级数据集为nil或无数据,不可配置参数来源父数据集';
        exit;
      end;
      QApiFilter.FFilterExpression := '包含';
      QStepResult.FPFilterField := QApiFilter.FPFilterField;
      QStepResult.FFilterField := QApiFilter.FFilterField;
      lData := QStepResult.FPStepResult.FDataSet;
      lData.First;
      while not lData.EOF do
      begin
        tempValue := tempValue + lData.FieldByName(QApiFilter.FPFilterField).AsString + '|';
        lData.Next;
      end;
    end
    else if QApiFilter.FFilterDefaultType = 'fromConst' then
    begin
      // 固定常量
      tempValue := QApiFilter.FFilterDefaultValue;
    end
    else if QApiFilter.FFilterDefaultType = 'fromJsonParam' then
    begin
      // 取Post上来的Json参数
      if QApiAll.paramJson = nil then
      begin
        QApiAll.errMsg := 'QParamJson参数为nil';
        exit;
      end;
      lJSONPair := nil;
      QApiAll.paramJson.Find(QApiFilter.FFilterJsonName, lJSONPair);
      if QApiFilter.FFilterbMust then
      begin
        if lJSONPair = nil then
        begin
          QApiAll.errMsg := '参数[' + QApiFilter.FFilterJsonName + ']是必需的参数';
          exit;
        end;
      end;
      if lJSONPair <> nil then
        tempValue := lJSONPair.Value;
    end
    else if QApiFilter.FFilterDefaultType = 'fromSysToken' then
    begin
      tempValue := QApiFilter.FFilterDefaultValue;
      if tempValue = 'TokenID' then
      begin
        tempValue := QApiAll.token.TokenID;
      end
      else if tempValue = 'TokenUserID' then
      begin
        tempValue := QApiAll.token.SysUserID;
      end
      else if tempValue = 'TokenUserCode' then
      begin
        tempValue := QApiAll.token.SysUserCode;
      end
      else if tempValue = 'TokenUserName' then
      begin
        tempValue := QApiAll.token.SysUserName;
      end
      else if tempValue = 'TokenLoginCode' then
      begin
        tempValue := QApiAll.token.LoginUserCode;
      end
      else
      begin
        QApiAll.errMsg := '参数配置[' + QApiFilter.FFilterName + '],类型[' + QApiFilter.FFilterDefaultType +
          ']值[' + tempValue + ']未实现';
        exit;
      end;
    end
    else if QApiFilter.FFilterDefaultType = 'fromPData' then
    begin
      // 获取父级数据
      tempValue := '';
      if QStepResult.FPStepResult = nil then
      begin
        QApiAll.errMsg := '数据集配置[' + QApiData.FDataName + ']无父级数据集,不可配置参数来源父数据集';
        exit;
      end;
      if (QStepResult.FPStepResult.FDataSet = nil) or (QStepResult.FPStepResult.FDataSet.RecordCount = 0) then
      begin
        QApiAll.errMsg := '数据集配置[' + QApiData.FDataName + ']父级数据集为nil或无数据,不可配置参数来源父数据集';
        exit;
      end;
      lData := QStepResult.FPStepResult.FDataSet;
      tempValue := lData.FieldByName(QApiFilter.FFilterDefaultValue).AsString;
    end
    else if QApiFilter.FFilterDefaultType = 'fromSys' then
    begin
      if QApiFilter.FFilterDefaultValue = 'UnionID' then
      begin
        tempValue := QApiAll.UnionID;
      end
      else if QApiFilter.FFilterDefaultValue = 'SysGUID' then
      begin
        tempValue := OneGUID.GetGUID32;
      end
      else if QApiFilter.FFilterDefaultValue = 'SysUUID' then
      begin
        tempValue := OneUUID.GetUUIDStr;
      end
      else if QApiFilter.FFilterDefaultValue = 'SysDateTime' then
      begin
        tempValue := FormatDateTime('yyyy-mm-dd hh:mm:ss', now);
      end
      else if QApiFilter.FFilterDefaultValue = 'SysDate' then
      begin
        tempValue := FormatDateTime('yyyy-mm-dd', now);
      end
      else if QApiFilter.FFilterDefaultValue = 'SysTime' then
      begin
        tempValue := FormatDateTime('hh:mm', now);
      end
      else if QApiFilter.FFilterDefaultValue = 'SysYear' then
      begin
        tempValue := FormatDateTime('yyyy', now);
      end
      else
      begin
        QApiAll.errMsg := '参数配置[' + QApiFilter.FFilterName + '],类型[' + QApiFilter.FFilterDefaultType +
          ']值[' + QApiFilter.FFilterDefaultValue + ']未实现';
        exit;
      end;
    end
    else
    begin
      QApiAll.errMsg := '参数配置[' + QApiFilter.FFilterName + '],类型[' + QApiFilter.FFilterDefaultType + ']未设计';
      exit;
    end;
  end;

  if QApiFilter.FFilterbValue then
  begin
    if tempValue.Trim = '' then
    begin
      QApiAll.errMsg := '参数[' + QApiFilter.FFilterJsonName + ']参数必需有值';
      exit;
    end;
  end;
  // 添加参数
  if tempValue = '' then
  begin
    // 空值，存储过程执行DML需要添加参数
    if QApiData.DataOpenMode in [openDataStore, doStore] then
    begin
      if QApiFilter.FbOutParam then
      begin
        lParam := QStepResult.FBuildParams.Add as TParam;
        lParam.Name := QApiFilter.FFilterField;
        lParam.ParamType := TParamType.ptInputOutput;
      end;
    end
    else if QApiData.DataOpenMode in [doDMLSQL] then
    begin
      // DMLSQL参数为空值时也要加参数
      lParam := QStepResult.FBuildParams.Add as TParam;
      lParam.Name := QApiFilter.FFilterField;
      lParam.Value := '';
    end;
  end
  else
  begin
    if QApiFilter.FFilterFieldMode = '值选择' then
    begin
      Result := QApiFilter.GetValueSQL(tempValue, tempSQL);
      if not Result then
      begin
        QStepResult.FBuildFilterSQL := QStepResult.FBuildFilterSQL + ' ' + tempSQL + ' ';
      end
      else
      begin
        QApiAll.errMsg := '参数[' + QApiFilter.FFilterJsonName + ']值选择找不到对应值的SQL语句';
      end;
      // 值选对SQL 这边就退出去了
      exit;
    end;

    lFieldNames := QApiFilter.FieldNames;
    // 非SQL固定字段需要组装SQL
    // 多字段过滤
    for iField := low(lFieldNames) to High(lFieldNames) do
    begin
      lFieldName := lFieldNames[iField];
      if QApiFilter.IsFixSQLParam or (QApiData.DataOpenMode in [doStore, openDataStore]) then
      begin
        // 保持参数名称
        lFieldParamName := lFieldName;
      end
      else
      begin
        lFieldParamName := 'ft' + QApiFilter.FFilterName + lFieldName;
      end;
      // 组装SQL
      if tempSQL <> '' then
        tempSQL := tempSQL + ' or ';
      if QApiFilter.FFilterExpression = '=' then
      begin
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + QApiFilter.FFilterExpression + ' :' + lFieldParamName + ' ) ';
        lParam := QStepResult.FBuildParams.Add as TParam;
        lParam.Name := lFieldParamName;
        if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, tempValue) then
          exit;
      end
      else if QApiFilter.FFilterExpression = '相似' then
      begin
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + 'like' + ' :' + lFieldParamName + ' ) ';
        lParam := QStepResult.FBuildParams.Add as TParam;
        lParam.Name := lFieldParamName;
        if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, '%' + tempValue + '%') then
          exit;
      end
      else if QApiFilter.FFilterExpression = '左相似' then
      begin
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + 'like' + ' :' + lFieldParamName + ' ) ';
        lParam := QStepResult.FBuildParams.Add as TParam;
        lParam.Name := lFieldParamName;
        if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, '%' + tempValue) then
          exit;
      end
      else if QApiFilter.FFilterExpression = '右相似' then
      begin
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + 'like' + ' :' + lFieldParamName + ' ) ';
        lParam := QStepResult.FBuildParams.Add as TParam;
        lParam.Name := lFieldParamName;
        if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, tempValue + '%') then
          exit;
      end
      else if QApiFilter.FFilterExpression = '包含' then
      begin
        lArrValues := tempValue.Split(['|']);
        lParamNames := '';
        for iValue := Low(lArrValues) to High(lArrValues) do
        begin
          if lParamNames <> '' then
            lParamNames := lParamNames + ',';
          lParamNames := lParamNames + ':' + lFieldParamName + iValue.ToString;
        end;
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + 'in' + ' ( ' + lParamNames + ') ) ';
        for iValue := Low(lArrValues) to High(lArrValues) do
        begin
          tempValue := lArrValues[iValue];
          lParam := QStepResult.FBuildParams.Add as TParam;
          lParam.Name := lFieldParamName + iValue.ToString;
          if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, tempValue) then
            exit;
        end;
      end
      else if QApiFilter.FFilterExpression = '不包含' then
      begin
        lArrValues := tempValue.Split(['|']);
        lParamNames := '';
        for iValue := Low(lArrValues) to High(lArrValues) do
        begin
          if lParamNames <> '' then
            lParamNames := lParamNames + ',';
          lParamNames := lParamNames + ':' + lFieldParamName + iValue.ToString;
        end;
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + 'not in' + ' ( ' + lParamNames + ') ) ';
        for iValue := Low(lArrValues) to High(lArrValues) do
        begin
          tempValue := lArrValues[iValue];
          lParam := QStepResult.FBuildParams.Add as TParam;
          lParam.Name := lFieldParamName + iValue.ToString;
          if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, tempValue) then
            exit;
        end;
      end
      else
      begin
        tempSQL := tempSQL + ' (' + lFieldName + ' ' + QApiFilter.FFilterExpression + ' :' + lFieldParamName + ' ) ';
        lParam := QStepResult.FBuildParams.Add as TParam;
        lParam.Name := lFieldParamName;
        if not BuildFilterParamValue(QApiAll, QApiFilter, lParam, tempValue) then
          exit;
      end;
    end;
    if (tempSQL <> '') and (not QApiFilter.IsFixSQLParam) then
    begin
      // 动态参数才组装SQL
      tempSQL := ' and ( ' + tempSQL + ' ) ';
      QStepResult.FBuildFilterSQL := QStepResult.FBuildFilterSQL + tempSQL;
    end;
  end;
  Result := True;
end;

function BuildFilterParamValue(QApiAll: TApiAll; QApiFilter: TFastApiFilter; QParam: TParam; QValue: string): boolean;
var
  tempI64: int64;
  tempFloat: double;
  tempDataTime: TDateTime;
begin
  Result := False;
  if QApiFilter.FFilterDataType = '字符串' then
  begin
    QParam.AsString := QValue;
  end
  else if QApiFilter.FFilterDataType = '整型' then
  begin
    if TryStrToInt64(QValue, tempI64) then
    begin
      QParam.AsLargeInt := tempI64;
    end
    else
    begin
      QApiAll.errMsg := '条件[' + QApiFilter.FFilterName + ']转化成整型出错,当前值[' + QValue + ']';
      exit;
    end;
  end
  else if QApiFilter.FFilterDataType = '数字' then
  begin
    if TryStrToFloat(QValue, tempFloat) then
    begin
      QParam.AsFloat := tempFloat;
    end
    else
    begin
      QApiAll.errMsg := '条件[' + QApiFilter.FFilterName + ']转化成数字出错,当前值[' + QValue + ']';
      exit;
    end;
  end
  else if QApiFilter.FFilterDataType = '布尔' then
  begin
    QParam.AsBoolean := False;
    if (QValue.ToLower = 'true') or (QValue = '1') or (QValue.ToLower = 't') then
    begin
      QParam.AsBoolean := True;
    end;
  end
  else if QApiFilter.FFilterDataType = '时间' then
  begin
    if TryStrToDateTime(QValue, tempDataTime) then
    begin
      if QApiFilter.FFilterFormat <> '' then
      begin
        if QApiFilter.FFilterFormat = 'yyyy-mm-dd' then
        begin
          QParam.AsDate := tempDataTime;
        end
        else if QApiFilter.FFilterFormat = 'hh:nn:ss' then
        begin
          QParam.AsTime := tempDataTime;
        end
        else
        begin
          QParam.AsDateTime := tempDataTime;
        end;
      end
      else
      begin
        QParam.AsDateTime := tempDataTime;
      end;
    end
    else
    begin
      QApiAll.errMsg := '条件[' + QApiFilter.FFilterName + ']转化成时间出错,当前值[' + QValue + ']';
      exit;
    end;
  end
  else
  begin
    QParam.AsString := QValue;
  end;
  Result := True;
end;

function BuildDataToJsonArray(QApiData: TFastApiData; Query: TDataSet; QApiAll: TApiAll): TJsonArray;
var
  lData: TFDMemtable;
  lJsonArray: TJsonArray;
  lJsonObj: TJsonObject;
  lApiField: TFastApiField;
  lField: TField;
  iField: integer;
  lFieldTemp: TApiFieldTemp;
  lFieldList: TList<TApiFieldTemp>;
  tempStream: TMemoryStream;
  tempStr: string;
begin
  Result := nil;
  lFieldList := TList<TApiFieldTemp>.Create;
  tempStream := TMemoryStream.Create;
  try
    if (QApiData.ChildFields = nil) or (QApiData.ChildFields.Count = 0) then
    begin
      for iField := 0 to Query.FieldCount - 1 do
      begin
        lFieldTemp := TApiFieldTemp.Create;
        lFieldList.Add(lFieldTemp);
        lFieldTemp.FField := Query.Fields[iField];
        lFieldTemp.FFieldName := lFieldTemp.FField.Name;
        lFieldTemp.FFormat := '';
        lFieldTemp.FJsonName := lFieldTemp.FField.Name;
      end;
    end
    else
    begin
      for iField := 0 to QApiData.ChildFields.Count - 1 do
      begin
        lApiField := QApiData.ChildFields[iField];
        lField := Query.FindField(lApiField.FFieldName);
        if lField = nil then
        begin
          QApiAll.errMsg := '数据集[' + QApiData.FDataName + ']字段[' + lApiField.FFieldName + ']打开的数据集找不到相关字段';
          exit;
        end;
        lFieldTemp := TApiFieldTemp.Create;
        lFieldList.Add(lFieldTemp);
        lFieldTemp.FField := lField;
        lFieldTemp.FFieldName := lFieldTemp.FField.Name;
        lFieldTemp.FJsonName := lApiField.FFieldJsonName;
        lFieldTemp.FFormat := lApiField.FFieldFormat;
      end;
    end;
    // 数据集转Json

    lJsonArray := TJsonArray.Create;
    try
      Query.First;
      while not Query.EOF do
      begin
        lJsonObj := TJsonObject.Create;
        lJsonArray.Add(lJsonObj);
        for iField := 0 to lFieldList.Count - 1 do
        begin
          lFieldTemp := lFieldList[iField];
          lField := lFieldTemp.FField;
          if lField.IsNull then
          begin
            lJsonObj.Add(lFieldTemp.FJsonName, TJsonNull.Create);
            continue;
          end;
          // 开始
          case lField.DataType of
            ftFixedChar, ftString:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsString);
            end;
            ftFixedWideChar, ftWideString:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsWideString);
            end;
            ftBoolean:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsBoolean);
            end;
            ftFloat:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsFloat);
            end;
            ftAutoInc, ftSmallInt, ftInteger:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsInteger);
            end;
            ftLargeInt:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsLargeInt);
            end;
            ftDate:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsDateTime);
            end;
            ftTime:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsDateTime);
            end;
            ftTimestamp, ftDateTime:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsDateTime);
            end;
            ftCurrency:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsCurrency);
            end;
            ftBCD, ftFmtBCD:
            begin
              lJsonObj.Add(lFieldTemp.FJsonName, lField.AsCurrency);
            end
            else
            begin
              if (lField.DataType in ftBlobTypes) then
              begin
                tempStream.Clear;
                TBlobField(lField).SaveToStream(tempStream);
                tempStr := OneStreamString.StreamToBase64Str(tempStream);
                lJsonObj.Add(lFieldTemp.FJsonName, tempStr);
              end
              else
                lJsonObj.Add(lFieldTemp.FJsonName, lField.AsString);
            end;
          end;
        end;
        Query.Next;
      end;
      Result := lJsonArray;
    except
      on e: Exception do
      begin
        lJsonArray.Free;
        QApiAll.errMsg := e.Message;
      end;
    end;
  finally
    for iField := 0 to lFieldList.Count - 1 do
    begin
      lFieldList[iField].Free;
    end;
    lFieldList.Clear;
    lFieldList.Free;
    tempStream.Free;
  end;

end;

function BlobFieldToBase64(ABlobField: TBlobField): string;
var
  LBlobStream: TMemoryStream;
begin
  LBlobStream := TMemoryStream.Create;
  try
    ABlobField.SaveToStream(LBlobStream);
    LBlobStream.Position := soFromBeginning;
    Result := OneStreamString.StreamToBase64Str(LBlobStream);
  finally
    LBlobStream.Free;
  end;
end;

function SetFieldValue(QDataField: TField; QApiField: TFastApiField; QDataJson: TJsonObject; QApiAll: TApiAll): boolean;
var
  lJSONPair: TJsonData;
  tempValue: string;
begin
  Result := False;
  lJSONPair := nil;
  tempValue := '';
  if 1 = 1 then
  begin
    if QApiField.FFieldDefaultValueType = 'fromConst' then
    begin
      // 固定常量
      tempValue := QApiField.FFieldDefaultValue;
    end
    else if QApiField.FFieldDefaultValueType = 'fromJsonParam' then
    begin
      // 取Post上来的Json参数
      if QDataJson.Find(QApiField.FFieldJsonName, lJSONPair) then
        tempValue := lJSONPair.Value
      else if QApiField.FFieldIsMust then
      begin
        // JSON必需上传相关参数
        QApiAll.errMsg := '参数配置[' + QApiField.FFieldJsonName + '],上传的数据必需要有此信息';
        exit;
      end;

    end
    else if QApiField.FFieldDefaultValueType = 'fromSysToken' then
    begin
      tempValue := QApiField.FFieldDefaultValue;
      if tempValue = 'TokenID' then
      begin
        tempValue := QApiAll.token.TokenID;
      end
      else if tempValue = 'TokenUserID' then
      begin
        tempValue := QApiAll.token.SysUserID;
      end
      else if tempValue = 'TokenUserCode' then
      begin
        tempValue := QApiAll.token.SysUserCode;
      end
      else if tempValue = 'TokenUserName' then
      begin
        tempValue := QApiAll.token.SysUserName;
      end
      else if tempValue = 'TokenLoginCode' then
      begin
        tempValue := QApiAll.token.LoginUserCode;
      end
      else
      begin
        QApiAll.errMsg := '参数配置[' + QApiField.FFieldName + '],类型[' + QApiField.FFieldDefaultValueType +
          ']值[' + tempValue + ']未实现';
        exit;
      end;
    end
    else if QApiField.FFieldDefaultValueType = 'fromPData' then
    begin
      // 获取父级数据
      tempValue := '';
      QApiAll.errMsg := '参数配置[' + QApiField.FFieldName + '],类型[' + QApiField.FFieldDefaultValueType + ']未实现';
      exit;
    end
    else if QApiField.FFieldDefaultValueType = 'fromSys' then
    begin
      if QApiField.FFieldDefaultValue = 'UnionID' then
      begin
        tempValue := QApiAll.UnionID;
      end
      else if QApiField.FFieldDefaultValue = 'SysGUID' then
      begin
        tempValue := OneGUID.GetGUID32;
      end
      else if QApiField.FFieldDefaultValue = 'SysUUID' then
      begin
        tempValue := OneUUID.GetUUIDStr;
      end
      else if QApiField.FFieldDefaultValue = 'SysDateTime' then
      begin
        tempValue := FormatDateTime('yyyy-mm-dd hh:mm:ss', now);
      end
      else if QApiField.FFieldDefaultValue = 'SysDate' then
      begin
        tempValue := FormatDateTime('yyyy-mm-dd', now);
      end
      else if QApiField.FFieldDefaultValue = 'SysTime' then
      begin
        tempValue := FormatDateTime('hh:mm', now);
      end
      else if QApiField.FFieldDefaultValue = 'SysYear' then
      begin
        tempValue := FormatDateTime('yyyy', now);
      end
      else
      begin
        QApiAll.errMsg := '参数配置[' + QApiField.FFieldName + '],类型[' + QApiField.FFieldDefaultValueType +
          ']值[' + QApiField.FFieldDefaultValue + ']未实现';
        exit;
      end;
    end
    else
    begin
      QApiAll.errMsg := '参数配置[' + QApiField.FFieldName + '],类型[' + QApiField.FFieldDefaultValueType + ']未设计';
      exit;
    end;
  end;
  // 要求有值
  if QApiField.FFieldIsMustValue then
  begin
    if tempValue.Trim = '' then
    begin
      QApiAll.errMsg := '参数[' + QApiField.FFieldName + ']参数必需有值';
      exit;
    end;
  end;
  // 赋值
  case QDataField.DataType of
    TFieldType.ftString:
      QDataField.AsString := tempValue;
    TFieldType.ftWideString:
      QDataField.AsWideString := tempValue;
    TFieldType.ftSmallint:
      QDataField.AsString := tempValue;
    TFieldType.ftInteger:
      QDataField.AsString := tempValue;
    TFieldType.ftWord:
      QDataField.AsString := tempValue;
    TFieldType.ftBoolean:
      QDataField.AsString := tempValue;
    TFieldType.ftFloat:
      QDataField.AsString := tempValue;
    TFieldType.ftCurrency:
      QDataField.AsString := tempValue;
    TFieldType.ftBCD:
      QDataField.AsString := tempValue;
    TFieldType.ftDate:
      QDataField.AsString := tempValue;
    TFieldType.ftTime:
      QDataField.AsString := tempValue;
    TFieldType.ftDateTime:
      QDataField.AsString := tempValue;
    TFieldType.ftBytes:
      // QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftVarBytes:
      // QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftAutoInc:
      QDataField.AsString := tempValue;
    TFieldType.ftBlob:
      // QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftMemo:
      QDataField.AsString := tempValue;
    TFieldType.ftGraphic:
      // QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftTypedBinary:
      //QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftFixedChar:
      QDataField.AsString := tempValue;

    TFieldType.ftLargeint:
      QDataField.AsString := tempValue;
    TFieldType.ftADT:
      //QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftArray:
      // QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftOraBlob:
      //QDataField.AsBytes := TNetEncoding.Base64.DecodeStringToBytes(tempValue);
    begin

    end;
    TFieldType.ftOraClob:
      QDataField.AsString := tempValue;
    TFieldType.ftVariant:
      QDataField.AsString := tempValue;
    TFieldType.ftGuid:
      QDataField.AsString := tempValue;
    TFieldType.ftTimeStamp:
      QDataField.AsString := tempValue;
    TFieldType.ftFMTBcd:
      QDataField.AsString := tempValue;
    TFieldType.ftFixedWideChar:
      QDataField.AsString := tempValue;
    TFieldType.ftWideMemo:
      QDataField.AsWideString := tempValue;
  end;
  Result := True;
end;

function BuildAppendDataSet(Query: TDataSet; QApiData: TFastApiData; QApiAll: TApiAll): boolean;
var
  lApiField: TFastApiField;
  iField, iArr: integer;
  lDataFieldDict: TDictionary<string, TField>;
  lField: TField;
  lDataJsonArr: TJsonArray;
  tempJson: TJsonData;
  tempJsonObj: TJsonObject;
  isMustFreeArr: boolean;
begin
  Result := False;
  lDataJsonArr := nil;
  isMustFreeArr := False;
  lDataFieldDict := TDictionary<string, TField>.Create;
  try
    // 遍历数据集字段，信息先保存起来
    for iField := 0 to Query.Fields.Count - 1 do
    begin
      lField := Query.Fields[iField];
      lDataFieldDict.Add(lField.FieldName.ToLower, lField);
    end;

    // 校验字段,是否全部存在
    for iField := 0 to QApiData.ChildFields.Count - 1 do
    begin
      lApiField := QApiData.ChildFields[iField];
      // 是否参与,后面看要不要这个标识，一般存添加数据字段多是要保存的
      // if lApiField.FFieldProvidFlagUpdate then
      if not lDataFieldDict.ContainsKey(lApiField.FFieldName.ToLower) then
      begin
        QApiAll.errMsg := 'Api设置字段[' + lApiField.FFieldName + ']在表[' + QApiData.FDataTable + ']不存在';
        exit;
      end;
    end;
    // 添加数据
    if QApiAll.dataJson is TJsonArray then
    begin
      lDataJsonArr := TJsonArray(QApiAll.dataJson);
    end
    else if QApiAll.dataJson is TJsonObject then
    begin
      lDataJsonArr := TJsonArray.Create();
      // 这边需要自已释放
      isMustFreeArr := True;
      lDataJsonArr.Add(TJsonObject(QApiAll.dataJson));
    end;
    for iArr := 0 to lDataJsonArr.Count - 1 do
    begin
      tempJson := lDataJsonArr.Items[iArr];
      if not (tempJson is TJsonObject) then
      begin
        QApiAll.errMsg := '上传的ApiData里面的数据必需是Json对象{key:value}';
        exit;
      end;
      tempJsonObj := TJsonObject(tempJson);
      // 开始处理数据
      Query.Append;
      for iField := 0 to QApiData.ChildFields.Count - 1 do
      begin
        lApiField := QApiData.ChildFields[iField];
        lField := lDataFieldDict.Items[lApiField.FFieldName.ToLower];
        // 字段取值
        if not SetFieldValue(lField, lApiField, tempJsonObj, QApiAll) then
        begin
          exit;
        end;
      end;
      Query.Post;
    end;

    Result := True;
  finally
    lDataFieldDict.Clear;
    lDataFieldDict.Free;
    if isMustFreeArr then
    begin
      for iArr := lDataJsonArr.Count - 1 downto 0 do
      begin
        lDataJsonArr.Delete(iArr);
      end;
      lDataJsonArr.Free;
    end;
    lDataJsonArr := nil;
  end;
end;

end.
