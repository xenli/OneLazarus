unit OneOrm;

{$mode DELPHI}{$H+}


interface

uses
  Generics.Collections, Variants, SysUtils, Classes,
  OneZTManage, Rtti, DB, TypInfo;

type
  emOneOrmCmd = (cmdNull, cmdQuery, cmdExecSQL, cmdSelect, cmdInsert, cmdUpdate, cmdDelete);

  IOneOrmCmd<T: class> = interface
    ['{9609C654-2947-4F36-A978-78BF1DD03948}']
    function ToCount(): integer;
    function ToList(): TList<T>;
    function ToObject(): T;
    function ToExecCommand(): integer;
  end;

  IOneOrm<T: class> = interface
    function ZTCode(QZTCode: string): IOneOrm<T>;
    function SetTableName(QTableName: string): IOneOrm<T>;
    function SetPrimaryKey(QFieldName: string): IOneOrm<T>;
    function SetPage(iPageIndex: integer; iPageSize: integer): IOneOrm<T>;
    // 执行原生SQL,查询数据
    function Query(QSQL: string; QParams: array of variant): IOneOrmCmd<T>;
    // 执行原生SQL,进行update,insert,del
    function ExecSQL(QSQL: string; QParams: array of variant): IOneOrmCmd<T>;

    function Select(QTableName: string = ''): IOneOrm<T>;
    function Inserter(QValue: T): IOneOrm<T>; overload;
    function Update(QValue: T): IOneOrm<T>; overload;
    function Delete(QValue: T): IOneOrm<T>; overload;
    function Inserter(QValues: TList<T>): IOneOrm<T>; overload;
    function Update(QValues: TList<T>): IOneOrm<T>; overload;
    function Delete(QValues: TList<T>): IOneOrm<T>; overload;
    // 字段
    function Fields(QFields: array of string): IOneOrm<T>;
    function DisableFields(QFields: array of string): IOneOrm<T>;

    function where(QWhereSQL: string; QParams: array of variant): IOneOrm<T>;
    function OrderBy(QOrderBySQL: string): IOneOrm<T>;

    function toCmd(): IOneOrmCmd<T>;
  end;

  TOneOrm<T: class> = class(TInterfacedObject, IOneOrm<T>, IOneOrmCmd<T>)
  private
    FZTCode: string;
    FCmd: emOneOrmCmd;
    // 最终组装的SQL语句
    FCmdSQL: string;
    FCmdParams: array of variant;

    FTableName: string;
    FPrimaryKey: string;
    FPageIndex: integer;
    FPageSize: integer;
    // Query原生SQL用法
    FQuerySQL: string;
    FQueryParams: array of variant;
    // 查询的字段或update set字段
    FFields: array of string;
    FDisableFields: array of string;
    // update set value值
    FFieldValues: array of variant;
    // where 条件SQL
    FWhereSQLs: array of string;
    // where 条件参数值
    FWhereSQLParams: array of variant;

    FOrderBySQL: string;

    FListValue: TList<T>;

    FErrMsg: string;
  private
    function buildSQL(): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class function Start(): IOneOrm<T>; static;
  public

    function ZTCode(QZTCode: string): IOneOrm<T>;
    function SetTableName(QTableName: string): IOneOrm<T>;
    function SetPrimaryKey(QFieldName: string): IOneOrm<T>;
    function SetPage(iPageIndex: integer; iPageSize: integer): IOneOrm<T>;
    // 执行原生SQL
    function Query(QSQL: string; QParams: array of variant): IOneOrmCmd<T>;
    function ExecSQL(QSQL: string; QParams: array of variant): IOneOrmCmd<T>;
    // 执行SQL
    function Select(QTableName: string = ''): IOneOrm<T>;
    // 插入语句，插入单个
    function Inserter(QValue: T): IOneOrm<T>; overload;
    // 更新语句,更新单个
    function Update(QValue: T): IOneOrm<T>; overload;
    // 删除语句,删除单个
    function Delete(QValue: T): IOneOrm<T>; overload;
    // 插入语句,批量插入
    function Inserter(QValues: TList<T>): IOneOrm<T>; overload;
    // 更新语句,批量更新
    function Update(QValues: TList<T>): IOneOrm<T>; overload;
    // 删除语句,批量删除
    function Delete(QValues: TList<T>): IOneOrm<T>; overload;
    // 字段
    function Fields(QFields: array of string): IOneOrm<T>;
    function DisableFields(QFields: array of string): IOneOrm<T>;

    function where(QWhereSQL: string; QParams: array of variant): IOneOrm<T>;
    function OrderBy(QOrderBySQL: string): IOneOrm<T>;

    function toCmd(): IOneOrmCmd<T>;

    function ToCount(): integer;
    function ToList(): TList<T>;
    function ToObject(): T;
    function ToExecCommand(): integer;

    function DataSetToList(QDataSet: TFDMemtable): TList<T>;
  end;

var
  // OneGlobal会初使化赋值
  unit_OrmZTManage: TOneZTManage = nil;

implementation

uses OneDataInfo, OneOrmRtti;

class function TOneOrm<T>.Start(): IOneOrm<T>;
begin
  Result := TOneOrm<T>.Create;
end;

constructor TOneOrm<T>.Create;
begin
  inherited Create;
  self.FPageIndex := -1;
  self.FPageSize := -1;
  self.FCmd := emOneOrmCmd.cmdNull;
  self.FListValue := TList<T>.Create;
end;

destructor TOneOrm<T>.Destroy;
begin
  self.FListValue.Clear;
  self.FListValue.Free;
  inherited Destroy;
end;

function TOneOrm<T>.ZTCode(QZTCode: string): IOneOrm<T>;
begin
  Result := self;
  self.FZTCode := QZTCode;
end;

function TOneOrm<T>.SetTableName(QTableName: string): IOneOrm<T>;
begin
  Result := self;
  self.FTableName := QTableName;
end;

function TOneOrm<T>.SetPrimaryKey(QFieldName: string): IOneOrm<T>;
begin
  Result := self;
  self.FPrimaryKey := QFieldName;
end;

function TOneOrm<T>.SetPage(iPageIndex: integer; iPageSize: integer): IOneOrm<T>;
begin
  Result := self;
  self.FPageIndex := iPageIndex;
  self.FPageSize := iPageSize;
end;

function TOneOrm<T>.Query(QSQL: string; QParams: array of variant): IOneOrmCmd<T>;
var
  iParam, iParamLen: integer;
begin
  Result := self;
  // 说明有其它命令，在语法就是错了
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdQuery]');
    exit;
  end;

  self.FCmd := emOneOrmCmd.cmdQuery;
  self.FQuerySQL := QSQL;
  iParamLen := length(QParams);
  setLength(self.FQueryParams, iParamLen);
  for iParam := 0 to iParamLen - 1 do
  begin
    self.FQueryParams[iParam] := QParams[iParam];
  end;
end;

function TOneOrm<T>.ExecSQL(QSQL: string; QParams: array of variant): IOneOrmCmd<T>;
var
  iParam, iParamLen: integer;
begin
  Result := self;
  // 说明有其它命令，在语法就是错了
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdExecSQL]');
    exit;
  end;

  self.FCmd := emOneOrmCmd.cmdExecSQL;
  self.FQuerySQL := QSQL;
  iParamLen := length(QParams);
  setLength(self.FQueryParams, iParamLen);
  for iParam := 0 to iParamLen - 1 do
  begin
    self.FQueryParams[iParam] := QParams[iParam];
  end;
end;

function TOneOrm<T>.Select(QTableName: string = ''): IOneOrm<T>;
begin
  Result := self;
  // 说明有其它命令，在语法就是错了
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdSelect]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdSelect;
  self.FTableName := QTableName;
end;

function TOneOrm<T>.Inserter(QValue: T): IOneOrm<T>;
begin
  Result := self;
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdInsert]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdInsert;
  self.FListValue.Add(QValue);
end;

function TOneOrm<T>.Update(QValue: T): IOneOrm<T>;
begin
  Result := self;
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdUpdate]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdUpdate;
  self.FListValue.Add(QValue);
end;

function TOneOrm<T>.Delete(QValue: T): IOneOrm<T>;
begin
  Result := self;
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdDelete]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdDelete;
  self.FListValue.Add(QValue);
end;

function TOneOrm<T>.Inserter(QValues: TList<T>): IOneOrm<T>;
var
  i: integer;
begin
  Result := self;
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdInsert]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdInsert;
  for i := 0 to QValues.Count - 1 do
  begin
    self.FListValue.Add(QValues[i]);
  end;
end;

function TOneOrm<T>.Update(QValues: TList<T>): IOneOrm<T>;
var
  i: integer;
begin
  Result := self;
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdUpdate]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdUpdate;
  for i := 0 to QValues.Count - 1 do
  begin
    self.FListValue.Add(QValues[i]);
  end;
end;

function TOneOrm<T>.Delete(QValues: TList<T>): IOneOrm<T>;
var
  i: integer;
begin
  Result := self;
  if self.FCmd <> emOneOrmCmd.cmdNull then
  begin
    raise Exception.Create('已设置命令模式,不可在设置命令模式[cmdDelete]');
    exit;
  end;
  self.FCmd := emOneOrmCmd.cmdDelete;
  for i := 0 to QValues.Count - 1 do
  begin
    self.FListValue.Add(QValues[i]);
  end;
end;

function TOneOrm<T>.Fields(QFields: array of string): IOneOrm<T>;
var
  i, iSourceLen, iLen: integer;
begin
  Result := self;
  iSourceLen := length(self.FFields);
  iLen := length(QFields);
  setLength(self.FFields, iSourceLen + iLen);
  for i := 0 to iLen - 1 do
  begin
    self.FFields[i + iSourceLen] := QFields[i];
  end;
end;

function TOneOrm<T>.DisableFields(QFields: array of string): IOneOrm<T>;
var
  i, iSourceLen, iLen: integer;
begin
  Result := self;
  iSourceLen := length(self.FDisableFields);
  iLen := length(QFields);
  setLength(self.FFields, iSourceLen + iLen);
  for i := 0 to iLen - 1 do
  begin
    self.FDisableFields[i + iSourceLen] := QFields[i];
  end;
end;
// function TOneOrm<T>.SetField(QFileName: string; QValue: Variant): IOneOrm<T>;
// var
// i, iLen, iValueLen: integer;
// begin
// iLen := length(self.FFields) + 1;
// setLength(self.FFields, iLen);
// self.FFields[iLen - 1] := QFileName;
// //
// iValueLen := length(self.FFieldValues) + 1;
// setLength(self.FFieldValues, iValueLen);
// self.FFieldValues[iValueLen - 1] := QValue;
// end;

function TOneOrm<T>.where(QWhereSQL: string; QParams: array of variant): IOneOrm<T>;
var
  iParam, iSourceWhereLen, iSourceParamLen, iParamLen: integer;
begin
  Result := self;
  iSourceWhereLen := length(FWhereSQLs);
  setLength(FWhereSQLs, iSourceWhereLen + 1);
  FWhereSQLs[iSourceWhereLen] := QWhereSQL;

  iParamLen := length(QParams);
  if iParamLen = 0 then
    exit;
  iSourceParamLen := length(FWhereSQLParams);
  setLength(FWhereSQLParams, iSourceParamLen + iParamLen);
  for iParam := 0 to iParamLen - 1 do
  begin
    FWhereSQLParams[iParam + iSourceParamLen] := QParams[iParam];
  end;
end;

function TOneOrm<T>.OrderBy(QOrderBySQL: string): IOneOrm<T>;
begin
  Result := self;
  FOrderBySQL := QOrderBySQL;
end;

function TOneOrm<T>.toCmd(): IOneOrmCmd<T>;
begin
  Result := self;
end;

function TOneOrm<T>.ToCount(): integer;
var
  LParams: TList<variant>;
  iParam, iParamLen: integer;
begin
  if (self.FCmd <> emOneOrmCmd.cmdQuery) and (self.FCmd <> emOneOrmCmd.cmdSelect) then
  begin
    raise Exception.Create('只有查询才能统计总条数');
    exit;
  end;
end;

function TOneOrm<T>.ToList(): TList<T>;
var
  lDataOpen: TOneDataOpen;
  lErrMsg: string;
  lDataSet: TFDMemtable;
begin
  Result := nil;
  if unit_OrmZTManage = nil then
  begin
    raise Exception.Create('orm账套未初始化[unit_OrmZTManage]');
    exit;
  end;
  if (self.FCmd <> emOneOrmCmd.cmdQuery) and (self.FCmd <> emOneOrmCmd.cmdSelect) then
  begin
    raise Exception.Create('只有查询语句才能转化成对象列表');
    exit;
  end;
  if not self.buildSQL() then
  begin
    raise Exception.Create('组装SQL语句异常,原因:' + self.FErrMsg);
    exit;
  end;
  // 执行SQL获得数据集
  lDataSet := nil;
  lDataOpen := TOneDataOpen.Create;
  try
    lDataOpen.OpenSQL := self.FCmdSQL;
    lDataOpen.ZTCode := self.FZTCode;
    lDataOpen.PageIndex := self.FPageIndex;
    lDataOpen.PageSize := self.FPageSize;
    lDataSet := unit_OrmZTManage.OpenData(lDataOpen, self.FCmdParams, lErrMsg);
    if lDataSet = nil then
    begin
      raise Exception.Create(lErrMsg);
      exit;
    end;
    // 把数据集转成List
    Result := self.DataSetToList(lDataSet)
  finally
    lDataOpen.Free;
    if lDataSet <> nil then
      lDataSet.Free;
  end;
end;

function TOneOrm<T>.ToObject(): T;
var
  lDataOpen: TOneDataOpen;
  lErrMsg: string;
  lDataSet: TFDMemtable;
  lList: TList<T>;
begin
  Result := nil;
  if unit_OrmZTManage = nil then
  begin
    raise Exception.Create('orm账套未初始化[unit_OrmZTManage]');
    exit;
  end;
  if (self.FCmd <> emOneOrmCmd.cmdQuery) and (self.FCmd <> emOneOrmCmd.cmdSelect) then
  begin
    raise Exception.Create('只有查询语句才能转化成对象列表');
    exit;
  end;
  if not self.buildSQL() then
  begin
    raise Exception.Create('组装SQL语句异常,原因:' + self.FErrMsg);
    exit;
  end;
  // 执行SQL获得数据集
  lDataSet := nil;
  lDataOpen := TOneDataOpen.Create;
  try
    lDataOpen.OpenSQL := self.FCmdSQL;
    lDataOpen.ZTCode := self.FZTCode;
    lDataOpen.PageIndex := self.FPageIndex;
    lDataOpen.PageSize := self.FPageSize;
    lDataSet := unit_OrmZTManage.OpenData(lDataOpen, self.FCmdParams, lErrMsg);
    if lDataSet = nil then
    begin
      raise Exception.Create(lErrMsg);
      exit;
    end;
    if lDataSet.RecordCount = 0 then
    begin
      exit;
    end;
    if lDataSet.RecordCount > 1 then
    begin
      raise Exception.Create('返回的数据记录不是唯一的，请检查');
      exit;
    end;
    // 把数据集转成List
    lList := TList<T>.Create;
    try
      lList := self.DataSetToList(lDataSet);
      Result := lList[0];
    finally
      lList.Clear;
      lList.Free;
    end;

  finally
    lDataOpen.Free;
    if lDataSet <> nil then
      lDataSet.Free;
  end;
end;

function TOneOrm<T>.ToExecCommand(): integer;
var
  lOrmRtti: IOrmRtti;
  lOrmRttiItem: TOneOrmRttiItem;
  lDictFieldRtti: TDictionary<string, TOneFieldRtti>;
  lFieldRtti: TOneFieldRtti;
  lDataSaveDML: TOneDataSaveDML;
  lFieldName, lErrMsg: string;
  lZTItem: TOneZTItem;
  lQuery: TFDQuery;
  iArrValue, iParam, iField: integer;
  lValue: T;
  iCommit: integer;
  isCommit: boolean;
  lTValue: TValue;
begin

  Result := -1;
  lErrMsg := '';
  if unit_OrmZTManage = nil then
  begin
    raise Exception.Create('orm账套未初始化[unit_OrmZTManage]');
    exit;
  end;
  if (self.FCmd = emOneOrmCmd.cmdQuery) or (self.FCmd = emOneOrmCmd.cmdSelect) then
  begin
    raise Exception.Create('cmdSelect命令只支持查询');
    exit;
  end;
  if not self.buildSQL() then
  begin
    raise Exception.Create('组装SQL语句异常,原因:' + self.FErrMsg);
    exit;
  end;

  case self.FCmd of
    cmdExecSQL:
    begin
      lDataSaveDML := TOneDataSaveDML.Create;
      try
        lDataSaveDML.ZTCode := self.FZTCode;
        lDataSaveDML.SQL := self.FCmdSQL;
        Result := unit_OrmZTManage.ExecSQL(lDataSaveDML, self.FCmdParams, lErrMsg);
        if lErrMsg <> 'true' then
        begin
          raise Exception.Create(lErrMsg);
          exit;
        end;
      finally
        lDataSaveDML.Free;
      end;
    end;
    cmdInsert, cmdUpdate, cmdDelete:
    begin
      lDictFieldRtti := TDictionary<string, TOneFieldRtti>.Create;
      try
        // 获取Rtti信息
        lOrmRtti := TOneOrmRtti.GetInstance();
        lOrmRttiItem := lOrmRtti.GetOrmRtti(system.TypeInfo(T));
        for iField := 0 to lOrmRttiItem.Fields.Count - 1 do
        begin
          lFieldRtti := lOrmRttiItem.Fields[iField];
          lDictFieldRtti.Add(lFieldRtti.FFieldName, lFieldRtti);
        end;

        lZTItem := unit_OrmZTManage.LockZTItem(self.FZTCode, lErrMsg);
        if lZTItem = nil then
        begin
          raise Exception.Create(lErrMsg);
          exit;
        end;
        isCommit := False;
        iCommit := 0;
        lZTItem.ADTransaction.TranStart;
        try
          try
            lQuery := lZTItem.ADQuery;
            lQuery.SQL.Text := self.FCmdSQL;

            if (self.FCmd = cmdUpdate) or (self.FCmd = cmdInsert) then
            begin
              for iArrValue := 0 to self.FListValue.Count - 1 do
              begin
                lValue := self.FListValue[iArrValue];
                for iField := 0 to length(self.FFields) - 1 do
                begin
                  // 最新主键是放在 FFields 最后一个 在buildSQL处理好了
                  lFieldName := self.FFields[iField];
                  if lDictFieldRtti.TryGetValue(lFieldName, lFieldRtti) then
                  begin
                    if lFieldRtti.FPropertyRtti <> nil then
                    begin
                      lTValue := lFieldRtti.FPropertyRtti.GetValue(TObject(lValue));
                    end;
                    //lQuery.Params[iField].Value := lTValue.as ;
                  end
                  else
                    lQuery.Params[iField].Clear();
                end;
                // 遍历更新
                lQuery.ExecSQL();
                iCommit := lQuery.RowsAffected;
                if iCommit <> 1 then
                begin
                  raise Exception.Create('第[' + (iArrValue + 1).ToString() + ']条数据,更新失败, 当前影响行数[' + iCommit.ToString + ']');
                  exit;
                end;
              end;
            end;
            // 删除只跟据主键来
            if (self.FCmd = cmdDelete) then
            begin
              lFieldName := self.FPrimaryKey;
              for iArrValue := 0 to self.FListValue.Count - 1 do
              begin
                if lDictFieldRtti.TryGetValue(lFieldName, lFieldRtti) then
                begin
                  lValue := self.FListValue[iArrValue];
                  if lFieldRtti.FPropertyRtti <> nil then
                  begin
                    lTValue := lFieldRtti.FPropertyRtti.GetValue(TObject(lValue));
                  end;
                  //lQuery.Params[0].Value := lTValue.AsVariant;
                end
                else
                begin
                  lQuery.Params[0].Clear();
                end;
                // 遍历更新
                lQuery.ExecSQL();
                iCommit := lQuery.RowsAffected;
                if iCommit <> 1 then
                begin
                  raise Exception.Create('第[' + (iArrValue + 1).ToString() + ']条数据,删除失败,当前影响行数[' + iCommit.ToString + ']');
                  exit;
                end;
              end;
            end;
            lZTItem.ADTransaction.TranCommit;
            isCommit := True;
            Result := self.FListValue.Count;
          except
            on e: Exception do
            begin
              raise Exception.Create('提交数据发生异常:' + e.Message);
              isCommit := False;
            end;
          end;
        finally
          if not isCommit then
          begin
            lZTItem.ADTransaction.TranRollback;
          end;
          lZTItem.UnLockWork;
        end;
      finally
        lDictFieldRtti.Clear;
        lDictFieldRtti.Free;
      end;
    end;
    else
    begin
      raise Exception.Create('未设计的cmd命令' + GetEnumName(system.TypeInfo(emOneOrmCmd), Ord(self.FCmd)));
      exit;
    end;
  end;
end;

function TOneOrm<T>.buildSQL(): boolean;
var
  LParams: TList<variant>;
  iField, iFieldLen, iParam, iParamLen: integer;
  lWhere: string;
  iWhere, iWhereLen: integer;
  lSQL: string;

  lOrmRtti: IOrmRtti;
  lOrmRttiItem: TOneOrmRttiItem;
  lFieldRtti: TOneFieldRtti;
  lDBFields, lValueFields, lDBFieldName, lDBPrimaryField: string;
  lList: TList<string>;
  lDict: TDictionary<string, boolean>;
  lDictFieldRtti: TDictionary<string, TOneFieldRtti>;
begin
  Result := False;
  self.FCmdSQL := '';
  setLength(self.FCmdParams, 0);
  self.FErrMsg := '';
  // 开始组装SQL
  LParams := TList<variant>.Create;
  try
    self.FCmdSQL := '';
    if self.FCmd = emOneOrmCmd.cmdQuery then
    begin
      self.FCmdSQL := self.FQuerySQL;
      iParamLen := length(self.FQueryParams);
      for iParam := 0 to iParamLen - 1 do
      begin
        LParams.Add(self.FQueryParams[iParam]);
      end;
    end
    else if self.FCmd = emOneOrmCmd.cmdExecSQL then
    begin
      self.FCmdSQL := self.FQuerySQL;
      iParamLen := length(self.FQueryParams);
      for iParam := 0 to iParamLen - 1 do
      begin
        LParams.Add(self.FQueryParams[iParam]);
      end;
    end
    else if self.FCmd = emOneOrmCmd.cmdSelect then
    begin
      lSQL := ' select ';
      if self.FTableName = '' then
      begin
        // 通过返身获取结构名称当表名
        self.FErrMsg := '未设置查询表名';
        exit;
      end;
      iFieldLen := length(self.FFields);
      if iFieldLen = 0 then
      begin
        lSQL := lSQL + ' * from ' + self.FTableName;
      end
      else
      begin

        for iField := 0 to iFieldLen - 1 do
        begin
          lSQL := lSQL + ' ' + self.FFields[iField] + ' ';
          if iField < iFieldLen - 1 then
          begin
            lSQL := lSQL + ' , ';
          end;
        end;
        lSQL := lSQL + ' from ' + self.FTableName;
      end;
      // 组装 where条件
      // FWhereSQLs,FWhereSQLParams
      lSQL := lSQL + ' where 1=1 ';
      iWhereLen := length(self.FWhereSQLs);
      for iWhere := 0 to iWhereLen - 1 do
      begin

        lWhere := self.FWhereSQLs[iWhere];
        lWhere := lWhere.Trim; // 去掉两边空格
        if lWhere.StartsWith('where ') then
        begin
          // 去掉where
          lWhere := lWhere.Substring(5);
          lSQL := lSQL + ' and ' + lWhere;
        end
        else if (not lWhere.StartsWith('and ')) and (not lWhere.StartsWith('or ')) then
        begin
          // 自动加个 and
          lSQL := lSQL + ' and ' + lWhere;
        end
        else
        begin
          lSQL := lSQL + ' ' + lWhere;
        end;
      end;
      // order by 处理
      if self.FOrderBySQL <> '' then
      begin
        self.FOrderBySQL := self.FOrderBySQL.Trim;
        if self.FOrderBySQL.StartsWith('order ') then
        begin
          lSQL := lSQL + ' ' + self.FOrderBySQL;
        end
        else
        begin
          lSQL := lSQL + ' order by ' + self.FOrderBySQL;
        end;
      end;
      iParamLen := length(self.FWhereSQLParams);
      for iParam := 0 to iParamLen - 1 do
      begin
        LParams.Add(self.FWhereSQLParams[iParam]);
      end;
      self.FCmdSQL := lSQL;
    end
    else if (self.FCmd = emOneOrmCmd.cmdInsert) or (self.FCmd = emOneOrmCmd.cmdUpdate) or (self.FCmd = emOneOrmCmd.cmdDelete) then
    begin
      lDBPrimaryField := '';
      // 组装字段
      lList := TList<string>.Create;
      lDict := TDictionary<string, boolean>.Create;
      lDictFieldRtti := TDictionary<string, TOneFieldRtti>.Create;
      try
        lOrmRtti := TOneOrmRtti.GetInstance();
        lOrmRttiItem := lOrmRtti.GetOrmRtti(system.TypeInfo(T));
        if self.FTableName = '' then
        begin
          self.FTableName := lOrmRttiItem.OrmName;
        end;
        if self.FPrimaryKey = '' then
        begin
          self.FPrimaryKey := lOrmRttiItem.PrimaryKey;
        end;
        if self.FPrimaryKey = '' then
        begin
          self.FErrMsg := '未设置实体主键';
          exit;
        end;
        for iField := 0 to lOrmRttiItem.Fields.Count - 1 do
        begin
          lFieldRtti := lOrmRttiItem.Fields[iField];
          lList.Add(lFieldRtti.FFieldName);
          lDictFieldRtti.Add(lFieldRtti.FFieldName, lFieldRtti);
          if self.FPrimaryKey.ToLower = lFieldRtti.FFieldName.ToLower then
          begin
            self.FPrimaryKey := lFieldRtti.FFieldName;
            lDBPrimaryField := lFieldRtti.FDBFieldName;
          end;
        end;

        if lDBPrimaryField = '' then
        begin
          self.FErrMsg := '实体主键列在实体属性字段找不到';
          exit;
        end;


        if length(self.FDisableFields) > 0 then
        begin
          lDict.Clear;
          for iField := 0 to length(self.FDisableFields) - 1 do
          begin
            lDict.Add(self.FDisableFields[iField].ToLower, True);
          end;
          for iField := lList.Count - 1 downto 0 do
          begin
            // 去掉不要的
            if lDict.ContainsKey(lList[iField].ToLower) then
            begin
              // 存在去掉
              lList.Delete(iField);
            end;
          end;
        end;
        if length(self.FFields) > 0 then
        begin
          // 指定字段
          lDict.Clear;
          for iField := 0 to length(self.FFields) - 1 do
          begin
            lDict.Add(self.FFields[iField].ToLower, True);
          end;
          for iField := lList.Count - 1 downto 0 do
          begin
            if not lDict.ContainsKey(lList[iField].ToLower) then
            begin
              // 不存在,去掉
              lList.Delete(iField);
            end;
          end;
        end;
        if lList.Count = 0 then
        begin
          self.FErrMsg := '无任何字段参与';
          exit;
        end;

        if (self.FCmd = emOneOrmCmd.cmdInsert) then
        begin
          self.FCmdSQL := ' insert into ' + self.FTableName + ' ';
          lDBFields := '';
          lValueFields := '';
          for iField := 0 to lList.Count - 1 do
          begin
            // 可能DB字段不一样
            lDBFieldName := lList[iField];
            if lDictFieldRtti.TryGetValue(lDBFieldName, lFieldRtti) then
            begin
              lDBFieldName := lFieldRtti.FDBFieldName;
            end;
            if iField = 0 then
            begin
              lDBFields := lDBFieldName;
              lValueFields := ':' + lDBFieldName;
            end
            else
            begin
              lDBFields := lDBFields + ' , ' + lDBFieldName;
              lValueFields := lValueFields + ' , ' + ':' + lDBFieldName;
            end;
          end;
          self.FCmdSQL := self.FCmdSQL + ' ( ' + lDBFields + ' ) values ( ' + lValueFields + ' ) ';
        end;
        if (self.FCmd = emOneOrmCmd.cmdUpdate) then
        begin
          // 更新去掉主键,不参与更新只参与条件
          lList.Remove(self.FPrimaryKey);
          self.FCmdSQL := 'update ' + self.FTableName + ' ';
          lDBFields := '';
          lValueFields := '';
          for iField := 0 to lList.Count - 1 do
          begin
            // 可能DB字段不一样
            lDBFieldName := lList[iField];
            if lDictFieldRtti.TryGetValue(lDBFieldName, lFieldRtti) then
            begin
              lDBFieldName := lFieldRtti.FDBFieldName;
            end;
            if iField = 0 then
            begin
              lDBFields := lDBFieldName + ' =:' + lDBFieldName;
            end
            else
            begin
              lDBFields := lDBFields + ' , ' + lDBFieldName + ' =:' + lDBFieldName;
            end;
          end;
          self.FCmdSQL := self.FCmdSQL + ' set ' + lDBFields;
          // 加上主键更新
          self.FCmdSQL := self.FCmdSQL + ' where ' + lDBPrimaryField + ' =:' + lDBPrimaryField;
          // 更新主键放在最后面
          lList.Add(self.FPrimaryKey);
        end;
        if (self.FCmd = emOneOrmCmd.cmdDelete) then
        begin
          self.FCmdSQL := 'delete from ' + self.FTableName + ' ';
          self.FCmdSQL := self.FCmdSQL + ' where ' + lDBPrimaryField + ' =:' + lDBPrimaryField;
        end;

        setLength(self.FFields, lList.Count);
        for iField := 0 to lList.Count - 1 do
        begin
          self.FFields[iField] := lList[iField];
        end;

      finally
        lList.Clear;
        lList.Free;
        lDict.Clear;
        lDict.Free;
        lDictFieldRtti.Clear;
        lDictFieldRtti.Free;
      end;
    end;


    if self.FCmdSQL.Trim = '' then
    begin
      self.FErrMsg := 'SQL语句为空';
      exit;
    end;
    setLength(self.FCmdParams, LParams.Count);
    for iParam := 0 to LParams.Count - 1 do
    begin
      self.FCmdParams[iParam] := LParams[iParam];
    end;
    // 执行SQL语句
    Result := True;
  finally
    LParams.Clear;
    LParams.Free;
  end;

end;

function TOneOrm<T>.DataSetToList(QDataSet: TFDMemtable): TList<T>;
var
  lOrmRtti: IOrmRtti;
  lOrmRttiItem: TOneOrmRttiItem;
  i: integer;
  lOrmFieldRttis: TList<TOneFieldRtti>;
  lDataFields: TDictionary<string, TField>;
  lOneFieldRtti: TOneFieldRtti;
  //lRttiField: TRttiField;
  lRttiProperty: TRttiProperty;
  lTypeKind: TTypeKind;
  lField: TField;
  lFieldNameLow: string;
  lTempT: T;
  tempStr: string;
  tempI: integer;
begin
  Result := TList<T>.Create;
  // system.TypeInfo(T)
  lDataFields := TDictionary<string, TField>.Create;
  lOrmFieldRttis := TList<TOneFieldRtti>.Create;
  try
    // 字段
    for i := 0 to QDataSet.Fields.Count - 1 do
    begin
      lField := QDataSet.Fields[i];
      lDataFields.Add(lField.FieldName.ToLower, lField);
    end;
    lOrmRtti := TOneOrmRtti.GetInstance();
    lOrmRttiItem := lOrmRtti.GetOrmRtti(system.TypeInfo(T));
    // orm字段
    for i := 0 to lOrmRttiItem.Fields.Count - 1 do
    begin
      // 极大缩减没用的字段
      lOneFieldRtti := lOrmRttiItem.Fields[i];
      lFieldNameLow := lOneFieldRtti.FDBFieldNameLow;
      if lDataFields.ContainsKey(lFieldNameLow) then
      begin
        lOrmFieldRttis.Add(lOrmRttiItem.Fields[i]);
      end;
    end;
    QDataSet.First;
    while not QDataSet.EOF do
    begin
      lTempT := T.Create;
      Result.Add(lTempT);
      // for  lRttiType.GetFields do
      for i := 0 to lOrmFieldRttis.Count - 1 do
      begin
        lOneFieldRtti := lOrmFieldRttis[i];
        // 这边要考虑很多的,比如不同类型的字段转化
        // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
        // tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
        // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString,
        // tkClassRef, tkPointer, tkProcedure, tkMRecord
        //lRttiField := lOneFieldRtti.FFieldRtti;
        lRttiProperty := lOneFieldRtti.FPropertyRtti;
        lFieldNameLow := lOneFieldRtti.FDBFieldNameLow;
        if lDataFields.TryGetValue(lFieldNameLow, lField) then
        begin
          if lRttiProperty <> nil then
          begin
            if not lRttiProperty.IsWritable then
              continue;
            lTypeKind := lRttiProperty.PropertyType.TypeKind;
          end;
          case lTypeKind of
            tkString, tkAString, tkChar, tkLString, tkUChar, tkUString:
            begin
              case lField.DataType of
                ftString:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsString);
                end;
                ftBlob, ftGraphic, ftTypedBinary:
                begin
                  //tempStr := TNetEncoding.Base64.EncodeBytesToString(lField.AsBytes);
                  //if lOneFieldRtti.FIsProperty then
                  //  lRttiProperty.SetValue(TObject(lTempT), tempStr)
                  //else
                  //  lRttiField.SetValue(TObject(lTempT), tempStr);
                end;
                else
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsString);
                end;
              end;
            end;
            tkWString:
            begin
              if lOneFieldRtti.FIsProperty then
                lRttiProperty.SetValue(TObject(lTempT), lField.AsWideString);
            end;
            tkInteger:
            begin
              case lField.DataType of
                ftSmallint, ftInteger, ftWord, ftAutoInc:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsInteger);
                end;
                ftLargeint:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsLargeInt);
                end;
                else
                begin

                end;
              end;
            end;
            tkInt64:
            begin
              case lField.DataType of
                ftLargeint:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsLargeInt);
                end;
                ftSmallint, ftInteger, ftWord, ftAutoInc:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsInteger);
                end;
                else
                begin

                end;
              end;
            end;
            tkFloat:
            begin
              case lField.DataType of
                ftFloat, ftBCD, ftFMTBcd:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsFloat);
                end;
                ftLargeint:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsLargeInt);
                end;
                ftSmallint, ftInteger, ftWord, ftAutoInc:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsInteger);
                end;
                ftTimeStamp:
                begin
                  if lOneFieldRtti.FIsProperty then
                    lRttiProperty.SetValue(TObject(lTempT), lField.AsDateTime);
                end;
                else
                begin

                end;
              end;
            end;
            tkEnumeration:
            begin
              if lOneFieldRtti.FIsBool then
              begin
                case lField.DataType of
                  ftBoolean:
                  begin
                    if lOneFieldRtti.FIsProperty then
                      lRttiProperty.SetValue(TObject(lTempT), lField.AsBoolean);
                  end;
                  ftString:
                  begin
                    if lField.AsString.ToLower = 'true' then
                    begin
                      if lOneFieldRtti.FIsProperty then
                        lRttiProperty.SetValue(TObject(lTempT), True);
                    end
                    else
                    begin
                      if lOneFieldRtti.FIsProperty then
                        lRttiProperty.SetValue(TObject(lTempT), False);
                    end;
                  end;
                  ftSmallint, ftInteger, ftWord, ftAutoInc:
                  begin
                    if lField.AsInteger = 1 then
                    begin
                      if lOneFieldRtti.FIsProperty then
                        lRttiProperty.SetValue(TObject(lTempT), True);
                    end
                    else
                    begin
                      if lOneFieldRtti.FIsProperty then
                        lRttiProperty.SetValue(TObject(lTempT), False);
                    end;
                  end;
                end;
              end
              else
              begin
                // 枚举型
                case lField.DataType of
                  ftSmallint, ftInteger:
                  begin
                    if lOneFieldRtti.FIsProperty then
                      lRttiProperty.SetValue(TObject(lTempT), lField.AsInteger);
                  end;
                  ftString:
                  begin
                    if lOneFieldRtti.FIsProperty then
                      lRttiProperty.SetValue(TObject(lTempT), GetEnumValue(lRttiProperty.PropertyType.Handle, lField.AsString));
                  end;
                end;
              end;
            end;
            tkVariant:
            begin
              if lOneFieldRtti.FIsProperty then
                lRttiProperty.SetValue(TObject(lTempT), VarToStr(lField.AsVariant));
            end;
          end;
        end;
      end;
      QDataSet.Next;
    end;
  finally
    lDataFields.Clear;
    lDataFields.Free;
    lOrmFieldRttis.Clear;
    lOrmFieldRttis.Free;
  end;
end;

end.
