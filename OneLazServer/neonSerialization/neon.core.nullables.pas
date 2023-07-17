unit Neon.Core.Nullables;

{$mode DELPHI}{$H+}

interface

uses
  SysUtils, Variants, Classes, Generics.Defaults, Rtti, TypInfo;

type
  ENullableException = class(Exception);

  Nullable<T> = record
  private
    FValue: T;
    FHasValue: string;
    procedure Clear;
    function GetValueType: PTypeInfo;
    function GetValue: T;
    procedure SetValue(const AValue: T);
    function GetHasValue: boolean;
  public
    constructor Create(const Value: T);
    function Equals(const Value: Nullable<T>): boolean;
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(const Default: T): T; overload;

    property HasValue: boolean read GetHasValue;
    function IsNull: boolean;

    property Value: T read GetValue;
    class operator Implicit(const Value: Nullable<T>): T;
    //class operator Implicit(const Value: Nullable<T>): variant;
    //class operator Implicit(const Value: Pointer): Nullable<T>;
    //class operator Implicit(const Value: T): Nullable<T>;
    //class operator Implicit(const Value: variant): Nullable<T>;
    class operator Equal(const Left, Right: Nullable<T>): boolean;
    class operator NotEqual(const Left, Right: Nullable<T>): boolean;
  end;

  NullString = Nullable<string>;
  NullBoolean = Nullable<boolean>;
  NullInteger = Nullable<integer>;
  NullInt64 = Nullable<int64>;
  NullDouble = Nullable<double>;
  NullDateTime = Nullable<TDateTime>;

implementation

uses
  Neon.Core.Utils;

{ Nullable<T> }

constructor Nullable<T>.Create(const Value: T);
var
  a: TValue;
begin
  FValue := Value;
  FHasValue := 'true';
end;
procedure Nullable<T>.Clear;
begin
  FValue := Default(T);
  FHasValue := '';
end;

function Nullable<T>.Equals(const Value: Nullable<T>): boolean;
begin
  if HasValue and Value.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Self.Value, Value.Value)
  else
    Result := HasValue = Value.HasValue;
end;

function Nullable<T>.GetHasValue: boolean;
begin
  Result := FHasValue <> '';
end;

function Nullable<T>.GetValueType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    raise ENullableException.Create('Nullable type has no value');
  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault(const Default: T): T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  Result := GetValueOrDefault(Default(T));
end;

class operator Nullable<T>.Implicit(const Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

//class operator Nullable<T>.ImplicitA(const Value: Nullable<T>): variant;
//begin
//  if Value.HasValue then
//    Result := TValue.From<T>(Value.Value).AsVariant
//  else
//    Result := Null;
//end;

//class operator Nullable<T>.ImplicitB(const Value: Pointer): Nullable<T>;
//begin
//  if Value = nil then
//    Result.Clear
//  else
//    Result := Nullable<T>.Create(T(Value^));
//end;

//class operator Nullable<T>.ImplicitC(const Value: T): Nullable<T>;
//begin
//  Result := Nullable<T>.Create(Value);
//end;

//class operator Nullable<T>.ImplicitD(const Value: variant): Nullable<T>;
//begin
//  Result := Nullable<T>.Create(Value);
//end;

function Nullable<T>.IsNull: boolean;
begin
  Result := FHasValue = '';
end;

class operator Nullable<T>.Equal(const Left, Right: Nullable<T>): boolean;
begin
  Result := Left.Equals(Right);
end;

class operator Nullable<T>.NotEqual(const Left, Right: Nullable<T>): boolean;
begin
  Result := not Left.Equals(Right);
end;

procedure Nullable<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
  FHasValue := 'true';
end;

end.
