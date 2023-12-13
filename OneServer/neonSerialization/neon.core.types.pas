unit Neon.Core.Types;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Rtti;

type
  ENeonException = class(Exception);

type
  TNeonCase = (LowerCase, UpperCase, PascalCase, CamelCase, SnakeCase, CustomCase);
  TNeonMemberType = (Unknown, Prop, Field, Indexed);
  TNeonMembers = (Standard, Fields, Properties);
  TNeonMembersSet = set of TNeonMembers;
  TNeonVisibility = set of TMemberVisibility;
  TNeonIncludeOption = (Default, Include, Exclude);
  TNeonOperation = (Serialize, Deserialize);

  TNeonIgnoreIfContext = record
  public
    MemberName: string;
    Operation: TNeonOperation;
    constructor Create(const AMemberName: string; AOperation: TNeonOperation);
  end;

type
  TNeonIgnoreCallback = function(const AContext: TNeonIgnoreIfContext): boolean of
    object;
  TCaseFunc = function(const AString: string): string;

implementation

{ TNeonIgnoreIfContext }

constructor TNeonIgnoreIfContext.Create(const AMemberName: string;
  AOperation: TNeonOperation);
begin
  MemberName := AMemberName;
  Operation := AOperation;
end;

end.
