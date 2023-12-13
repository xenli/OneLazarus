unit Admin_GroupInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  PGroupTreeData = ^TGroupTreeData;

  TGroupTreeData = record
    FGroupID,
    FGroupCaption,
    FGroupTreeCode: string;
  end;

  PRoleTreeData = ^TRoleTreeData;

  TRoleTreeData = record
    FRoleID,
    FRoleCaption,
    FRoleTreeCode: string;
  end;

implementation

end.
