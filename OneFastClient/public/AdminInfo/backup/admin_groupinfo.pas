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

implementation

end.
