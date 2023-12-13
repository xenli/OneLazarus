unit OneMonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMonitor = class(TMultiReadExclusiveWriteSynchronizer)

  public
    procedure Enter(aObject: TObject);
    procedure Exit(aObject: TObject);
  end;

implementation

procedure TMonitor.Enter(aObject: TObject);
begin
  Self.Beginread;
  Self.Beginwrite;
end;

procedure TMonitor.Exit(aObject: TObject);
begin
  Self.Endwrite;
  Self.Endread;
end;

end.
