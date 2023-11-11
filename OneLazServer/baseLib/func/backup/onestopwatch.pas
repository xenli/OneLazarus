unit OneStopwatch;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TStopwatch = record
  private
    FStatTime: TDateTime;
    FEndTime: TDateTime;
  public
    class function StartNew: TStopwatch; static;
    procedure Stop;
    function ElapsedMilliseconds: int64;
  end;


implementation

class function TStopwatch.StartNew: TStopwatch;
begin
  Result.FStatTime := now;
end;

procedure TStopwatch.Stop;
begin
  FEndTime := now;
end;

function TStopwatch.ElapsedMilliseconds: int64;
begin
  MilliSecondsBetween(FStatTime,FEndTime);
end;

end.
