unit OneAsynThread;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  evenAsynProcObj = procedure() of object;

  TOneAsynThread = class(TThread)
  private
    FMyProce: evenAsynProcObj;
  private
    procedure doProce();
  public
    class function CreateAnonymousThread(QProce: evenAsynProcObj): TOneAsynThread; static;
  public
    procedure Execute; override;
    constructor Create(CreateSuspended: boolean; QProce: evenAsynProcObj); overload;
  end;

implementation

{使用方法: TOneAsynThread.CreateAnonymousThread(QProce).start;}
class function TOneAsynThread.CreateAnonymousThread(QProce: evenAsynProcObj): TOneAsynThread;
begin
  Result := TOneAsynThread.Create(True, QProce);
end;

constructor TOneAsynThread.Create(CreateSuspended: boolean; QProce: evenAsynProcObj);
begin
  inherited Create(CreateSuspended);
  FMyProce := QProce;
end;

procedure TOneAsynThread.doProce();
begin
  FMyProce();
end;

procedure TOneAsynThread.Execute;
begin
  if Assigned(FMyProce) then
  begin
    TThread.Synchronize(nil, doProce);
  end;
end;

end.
