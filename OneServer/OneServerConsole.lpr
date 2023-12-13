program OneServerConsole;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
    {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  OneGlobal;

type

  { TOneApplication }

  TOneApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TOneApplication }

  procedure TOneApplication.DoRun;
  var
    lCmd: string;
    lOneGlobal: TOneGlobal;
    lErrMsg: string;
  begin
    writeln(Utf8DeCode('***********命令提示***********'));
    writeln(Utf8DeCode('输入命令[exit]退出'));
    writeln(Utf8DeCode('输入命令[start]启动服务'));
    writeln(Utf8DeCode('***********命令结束***********'));
    readln(lCmd);
    if lCmd = 'exit' then
    begin
      // stop program loop
      Terminate;
      exit;
    end;
    if lCmd = 'start' then
    begin
      lOneGlobal := TOneGlobal.GetInstance(True);
      if not lOneGlobal.StarWork(lErrMsg) then
      begin
        writeln(Utf8DeCode('***********启动服务异常***********'));
        writeln(Utf8DeCode(lErrMsg));
      end
      else
      begin
        writeln(Utf8DeCode('***********启动服务成功***********'));
        writeln(Utf8DeCode('当前启动端口:' +
          IntToStr(lOneGlobal.HttpServer.Port)));
      end;
    end;
  end;

  constructor TOneApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := False;
  end;

  destructor TOneApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TOneApplication.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TOneApplication;
begin
  Application := TOneApplication.Create(nil);
  Application.Title := 'One控制台';
  Application.Run;
  Application.Free;
end.
