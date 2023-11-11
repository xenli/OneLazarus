unit OneThread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TOneTimerThread = class;
  TOneSingleWorkThread = class;
  EventOneWorkSend = procedure(send: TObject) of object;
  EventOneWork = procedure();

  // 定时器，多久干一次活,固定间隔
  TOneTimerThread = class(TThread)
  private
    FCreateID: string;
    FbWork: boolean;
    FEventWorkSend: EventOneWorkSend;
    FEventWork: EventOneWork;
    FEven: TEventObject;
    FIntervalSec: integer;
  private
    procedure DoOnThreadSuspend;
    procedure Execute; override;
  public
    constructor Create(QEventWorkSend: EventOneWorkSend = nil;
      QEventOneWork: EventOneWork = nil);
    destructor Destroy; override;
    procedure StartWork;
    procedure StopWork;
    procedure FreeWork;
  public
    property IntervalSec: integer read FIntervalSec write FIntervalSec;
    property bWork: boolean read FbWork;
  end;

  // 有信号就干活,
  // 并发信号，在干活期间,信号是没用的,在未干活只要接收到信号就干活
  TOneSingleWorkThread = class(TThread)
  private
    FCreateID: string;
    FbWork: boolean;
    FEventWorkSend: EventOneWorkSend;
    FEventWork: EventOneWork;
    FEven: TEventObject;
  private
    procedure DoOnThreadSuspend;
    procedure Execute; override;
  public
    constructor Create(QEventWorkSend: EventOneWorkSend = nil;
      QEventOneWork: EventOneWork = nil);
    destructor Destroy; override;
    procedure StartWork;
    procedure StopWork;
    procedure FreeWork;
  end;

implementation

constructor TOneTimerThread.Create(QEventWorkSend: EventOneWorkSend = nil;
  QEventOneWork: EventOneWork = nil);
var
  ii: TGUID;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  CreateGUID(ii);
  FCreateID := GUIDToString(ii);
  // 默认30秒
  FIntervalSec := 30;
  FbWork := False;
  FEven := TEventObject.Create(nil, True, False, FCreateID);
  FEven.ResetEvent;
  self.FEventWorkSend := QEventWorkSend;
  self.FEventWork := QEventOneWork;

end;

destructor TOneTimerThread.Destroy;
begin
  FbWork := False;
  FEven.SetEvent;
  FEven.Free;
  inherited Destroy;
end;

procedure TOneTimerThread.Execute;
begin
  while not Terminated do
  begin
    try
      DoOnThreadSuspend;
      { 沉睡多少秒 }
    finally
      if not Terminated then
      begin
        // ResetEvent放前面好点,设置成无信号状态
        // 不然 SetEvent设成有状态时  FEven.WaitFor(INFINITE) 无效
        // 干活完 设置无信号状态,所以在干活期间发信号量过来是没用的
        // 一定要等干完活,信号过来了在继续干活
        // ResetEvent放在前后意义差别很大
        FEven.ResetEvent;
        // 等待几秒继续干活
        FEven.WaitFor(FIntervalSec * 1000);
      end;
    end;
  end;
end;

procedure TOneTimerThread.DoOnThreadSuspend;
begin
  if not FbWork then
    exit;
  if (Assigned(FEventWorkSend)) then
  begin
    try
      FEventWorkSend(self);
    except
    end;
  end;
  if (Assigned(FEventWork)) then
  begin
    try
      FEventWork();
    except
    end;
  end;
end;

procedure TOneTimerThread.StartWork;
begin
  // 发出信号量
  if not FbWork then
  begin
    FbWork := True;
    // 这边只要恢复线程就行
    self.Resume;
    // 不需要发送信号量,否则会执行两次
    // FEven.SetEvent;
  end;
end;

procedure TOneTimerThread.StopWork;
begin
  FbWork := False;
end;

procedure TOneTimerThread.FreeWork;
begin
  self.Free;
end;

constructor TOneSingleWorkThread.Create(QEventWorkSend: EventOneWorkSend = nil;
  QEventOneWork: EventOneWork = nil);
var
  ii: TGUID;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  CreateGUID(ii);
  FCreateID := Copy(AnsiReplaceStr(GUIDToString(ii), '-', ''), 2, 32);
  FbWork := False;
  FEven := TEventObject.Create(nil, True, False, FCreateID);
  FEven.ResetEvent;
  self.FEventWorkSend := QEventWorkSend;
  self.FEventWork := QEventOneWork;
end;

destructor TOneSingleWorkThread.Destroy;
begin
  FbWork := False;
  FEven.SetEvent;
  FEven.Free;
  inherited Destroy;
end;

procedure TOneSingleWorkThread.Execute;
begin
  while not Terminated do
  begin
    try
      DoOnThreadSuspend;
      { 沉睡多少秒 }
    finally
      if not Terminated then
      begin
        // ResetEvent放前面好点,设置成无信号状态
        // 不然 SetEvent设成有状态时  FEven.WaitFor(INFINITE) 无效
        // 干活完 设置无信号状态,所以在干活期间发信号量过来是没用的
        // 一定要等干完活,信号过来了在继续干活
        // ResetEvent放在前后意义差别很大
        FEven.ResetEvent;
        // 无限等待，直到唤醒
        FEven.WaitFor(INFINITE);
      end;
    end;
  end;
end;

procedure TOneSingleWorkThread.DoOnThreadSuspend;
begin
  if not FbWork then
    exit;
  if (Assigned(FEventWorkSend)) then
  begin
    try
      FEventWorkSend(self);
    except
    end;
  end;
  if (Assigned(FEventWork)) then
  begin
    try
      FEventWork();
    except
    end;
  end;
end;

procedure TOneSingleWorkThread.StartWork;
begin
  // 发出信号量
  if not FbWork then
  begin
    FbWork := True;
    // 这边只要恢复线程就行
    self.Resume;
    // 不需要发送信号量,否则会执行两次
    // FEven.SetEvent;
  end
  else
  begin
    FbWork := True;
    FEven.SetEvent;
  end;
end;

procedure TOneSingleWorkThread.StopWork;
begin
  FbWork := False;
end;

procedure TOneSingleWorkThread.FreeWork;
begin
  self.Free;
end;



end.
