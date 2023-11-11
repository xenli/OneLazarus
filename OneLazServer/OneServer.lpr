program OneServer;

{$mode objfpc}{$H+}
{$I mormot.uses.inc}
uses
 {$IFDEF UNIX}
  cthreads,
     {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
     {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, frm_main, OneILog, OneLog, OneThread,
  OneFileHelper, OneAttribute, OneCrypto, OneSQLCrypto, OneStreamString,
  OneDateTimeHelper, OneGUID, OneVirtualFile, OneTokenManage, OneDataInfo,
  OneZTManage, OneMonitor, OneControllerResult, OneHttpConst,
  OneHttpControllerRtti, OneHttpRouterManage, OneHttpCtxtResult, OneStopwatch,
  OneGlobal, OneOrmRtti, OneOrm, OneSerialization, OneHttpController,
  DemoController, TokenController, DataController, VirtualFileController,
  DemoDataController, OneDataJson, DemoJsonController, DemoCustResult,
  DemoZTController, OneMultipart, UniDemoController, OneFastLoginController;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
