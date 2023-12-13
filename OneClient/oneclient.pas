{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OneClient;

{$warn 5023 off : no warning about unused units}
interface

uses
  OneClientConnect, OneClientConst, OneClientDataSet, OneClientHelper, 
  OneClientResult, OneClientVirtualFile, OneClientDataInfo, OneCrypto, 
  OneFileHelper, OneSerialization, OneSQLCrypto, OneStreamString, OneThread, 
  OneZipStream, OneDataJson, OneLog, OneAsynThread, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('OneClientConnect', @OneClientConnect.Register);
  RegisterUnit('OneClientDataSet', @OneClientDataSet.Register);
  RegisterUnit('OneClientVirtualFile', @OneClientVirtualFile.Register);
end;

initialization
  RegisterPackage('OneClient', @Register);
end.
