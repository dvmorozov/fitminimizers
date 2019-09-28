{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FitMinimizers;

interface

uses
    Tools, SimpMath, MyExceptions, CBRCComponent, Algorithm,
    SelfCheckedComponentList, Decisions, DownhillSimplexAlgorithm,
    LazarusPackageIntf;

implementation

procedure Register;
begin
    RegisterUnit('DownhillSimplexAlgorithm', @DownhillSimplexAlgorithm.Register);
end;

initialization
    RegisterPackage('FitMinimizers', @Register);
end.
