{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FitMinimizers;

{$warn 5023 off : no warning about unused units}
interface

uses
  Tools, SimpMath, MyExceptions, Algorithm, Decisions, 
  DownhillSimplexAlgorithm, AlgorithmContainer, RunningThread, CombEnumerator, 
  DownhillSimplexServer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DownhillSimplexAlgorithm', @DownhillSimplexAlgorithm.Register);
  RegisterUnit('RunningThread', @RunningThread.Register);
end;

initialization
  RegisterPackage('FitMinimizers', @Register);
end.
