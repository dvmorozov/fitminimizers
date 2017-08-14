{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FitMinimizers;

interface

uses
  Tools, SimpMath, MyExceptions, CBRCComponent, Algorithm, 
  SelfCheckedComponentList, Decisions, DownhillSimplexAlgorithm
{$ifdef Lazarus}
  ,LazarusPackageIntf
{$endif}
  ;

implementation

procedure Register;
begin
{$ifdef Lazarus}
  RegisterUnit('DownhillSimplexAlgorithm', @DownhillSimplexAlgorithm.Register);
{$endif}
end;

initialization
{$ifdef Lazarus}
  RegisterPackage('FitMinimizers', @Register);
{$endif}
end.
