{
This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                    LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                    Facebook: https://www.facebook.com/dmitry.v.morozov

@abstract(Contains definitions of thread containers.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/,
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit RunningThread;

{$MODE Delphi}

interface

uses Classes, Tools;

type
    TComputingProcedure = procedure of object;
    TOutputProcedure = procedure of object;

    TRunningThread = class(TThread)
    { If process was terminated by means of object destruction then termination procedure is not called. }
    public
        ComputingProcedure: TComputingProcedure;
        OutputProcedure: TOutputProcedure;
        procedure Execute; override;
    end;

    { Class-container for TRunningThread. }
    TRunner = class(TComponent)
    protected
        FComputingProcedure: TComputingProcedure;
        FOutputProcedure: TOutputProcedure;
        RunningThread: TRunningThread;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Run;
        procedure Wait;

    published
        { Computation method running in separate thread. }
        property OnComputingProcedure: TComputingProcedure
            read FComputingProcedure write FComputingProcedure;
        { Method for printing results, it is synchronized with VCL thread. }
        property OnOutputProcedure: TOutputProcedure
            read FOutputProcedure write FOutputProcedure;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('FitMinimizers', [TRunner]);
    (*???
    RegisterPropertyEditor(TypeInfo(TComputingProcedure),TRunner,'OnRunningProcedure',TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TOutputProcedure),TRunner,'OnEndRunningProcedure',TMethodProperty);
    *)
end;

procedure TRunningThread.Execute;
begin
    if Assigned(ComputingProcedure) then
        ComputingProcedure;
    if (not Terminated) and Assigned(OutputProcedure) then
        Synchronize(OutputProcedure);
end;

constructor TRunner.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    RunningThread := TRunningThread.Create(True);
end;

destructor TRunner.Destroy;
begin
    UtilizeObject(RunningThread);
    inherited Destroy;
end;

{$warnings off}
procedure TRunner.Run;
begin
    RunningThread.ComputingProcedure := OnComputingProcedure;
    RunningThread.OutputProcedure := OnOutputProcedure;
    RunningThread.Resume;
end;

procedure TRunner.Wait;
begin
    RunningThread.WaitFor;
end;
{$warnings on}

end.
