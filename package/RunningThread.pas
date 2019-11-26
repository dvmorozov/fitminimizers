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

uses Classes, Tools,
    {$IFNDEF Lazarus}
      DesignIntf;
    {$ELSE}
      PropEdits;
    {$ENDIF}

type
    TComputingProcedure = procedure of object;
    TOutputProcedure = procedure of object;
    TRunner = class;
    TCreatingProcedure = procedure(Runner: TRunner) of object;

    TRunningThread = class(TThread)
    { If process was terminated by means of object destruction then termination procedure is not called. }
    public
        { Main computing procedure, it is not synchronized with VCL thread. }
        ComputingProcedure: TComputingProcedure;
        { Procedure displaying results, it is synchronized with VCL thread. }
        OutputProcedure: TOutputProcedure;
        procedure Execute; override;
    end;

    { Visual component, container for TRunningThread. }
    TRunner = class(TComponent)
    protected
        FCompute: TComputingProcedure;
        FOutput: TOutputProcedure;
        FCreate: TCreatingProcedure;
        FRunningThread: TRunningThread;

    public
        { Thread is created in suspended state. Run should be called to start execution. }
        constructor Create(AOwner: TComponent); override;
        { Waits for finishing execution and terminates the thread. }
        destructor Destroy; override;
        { Starts execution. }
        procedure Run;
        { Waits for finishing execution. }
        procedure Wait;
        { Calls OnCreate if assigned. }
        procedure Loaded; override;

    published
        { Main computing procedure, it is not synchronized with VCL thread. }
        property OnCompute: TComputingProcedure
            read FCompute write FCompute;
        { Procedure displaying results, it is synchronized with VCL thread. }
        property OnOutput: TOutputProcedure
            read FOutput write FOutput;
        property OnCreate: TCreatingProcedure
            read FCreate write FCreate;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('FitMinimizers', [TRunner]);
    RegisterPropertyEditor(TypeInfo(TComputingProcedure),TRunner,'OnCompute',TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TOutputProcedure),TRunner,'OnOutput',TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TCreatingProcedure),TRunner,'OnCreate',TMethodProperty);
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
    FRunningThread := TRunningThread.Create(True);
end;

procedure TRunner.Loaded;
begin
    { Should be called after component construction. }
    if Assigned(OnCreate) then
        OnCreate(Self);
end;

destructor TRunner.Destroy;
begin
    Wait;
    UtilizeObject(FRunningThread);
    inherited Destroy;
end;

{$warnings off}
procedure TRunner.Run;
begin
    FRunningThread.ComputingProcedure := OnCompute;
    FRunningThread.OutputProcedure := OnOutput;
    FRunningThread.Resume;
end;

procedure TRunner.Wait;
begin
    if FRunningThread.Suspended then
        FRunningThread.Resume;
    if not FRunningThread.Finished then
        FRunningThread.WaitFor;
end;
{$warnings on}

end.
