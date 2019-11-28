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

interface

uses Classes, Tools,
    {$IFNDEF Lazarus}
      //TODO: set up proper module name for Delhpi build.
      //DesignIntf;
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
{$IFDEF Lazarus}
    RegisterComponents('FitMinimizers', [TRunner]);
    RegisterPropertyEditor(TypeInfo(TComputingProcedure),TRunner,'OnCompute',TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TOutputProcedure),TRunner,'OnOutput',TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TCreatingProcedure),TRunner,'OnCreate',TMethodProperty);
{$ENDIF}
end;

procedure TRunningThread.Execute;
begin
    if Assigned(ComputingProcedure) then
        ComputingProcedure;
    if (not Terminated) and Assigned(OutputProcedure) then
        Synchronize(OutputProcedure);
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
    inherited Destroy;
end;

{$warnings off}
procedure TRunner.Run;
begin
    Wait;
    { Thread is created in suspended state. }
    FRunningThread := TRunningThread.Create(True);
    FRunningThread.ComputingProcedure := OnCompute;
    FRunningThread.OutputProcedure := OnOutput;
    FRunningThread.Resume;
end;

procedure TRunner.Wait;
begin
    if Assigned(FRunningThread) then
    begin
        if FRunningThread.Suspended then
            FRunningThread.Resume;
        FRunningThread.WaitFor;
        UtilizeObject(FRunningThread);
        FRunningThread := nil;
    end;
end;
{$warnings on}

end.
