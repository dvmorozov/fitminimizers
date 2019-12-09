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
      PropEdits,
    {$ENDIF}
    Contnrs, Windows;

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
        function GetHandle: THandle;

    public
        { Waits for finishing execution and terminates the thread. }
        destructor Destroy; override;
        { Starts execution. }
        procedure Run;
        { Waits for finishing execution. }
        procedure Wait;
        { Calls OnCreate if assigned. }
        procedure Loaded; override;
        function Finished: Boolean;

    published
        { Main computing procedure, it is not synchronized with VCL thread. }
        property OnCompute: TComputingProcedure
            read FCompute write FCompute;
        { Procedure displaying results, it is synchronized with VCL thread. }
        property OnOutput: TOutputProcedure
            read FOutput write FOutput;
        property OnCreate: TCreatingProcedure
            read FCreate write FCreate;
        property Handle: THandle read GetHandle;
    end;

    TRunnerPool = class(TObject)
    private
        FRunners: TComponentList;

    public
        constructor Create; reintroduce;
        destructor Destroy; override;
        { Returns instance of runner ready to start new task.
          Waits internally if necessary. }
        function GetFreeRunner: TRunner;
        { Waits for all runners finishing. }
        procedure WaitAll;
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
    inherited;
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

function TRunner.GetHandle: THandle;
begin
    Result:= FRunningThread.Handle;
end;

function TRunner.Finished: Boolean;
begin
    Result := True;
    if FRunningThread <> nil then
        Result := FRunningThread.Finished;
end;

{$warnings on}

constructor TRunnerPool.Create;
var i: Integer;
begin
    inherited;
    { Owns runner instances. }
    FRunners := TComponentList.Create(True);
    { Creates number of runners equal to number of CPU cores. }
    for i := 0 to TThread.ProcessorCount - 1 do
    begin
        FRunners.Add(TRunner.Create(nil));
    end;
end;

destructor TRunnerPool.Destroy;
begin
    { Waits for finishing. }
    WaitAll;
    { Destroys all runner instances. }
    FRunners.Free;
    inherited;
end;

function TRunnerPool.GetFreeRunner: TRunner;
var
    i: LongInt;
    Runner: TRunner;
    ThreadHandles: array of THandle;
    PHandle, PCurHandle: ^THandle;
    FreeThreadIndex, LastError: DWord;
    RunnerCount: DWord;
begin
    Result := nil;
    if FRunners.Count > 0 then
    begin
        { Returns the first free runner if any exsits. }
        for i := 0 to FRunners.Count - 1 do
        begin
            Runner := TRunner(FRunners[i]);
            if Runner.Finished then
            begin
                Result := Runner;
                Exit;
            end;
        end;
        { All runners are busy, waits for finishing of any. }
        SetLength(ThreadHandles, FRunners.Count);
        RunnerCount := FRunners.Count;
        GetMem(PHandle, RunnerCount * SizeOf(THandle));
        PCurHandle := PHandle;

        for i := 0 to FRunners.Count - 1 do
        begin
            Runner := TRunner(FRunners[i]);
            ThreadHandles[i] := Runner.Handle;
            PCurHandle^ := Runner.Handle;
            Inc(PCurHandle);
        end;
        { Gets free thread index. }
        FreeThreadIndex := WaitForMultipleObjects(
            RunnerCount, PWOHandleArray(PHandle), False, INFINITE) - WAIT_OBJECT_0;
        Assert(FreeThreadIndex < RunnerCount);
        { Checks errors. }
        if FreeThreadIndex <> $FFFFFFFF then
            Result := TRunner(FRunners[FreeThreadIndex])
        else
        begin
            LastError := GetLastError;
        end;

        FreeMem(PHandle);
    end;
end;

procedure TRunnerPool.WaitAll;
var
    i: LongInt;
    Runner: TRunner;
begin
    for i := 0 to FRunners.Count - 1 do
        begin
            Runner := TRunner(FRunners[i]);
            if not Runner.Finished then
                Runner.Wait;
        end;
end;

end.
