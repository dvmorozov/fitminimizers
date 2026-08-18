{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit DownhillSimplexAlgorithm;

interface

uses
{$IF NOT DEFINED(FPC)}
    System.Types,
{$ENDIF}
    Classes, Contnrs, Decisions, Algorithm, Tools, SysUtils;

type
    TDownhillSimplexDecision = class(TFloatDecision)
    public
    end;

    //  Component-decision for simulated annealing optimization.
    TDownhillSimplexSADecision = class(TDownhillSimplexDecision)
    protected
        FFluctEvaluation: Double;

    public
        function GetCopy: TAbstractDecision; override;

    published
        //  Value of estimation function with random additive value depending on the "temperature".
        property FluctEvaluation: Double read FFluctEvaluation write FFluctEvaluation;
    end;

    IDownhillSimplexServer = interface
        //  Return initial characteristic length for every parameter.
        function GetVariationStep(Sender: TComponent; index: LongInt): Double;
        //  Set inital calculation point in internal representation.
        //  The number of array element is equal to the number of parameters of task to be solved.
        procedure FillStartDecision(Sender: TComponent; StartDecision: TFloatDecision);
        //  Calculate evaluation function for the point given in internal representation.
        procedure EvaluateDecision(Sender: TComponent; Decision: TFloatDecision);
        procedure UpdateResults(Sender: TComponent; Decision: TFloatDecision);
        //  Return flag of calculation termination.
        function EndOfCalculation(Sender: TComponent): Boolean;
    end;

    EDownhillSimplexAlgorithm = class(Exception);

const
    { Longest stagnation window, in cycles. Comfortably more than the largest
      well-posed problem needs for a full pass over its simplex, and small enough
      that a degenerate one still terminates. }
    MAX_STAGNATION_WINDOW = 600;

type

    TDownhillSimplexAlgorithm = class(TAlgorithm)
    protected
        FDownhillSimplexServer: IDownhillSimplexServer;
        FCycleCount: LongInt;
        FEvaluationCount: LongInt;
        FRestartCount: LongInt;
        { Disables algorithm restarting after reaching local minimum.
          Restarting can in some configuration spaces help to get to
          better solution. }
        FRestartDisabled: Boolean;
        { Set exit values }
        FMaxCycles: integer;
        FMaxRestarts: integer;
        FFinalTolerance: Double;
        FFinalTolDefined: Boolean;
        { If difference in evaluation of best decision for the cycle
          is less than given value then exit. }
        FExitDerivative: Double;
        { CONVERGENCE BY SPEED, not by the spread of the goal function's values.

          FFinalTolerance compares that spread against the function's own
          MAGNITUDE. That only works when the magnitude is set by the thing being
          fitted. When the goal function carries a large constant term - a model
          that covers part of the data, a normalisation that does not subtract a
          baseline - the useful variation is a tiny fraction of the value and the
          test reports convergence while the fit has barely started. Observed:
          a fit stopping after two cycles with eleven of twelve parameters still
          at their initial values.

          These two ask a question that constant cannot distort: is the best
          decision still IMPROVING? Three details are load-bearing, and each was
          learned by getting it wrong:

          * Progress is measured over a WINDOW, not cycle by cycle. A simplex
            routinely spends several cycles contracting without bettering its
            best vertex, so a per-cycle test reads normal operation as
            convergence - it cut a diffraction fit off after twelve cycles,
            leaving a residual 19% below the unfitted baseline where that fit
            reaches 95%.

          * The window is counted in PASSES over the simplex, capped at
            MAX_STAGNATION_WINDOW. One cycle replaces one vertex, so N + 1 cycles
            pass before every parameter has been touched, and a shorter window
            cannot tell "has not started" from "has finished". Uncapped it does
            not survive a degenerate model: the automatic path builds one pattern
            per data point, which on a 100-point profile is ~1200 parameters and
            a 14000-cycle window, and that fit ran without end.

          * Improvement is judged against the gain the fit has ALREADY made, not
            only against the current value. Against the current value alone the
            test cannot terminate on a model that fits almost exactly: as the
            goal function approaches zero so does the threshold, so a millionth
            of it stays reachable forever. A synthetic impulse - which the
            Elliott pack reproduces to 1e-20 - hung on precisely that.

          FStagnationLimit = 0 disables the test, so existing callers are
          unaffected. }
        FMinRelImprovement: Double;
        FStagnationLimit: Integer;
        FParametersNumber: LongInt;
        FSimplexStartStepRandomEnabled: Boolean;
        FSimplexDirectionChangingEnabled: Boolean;
        FSimplexStartStepMultiplierEnabled: Boolean;
        //  Initial simplex size is multiplied by this number.
        //  If enabled it is used on optimization restarting (experimental feature).
        FSimplexStartStepMultiplier: Double;
        //  Set of solutions - vertexes of the simplex.
        FSimplex: TComponentList;
        FParametersSum: array of Double;
        //  Best solution found over all optimization cycles.
        FBestDecision: TDownhillSimplexDecision;

        function TryNewDecision(const Highest: LongInt; Factor: Double): Double; virtual;
        function MoveWorstDecision(const Highest: LongInt;
            Factor: Double): TDownhillSimplexDecision;
        //  Return new object-solution of the type appropriate for given algorithm.
        function CreateAppropriateDecision: TDownhillSimplexDecision; virtual;
        //  Return vertex of the simplex containing minimum value of goal function.
        function GetBestDecision: TDownhillSimplexDecision;
        procedure CreateSimplexVertices(StartDecision: TDownhillSimplexDecision);
        //  Replace selected solution with modified one.
        procedure ReplaceDecision(OldDecision, NewDecision: TDownhillSimplexDecision);
        //  Return indicies of the best solution, solution next to the best and worst solution.
        procedure GetIndicativeDecisions(var Highest, NextHighest, Lowest: LongInt);
            virtual;
        //  For each parameter index computes sum of values for all vertexes.
        procedure GetParametersSum;
        procedure Start;
        procedure Restart;
        //  Perform single optimization cycle.
        procedure BasicCalcCycle(const Highest, NextHighest, Lowest: LongInt);

        procedure SetParametersNumber(AParametersNumber: LongInt);

    public
        procedure AlgorithmRealization; override;
        constructor Create(AOwner: TComponent;
            AFinalTolerance: Double;
            ARestartDisabled: Boolean;
            AExitDerivative: Double); overload;
        destructor Destroy; override;
        //  The total number of optimization cycles.
        property CycleCount: Integer read FCycleCount;
        //  The total number of target function evaluations during optimization.
        property EvaluationCount: Integer read FEvaluationCount;
        //  The total number of algorithm restarts during optimization.
        property RestartCount: Integer read FRestartCount;
        property MaxCycles: Integer read FMaxCycles write FMaxCycles;
        property MaxRestarts: Integer read FMaxRestarts write FMaxRestarts;
        //  Stop when the best decision has gained less than this fraction over
        //  a whole window - see FMinRelImprovement for what it is a fraction of.
        property MinRelImprovement: Double
            read FMinRelImprovement write FMinRelImprovement;
        //  Window length, in passes over the simplex. 0 disables the test.
        property StagnationLimit: Integer
            read FStagnationLimit write FStagnationLimit;

        property DownhillSimplexServer: IDownhillSimplexServer
            read FDownhillSimplexServer write FDownhillSimplexServer;
        //  Total number of parameters of the problem to be solved.
        //  The number is defined after executing CreateSimplexVertices, should not be set up by client.
        property ParametersNumber: LongInt read FParametersNumber;
        //  Enables using FSimplexStartStepMultiplier on optimization restarting.
        //  The flag should not be used together with other SimplexXXXX flags.
        property SimplexStartStepMultiplierEnabled: Boolean
            read FSimplexStartStepMultiplierEnabled write FSimplexStartStepMultiplierEnabled;
        //  Enables sequential changing of directions of initial steps
        //  forming initial simplex vertices. Steps are taken into different
        //  directions from the initial point according to restart counter.
        //  Every new optimization cycle starts with its own initial simplex.
        //  The flag should not be used together with other SimplexXXXX flags.
        property SimplexDirectionChangingEnabled: Boolean
            read FSimplexDirectionChangingEnabled write FSimplexDirectionChangingEnabled;
        //  Enables random multiplier in creating initial simplex vertices.
        //  The flag should not be used together with other SimplexXXXX flags.
        property SimplexStartStepRandomEnabled: Boolean
            read FSimplexStartStepRandomEnabled write FSimplexStartStepRandomEnabled;
    end;

    TDownhillSimplexSAAlgorithm = class(TDownhillSimplexAlgorithm)
    protected
        FTemperature: Double;
        //  Return indicies of the best solution, solution next to the best and
        //  worst solution after adding random fluctiations to evaluated values.
        procedure GetIndicativeDecisions(
            var Highest, NextHighest, Lowest: LongInt); override;
        function TryNewDecision(const Highest: LongInt;
            Factor: Double): Double; override;
        function CreateAppropriateDecision: TDownhillSimplexDecision; override;
        function GetRandomFluct: Double;

    public
        procedure AlgorithmRealization; override;

    published
        property Temperature: Double read FTemperature write FTemperature;
    end;

const
    TINY = 1e-10;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Fit', [TDownhillSimplexAlgorithm]);
    RegisterComponents('Fit', [TDownhillSimplexSAAlgorithm]);
end;

procedure TDownhillSimplexAlgorithm.Restart;
var
    Best, Temp: TDownhillSimplexDecision;
begin
    Inc(FRestartCount);
    //  Searches for solution having minimum value of goal function.
    //  Reevaluates it to put the "server" into proper state.
    //  This solution can be used (depending on server configuration)
    //  as starting point in creating new simplex.
    Best := TDownhillSimplexDecision(GetBestDecision.GetCopy);
    with DownhillSimplexServer do
        EvaluateDecision(Self, Best);
    Inc(FEvaluationCount);

    //  Initial simplex size is reduced by the factor if it's enabled.
    if FSimplexStartStepMultiplierEnabled then
    begin
        FSimplexStartStepMultiplier := FSimplexStartStepMultiplier / 2;
    end;

    //  Creates new starting point for recreating simplex.
    Temp := CreateAppropriateDecision;
    with DownhillSimplexServer do
    begin
        FillStartDecision(Self, Temp);
        //  It is up to the "server" to propose new starting point.
        //  In the case if it is different from the best point
        //  found on previous cycle, goal function should be computed.
        if not Best.Coincide(Temp) then
        begin
            EvaluateDecision(Self, Temp);
            Inc(FEvaluationCount);
        end
        else
        begin
            UtilizeObject(Temp);
            Temp := Best;
        end;
    end;
    //  Recreates simplex points.
    CreateSimplexVertices(Temp);
end;

procedure TDownhillSimplexAlgorithm.Start;
var
    TempDecision: TDownhillSimplexDecision;
begin
    FCycleCount := 0;
    FEvaluationCount := 0;
    FRestartCount := 0;
    FSimplexStartStepMultiplier := 1;
    //  Creates new starting point for recreating FSimplex.
    TempDecision := CreateAppropriateDecision;
    with DownhillSimplexServer do
    begin
        FillStartDecision(Self, TempDecision);
        EvaluateDecision(Self, TempDecision);
        Inc(FEvaluationCount);
    end;
    //  Recreates simplex vertexes.
    CreateSimplexVertices(TempDecision);
    //  Searches for the best solution in FSimplex and stores it.
    UtilizeObject(FBestDecision);
    FBestDecision := TDownhillSimplexDecision(GetBestDecision.GetCopy);
    DownhillSimplexServer.UpdateResults(Self, FBestDecision);
end;

function TDownhillSimplexAlgorithm.GetBestDecision: TDownhillSimplexDecision;
var
    MinDecision, TempDecision: TDownhillSimplexDecision;
    i: LongInt;
begin
    MinDecision := TDownhillSimplexDecision(FSimplex.Items[0]);
    for i := 1 to FSimplex.Count - 1 do
    begin
        TempDecision := TDownhillSimplexDecision(FSimplex.Items[i]);
        if TempDecision.Evaluation < MinDecision.Evaluation then
            MinDecision := TempDecision;
    end;
    Result := MinDecision;
end;

procedure TDownhillSimplexAlgorithm.CreateSimplexVertices(
    StartDecision: TDownhillSimplexDecision);
var
    i, j: LongInt;
    Decision: TDownhillSimplexDecision;
    SimplexStartStepDirection, SimplexStartStepRandom: Double;
begin
    if FSimplexStartStepRandomEnabled then
        Randomize;

    with DownhillSimplexServer do
    begin
        //  Initializes parameter number.
        SetParametersNumber(StartDecision.ParametersNumber);
        FSimplex.Clear;
        //  Original point is added as a vertex.
        FSimplex.Add(StartDecision);
        for i := 0 to ParametersNumber - 1 do
        begin
            //  Other N vertices are added.
            Decision := CreateAppropriateDecision;
            Decision.ParametersNumber := ParametersNumber;
            //  Copying original vertex parameters to new vertex.
            for j := 0 to ParametersNumber - 1 do
                Decision.Parameters[j] := StartDecision.Parameters[j];

            //  The i-th component is moved along corresponding basis vector.

            //  Steps from original point are added along basis vectors
            //  in opposite directions accorging to restart counter.
            //  Basis vector is enumerated by parameter index.
            SimplexStartStepDirection := 1;
            if FSimplexDirectionChangingEnabled then
            begin
                //  Inverts direction.
                if FRestartCount and (1 shl i) <> 0 then
                    SimplexStartStepDirection := -1;
            end;

            SimplexStartStepRandom := 1;
            if FSimplexStartStepRandomEnabled then
                SimplexStartStepRandom := Random();

            Decision.Parameters[i] := Decision.Parameters[i] +
                //  Takes into account all multipliers. All of them
                //  should have default value 1.
                SimplexStartStepRandom * 
                SimplexStartStepDirection *
                FSimplexStartStepMultiplier *
                GetVariationStep(Self, i);

            EvaluateDecision(Self, Decision);
            Inc(FEvaluationCount);

            //  IF THE STEP WAS REFUSED, TRY THE OTHER WAY.
            //
            //  A parameter already sitting against a limit cannot move in the
            //  direction the basis vector points, so this vertex came back
            //  identical to the starting one - a simplex with no extent along
            //  that axis, which no amount of reflecting or contracting can ever
            //  recover. It is not a rare corner: a curve width capped at the edge
            //  of the data, an amplitude held non-negative at zero, a position on
            //  the last point all land there, whatever the curve type.
            //
            //  Stepping the opposite way costs one evaluation and is always
            //  available, because a limit binds on one side only. If both are
            //  refused the parameter genuinely cannot move, and the vertex is
            //  left as it is.
            if Decision.Parameters[i] = StartDecision.Parameters[i] then
            begin
                Decision.Parameters[i] := StartDecision.Parameters[i] -
                    SimplexStartStepRandom *
                    SimplexStartStepDirection *
                    FSimplexStartStepMultiplier *
                    GetVariationStep(Self, i);
                EvaluateDecision(Self, Decision);
                Inc(FEvaluationCount);
            end;

            FSimplex.Add(Decision);
        end;    //  for i := 0 to StartDecision.ParametersNumber - 1 do...
    end;    //  with DownhillSimplexServer do...
    GetParametersSum;
end;

procedure TDownhillSimplexAlgorithm.GetIndicativeDecisions(
    var Highest, NextHighest, Lowest: LongInt);
var
    i: LongInt;
begin
    if TDownhillSimplexDecision(FSimplex.Items[0]).Evaluation >
        TDownhillSimplexDecision(FSimplex.Items[1]).Evaluation then
    begin
        Highest := 0;
        NextHighest := 1;
        Lowest := 1;
    end
    else
    begin
        Highest := 1;
        NextHighest := 0;
        Lowest := 0;
    end;

    for i := 2 to FSimplex.Count - 1 do
    begin
        if TDownhillSimplexDecision(FSimplex.Items[i]).Evaluation <
            TDownhillSimplexDecision(FSimplex.Items[Lowest]).Evaluation then
            Lowest := i;

        if TDownhillSimplexDecision(FSimplex.Items[i]).Evaluation >
            TDownhillSimplexDecision(FSimplex.Items[Highest]).Evaluation then
        begin
            NextHighest := Highest;
            Highest := i;
        end
        else
        begin
            if TDownhillSimplexDecision(FSimplex.Items[i]).Evaluation >
                TDownhillSimplexDecision(FSimplex.Items[NextHighest]).Evaluation then
                NextHighest := i;
        end;
    end;    //  for i := 2 to FSimplex.Count - 1 do...
end;

procedure TDownhillSimplexSAAlgorithm.GetIndicativeDecisions(
    var Highest, NextHighest, Lowest: LongInt);
var
    i: LongInt;
begin
    with FSimplex.Items[0] as TDownhillSimplexSADecision do
        FluctEvaluation := Evaluation + GetRandomFluct;

    with FSimplex.Items[1] as TDownhillSimplexSADecision do
        FluctEvaluation := Evaluation + GetRandomFluct;

    if TDownhillSimplexSADecision(FSimplex.Items[0]).FluctEvaluation >
        TDownhillSimplexSADecision(FSimplex.Items[1]).FluctEvaluation then
    begin
        Highest := 0;
        NextHighest := 1;
        Lowest := 1;
    end
    else
    begin
        Highest := 1;
        NextHighest := 0;
        Lowest := 0;
    end;

    for i := 2 to FSimplex.Count - 1 do
    begin
        with FSimplex.Items[i] as TDownhillSimplexSADecision do
            FluctEvaluation := Evaluation + GetRandomFluct;

        if TDownhillSimplexSADecision(FSimplex.Items[i]).FluctEvaluation <
            TDownhillSimplexSADecision(FSimplex.Items[Lowest]).FluctEvaluation then
            Lowest := i;

        if TDownhillSimplexSADecision(FSimplex.Items[i]).FluctEvaluation >
            TDownhillSimplexSADecision(FSimplex.Items[Highest]).FluctEvaluation then
        begin
            NextHighest := Highest;
            Highest := i;
        end
        else
        begin
            if TDownhillSimplexSADecision(FSimplex.Items[i]).FluctEvaluation >
                TDownhillSimplexSADecision(FSimplex.Items[NextHighest]).FluctEvaluation then
                NextHighest := i;
        end;
    end;    //  for i := 2 to FSimplex.Count - 1 do...
end;

function TDownhillSimplexAlgorithm.CreateAppropriateDecision: TDownhillSimplexDecision;
begin
    Result := TDownhillSimplexDecision.Create(nil);
end;

function TDownhillSimplexSAAlgorithm.CreateAppropriateDecision: TDownhillSimplexDecision;
begin
    Result := TDownhillSimplexSADecision.Create(nil);
end;

function TDownhillSimplexAlgorithm.MoveWorstDecision(const Highest: LongInt;
    Factor: Double): TDownhillSimplexDecision;
var
    HighestDecision, TempDecision: TDownhillSimplexDecision;
    Factor1, Factor2: Double;
    j: LongInt;
begin
    HighestDecision := TDownhillSimplexDecision(FSimplex.Items[Highest]);
    TempDecision := CreateAppropriateDecision;
    TempDecision.ParametersNumber := ParametersNumber;

    //  Vector is calculated to move the vertex through the center of mass.
    Factor1 := (1 - Factor) / ParametersNumber;
    Factor2 := Factor1 - Factor;
    for j := 0 to ParametersNumber - 1 do
        TempDecision.Parameters[j] :=
            FParametersSum[j] * Factor1 - HighestDecision.Parameters[j] * Factor2;

    DownhillSimplexServer.EvaluateDecision(Self, TempDecision);
    Inc(FEvaluationCount);
    Result := TempDecision;

    if TempDecision.Evaluation < FBestDecision.Evaluation then
    begin
        UtilizeObject(FBestDecision);
        FBestDecision := TDownhillSimplexDecision(TempDecision.GetCopy);
        DownhillSimplexServer.UpdateResults(Self, FBestDecision);
    end;
end;

procedure TDownhillSimplexAlgorithm.ReplaceDecision(
    OldDecision, NewDecision: TDownhillSimplexDecision);
var
    Index: LongInt;
begin
    //  It's important to preserve order of items in the list!
    Index := FSimplex.IndexOf(OldDecision);
    FSimplex.Extract(OldDecision);
    UtilizeObject(OldDecision);
    FSimplex.Insert(Index, NewDecision);
    GetParametersSum;
end;

function TDownhillSimplexAlgorithm.TryNewDecision(const Highest: LongInt;
    Factor: Double): Double;
var
    HighestDecision, TempDecision: TDownhillSimplexDecision;
begin
    TempDecision := MoveWorstDecision(Highest, Factor);
    HighestDecision := TDownhillSimplexDecision(FSimplex.Items[Highest]);

    Result := TempDecision.Evaluation;

    if TempDecision.Evaluation < HighestDecision.Evaluation then
        ReplaceDecision(HighestDecision, TempDecision)
    else
        UtilizeObject(TempDecision);
end;

function TDownhillSimplexSAAlgorithm.TryNewDecision(const Highest: LongInt;
    Factor: Double): Double;
var
    HighestDecision, TempDecision: TDownhillSimplexSADecision;
begin
    TempDecision := TDownhillSimplexSADecision(MoveWorstDecision(Highest, Factor));
    HighestDecision := TDownhillSimplexSADecision(FSimplex.Items[Highest]);

    TempDecision.FluctEvaluation := TempDecision.Evaluation - GetRandomFluct;

    Result := TempDecision.FluctEvaluation;

    if TempDecision.FluctEvaluation < HighestDecision.FluctEvaluation then
        ReplaceDecision(HighestDecision, TempDecision)
    else
        UtilizeObject(TempDecision);
end;

procedure TDownhillSimplexAlgorithm.GetParametersSum;
var
    i, j: LongInt;
    Sum: Double;
begin
    for j := 0 to ParametersNumber - 1 do
    begin
        Sum := 0;
        for i := 0 to FSimplex.Count - 1 do
            Sum := Sum + TDownhillSimplexDecision(FSimplex.Items[i]).Parameters[j];
        FParametersSum[j] := Sum;
    end;
end;

procedure TDownhillSimplexAlgorithm.BasicCalcCycle(
    const Highest, NextHighest, Lowest: LongInt);
var
    TryResult, SavedResult: Double;
    LowestParamValue, CurParamValue: Double;
    i, j: LongInt;
    SimplexCount: LongInt;
begin
    Inc(FCycleCount);

    with DownhillSimplexServer do
    begin
        TryResult := TryNewDecision(Highest, -1);
        //  Order of items must be preserved!
        if TryResult < TDownhillSimplexDecision(
            FSimplex.Items[Lowest]).Evaluation then
            TryNewDecision(Highest, 2)
        else
        begin
            if TryResult >= TDownhillSimplexDecision(
                FSimplex.Items[NextHighest]).Evaluation then
            begin
                SavedResult :=
                    TDownhillSimplexDecision(FSimplex.Items[Highest]).Evaluation;
                TryResult := TryNewDecision(Highest, 0.5);
                if TryResult >= SavedResult then
                begin
                    //  Decrements sizes of simplex toward best vertex.
                    //  Calculates average positions between best vertex and
                    //  every other vertex. Obtained values determine new
                    //  position of the simplex.
                    SimplexCount := FSimplex.Count;
                    for i := 0 to SimplexCount - 1 do
                    begin
                        if i <> Lowest then
                        begin
                            for j := 0 to ParametersNumber - 1 do
                            begin
                                LowestParamValue :=
                                    TDownhillSimplexDecision(FSimplex.Items[Lowest]).Parameters[j];
                                CurParamValue :=
                                    TDownhillSimplexDecision(FSimplex.Items[i]).Parameters[j];
                                //  Computes middle point of FSimplex edge.
                                TDownhillSimplexDecision(
                                    FSimplex.Items[i]).Parameters[j] :=
                                    0.5 * (CurParamValue + LowestParamValue);
                            end;
                            EvaluateDecision(Self,
                                TDownhillSimplexDecision(FSimplex.Items[i]));
                            Inc(FEvaluationCount);
                        end;    //  if i <> Lowest then...
                    end;
                    GetParametersSum;
                end;    //  if TryResult >= SavedResult then...
            end;    //  if TryResult >= TDownhillSimplexDecision(
        end;    //  else...
    end;    //  with DownhillSimplexServer do...
end;

procedure TDownhillSimplexAlgorithm.AlgorithmRealization;
var
    Highest, NextHighest, Lowest: LongInt;
    Tolerance, PrevTolerance: Double;
    EvalHi, EvalLo: Double;
    SavedLoEval, CurLoEval: Double;
    WindowStartBest, CurBest, InitialBest: Double;
    CyclesInWindow, StagnationWindow: LongInt;
    PrevTolDefined: Boolean;
begin
    if not Assigned(DownhillSimplexServer) then
        raise EDownhillSimplexAlgorithm.Create('Server is not assigned...');

    Start;
    //  Saves minimum value of goal function from initial simplex.
    SavedLoEval := GetBestDecision.Evaluation;

    PrevTolDefined := False;
    PrevTolerance := 0;
    WindowStartBest := SavedLoEval;
    InitialBest := SavedLoEval;
    CyclesInWindow := 0;
    StagnationWindow := FStagnationLimit * (ParametersNumber + 1);
    if StagnationWindow > MAX_STAGNATION_WINDOW then
        StagnationWindow := MAX_STAGNATION_WINDOW;

    with DownhillSimplexServer do
    begin
        while (not EndOfCalculation(Self)) and (FCycleCount < FMaxCycles) do
        begin
            Highest := 0;
            NextHighest := 0;
            Lowest := 0;
            GetIndicativeDecisions(Highest, NextHighest, Lowest);

            EvalHi := TDownhillSimplexDecision(
                FSimplex.Items[Highest]).Evaluation;
            EvalLo := TDownhillSimplexDecision(
                FSimplex.Items[Lowest]).Evaluation;

            Tolerance := 2 * Abs(EvalHi - EvalLo) /
                (Abs(EvalHi) + Abs(EvalLo) + TINY);

            //  Tolerance directly depends on height of the simplex along
            //  the axis of minimized function. Therefore when tolerance stops
            //  decrease substantially for cycle it is necessary to terminate
            //  calculation.
            if FFinalTolDefined then
            begin
                if Tolerance < FFinalTolerance then
                begin
                    CurLoEval := GetBestDecision.Evaluation;
                    //  Size of simplex was reduced to minimal admissible value.
                    if (not FRestartDisabled)
                        //  Checks other termination conditions.
                        and (
                            (FSimplexDirectionChangingEnabled and (FRestartCount < (1 shl ParametersNumber) - 1))
                         or (FSimplexStartStepMultiplierEnabled and (FSimplexStartStepMultiplier > 0.01))
                         or ((not FSimplexDirectionChangingEnabled) and (not FSimplexStartStepMultiplierEnabled)
                              and (Abs(CurLoEval - SavedLoEval) > FExitDerivative))
                         )
                        and (FRestartCount < FMaxRestarts)
                    then
                    begin
                        //  Saves minimum value of goal function among simplex vertices.
                        SavedLoEval := GetBestDecision.Evaluation;
                        Restart;
                        Continue;
                    end
                    else
                        Break;
                end;
            end
            else
            if PrevTolDefined then
            begin
                if Abs(PrevTolerance - Tolerance) < TINY then
                    Break;
            end;

            PrevTolerance := Tolerance;
            PrevTolDefined := True;

            BasicCalcCycle(Highest, NextHighest, Lowest);

            //  HAS THE SEARCH STOPPED IMPROVING? Measured against the best
            //  decision's own value, so it is scale-free, and over a whole
            //  window of cycles, because individual unproductive cycles are
            //  normal in a simplex - see the field comment.
            if FStagnationLimit > 0 then
            begin
                Inc(CyclesInWindow);
                if CyclesInWindow >= StagnationWindow then
                begin
                    CurBest := GetBestDecision.Evaluation;
                    if Abs(WindowStartBest - CurBest) <= FMinRelImprovement *
                        (Abs(InitialBest - CurBest) + Abs(CurBest) + TINY) then
                        Break;
                    WindowStartBest := CurBest;
                    CyclesInWindow := 0;
                end;
            end;
        end;
        //  Set up parameters of best solution.
        EvaluateDecision(Self, FBestDecision);
        Inc(FEvaluationCount);
    end;
end;

procedure TDownhillSimplexSAAlgorithm.AlgorithmRealization;
var
    Highest, NextHighest, Lowest: LongInt;
    Tolerance: Double;
    EvalHi, EvalLo: Double;
    SavedLoEval: Double;
    CycleCounter: LongInt;
begin
    if not Assigned(DownhillSimplexServer) then
        raise EDownhillSimplexAlgorithm.Create('Server is not assigned...');

    Randomize;
    Start;
    SavedLoEval := GetBestDecision.Evaluation;
    CycleCounter := 0;

    with DownhillSimplexServer do
    begin
        while (not EndOfCalculation(Self)) and (FCycleCount < FMaxCycles) do
        begin
            Highest := 0;
            NextHighest := 0;
            Lowest := 0;
            GetIndicativeDecisions(Highest, NextHighest, Lowest);

            EvalHi := TDownhillSimplexSADecision(
                FSimplex.Items[Highest]).FluctEvaluation;
            EvalLo := TDownhillSimplexSADecision(
                FSimplex.Items[Lowest]).FluctEvaluation;

            Tolerance := 2 * Abs(EvalHi - EvalLo) /
                (Abs(EvalHi) + Abs(EvalLo) + TINY);

            if FFinalTolDefined and (Tolerance < FFinalTolerance) then
            begin
                if (GetBestDecision.Evaluation < SavedLoEval) and
                    (FRestartCount < FMaxRestarts) then


                begin
                    SavedLoEval := GetBestDecision.Evaluation;
                    Restart;
                    Continue;
                end
                else
                    Break;
            end;    //  if Tolerance < FinalTolerance then...

            BasicCalcCycle(Highest, NextHighest, Lowest);

            Inc(CycleCounter);
            if CycleCounter = 1000 then
            begin
                CycleCounter := 0;
                Temperature := Temperature * 0.95;
            end;    //  if CycleCounter = 1000 then...
        end;
        //  Set up parameters of best solution.
        EvaluateDecision(Self, FBestDecision);
        Inc(FEvaluationCount);
    end;
end;

constructor TDownhillSimplexAlgorithm.Create(AOwner: TComponent;
            AFinalTolerance: Double;
            ARestartDisabled: Boolean;
            AExitDerivative: Double);
begin
    inherited Create(AOwner);
    FSimplex := TComponentList.Create;
    FSimplexStartStepMultiplierEnabled := False;
    FSimplexStartStepRandomEnabled := False;
    FSimplexDirectionChangingEnabled := False;
    FMaxCycles := MaxInt;
    FMaxRestarts := MaxInt;
    {  Final tolerance should have non zero value,
       otherwise computation will never end. }
    FFinalTolDefined := False;
    if AFinalTolerance <> 0 then
    begin
        FFinalTolerance := AFinalTolerance;
        FFinalTolDefined := True;
    end;
    FRestartDisabled := ARestartDisabled;
    FExitDerivative := AExitDerivative;
    //  Off by default: an existing caller must opt in and get exactly the
    //  behaviour it had before.
    FMinRelImprovement := 0;
    FStagnationLimit := 0;
end;

procedure TDownhillSimplexAlgorithm.SetParametersNumber(AParametersNumber: LongInt);
begin
    SetLength(FParametersSum, AParametersNumber);
    FParametersNumber := AParametersNumber;
end;

destructor TDownhillSimplexAlgorithm.Destroy;
begin
    UtilizeObject(FSimplex);
    UtilizeObject(FBestDecision);
    inherited Destroy;
end;

function TDownhillSimplexSAAlgorithm.GetRandomFluct: Double;
begin
    Result := (-1) * Temperature * Ln(Random + TINY);
end;

function TDownhillSimplexSADecision.GetCopy: TAbstractDecision;
var
    i: LongInt;
    TempDecision: TDownhillSimplexSADecision;
begin
    TempDecision := TDownhillSimplexSADecision.Create(nil);
    TempDecision.ParametersNumber := ParametersNumber;
    for i := 0 to ParametersNumber - 1 do
        TempDecision.Parameters[i] := Parameters[i];
    TempDecision.Evaluation := Evaluation;
    TempDecision.FluctEvaluation := FluctEvaluation;
    Result := TempDecision;
end;

initialization
end.
