{
This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

Copyright (C) Dmitry Morozov

@abstract(Levenberg-Marquardt: least squares from the residuals themselves.)

WHEN TO USE IT RATHER THAN THE SIMPLEX. On a smooth least-squares problem -
above all one whose answer is exact, a shape whose points are to go to given
places - the residuals' derivatives show the way down, and this converges to
the last digit where the simplex, which sees only their sum, stalls short. On a
fit to noisy data with several valleys the simplex's wider search is the better
tool. Both run in the same container (LevenbergMarquardtServer inherits the
simplex's), so parameters, limits, the best point found and Stop are one
mechanism.

THE METHOD: a forward-difference Jacobian at the current point, the damped
normal equations (J'J + lambda diag) step = -J'r solved by Gaussian
elimination, the step taken only when it lowers the sum of squares, lambda
lowered after a success and raised after a failure. A handful of parameters,
so each step is a small dense solve.
}
unit LevenbergMarquardtAlgorithm;

interface

uses
    Classes, SysUtils, Math, Types, Algorithm, Decisions,
    DownhillSimplexAlgorithm;

type
    { What the algorithm asks of its container beyond what the simplex asks:
      the residuals at the point last evaluated. }
    ILevenbergMarquardtServer = interface(IDownhillSimplexServer)
        function GetResidualsNumber(Sender: TComponent): LongInt;
        function GetResidual(Sender: TComponent; index: LongInt): Double;
    end;

    ELevenbergMarquardtAlgorithm = class(Exception);

    TLevenbergMarquardtAlgorithm = class(TAlgorithm)
    protected
        FServer: ILevenbergMarquardtServer;
        FMaxIterations: LongInt;
        { The residuals at ADecision, evaluated - which also writes back what
          each parameter became, limits applied. }
        function Evaluate(ADecision: TFloatDecision): TDoubleDynArray;
    public
        constructor Create(AOwner: TComponent); override;
        procedure AlgorithmRealization; override;
        property Server: ILevenbergMarquardtServer read FServer write FServer;
        { Iterations at most; the default is ample for a few parameters. }
        property MaxIterations: LongInt read FMaxIterations write FMaxIterations;
    end;

implementation

const
    DEFAULT_MAX_ITERATIONS = 200;

function SumOfSquares(const R: TDoubleDynArray): Double;
var
    i: LongInt;
begin
    Result := 0;
    for i := 0 to High(R) do
        Result := Result + Sqr(R[i]);
end;

{ Solves A x = b by Gaussian elimination with partial pivoting. False for a
  singular system. }
function SolveLinear(A: array of TDoubleDynArray; b: TDoubleDynArray;
    out x: TDoubleDynArray): Boolean;
var
    n, i, j, k, Best: LongInt;
    Factor, T: Double;
    Row: TDoubleDynArray;
begin
    n := Length(b);
    SetLength(x, n);
    for k := 0 to n - 1 do
    begin
        Best := k;
        for i := k + 1 to n - 1 do
            if Abs(A[i][k]) > Abs(A[Best][k]) then
                Best := i;
        if Abs(A[Best][k]) < 1e-300 then
            Exit(False);
        if Best <> k then
        begin
            Row := A[k]; A[k] := A[Best]; A[Best] := Row;
            T := b[k]; b[k] := b[Best]; b[Best] := T;
        end;
        for i := k + 1 to n - 1 do
        begin
            Factor := A[i][k] / A[k][k];
            for j := k to n - 1 do
                A[i][j] := A[i][j] - Factor * A[k][j];
            b[i] := b[i] - Factor * b[k];
        end;
    end;
    for i := n - 1 downto 0 do
    begin
        T := b[i];
        for j := i + 1 to n - 1 do
            T := T - A[i][j] * x[j];
        x[i] := T / A[i][i];
    end;
    Result := True;
end;

constructor TLevenbergMarquardtAlgorithm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FMaxIterations := DEFAULT_MAX_ITERATIONS;
end;

function TLevenbergMarquardtAlgorithm.Evaluate(
    ADecision: TFloatDecision): TDoubleDynArray;
var
    i: LongInt;
begin
    FServer.EvaluateDecision(Self, ADecision);
    SetLength(Result, FServer.GetResidualsNumber(Self));
    for i := 0 to High(Result) do
        Result[i] := FServer.GetResidual(Self, i);
    ADecision.Evaluation := SumOfSquares(Result);
end;

procedure TLevenbergMarquardtAlgorithm.AlgorithmRealization;
var
    Current, Trial: TFloatDecision;
    R, TrialR, Grad, Step: TDoubleDynArray;
    J, Normal: array of TDoubleDynArray;
    i, k, q, n, m, Iter: LongInt;
    H, Lambda: Double;
    Converged: Boolean;
begin
    if not Assigned(FServer) then
        raise ELevenbergMarquardtAlgorithm.Create('Server must be assigned...');
    Current := TFloatDecision.Create(nil);
    Trial := nil;
    try
        FServer.FillStartDecision(Self, Current);
        n := Current.ParametersNumber;
        R := Evaluate(Current);
        FServer.UpdateResults(Self, Current);
        m := Length(R);
        Lambda := 1e-3;
        for Iter := 1 to FMaxIterations do
        begin
            if FServer.EndOfCalculation(Self) then
                Break;
            SetLength(J, m);
            for i := 0 to m - 1 do
                SetLength(J[i], n);
            for k := 0 to n - 1 do
            begin
                FreeAndNil(Trial);
                Trial := TFloatDecision(Current.GetCopy);
                H := 1e-6 * Max(1, Abs(Current.Parameters[k]));
                Trial.Parameters[k] := Current.Parameters[k] + H;
                TrialR := Evaluate(Trial);
                //  The step actually taken, which a limit may have shortened -
                //  to nothing, at a limit, where the column is zero.
                H := Trial.Parameters[k] - Current.Parameters[k];
                for i := 0 to m - 1 do
                    if H <> 0 then
                        J[i][k] := (TrialR[i] - R[i]) / H
                    else
                        J[i][k] := 0;
            end;
            SetLength(Normal, n);
            SetLength(Grad, n);
            for k := 0 to n - 1 do
            begin
                SetLength(Normal[k], n);
                Grad[k] := 0;
                for i := 0 to m - 1 do
                    Grad[k] := Grad[k] - J[i][k] * R[i];
                for q := 0 to n - 1 do
                begin
                    Normal[k][q] := 0;
                    for i := 0 to m - 1 do
                        Normal[k][q] := Normal[k][q] + J[i][k] * J[i][q];
                end;
            end;
            for k := 0 to n - 1 do
                Normal[k][k] := Normal[k][k] * (1 + Lambda) + 1e-12;
            if not SolveLinear(Normal, Grad, Step) then
                Break;
            FreeAndNil(Trial);
            Trial := TFloatDecision(Current.GetCopy);
            for k := 0 to n - 1 do
                Trial.Parameters[k] := Current.Parameters[k] + Step[k];
            TrialR := Evaluate(Trial);
            if Trial.Evaluation < Current.Evaluation then
            begin
                Converged := (Current.Evaluation - Trial.Evaluation) <
                    1e-14 * Max(1, Current.Evaluation);
                FreeAndNil(Current);
                Current := Trial;
                Trial := nil;
                R := TrialR;
                FServer.UpdateResults(Self, Current);
                Lambda := Max(Lambda / 3, 1e-12);
                if Converged then
                    Break;
            end
            else
            begin
                Lambda := Lambda * 4;
                if Lambda > 1e12 then
                    Break;
            end;
        end;
        //  THE CALLER LEFT AT THE BEST POINT: the last point evaluated was a
        //  trial - a Jacobian's step or a step refused.
        Evaluate(Current);
    finally
        Trial.Free;
        Current.Free;
    end;
end;

end.
