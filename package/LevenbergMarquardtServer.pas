{
This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

Copyright (C) Dmitry Morozov

@abstract(The container Levenberg-Marquardt runs in: the simplex's, with the
residuals added.)

INHERITED, NOT WRITTEN AGAIN. The simplex's container already presents a
caller's parameters as one vector, applies their limits and writes back what
each became, keeps the best point found and honours Stop. A second container
would be a second copy of all of it; this one adds only what the algorithm
needs beyond the simplex - the residuals - through IOptimizedResiduals, which
extends the IOptimizedFunction a caller already implements, and a cap on its
iterations.
}
unit LevenbergMarquardtServer;

interface

uses
    Classes, SysUtils, Decisions, DownhillSimplexServer,
    LevenbergMarquardtAlgorithm, Tools;

type
    { The residuals whose sum of squares the caller's function is: read after
      the point is evaluated. }
    IOptimizedResiduals = interface(IOptimizedFunction)
        function GetResidualsNumber: LongInt;
        function GetResidual(index: LongInt): Double;
    end;

    TLevenbergMarquardtServer = class(TDownhillSimplexServer,
        ILevenbergMarquardtServer)
    protected
        FOptimizedResiduals: IOptimizedResiduals;
        FMaxIterations: Integer;
        procedure CreateAlgorithm; override;
        { The point evaluated, then WHAT EACH PARAMETER BECAME BY THE END OF IT
          written back into the decision. A caller may hold a value back only
          as it evaluates - a pattern applies its rules when its proportions
          are set together - and the derivatives must be of the step actually
          taken: read back before the evaluation, a seed missed a turn by 0.02
          (the module's own test). }
        procedure EvaluateDecision(Sender: TComponent; Decision: TFloatDecision);
        { ILevenbergMarquardtServer }
        function GetResidualsNumber(Sender: TComponent): LongInt;
        function GetResidual(Sender: TComponent; index: LongInt): Double;
        procedure SetOptimizedResiduals(const AValue: IOptimizedResiduals);
    public
        { The function and its residuals, one object: setting this sets
          OptimizedFunction too. }
        property OptimizedResiduals: IOptimizedResiduals
            read FOptimizedResiduals write SetOptimizedResiduals;
        { 0 leaves the algorithm's own default. }
        property MaxIterations: Integer write FMaxIterations;
    end;

implementation

procedure TLevenbergMarquardtServer.SetOptimizedResiduals(
    const AValue: IOptimizedResiduals);
begin
    FOptimizedResiduals := AValue;
    FOptimizedFunction := AValue;
end;

procedure TLevenbergMarquardtServer.CreateAlgorithm;
begin
    UtilizeObject(Algorithm);
    Algorithm := TLevenbergMarquardtAlgorithm.Create(nil);
    with Algorithm as TLevenbergMarquardtAlgorithm do
    begin
        Server := Self;
        if FMaxIterations > 0 then
            MaxIterations := FMaxIterations;
    end;
end;

procedure TLevenbergMarquardtServer.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
var
    i: LongInt;
begin
    inherited EvaluateDecision(Sender, Decision);
    for i := 0 to ParametersNumber - 1 do
        Decision.Parameters[i] := Parameter[i].Value;
end;

function TLevenbergMarquardtServer.GetResidualsNumber(Sender: TComponent): LongInt;
begin
    Result := FOptimizedResiduals.GetResidualsNumber;
end;

function TLevenbergMarquardtServer.GetResidual(Sender: TComponent;
    index: LongInt): Double;
begin
    Result := FOptimizedResiduals.GetResidual(index);
end;

end.
