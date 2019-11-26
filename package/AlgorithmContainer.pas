{
This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                    LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                    Facebook: https://www.facebook.com/dmitry.v.morozov

@abstract(Contains definitions of auxiliary classes used for algorithm execution.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/,
Facebook: https://www.facebook.com/dmitry.v.morozov)
}

unit AlgorithmContainer;

interface

uses
    Classes, RunningThread, Algorithm, Tools;

type
    { Defines abstract methods to control any type of algorithms. }
    TAlgorithmContainer = class(TComponent)
    protected
        Algorithm: TAlgorithm;
        {Method creates appropriate environment for executing algorithm,
        create algorithm object and start execution.}
        procedure Running; virtual; abstract;
        {Method is called after finishing execution of algorithm. 
        Can be used to do post processing and displaying results.}
        procedure RunningFinished; virtual; abstract;
        {Descendants override this method to create algorithm object of appropriate type.}
        procedure CreateAlgorithm; virtual; abstract;
        {Method destroys algorithm object.}
        procedure DestroyAlgorithm; virtual; abstract;

    public
        {Method implements actions necessary to abort execution of algorithm.}
        procedure StopAlgorithm; virtual; abstract;
        procedure Run; virtual;
    end;

    { Allows executing algorithm in separate thread. }
    TThreadAlgorithmContainer = class(TAlgorithmContainer)
    protected
        {Object implementing separate thread.
        DestroyAlgorithm must be called after destroying the object.}
        Runner: TRunner;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure Run; override;
    end;

    { This type is deprecated, it is defined only for backward compatibility. }
    TRunningAlgorithmContainer = TThreadAlgorithmContainer;

procedure Register;

implementation

procedure Register;
begin
end;

constructor TRunningAlgorithmContainer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Runner := TRunner.Create(nil);
    Runner.OnCompute := Running;
    Runner.OnOutput := RunningFinished;
end;

destructor TRunningAlgorithmContainer.Destroy;
begin
    UtilizeObject(Runner);
    inherited;
end;

procedure TRunningAlgorithmContainer.Run;
begin
    Runner.Run;
end;

procedure TAlgorithmContainer.Run;
begin
    Running;
    RunningFinished;
end;

initialization
    RegisterClass(TAlgorithmContainer);
    RegisterClass(TRunningAlgorithmContainer);
end.


