program BoundingBoxConsole;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Classes,
    SysUtils,
    CustApp,
    int_user_interaction;

type

    { TBoundingBoxConsole }

    TBoundingBoxConsole = class(TCustomApplication, IUserInteraction)
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;

        { IUserInteraction }

        function GetEditExitDerivate: double;
        { Prints final results among a few runs. }
        procedure DisplayPointCloud(PointCloud: TList);
        { Displays computation results and removes container.
          Should be member of form because works with form controls.
          Removes handler from FHandlers list. }
        procedure DisplayGlobalMinVolume(Handler: TBoundingBoxServer;
            BoxSizes: TDoubleVector3);
        { Displays computation results of single run.
          Should be member of form because works with form controls.
          Removes handler from FHandlers list. }
        procedure DisplayCurrentMinVolume(Handler: TBoundingBoxServer);
        { Displays computation results of single run of brute force search. }
        procedure DisplayBruteForceResult(Handler: TBoundingBoxServer;
            DeltaVolume: single; BoxSizes: TDoubleVector3);
        procedure DisplayInitialAngles(Alpha, Beta, Gamma: single; ShowDetails: boolean);
        procedure DisplayListOfModels(ListOfFiles: TStringList);
        procedure DisplayComputationTime(ComputationTime: TComputationTime);
        procedure DisplayInitialBoxVolume(InitialBoxVolume: double);
    end;

    { TBoundingBoxConsole }

    procedure TBoundingBoxConsole.DoRun;
    var
        ErrorMsg: string;
    begin
        // quick check parameters
        ErrorMsg := CheckOptions('h', 'help');
        if ErrorMsg <> '' then
        begin
            ShowException(Exception.Create(ErrorMsg));
            Terminate;
            Exit;
        end;

        // parse parameters
        if HasOption('h', 'help') then
        begin
            WriteHelp;
            Terminate;
            Exit;
        end;

        { add your program here }

        // stop program loop
        Terminate;
    end;

    constructor TBoundingBoxConsole.Create(TheOwner: TComponent);
    begin
        inherited Create(TheOwner);
        StopOnException := True;
    end;

    destructor TBoundingBoxConsole.Destroy;
    begin
        inherited Destroy;
    end;

    procedure TBoundingBoxConsole.WriteHelp;
    begin
        { add your help code here }
        writeln('Usage: ', ExeName, ' -h');
    end;

function TBoundingBoxConsole.GetEditExitDerivate: double;
begin

end;

procedure TBoundingBoxConsole.DisplayPointCloud(PointCloud: TList);
begin

end;

procedure TBoundingBoxConsole.DisplayGlobalMinVolume(Handler: TBoundingBoxServer;
    BoxSizes: TDoubleVector3);
begin

end;

procedure TBoundingBoxConsole.DisplayCurrentMinVolume(Handler: TBoundingBoxServer);
begin

end;

procedure TBoundingBoxConsole.DisplayBruteForceResult(Handler: TBoundingBoxServer;
    DeltaVolume: single; BoxSizes: TDoubleVector3);
begin

end;

procedure TBoundingBoxConsole.DisplayInitialAngles(Alpha, Beta, Gamma: single;
    ShowDetails: boolean);
begin

end;

procedure TBoundingBoxConsole.DisplayListOfModels(ListOfFiles: TStringList);
begin

end;

procedure TBoundingBoxConsole.DisplayComputationTime(
    ComputationTime: TComputationTime);
begin

end;

procedure TBoundingBoxConsole.DisplayInitialBoxVolume(InitialBoxVolume: double);
begin

end;

var
    Application: TBoundingBoxConsole;
begin
    Application := TBoundingBoxConsole.Create(nil);
    Application.Title := 'Bounding Box Console Demo';
    Application.Run;
    Application.Free;
end.
