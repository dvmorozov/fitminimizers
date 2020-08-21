unit optimizing_app;

{$mode delphi}

interface

uses
    Classes, SysUtils, Contnrs, RunningThread, SimpMath, Math3d,
    downhill_simplex_handler;

type
    { Contains all application objects. }
    TOptimizingApp = class
    private
        FFilePath: String;
        { Notifies that selected file has been changed. }
        FReloadPointCloud: Boolean;
        { Contains cached data to avoid redundand file reading. }
        PointCloudCache: TPointCloud;

        FShowPassed: Boolean;
        FStop: Boolean;
        { Keeps all instances of "handler" class for asynchronous operations. }
        FHandlers: TComponentList;
        { Minimum volume obtained for a few optimization runs.
          It is used as etalon value. }
        FGlobalMinVolume: Double;
        { Differences between value of volume obtained at the last step
          and etalon value. }
        FMaxDeltaVolume, FMinDeltaVolume: Single;
        { Mininum and maximum box sizes obtained by different threads
          of "brute force" search. }
        FMinBoxSizes, FMaxBoxSizes: TDoubleVector3;
        { Sets of maximum and minimus coordinates corresponding to globally
          minimum volume. }
        FMinCoords, FMaxCoords: TDoubleVector3;
        { Optimization results of several algorithm runs. }
        FOptiResultBoxMinCoords, FOptiResultBoxMaxCoords: TDoubleVector3;
        FOptiResultBoxVolume: Double;
        { Measures total computation time of multiple runs. }
        FComputationTime: TComputationTime;
        { Single pass runner. }
        FRunner: TRunner;

    public
        procedure StopComputing;
        { Computes minimum box volume starting from a few initial points. }
        procedure FindGlobalMinVolume;
        { Waits until all runners finish computing. }
        procedure WaitForRunnerFinishing(Runners: TComponentList);
        { Creates and returns container instance which should be destroyed by calling method. }
        function CreateHandler(Alpha, Beta, Gamma: Double;
            InitialAngleStep: Double; ShowDetails: Boolean;
            RunId: Integer; PointCloud: TPointCloud;
            OwnsPointCloud: Boolean): TDownHillSimplexHandler;
        { Releases point cloud data. }
        procedure FreePointCloud(PointCloud: TPointCloud);
        { Loads point cloud from file selected by drop-down list.
          Rotates point coordinates by given angles. }
        function LoadPointCloud(Alpha, Beta, Gamma: Single; ShowDetails: Boolean): TPointCloud;
        { Generates point cloud from random data. }
        function GenerateRandomPointCloud: TPointCloud;
        procedure FreeSinglePassRunner;

    public
        { Creates FHanlders before other operations. }
        constructor Create;
        destructor Destroy; override;

        procedure FindMinimumBoundingBox(RandomData: Boolean);

        property ReloadPointCloud: Boolean write FReloadPointCloud;
    end;

var
    OptimizingApp: TOptimizingApp;

implementation

constructor TOptimizingApp.Create;
var
    SearchRec: TSearchRec;
    ListOfFiles: TStringList;
    Ext: string;
begin
    inherited;
    { Must be created before inherited constructor which causes initializing
      other components. Keeps ownership and destroys all collection items. }
    FHandlers := TComponentList.Create(True);
    FComputationTime := TComputationTime.Create;
    { Reads file list from the directory adjacent to the program directory. }
    FFilePath := ExtractFilePath(ParamStr(0));
    FFilePath := IncludeTrailingPathDelimiter(FFilePath) + '..' + PathDelim;
    FFilePath := ExpandFileName(FFilePath) + 'Models' + PathDelim;

    ListOfFiles := TStringList.Create;
    ListOfFiles.Clear;
    if FindFirst(FFilePath + '*.*', faAnyFile, SearchRec) = 0 then
    begin
        repeat
            Ext := LowerCase(ExtractFileExt(SearchRec.Name));
            if (Ext = '.obj') then
            begin
                ListOfFiles.Add(SearchRec.Name);
            end;
        until FindNext(SearchRec) <> 0;
    end;

    { For first load. }
    FReloadPointCloud := True;
end;

destructor TOptimizingApp.Destroy;
begin
    FreeSinglePassRunner;
    FComputationTime.Free;
    FHandlers.Free;
    inherited;
end;

procedure TOptimizingApp.FreeSinglePassRunner;
begin
    if FRunner <> nil then
    begin
        FRunner.Wait;
        FRunner.Free;
        FRunner := nil;
    end;
end;

procedure TOptimizingApp.StopComputing;
var
    i: LongInt;
begin
    FStop := True;
    { Stops all handlers. }
    for i := 0 to FHandlers.Count - 1 do
        TDownHillSimplexHandler(FHandlers[i]).Stop;
end;

procedure TOptimizingApp.FindGlobalMinVolume;
const
    StartAngle5Runs: array[0..4] of TDoubleVector3 = (
        (0, 0, 0),
        (45, 45, 45),
        (-45, -45, -45),
        (45, -45, 45),
        (-45, 45, -45)
        );
const
    StartAngle9Runs: array[0..8] of TDoubleVector3 = (
        (0, 0, 0),
        (45, 45, 45),
        (-45, -45, -45),
        (45, -45, 45),
        (-45, 45, -45),
        (-45, 45, 45),
        (-45, -45, 45),
        (45, -45, -45),
        (45, 45, -45)
        );
var
    i: integer;
    RunCount: integer;
    StartAngles: TDoubleVector3;
    Handler: TDownHillSimplexHandler;
    Runner: TRunner;
    Runners: TComponentList;
    PointCloud: TPointCloud;
begin
    { Loads model data in original orientation. }
    { Data are accessed from different threads.
      That's ok until data aren't changed. }
    PointCloud := LoadPointCloud(0, 0, 0, False);

    RunCount := 3;
    if PointCloud.Count < 100000 then
        RunCount := 5;
    if PointCloud.Count < 25000 then
        RunCount := 9;
    FGlobalMinVolume := 1e30;

    FComputationTime.StartMeasurement;
    Runners := TComponentList.Create(True);
    for i := 0 to RunCount - 1 do
    begin
        if not FStop then
        begin
            if RunCount <= 5 then
                StartAngles := StartAngle5Runs[i]
            else
                StartAngles := StartAngle9Runs[i];

            { Runs optimization to get the minimum volume.
              CreateHandler adds hanlder to FHandlers list.
              Handler should not own data because they are
              shared between handler instances. It is up to
              this method to release them. See below. }
            Handler :=
                CreateHandler(StartAngles[1], StartAngles[2],
                StartAngles[3], GetInitialAngleStep, False, i + 1, PointCloud, False);
            { OuputGlobalMinVolume removes hanlder from FHandlers list. }
            Handler.HandlerOutputProcedure := OuputGlobalMinVolume;
            { Creates runner. }
            Runner := TRunner.Create(nil);
            { Assign computing method. }
            Runner.OnCompute := Handler.OptimizeBoundingBox;
            { Assign output method. It is synchronized with main VCL thread. }
            Runner.OnOutput := Handler.DisplayOutput;
            { Adds runner to the pool. }
            Runners.Add(Runner);
            { Starts execution. }
            Runner.Run;
        end;
    end;

    WaitForRunnerFinishing(Runners);
    { It is not necessarily to free separately all runners,
      because the list owns them and removes them itself. }
    Runners.Free;
    Assert(FHandlers.Count = 0, 'All handlers should be freed by the output method.');

    FComputationTime.EndMeasurement;
    FOptiResultBoxVolume := FGlobalMinVolume;
    FOptiResultBoxMaxCoords := FMaxCoords;
    FOptiResultBoxMinCoords := FMinCoords;
    OutputResults(PointCloud);
    { Prints total computation time. }
    Memo1.Lines.Add('Total Calc Time     : ' + Format(' %.4f', [FComputationTime.Time]));
    { Releases model data. }
    FreePointCloud(PointCloud);
end;

procedure TOptimizingApp.WaitForRunnerFinishing(Runners: TComponentList);
var
    i: LongInt;
    Runner: TRunner;
{$IF NOT DEFINED(FPC)}
    j: LongInt;
    MustContinue: Boolean;
    WaitResult: DWord;
    Handle: THandle;
    KeyState: Byte;
    Msg: TMsg;
{$ENDIF}
begin
    for i := 0 to Runners.Count - 1 do
    begin
        Runner := TRunner(Runners[i]);
{$IF NOT DEFINED(FPC)}
        Handle := Runner.Handle;
        MustContinue := True;
        while MustContinue do
        begin
            { Waits for thread finishing or any input event. }
            WaitResult := MsgWaitForMultipleObjects(1, Handle,
                False, INFINITE, QS_ALLINPUT);
            if (WaitResult = WAIT_OBJECT_0) then
                { Thread was finished, break the loop and wait for next. }
                MustContinue := False;
            if WaitResult = WAIT_OBJECT_0 + 1 then
            begin
                { Reads the ESC key's status. }
                KeyState := GetAsyncKeyState(27);
                Application.ProcessMessages;
                if (KeyState > 0) then
                begin
                    { ESC was pressed. }
                    Application.Restore;
{$hints off}
                    while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST,
                            PM_REMOVE or PM_NOYIELD) do ;
{$hints on}
                    GetAsyncKeyState(27);
                    { Stops calculation of other threads. }
                    for j := 0 to FHandlers.Count - 1 do
                        TDownHillSimplexHandler(FHandlers[j]).Stop;
                end;
            end;
            if WaitResult = WAIT_FAILED then
                MustContinue := False;
        end;
{$ELSE}
        { Lazarus variant is more limited but portable. }
        Runner.Wait;
{$ENDIF}
    end;
end;

function TOptimizingApp.CreateHandler(Alpha, Beta, Gamma: Double;
    InitialAngleStep: Double; ShowDetails: Boolean; RunId: Integer;
    PointCloud: TPointCloud; OwnsPointCloud: Boolean): TDownHillSimplexHandler;
var
    FinalTolerance, ExitDerivate: double;
begin
    { This suppresses useless hints in Lazarus. }
    FinalTolerance := 0.00001;
    ExitDerivate := 0.5;

    if not StrToValue(EditFinalTolerance.Text, FinalTolerance) then
    begin
        FinalTolerance := 0.00001; // default value
    end;
    if not StrToValue(EditExitDerivate.Text, ExitDerivate) then
    begin
        ExitDerivate := 0.5;       // default value
    end;
    Result := TDownHillSimplexHandler.Create(self, Alpha, Beta,
        Gamma, InitialAngleStep, FinalTolerance, ExitDerivate,
        ShowDetails, RunId, PointCloud, OwnsPointCloud);
    { Adds to the list for asynchronous operations. }
    FHandlers.Add(Result);
end;

procedure TOptimizingApp.FreePointCloud(PointCloud: TPointCloud);
var
    x: Integer;
    Point: P3DVector;
begin
    if PointCloud <> nil then
    begin
        for x := 0 to PointCloud.Count - 1 do
        begin
            Point := PointCloud[x];
            Dispose(Point);
        end;
        PointCloud.Free;
    end;
end;

function TOptimizingApp.LoadPointCloud(
    Alpha, Beta, Gamma: Single; ShowDetails: Boolean): TPointCloud;
type
    TOBJCoord = record // Stores X, Y, Z coordinates
        X, Y, Z: Single;
    end;

    function GetCoords(iString: string): TOBJCoord;
    var
        P, P2, P3: Integer;
        Coord: TOBJCoord;
    begin
        iString := Trim(Copy(iString, 3, Length(iString)));
        P := Pos(' ', iString);
        P2 := PosEx(' ', iString, P + 1);
        P3 := PosEx(' ', iString, P2 + 1);
        if P3 = 0 then
            P3 := 1000;
        iString := StringReplace(iString, '.', FormatSettings.DecimalSeparator,
            [rfReplaceAll]);
        Coord.X := StrToFloat(Copy(iString, 1, P - 1));
        Coord.Y := StrToFloat(Copy(iString, P + 1, P2 - P - 1));
        Coord.Z := StrToFloat(Copy(iString, P2 + 1, P3 - P2 - 1));
        Result := Coord;
    end;

    procedure LoadDataFromFile;
    var
        F: TextFile;
        S, FileName: string;
        OriginalPoint: P3DVector;
        Coord: TOBJCoord;
        Vector: T3Vector;
    begin
        FreePointCloud(PointCloudCache);
        PointCloudCache := TPointCloud.Create(0, 0, 0);

        FileName := FFilePath + ComboBoxFiles.Text;
        if FileExists(FileName) then
        begin
            { Data are loaded in original position. }
            AssignFile(F, FileName);
            Reset(F);
            while not (EOF(F)) do
            begin
                Application.ProcessMessages;
                Readln(F, S);
                if (Length(S) >= 2) and (S[1] <> '#') then
                begin
                    S := Uppercase(S);
                    if (S[1] = 'V') and (S[2] = ' ') then
                    begin
                        { Reads vertex data. }
                        New(OriginalPoint);
                        Coord := GetCoords(S);
                        Vector[1] := Coord.X;
                        Vector[2] := Coord.Y;
                        Vector[3] := Coord.Z;

                        OriginalPoint^.FVector := Vector;
                        PointCloudCache.Add(OriginalPoint);
                    end;
                end;
            end;
            CloseFile(F);
        end;
    end;

var
    OriginalPoint, RotatedPoint: P3DVector;
    Matr: TMatrix;
    Vector: T3Vector;
    i: LongInt;
begin
    if FReloadPointCloud then
    begin
        LoadDataFromFile;
        FReloadPointCloud := False;
    end;

    BoundingBoxServerForm.OutputInitialAngles(Alpha, Beta, Gamma, ShowDetails);

    Matr := GetRotationMatrix(Alpha, Beta, Gamma);

    Result := TPointCloud.Create(Alpha, Beta, Gamma);

    { Rotates data point. }
    for i := 0 to PointCloudCache.Count - 1 do
    begin
        New(RotatedPoint);
        OriginalPoint := P3DVector(PointCloudCache.Items[i]);
        Vector := OriginalPoint^.FVector;
        MulVectMatr(Matr, Vector);
        RotatedPoint^.FVector := Vector;
        Result.Add(RotatedPoint);
    end;
end;

{$warnings off}
{$hints off}
function TOptimizingApp.GenerateRandomPointCloud: TPointCloud;
const
    PointCount: LongInt = 10;     //  Number of points in the cloud.
    { Dispersion boundaries. }
    MaxX: double = 0.5;
    MinX: double = -0.5;
    MaxY: double = 0.5;
    MinY: double = -0.5;
    MaxZ: double = 0.5;
    MinZ: double = -0.5;
    { Boundaries along (1,1,1) axis. }
    Max111: double = 10.0;
    Min111: double = -10.0;
var
    i: LongInt;
    Point: P3DVector;
    Translation111: double;
begin
    Randomize;
    Result := TPointCloud.Create(0, 0, 0);

    for i := 0 to PointCount - 1 do
    begin
        New(Point);
        { Coordinates are located mainly along (1,1,1) axis
          with relatively small dispersion. }
        Translation111 := Min111 + Random * (Max111 - Min111);
        Point^.FVector[1] := Translation111 + MinX + Random * (MaxX - MinX);
        Point^.FVector[2] := Translation111 + MinY + Random * (MaxY - MinY);
        Point^.FVector[3] := Translation111 + MinZ + Random * (MaxZ - MinZ);

        Result.Add(Point);
    end;
end;
{$hints on}
{$warnings on}

procedure TOptimizingApp.FindMinimumBoundingBox(RandomData: Boolean);
var
    { This "handler" instance is used to demonstrate execution of algorithm
      in separate thread by visual component TRunner attached to the form. }
    Handler: TDownHillSimplexHandler;
    PointCloud: TPointCloud;
    InitialBoxVolume: Double;
begin
    FStop := False;

    if RandomData then
    begin
        PointCloud := GenerateRandomPointCloud;
    end
    else
    begin
        { Uses model data. }
        PointCloud := LoadPointCloud(0, 45, 45, True);
    end;
    FreeSinglePassRunner;

    { Executes optimization algorithms in separate thread. }
    FRunner := TRunner.Create(nil);
    { Creates optimization container, which will be executed by separated thread.
      Handler owns point cloud, don't release it! }
    Handler := CreateHandler(0, 0, 0, GetInitialAngleStep, True, 1, PointCloud, True);
    { Displays initial box volume. }
    InitialBoxVolume := Handler.GetBoxVolume;
    Memo1.Lines.Add('Initial box volume :' + Format(' %10.4f', [InitialBoxVolume]));
    { OuputMinVolume removes hanlder from FHandlers list. }
    Handler.HandlerOutputProcedure := OuputMinVolume;
    { Assign runner procedures. }
    { Executes optimization method in separated thread. This method
      should not modify any data except members of container instance. }
    FRunner.OnCompute := Handler.OptimizeBoundingBox;
    { Displays optimization results, this method is synchronized with
      main VCL thread. This method can modify any data of the form.
      Should not remove handler to allow subsequent runs. }
    FRunner.OnOutput := Handler.DisplayOutput;
    { Starts computation in separate thread. }
    FRunner.Run;
    { Waits for termination. Otherwise under Linux segmentation fault is caused. }
    FRunner.Wait;
end;

end.

