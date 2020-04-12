unit bounding_box_server_form;

interface

uses
{$IF NOT DEFINED(FPC)}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons, System.StrUtils, System.Types,
{$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    StdCtrls, StrUtils,
{$ENDIF}
    Contnrs, RunningThread, SimpMath, Math3d, downhill_simplex_handler;

{$ASSERTIONS ON}

type
    { TBoundingBoxServerForm }
    { Demonstrates integration of algorithm into application by implementing
      special server interface. }
    TBoundingBoxServerForm = class(TForm)
        ComboBoxFiles: TComboBox;
        CheckBoxRandomData: TCheckBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        EditInitialAngleStep: TEdit;
        EditFinalTolerance: TEdit;
        EditExitDerivate: TEdit;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Memo1: TMemo;
        Memo2: TMemo;
        BitBtnFindMinimumBoundingBox: TBitBtn;
        ButtonRandomTest: TButton;
        ButtonBruteForce: TButton;
        ButtonStop: TButton;
        procedure ComboBoxFilesChange(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure PostProcessStatistics;
        procedure ButtonBruteForceClick(Sender: TObject);
        procedure ButtonStopClick(Sender: TObject);
        procedure ButtonRandomTestClick(Sender: TObject);

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

        function GetInitialAngleStep: Double;
        procedure StopComputing;
        { Prints final results among a few runs. }
        procedure OutputResults(PointCloud: TList);
        { Computes minimum box volume starting from a few initial points. }
        procedure FindGlobalMinVolume;
        { Waits until all runners finish computing. }
        procedure WaitForRunnerFinishing(Runners: TComponentList);
        { Displays computation results and removes container.
          Should be member of form because works with form controls.
          Removes handler from FHandlers list. }
        procedure OuputGlobalMinVolume(Handler: TDownHillSimplexHandler);
        { Displays computation results of single run.
          Should be member of form because works with form controls.
          Removes handler from FHandlers list. }
        procedure OuputMinVolume(Handler: TDownHillSimplexHandler);
        { Displays computation results of single run of brute force search. }
        procedure OuputBruteForce(Handler: TDownHillSimplexHandler);
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
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
    end;

var
    BoundingBoxServerForm: TBoundingBoxServerForm;

implementation

{$R *.dfm}

procedure SortUp(var S1, S2, S3: Double);
var
    Tmp: Double;
begin
    if S2 < S1 then
    begin
        Tmp := S1;
        S1 := S2;
        S2 := Tmp;
    end;
    if S3 < S2 then
    begin
        Tmp := S2;
        S2 := S3;
        S3 := Tmp;
        if S2 < S1 then
        begin
            Tmp := S1;
            S1 := S2;
            S2 := Tmp;
        end;
    end;
end;

function StrToValue(Str: String; var Value: double): Boolean;
var
    Code: Integer;
begin
    Result := True;
    if Pos(',', Str) > 0 then
        Str[Pos(',', Str)] := '.';
    if Pos('°', Str) > 0 then
        Str[Pos('°', Str)] := ' ';
    Str := Trim(Str);
    Val(Str, Value, Code);
    if Code <> 0 then
    begin
        Result := False;
    end;
end;

{ TBoundingBoxServerForm }

procedure TBoundingBoxServerForm.FormCreate(Sender: TObject);
var
    SearchRec: TSearchRec;
    Ext: string;
begin
    { Reads file list from the directory adjacent to the program directory. }
    FFilePath := ExtractFilePath(ParamStr(0));
    FFilePath := IncludeTrailingPathDelimiter(FFilePath) + '..' + PathDelim;
    FFilePath := ExpandFileName(FFilePath) + 'Models' + PathDelim;
    ComboBoxFiles.Items.Clear;
    if FindFirst(FFilePath + '*.*', faAnyFile, SearchRec) = 0 then
    begin
        repeat
            Ext := LowerCase(ExtractFileExt(SearchRec.Name));
            if (Ext = '.obj') then
            begin
                ComboBoxFiles.Items.Add(SearchRec.Name);
            end;
        until FindNext(SearchRec) <> 0;
    end;
    ComboBoxFiles.ItemIndex := 0;
    { For first load. }
    FReloadPointCloud := True;
end;

procedure TBoundingBoxServerForm.FreeSinglePassRunner;
begin
    if FRunner <> nil then
    begin
        FRunner.Wait;
        FRunner.Free;
        FRunner := nil;
    end;
end;

constructor TBoundingBoxServerForm.Create(AOwner: TComponent);
begin
    { Must be created before inherited constructor which causes initializing
      other components. Keeps ownership and destroys all collection items. }
    FHandlers := TComponentList.Create(True);
    FComputationTime := TComputationTime.Create;
    inherited Create(AOwner);
end;

destructor TBoundingBoxServerForm.Destroy;
begin
    FreeSinglePassRunner;
    FComputationTime.Free;
    FHandlers.Free;
    inherited;
end;

function TBoundingBoxServerForm.CreateHandler(Alpha, Beta, Gamma: Double;
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

function TBoundingBoxServerForm.GetInitialAngleStep: Double;
begin
    { This suppresses useless hints in Lazarus. }
    Result := 37;
    if not StrToValue(EditInitialAngleStep.Text, Result) then
    begin
        Result := 37; //default value
    end;
end;

procedure TBoundingBoxServerForm.FormDestroy(Sender: TObject);
begin
    StopComputing;
end;

procedure TBoundingBoxServerForm.ComboBoxFilesChange(Sender: TObject);
begin
    FReloadPointCloud := True;
end;

procedure TBoundingBoxServerForm.OuputMinVolume(Handler: TDownHillSimplexHandler);
var
    Matr: TMatrix;
    Vector: T3Vector;
begin
    with Handler do
    begin
        FOptiResultBoxVolume := BoxVolume;
        FOptiResultBoxMaxCoords := BoxMaxCoords;
        FOptiResultBoxMinCoords := BoxMinCoords;

        { Displays final angles. }
        Memo1.Lines.Add('Final angles       :' + Format(' %10.4f %10.4f %10.4f',
            [Alpha, Beta, Gamma]));

        { Displays final vector. }
        Matr := GetRotationMatrix(Alpha, Beta, Gamma);
        { Rotates and displays etalon unit vector. }
        Vector[1] := 1; Vector[2] := 0; Vector[3] := 0;
        MulVectMatr(Matr, Vector);
        Memo1.Lines.Add('Final vector       :' +
            Format(' %10.4f %10.4f %10.4f', [Vector[1], Vector[2], Vector[3]]));

        OutputResults(PointCloud);
    end;
    { Removes and frees container. }
    FHandlers.Remove(Handler);
end;

procedure TBoundingBoxServerForm.BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
var
    { This "handler" instance is used to demonstrate execution of algorithm
      in separate thread by visual component TRunner attached to the form. }
    Handler: TDownHillSimplexHandler;
    PointCloud: TPointCloud;
    InitialBoxVolume: Double;
begin
    FStop := False;
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;

    if CheckBoxRandomData.Checked then
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

procedure TBoundingBoxServerForm.PostProcessStatistics;
const
    Criterion01 = 0.001;
    // criterion for relative deviation pass/fail; e.g. 0.0 1 => 0.1%
    Criterion1 = 0.01;
    // criterion for relative deviation pass/fail; e.g. 0.01 => 1%

var
    i: Integer;
    Pos, Code: Integer;
    String1, String2, Rate: string;
    Value: Single;
    MinVolume, Deviation, TotalTime, X, Y, Z: Double;
    PassCount01, FailCount01, PassCount1, FailCount1, TotalTimeCount: Integer;
    StringList: TStringList;
    Passed: Boolean;
begin
    MinVolume := 1e20;
    { Gets optimized MinVolume. }
    for i := 0 to Memo2.Lines.Count - 1 do
    begin
        String1 := Trim(Memo2.Lines[i]);
        Pos := PosEx(' ', String1, 1);
        if Pos > 0 then
        begin
            String1 := Trim(Copy(String1, Pos, 1024));
            Pos := PosEx(' ', String1, 1);
            if Pos > 0 then
            begin
                String2 := Trim(Copy(String1, Pos, 1024));
                String1 := Trim(Copy(String1, 1, Pos - 1));
                String1 := StringReplace(String1, ',', '.', [rfReplaceAll]);
                Val(String1, Value, Code);
                if Code = 0 then
                begin
                    if Value < MinVolume then
                    begin
                        MinVolume := Value;
                        Pos := PosEx('(', String2, 1);
                        String2 := Trim(Copy(String2, Pos + 1, 1024));
                        Pos := PosEx(')', String2, 1);
                        String2 := Trim(Copy(String2, 1, Pos - 1));
                        String2 := StringReplace(String2, ',', '.', [rfReplaceAll]);
                        Pos := PosEx(' ', String2, 1);
                        String1 := Trim(Copy(String2, 1, Pos - 1));
                        Val(String1, X, Code);
                        String2 := Trim(Copy(String2, Pos + 1, 1024));
                        Pos := PosEx(' ', String2, 1);
                        String1 := Trim(Copy(String2, 1, Pos - 1));
                        Val(String1, Y, Code);
                        String1 := Trim(Copy(String2, Pos + 1, 1024));
                        Val(String1, Z, Code);
                    end;
                end;
            end;
        end;
    end;
    { Get optimized Volume Pass/Fail statistics. }
    PassCount01 := 0;
    FailCount01 := 0;
    PassCount1 := 0;
    FailCount1 := 0;
    Memo2.Lines.BeginUpdate;
    StringList := TStringList.Create;
    StringList.Duplicates := dupAccept;
    if MinVolume > 0 then
    begin
        for i := 0 to Memo2.Lines.Count - 1 do
        begin
            String1 := Trim(Memo2.Lines[i]);
            Pos := PosEx(' ', String1, 1);
            if Pos > 0 then
            begin
                String1 := Trim(Copy(String1, Pos, 1024));
                Pos := PosEx(' ', String1, 1);
                if Pos > 0 then
                begin
                    String1 := Trim(Copy(String1, 1, Pos - 1));
                    String1 := StringReplace(String1, ',', '.', [rfReplaceAll]);
                    Val(String1, Value, Code);
                    if Code = 0 then
                    begin
                        Deviation := (Value - MinVolume) / MinVolume;
                        Rate := 'Pass';
                        Passed := True;
                        if Deviation < Criterion1 then
                            Inc(PassCount1)
                        else
                        begin
                            Inc(FailCount1);
                            Rate := 'F1';
                            Passed := False;
                        end;
                        if Deviation < Criterion01 then
                            Inc(PassCount01)
                        else
                        begin
                            Inc(FailCount01);
                            Rate := 'F01';
                            Passed := False;
                        end;
                        if not Passed or FShowPassed then
                        begin
                            { Only "failed" tests are added to the resulting list. }
                            StringList.Add(Memo2.Lines[i] + ' - ' + Rate);
                        end;
                    end;
                end;
            end;
        end;
    end;
    Memo2.Clear;
    Memo2.Text := StringList.Text;
    Memo2.Lines.EndUpdate;

    { Get Time to proccess. }
    TotalTime := 0;
    TotalTimeCount := 0;
    for i := 0 to Memo2.Lines.Count - 1 do
    begin
        String1 := Trim(Memo2.Lines[i]);
        Pos := PosEx('---', String1, 1);
        if Pos > 0 then
        begin
            String1 := Trim(Copy(String1, Pos + 3, 1024));
            Pos := PosEx('--', String1, 1);
            if Pos > 0 then
            begin
                String1 := Trim(Copy(String1, 1, Pos - 1));
                String1 := StringReplace(String1, ',', '.', [rfReplaceAll]);
                Val(String1, Value, Code);
                if Code = 0 then
                begin
                    TotalTime := TotalTime + Value;
                    Inc(TotalTimeCount);
                end;
            end;
        end;
    end;
    if TotalTimeCount > 0 then
        TotalTime := TotalTime / TotalTimeCount
    else
        TotalTime := 0;

    if (PassCount01 > 0) then
    begin
        SortUp(X, Y, Z);
        Memo1.Lines.Add('');
        Memo1.Lines.Add('');
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Minimum Volume     : ' +
            Format('%.4f (%6.3f %6.3f %6.3f)', [MinVolume, X, Y, Z]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Deviation := (FOptiResultBoxVolume - MinVolume) / MinVolume * 100;
        Memo1.Lines.Add('Calculation Delta  : ' +
            Format('%.4f (%.2f%%)', [(FOptiResultBoxVolume - MinVolume), Deviation]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 0.1%     : ' + Format('%.4f%%',
            [PassCount01 / (PassCount01 + FailCount01) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 1%       : ' + Format('%.4f%%',
            [PassCount1 / (PassCount1 + FailCount1) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Time Average      : ' + Format('%.4f', [TotalTime]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
    end
    else
    begin
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Someting''s was wrong');
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
    end;
end;

procedure TBoundingBoxServerForm.OuputBruteForce(Handler: TDownHillSimplexHandler);
var
    Line: string;
    DeltaVolume: Single;
    BoxSizes: TDoubleVector3;
begin
    if not FStop then
    begin
        { Computes difference in volumes calculated
          for original and rotated orientation. }
        with Handler do
        begin
            DeltaVolume := (BoxVolume - FGlobalMinVolume);
            { Computes lengths of edges of bounding box. }
            BoxSizes[1] := BoxMaxCoords[1] - BoxMinCoords[1];
            BoxSizes[2] := BoxMaxCoords[2] - BoxMinCoords[2];
            BoxSizes[3] := BoxMaxCoords[3] - BoxMinCoords[3];
            { Sorts edges. }
            SortUp(BoxSizes[1], BoxSizes[2], BoxSizes[3]);
            Line :=
                Format(
                ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
                [DeltaVolume, BoxVolume, BoxSizes[1],
                BoxSizes[2], BoxSizes[3], Alpha, Beta, Gamma,
                PointCloud.Alpha, PointCloud.Beta, PointCloud.Gamma,
                ComputationTime.Time, CycleCount, EvaluationCount, RestartCount]);
            if DeltaVolume > FMaxDeltaVolume then
            begin
                FMaxDeltaVolume := DeltaVolume;
                FMaxBoxSizes[1] := BoxSizes[1];
                FMaxBoxSizes[2] := BoxSizes[2];
                FMaxBoxSizes[3] := BoxSizes[3];
                SortUp(FMaxBoxSizes[1], FMaxBoxSizes[2],
                    FMaxBoxSizes[3]);
            end;
            if DeltaVolume < FMinDeltaVolume then
            begin
                FMinDeltaVolume := DeltaVolume;
                FMinBoxSizes[1] := BoxSizes[1];
                FMinBoxSizes[2] := BoxSizes[2];
                FMinBoxSizes[3] := BoxSizes[3];
                SortUp(FMinBoxSizes[1], FMinBoxSizes[2],
                    FMinBoxSizes[3]);
            end;
            Memo2.Lines.Add(Line);
            Label2.Caption :=
                Format(
                'MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)',
                [FMinDeltaVolume, FMinBoxSizes[1], FMinBoxSizes[2],
                FMinBoxSizes[3], FMaxDeltaVolume, FMaxBoxSizes[1],
                FMaxBoxSizes[2], FMaxBoxSizes[3]]);
        end;
    end;
    { Removes and frees inserted container. }
    FHandlers.Remove(Handler);
end;

procedure TBoundingBoxServerForm.ButtonBruteForceClick(Sender: TObject);
const
    Steps = 2;
var
    x, y, z: Integer;
    Alpha, Beta, Gamma: Single;
    Handler: TDownHillSimplexHandler;
    RunId: Integer;
    PointCloud: TPointCloud;
    Runner: TRunner;
    ThreadPool: TRunnerPool;
begin
    FShowPassed := True;
    FStop := False;
    { Initializes global minimum parameters. }
    FMaxDeltaVolume := -1.0e20;
    FMinDeltaVolume := 1.0e20;
    { Adds space. }
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    Application.ProcessMessages;

    { Computes optimized volume and box sizes. }
    FindGlobalMinVolume;

    ThreadPool := TRunnerPool.Create;
    RunId := 1;
    { Does the test for brute force orientation. }
    for x := 0 to (179 div Steps) do
        for y := 0 to (179 div Steps) do
            for z := 0 to (179 div Steps) do
            begin
                if not FStop then
                begin
                    Alpha := x * Steps;
                    Beta := y * Steps;
                    Gamma := z * Steps;
                    { Loads data and rotates them by given angles. }
                    PointCloud := LoadPointCloud(Alpha, Beta, Gamma, False);
                    { Creates optimization container, which will be executed by separated thread.
                      Handler owns point cloud, don't release it! }
                    Handler :=
                        CreateHandler(0, 0, 0, GetInitialAngleStep,
                        False, RunId, PointCloud, True);
                    { Searches for free runner. Synchronous calls are processed internally. }
                    Runner := ThreadPool.GetFreeRunner;
                    { OuputBruteForce removes hanlder from FHandlers list. }
                    Handler.HandlerOutputProcedure := OuputBruteForce;
                    { Assign runner procedures. }
                    { Executes optimization method in separated thread. This method
                      should not modify any data except members of container instance. }
                    Runner.OnCompute := Handler.OptimizeBoundingBox;
                    { Displays optimization results, this method is synchronized with
                      main VCL thread. This method can modify any data of the form.
                      Should not remove handler to allow subsequent runs. }
                    Runner.OnOutput := Handler.DisplayOutput;
                    { Starts computation in separate thread. }
                    Runner.Run;
                    Inc(RunId);
                end;
            end;
    PostProcessStatistics;
    ThreadPool.Free;
end;

procedure TBoundingBoxServerForm.ButtonRandomTestClick(Sender: TObject);
var
    x: Integer;
    Line: string;
    Alpha, Beta, Gamma: Single;
    DeltaVolume: Single;
    BoxSizes: TDoubleVector3;
    Handler: TDownHillSimplexHandler;
    PointCloud: TPointCloud;
begin
    FStop := False;
    { Initializes global minimum parameters. }
    FMaxDeltaVolume := -1.0e20;
    FMinDeltaVolume := 1.0e20;
    { Adds space. }
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    Application.ProcessMessages;

    { Computes optimized volume and box sizes. }
    FindGlobalMinVolume;

    { Does the test for random orientation. }
    Randomize;
    for x := 0 to 99999 do
    begin
        if not FStop then
        begin
            Alpha := Random * 180;
            Beta := Random * 180;
            Gamma := Random * 180;

            PointCloud := LoadPointCloud(Alpha, Beta, Gamma, False);
            Handler := CreateHandler(0, 0, 0, GetInitialAngleStep,
                False, x, PointCloud, True);
            { Computes minimum volume directly in the calling thread.
              It could be refactored to use thread pool as it was done
              for "brute force" search. }
            Handler.OptimizeBoundingBox;
            if not FStop then
            begin
                with Handler do
                begin
                    DeltaVolume := (BoxVolume - FGlobalMinVolume);
                    BoxSizes[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                    BoxSizes[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                    BoxSizes[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                    SortUp(BoxSizes[1], BoxSizes[2], BoxSizes[3]);
                    Line :=
                        Format(
                        ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
                        [DeltaVolume, BoxVolume, BoxSizes[1],
                        BoxSizes[2], BoxSizes[3], Alpha, Beta,
                        Gamma, Alpha, Beta, Gamma, ComputationTime.Time,
                        CycleCount, EvaluationCount, RestartCount]);
                    if DeltaVolume > FMaxDeltaVolume then
                    begin
                        FMaxDeltaVolume := DeltaVolume;
                        FMaxBoxSizes[1] := BoxSizes[1];
                        FMaxBoxSizes[2] := BoxSizes[2];
                        FMaxBoxSizes[3] := BoxSizes[3];
                        SortUp(FMaxBoxSizes[1], FMaxBoxSizes[2], FMaxBoxSizes[3]);
                    end;
                    if DeltaVolume < FMinDeltaVolume then
                    begin
                        FMinDeltaVolume := DeltaVolume;
                        FMinBoxSizes[1] := BoxSizes[1];
                        FMinBoxSizes[2] := BoxSizes[2];
                        FMinBoxSizes[3] := BoxSizes[3];
                        SortUp(FMinBoxSizes[1], FMinBoxSizes[2], FMinBoxSizes[3]);
                    end;
                    Memo2.Lines.Add(Line);
                    Label2.Caption :=
                        Format(
                        'MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)',
                        [FMinDeltaVolume, FMinBoxSizes[1], FMinBoxSizes[2],
                        FMinBoxSizes[3], FMaxDeltaVolume, FMaxBoxSizes[1],
                        FMaxBoxSizes[2], FMaxBoxSizes[3]]);
                end;
            end;
            { Removes and frees inserted container. }
            FHandlers.Remove(Handler);
            Application.ProcessMessages;
        end;
    end;
    PostProcessStatistics;
end;

procedure TBoundingBoxServerForm.StopComputing;
var
    i: LongInt;
begin
    FStop := True;
    { Stops all handlers. }
    for i := 0 to FHandlers.Count - 1 do
        TDownHillSimplexHandler(FHandlers[i]).Stop;
end;

procedure TBoundingBoxServerForm.ButtonStopClick(Sender: TObject);
begin
    StopComputing;
end;

procedure TBoundingBoxServerForm.OuputGlobalMinVolume(Handler: TDownHillSimplexHandler);
var
    Line: string;
    BoxSizes: TDoubleVector3;
begin
    with Handler do
    begin
        if BoxVolume < FGlobalMinVolume then
        begin
            FGlobalMinVolume := BoxVolume;
            FMaxCoords := BoxMaxCoords;
            FMinCoords := BoxMinCoords;
        end;
        BoxSizes[1] := BoxMaxCoords[1] - BoxMinCoords[1];
        BoxSizes[2] := BoxMaxCoords[2] - BoxMinCoords[2];
        BoxSizes[3] := BoxMaxCoords[3] - BoxMinCoords[3];
        SortUp(BoxSizes[1], BoxSizes[2], BoxSizes[3]);
        Line := Format(
            ' Run %d: %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) --- %7.4f -- %4d -- %4d -- %2d',
            [RunId, BoxVolume, BoxSizes[1], BoxSizes[2], BoxSizes[3],
            Alpha, Beta, Gamma, ComputationTime.Time,
            CycleCount, EvaluationCount,
            RestartCount]);
        Memo1.Lines.Add(Line);
        Application.ProcessMessages;
    end;
    { Removes and frees container. }
    FHandlers.Remove(Handler);
end;

procedure TBoundingBoxServerForm.OutputResults(PointCloud: TList);
var
    BoxSizes: TDoubleVector3;
begin
    Memo1.Lines.Add('');
    if CheckBoxRandomData.Checked then
        Memo1.Lines.Add('Random Points')
    else
        Memo1.Lines.Add('File: ' + ComboBoxFiles.Text);
    Memo1.Lines.Add('No of Points: ' + Format(' %10d', [PointCloud.Count]));
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Minimum Volume     :' + Format(' %10.4f', [FOptiResultBoxVolume]));
    BoxSizes[1] := FOptiResultBoxMaxCoords[1] - FOptiResultBoxMinCoords[1];
    BoxSizes[2] := FOptiResultBoxMaxCoords[2] - FOptiResultBoxMinCoords[2];
    BoxSizes[3] := FOptiResultBoxMaxCoords[3] - FOptiResultBoxMinCoords[3];
    SortUp(BoxSizes[1], BoxSizes[2], BoxSizes[3]);
    Memo1.Lines.Add('Minimum Box        :' + Format(' %10.4f %10.4f %10.4f',
        [BoxSizes[1], BoxSizes[2], BoxSizes[3]]));
    Application.ProcessMessages;
end;

procedure TBoundingBoxServerForm.WaitForRunnerFinishing(Runners: TComponentList);
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

procedure TBoundingBoxServerForm.FindGlobalMinVolume;
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

procedure TBoundingBoxServerForm.FreePointCloud(PointCloud: TPointCloud);
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

function TBoundingBoxServerForm.LoadPointCloud(
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

    Matr := GetRotationMatrix(Alpha, Beta, Gamma);

    if ShowDetails then
    begin
        Memo1.Lines.Add('Initial angles     :' +
            Format(' %10.4f %10.4f %10.4f', [Alpha, Beta, Gamma]));

        { Rotates and displays etalon unit vector. }
        Vector[1] := 1; Vector[2] := 0; Vector[3] := 0;
        MulVectMatr(Matr, Vector);
        Memo1.Lines.Add('Initial vector     :' +
            Format(' %10.4f %10.4f %10.4f', [Vector[1], Vector[2], Vector[3]]));
    end;

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
function TBoundingBoxServerForm.GenerateRandomPointCloud: TPointCloud;
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

end.
