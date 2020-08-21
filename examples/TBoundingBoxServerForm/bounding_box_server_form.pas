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

    public
        function GetInitialAngleStep: Double;
        { Prints final results among a few runs. }
        procedure OutputResults(PointCloud: TList);
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
        { TODO: Make ShowDetails private member. }
        procedure OutputInitialAngles(Alpha, Beta, Gamma: Single; ShowDetails: Boolean);
        procedure DisplayListOfModels;
    end;

var
    BoundingBoxServerForm: TBoundingBoxServerForm;

implementation

uses optimizing_app;

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
    OptimizingApp.StopComputing;
end;

procedure TBoundingBoxServerForm.ComboBoxFilesChange(Sender: TObject);
begin
    OptimizingApp.ReloadPointCloud := True;
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
begin
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;

    OptimizingApp.FindMinimumBoundingBox(CheckBoxRandomData.Checked);
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

procedure TBoundingBoxServerForm.OutputInitialAngles(Alpha, Beta, Gamma: Single;
    ShowDetails: Boolean);
var
    Matr: TMatrix;
begin
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
end;

procedure TBoundingBoxServerForm.DisplayListOfModels;
begin
        ComboBoxFiles.ItemIndex := 0;
end;

end.
