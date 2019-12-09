unit bounding_box_server_form;

interface

uses
  {$IFNDEF Lazarus}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons, System.StrUtils, System.Types,
  {$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    StdCtrls, StrUtils, Windows,
  {$ENDIF}
    Contnrs, RunningThread, SimpMath, Math3d, downhill_simplex_handler;

{$ASSERTIONS ON}

type
    { TBoundingBoxServerForm }
    { Demonstrates the simplest way of integration of algorithm into application. }
    TBoundingBoxServerForm = class(TForm)
        ComboBoxFiles: TComboBox;
        CheckBoxRandomData: TCheckBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Ed_IniParamLenght: TEdit;
        Ed_FinalTolerance: TEdit;
        Ed_ExitDerivate: TEdit;
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

        FShowAlgoDetails: Boolean;
        FShowPassed: Boolean;
        FStop: Boolean;
        { Keeps all instances of "handler" class for asynchronous operations. }
        FHandlers: TComponentList;
        { Minimum volume obtained for a few optimization runs.
          It is used as etalon value. }
        FGlobalMinVolume: Double;
        FMaxDeltaVolume, FMinDeltaVolume: Single;
        FMinDeltaCord, FMaxDeltaCord: TDoubleVector3;
        FMinCoords, FMaxCoords: TDoubleVector3;
        { Optimization results of several algorithm runs. }
        FOptiResultBoxMinCoords, FOptiResultBoxMaxCoords: TDoubleVector3;
        FOptiResultBoxVolume: Double;

        function GetIniParamLenght: Double;
        procedure StopComputing;
        { Prints final results among a few runs. }
        procedure OutputResults(PointCloud: TList);
        { Computes minimum box volume starting from a few initial points. }
        procedure FindGlobalMinVolume;
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
            InitParamLength: Double; ShowDetails: Boolean;
            RunId: Integer; PointCloud: TPointCloud; OwnsPointCloud: Boolean): TDownHillSimplexHandler;

        procedure FreePointCloud(PointCloud: TPointCloud);
        function LoadPointCloud(Alpha, Beta, Gamma: single): TPointCloud;
        function GenerateRandomPointCloud: TPointCloud;

    public
        { Creates FHanlders before other operations. }
        constructor Create(AOwner: TComponent); override;
    end;

var
    BoundingBoxServerForm: TBoundingBoxServerForm;

implementation

{$R *.dfm}

procedure SortUp(var iS1, iS2, iS3: double);
var
    fTmp: double;
begin
    if iS2 < iS1 then
    begin
        fTmp := iS1;
        iS1 := iS2;
        iS2 := fTmp;
    end;
    if iS3 < iS2 then
    begin
        fTmp := iS2;
        iS2 := iS3;
        iS3 := fTmp;
        if iS2 < iS1 then
        begin
            fTmp := iS1;
            iS1 := iS2;
            iS2 := fTmp;
        end;
    end;
end;

function ConvertValue(iConvStr: String; var iValue: double): Boolean;
var
    fCode: Integer;
begin
    Result := True;
    if Pos(',', iConvStr) > 0 then
        iConvStr[Pos(',', iConvStr)] := '.';
    if Pos('°', iConvStr) > 0 then
        iConvStr[Pos('°', iConvStr)] := ' ';
    iConvStr := Trim(iConvStr);
    Val(iConvStr, iValue, fCode);
    if fCode <> 0 then
    begin
        Result := False;
    end;
end;

{ TBoundingBoxServerForm }

procedure TBoundingBoxServerForm.FormCreate(Sender: TObject);
var
    fSearchResult: TSearchRec;
    fExt: string;
begin
    { Reads file list from the directory adjacent to the program directory. }
    FFilePath := ExtractFilePath(ParamStr(0));
    FFilePath := IncludeTrailingPathDelimiter(FFilePath) + '..' + PathDelim;
    FFilePath := ExpandFileName(FFilePath) + 'Models' + PathDelim;
    ComboBoxFiles.Items.Clear;
    if FindFirst(FFilePath + '*.*', faAnyFile, fSearchResult) = 0 then
    begin
        repeat
            fExt := LowerCase(ExtractFileExt(fSearchResult.Name));
            if (fExt = '.obj') then
            begin
                ComboBoxFiles.Items.Add(fSearchResult.Name);
            end;
        until FindNext(fSearchResult) <> 0;
    end;
    ComboBoxFiles.ItemIndex := 0;
    { For first load. }
    FReloadPointCloud := True;
end;

constructor TBoundingBoxServerForm.Create(AOwner: TComponent);
begin
    { Must be created before inherited constructor which causes initializing
      other components. Keeps ownership and destroys all collection items. }
    FHandlers := TComponentList.Create(True);
    inherited Create(AOwner);
end;

function TBoundingBoxServerForm.CreateHandler(Alpha, Beta, Gamma: Double;
    InitParamLength: Double; ShowDetails: Boolean;
    RunId: Integer; PointCloud: TPointCloud; OwnsPointCloud: Boolean): TDownHillSimplexHandler;
var
    fFinalTolerance, fExitDerivate: double;
begin
    { This suppresses useless hints in Lazarus. }
    fFinalTolerance := 0.00001;
    fExitDerivate := 0.5;

    if not ConvertValue(Ed_FinalTolerance.Text, fFinalTolerance) then
    begin
        fFinalTolerance := 0.00001; // default value
    end;
    if not ConvertValue(Ed_ExitDerivate.Text, fExitDerivate) then
    begin
        fExitDerivate := 0.5;       // default value
    end;
    Result := TDownHillSimplexHandler.Create(self, Alpha,
        Beta, Gamma, InitParamLength, fFinalTolerance,
        fExitDerivate, ShowDetails, RunId, PointCloud, OwnsPointCloud);
    { Adds to the list for asynchronous operations. }
    FHandlers.Add(Result);
end;

function TBoundingBoxServerForm.GetIniParamLenght: Double;
begin
    { This suppresses useless hints in Lazarus. }
    Result := 37;
    if not ConvertValue(Ed_IniParamLenght.Text, Result) then
    begin
        Result := 37; //default value
    end;
end;

procedure TBoundingBoxServerForm.FormDestroy(Sender: TObject);
begin
    StopComputing;
    FHandlers.Free;
end;

procedure TBoundingBoxServerForm.ComboBoxFilesChange(Sender: TObject);
begin
    FReloadPointCloud := True;
end;

procedure TBoundingBoxServerForm.OuputMinVolume(Handler: TDownHillSimplexHandler);
begin
    FOptiResultBoxVolume := Handler.BoxVolume;
    FOptiResultBoxMaxCoords := Handler.BoxMaxCoords;
    FOptiResultBoxMinCoords := Handler.BoxMinCoords;
    OutputResults(Handler.PointCloud);
    { Removes and frees container. }
    FHandlers.Remove(Handler);
end;

procedure TBoundingBoxServerForm.BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
var
    Runner: TRunner;
    { This "handler" instance is used to demonstrate execution of algorithm
      in separate thread by visual component TRunner attached to the form. }
    Handler: TDownHillSimplexHandler;
    PointCloud: TPointCloud;
begin
    FShowAlgoDetails := True;
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
        PointCloud := LoadPointCloud(0, 45, 45);
    end;
    { Executes optimization algorithms in separate thread. }
    Runner := TRunner.Create(nil);
    { Creates optimization container, which will be executed by separated thread.
      Handler owns point cloud, don't release it! }
    Handler := CreateHandler(0, 0, 0, GetIniParamLenght, True, 1, PointCloud, True);
    { OuputMinVolume removes hanlder from FHandlers list. }
    Handler.HandlerOutputProcedure := OuputMinVolume;
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
end;

procedure TBoundingBoxServerForm.PostProcessStatistics;
const
    cCriterion01 = 0.001;
    // criterion for relative deviation pass/fail; e.g. 0.0 1 => 0.1%
    cCriterion1 = 0.01;
    // criterion for relative deviation pass/fail; e.g. 0.01 => 1%

var
    x: Integer;
    fP1, fCode: Integer;
    fString, fString2, fRateing: string;
    fValue: Single;
    fMinVolume, fDeviation, fSumTime, fX, fY, fZ: Double;
    fPassCount01, fFailCount01, fPassCount1, fFailCount1, fSumTimeCount: Integer;
    fSL: TStringList;
    Passed: Boolean;
begin
    fMinVolume := 1e20;
    { Gets optimized MinVolume. }
    for x := 0 to Memo2.Lines.Count - 1 do
    begin
        fString := Trim(Memo2.Lines[x]);
        fP1 := PosEx(' ', fString, 1);
        if fP1 > 0 then
        begin
            fString := Trim(Copy(fString, fP1, 1024));
            fP1 := PosEx(' ', fString, 1);
            if fP1 > 0 then
            begin
                fString2 := Trim(Copy(fString, fP1, 1024));
                fString := Trim(Copy(fString, 1, fP1 - 1));
                fString := StringReplace(fString, ',', '.', [rfReplaceAll]);
                Val(fString, fValue, fCode);
                if fCode = 0 then
                begin
                    if fValue < fMinVolume then
                    begin
                        fMinVolume := fValue;
                        fP1 := PosEx('(', fString2, 1);
                        fString2 := Trim(Copy(fString2, fP1 + 1, 1024));
                        fP1 := PosEx(')', fString2, 1);
                        fString2 := Trim(Copy(fString2, 1, fP1 - 1));
                        fString2 := StringReplace(fString2, ',', '.', [rfReplaceAll]);
                        fP1 := PosEx(' ', fString2, 1);
                        fString := Trim(Copy(fString2, 1, fP1 - 1));
                        Val(fString, fX, fCode);
                        fString2 := Trim(Copy(fString2, fP1 + 1, 1024));
                        fP1 := PosEx(' ', fString2, 1);
                        fString := Trim(Copy(fString2, 1, fP1 - 1));
                        Val(fString, fY, fCode);
                        fString := Trim(Copy(fString2, fP1 + 1, 1024));
                        Val(fString, fZ, fCode);
                    end;
                end;
            end;
        end;
    end;
    { Get optimized Volume Pass/Fail statistics. }
    fPassCount01 := 0;
    fFailCount01 := 0;
    fPassCount1 := 0;
    fFailCount1 := 0;
    Memo2.Lines.BeginUpdate;
    fSL := TStringList.Create;
    fSL.Duplicates := dupAccept;
    if fMinVolume > 0 then
    begin
        for x := 0 to Memo2.Lines.Count - 1 do
        begin
            fString := Trim(Memo2.Lines[x]);
            fP1 := PosEx(' ', fString, 1);
            if fP1 > 0 then
            begin
                fString := Trim(Copy(fString, fP1, 1024));
                fP1 := PosEx(' ', fString, 1);
                if fP1 > 0 then
                begin
                    fString := Trim(Copy(fString, 1, fP1 - 1));
                    fString := StringReplace(fString, ',', '.', [rfReplaceAll]);
                    Val(fString, fValue, fCode);
                    if fCode = 0 then
                    begin
                        fDeviation := (fValue - fMinVolume) / fMinVolume;
                        fRateing := 'Pass';
                        Passed := True;
                        if fDeviation < cCriterion1 then
                            Inc(fPassCount1)
                        else
                        begin
                            Inc(fFailCount1);
                            fRateing := 'F1';
                            Passed := False;
                        end;
                        if fDeviation < cCriterion01 then
                            Inc(fPassCount01)
                        else
                        begin
                            Inc(fFailCount01);
                            fRateing := 'F01';
                            Passed := False;
                        end;
                        if not Passed or FShowPassed then
                        begin
                            { Only "failed" tests are added to the resulting list. }
                            fSL.Add(Memo2.Lines[x] + ' - ' + fRateing);
                        end;
                    end;
                end;
            end;
        end;
    end;
    Memo2.Clear;
    Memo2.Text := fSL.Text;
    Memo2.Lines.EndUpdate;

    { Get Time to proccess. }
    fSumTime := 0;
    fSumTimeCount := 0;
    for x := 0 to Memo2.Lines.Count - 1 do
    begin
        fString := Trim(Memo2.Lines[x]);
        fP1 := PosEx('---', fString, 1);
        if fP1 > 0 then
        begin
            fString := Trim(Copy(fString, fP1 + 3, 1024));
            fP1 := PosEx('--', fString, 1);
            if fP1 > 0 then
            begin
                fString := Trim(Copy(fString, 1, fP1 - 1));
                fString := StringReplace(fString, ',', '.', [rfReplaceAll]);
                Val(fString, fValue, fCode);
                if fCode = 0 then
                begin
                    fSumTime := fSumTime + fValue;
                    Inc(fSumTimeCount);
                end;
            end;
        end;
    end;
    if fSumTimeCount > 0 then
        fSumTime := fSumTime / fSumTimeCount
    else
        fSumTime := 0;

    if (fPassCount01 > 0) then
    begin
        SortUp(fX, fY, fZ);
        Memo1.Lines.Add('');
        Memo1.Lines.Add('');
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Minimum Volume    : ' +
            Format('%.4f (%6.3f %6.3f %6.3f)', [fMinVolume, fX, fY, fZ]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        fDeviation := (FOptiResultBoxVolume - fMinVolume) / fMinVolume * 100;
        Memo1.Lines.Add('Calculation Delta : ' +
            Format('%.4f (%.2f%%)', [(FOptiResultBoxVolume - fMinVolume), fDeviation]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 0.1%     : ' + Format('%.4f%%',
            [fPassCount01 / (fPassCount01 + fFailCount01) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 1%       : ' + Format('%.4f%%',
            [fPassCount1 / (fPassCount1 + fFailCount1) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Time Average      : ' + Format('%.4f', [fSumTime]));
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
    fResult: string;
    fDeltaVolume: Single;
    fDeltaCord: TDoubleVector3;
begin
    if not FStop then
    begin
        { Computes difference in volumes calculated
          for original and rotated orientation. }
        with Handler do
        begin
            fDeltaVolume := (BoxVolume - FGlobalMinVolume);
            { Computes lengths of edges of bounding box. }
            fDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
            fDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
            fDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
            { Sorts edges. }
            SortUp(fDeltaCord[1], fDeltaCord[2], fDeltaCord[3]);
            fResult :=
                Format(
                ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
                [fDeltaVolume, BoxVolume,
                 fDeltaCord[1], fDeltaCord[2], fDeltaCord[3],
                 Alpha, Beta, Gamma,
                 PointCloud.Alpha, PointCloud.Beta, PointCloud.Gamma,
                 ComputationTime, CycleCount, EvaluationCount, RestartCount]);
            if fDeltaVolume > fMaxDeltaVolume then
            begin
                fMaxDeltaVolume := fDeltaVolume;
                FMaxDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                FMaxDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                FMaxDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                SortUp(FMaxDeltaCord[1], FMaxDeltaCord[2],
                    FMaxDeltaCord[3]);
            end;
            if fDeltaVolume < fMinDeltaVolume then
            begin
                fMinDeltaVolume := fDeltaVolume;
                FMinDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                FMinDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                FMinDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                SortUp(FMinDeltaCord[1], FMinDeltaCord[2],
                    FMinDeltaCord[3]);
            end;
            Memo2.Lines.Add(fResult);
            Label2.Caption :=
                Format(
                'MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)',
                [fMinDeltaVolume, fMinDeltaCord[1],
                fMinDeltaCord[2], fMinDeltaCord[3],
                fMaxDeltaVolume, fMaxDeltaCord[1],
                fMaxDeltaCord[2], fMaxDeltaCord[3]]);
        end;
    end;
    { Removes and frees inserted container. }
    FHandlers.Remove(Handler);
end;

procedure TBoundingBoxServerForm.ButtonBruteForceClick(Sender: TObject);
const
    cSteps = 2;
var
    x, y, z: Integer;
    fAlpha, fBeta, fGamma: Single;
    Handler: TDownHillSimplexHandler;
    RunId: Integer;
    PointCloud: TPointCloud;
    Runner: TRunner;
    ThreadPool: TRunnerPool;
begin
    FShowAlgoDetails := False;
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
    for x := 0 to (179 div cSteps) do
        for y := 0 to (179 div cSteps) do
            for z := 0 to (179 div cSteps) do
            begin
                if not FStop then
                begin
                    fAlpha := x * cSteps;
                    fBeta := y * cSteps;
                    fGamma := z * cSteps;

                    PointCloud := LoadPointCloud(fAlpha, fBeta, fGamma);
                    { Creates optimization container, which will be executed by separated thread.
                      Handler owns point cloud, don't release it! }
                    Handler := CreateHandler(0, 0, 0, GetIniParamLenght, False, RunId, PointCloud, True);

                    { Executes optimization algorithms in separate thread. }
                    Runner := ThreadPool.GetFreeRunner;

                    Memo1.Lines.Add('%d', [PtrInt(Runner)]);

                    { OuputMinVolume removes hanlder from FHandlers list. }
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
    fResult: string;
    fAlpha, fBeta, fGamma: Single;
    fMinDeltaVolume, fMaxDeltaVolume, fDeltaVolume: Single;
    fMinDeltaCord, fMaxDeltaCord, fDeltaCord: TDoubleVector3;
    Handler: TDownHillSimplexHandler;
    PointCloud: TPointCloud;
begin
    FShowAlgoDetails := False;
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
            fAlpha := Random * 180;
            fBeta := Random * 180;
            fGamma := Random * 180;

            PointCloud := LoadPointCloud(fAlpha, fBeta, fGamma);
            Handler := CreateHandler(0, 0, 0, GetIniParamLenght, False, x, PointCloud, True);
            Handler.OptimizeBoundingBox;
            if not FStop then
            begin
                with Handler do
                begin
                    fDeltaVolume := (BoxVolume - FGlobalMinVolume);
                    fDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                    fDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                    fDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                    SortUp(fDeltaCord[1], fDeltaCord[2], fDeltaCord[3]);
                    fResult :=
                        Format(
                        ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
                        [fDeltaVolume, BoxVolume, fDeltaCord[1],
                        fDeltaCord[2], fDeltaCord[3], Alpha, Beta,
                        Gamma, fAlpha, fBeta, fGamma, ComputationTime,
                        CycleCount, EvaluationCount, RestartCount]);
                    if fDeltaVolume > fMaxDeltaVolume then
                    begin
                        fMaxDeltaVolume := fDeltaVolume;
                        fMaxDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                        fMaxDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                        fMaxDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                        SortUp(fMaxDeltaCord[1], fMaxDeltaCord[2], fMaxDeltaCord[3]);
                    end;
                    if fDeltaVolume < fMinDeltaVolume then
                    begin
                        fMinDeltaVolume := fDeltaVolume;
                        fMinDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                        fMinDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                        fMinDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                        SortUp(fMinDeltaCord[1], fMinDeltaCord[2], fMinDeltaCord[3]);
                    end;
                    Memo2.Lines.Add(fResult);
                    Label2.Caption :=
                        Format(
                        'MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)',
                        [fMinDeltaVolume, fMinDeltaCord[1], fMinDeltaCord[2],
                        fMinDeltaCord[3], fMaxDeltaVolume, fMaxDeltaCord[1],
                        fMaxDeltaCord[2], fMaxDeltaCord[3]]);
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
    fResult: string;
    fBoxSize: TDoubleVector3;
begin
    with Handler do
    begin
        if BoxVolume < FGlobalMinVolume then
        begin
            FGlobalMinVolume := BoxVolume;
            FMaxCoords := BoxMaxCoords;
            FMinCoords := BoxMinCoords;
        end;
        fBoxSize[1] := BoxMaxCoords[1] - BoxMinCoords[1];
        fBoxSize[2] := BoxMaxCoords[2] - BoxMinCoords[2];
        fBoxSize[3] := BoxMaxCoords[3] - BoxMinCoords[3];
        SortUp(fBoxSize[1], fBoxSize[2], fBoxSize[3]);
        fResult := Format(
            ' Run %d: %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) --- %7.4f -- %4d -- %4d -- %2d',
            [RunId, BoxVolume, fBoxSize[1], fBoxSize[2], fBoxSize[3],
            Alpha, Beta, Gamma, ComputationTime, CycleCount, EvaluationCount,
            RestartCount]);
        Memo1.Lines.Add(fResult);
        Application.ProcessMessages;
    end;
    { Removes and frees container. }
    FHandlers.Remove(Handler);
end;

procedure TBoundingBoxServerForm.OutputResults(PointCloud: TList);
var
    fDelta: TDoubleVector3;
begin
    Memo1.Lines.Add('');
    if CheckBoxRandomData.Checked then
        Memo1.Lines.Add('Random Points')
    else
        Memo1.Lines.Add('File: ' + ComboBoxFiles.Text);
    Memo1.Lines.Add('No of Points: ' + Format(' %10d', [PointCloud.Count]));
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Minimum Volume    : ' + Format(' %10.4f', [FOptiResultBoxVolume]));
    fDelta[1] := FOptiResultBoxMaxCoords[1] - FOptiResultBoxMinCoords[1];
    fDelta[2] := FOptiResultBoxMaxCoords[2] - FOptiResultBoxMinCoords[2];
    fDelta[3] := FOptiResultBoxMaxCoords[3] - FOptiResultBoxMinCoords[3];
    SortUp(fDelta[1], fDelta[2], fDelta[3]);
    Memo1.Lines.Add('Minimum Box       : ' + Format(' %10.4f %10.4f %10.4f',
        [fDelta[1], fDelta[2], fDelta[3]]));
    Application.ProcessMessages;
end;

procedure TBoundingBoxServerForm.FindGlobalMinVolume;
const
    cStartAngle5Runs: array[0..4] of TDoubleVector3 = (
        (0, 0, 0),
        (45, 45, 45),
        (-45, -45, -45),
        (45, -45, 45),
        (-45, 45, -45)
        );
const
    cStartAngle9Runs: array[0..8] of TDoubleVector3 = (
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
    i, j: integer;
    fRuns: integer;
    fStartAngle: TDoubleVector3;
    Handler: TDownHillSimplexHandler;
    Runners: TComponentList;
    Runner: TRunner;
    fPerformanceFrequency, fStartTime, fEndTime: Int64;
    FComputationTime: single;
    fMustContinue: Boolean;
    fWaitResult: DWord;
    fHandle: THandle;
    fKeyState: Byte;
    fMsg: TMsg;
    PointCloud: TPointCloud;
begin
    { Loads model data in original orientation. }
    { Data are accessed from different threads.
      That's ok until data aren't changed. }
    PointCloud := LoadPointCloud(0, 0, 0);

    fRuns := 3;
    if PointCloud.Count < 100000 then
        fRuns := 5;
    if PointCloud.Count < 25000 then
        fRuns := 9;
    FGlobalMinVolume := 1e30;

    { Initializing performance counters. }
    fPerformanceFrequency := 0;
    fStartTime := 0;
    fEndTime := 0;
    QueryPerformanceFrequency(fPerformanceFrequency);
    QueryPerformanceCounter(fStartTime);
    Runners := TComponentList.Create(True);
    for i := 0 to fRuns - 1 do
    begin
        if not FStop then
        begin
            if fRuns <= 5 then
                fStartAngle := cStartAngle5Runs[i]
            else
                fStartAngle := cStartAngle9Runs[i];

            { Runs optimization to get the minimum volume.
              CreateHandler adds hanlder to FHandlers list.
              Handler should not own data because they are
              shared between handler instances. It is up to
              this method to release them. See below. }
            Handler :=
                CreateHandler(fStartAngle[1], fStartAngle[2],
                fStartAngle[3], GetIniParamLenght, False, i + 1, PointCloud, False);
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
    { Waits until all runners finish computing. }
    for i := 0 to Runners.Count - 1 do
    begin
        Runner:= TRunner(Runners[i]);
        fHandle := Runner.Handle;
        fMustContinue := True;
        while fMustContinue do
        begin
            { Waits for thread finishing or any input event. }
            fWaitResult := MsgWaitForMultipleObjects(1, fHandle, False,
                INFINITE, QS_ALLINPUT);
            if (fWaitResult = WAIT_OBJECT_0) then
                { Thread was finished, break the loop and wait for next. }
                fMustContinue := False;
            if fWaitResult = WAIT_OBJECT_0 + 1 then
            begin
                { Reads the ESC key's status. }
                fKeyState := GetAsyncKeyState(27);
                Application.ProcessMessages;
                if (fKeyState > 0) then
                begin
                    { ESC was pressed. }
                    Application.Restore;
{$hints off}
                    while PeekMessage(fMsg, 0, WM_KEYFIRST, WM_KEYLAST,
                            PM_REMOVE or PM_NOYIELD) do ;
{$hints on}
                    GetAsyncKeyState(27);
                    { Stops calculation of other threads. }
                    for j := 0 to FHandlers.Count - 1 do
                        TDownHillSimplexHandler(FHandlers[j]).Stop;
                end;
            end;
            if fWaitResult = WAIT_FAILED then
                fMustContinue := False;
        end;
    end;
    { It is not necessarily to free separately all runners,
      because the list owns them and removes them itself. }
    Runners.Free;
    Assert(FHandlers.Count = 0, 'All handlers should be freed by the output method.');

    QueryPerformanceCounter(fEndTime);
    FComputationTime := 0;
    if fPerformanceFrequency <> 0 then
        FComputationTime := (fEndTime - fStartTime) / fPerformanceFrequency;
    FOptiResultBoxVolume := FGlobalMinVolume;
    FOptiResultBoxMaxCoords := FMaxCoords;
    FOptiResultBoxMinCoords := FMinCoords;
    OutputResults(PointCloud);
    Memo1.Lines.Add('Full Calc Time     : ' + Format(' %.4f', [FComputationTime]));
    { Releases model data. }
    FreePointCloud(PointCloud);
end;

procedure TBoundingBoxServerForm.FreePointCloud(PointCloud: TPointCloud);
var
    x: Integer;
    fPoint: p3DVector;
begin
    if PointCloud <> nil then
    begin
        for x := 0 to PointCloud.Count - 1 do
        begin
            fPoint := PointCloud[x];
            Dispose(fPoint);
        end;
        PointCloud.Free;
    end;
end;

function TBoundingBoxServerForm.LoadPointCloud(Alpha, Beta, Gamma: Single): TPointCloud;
type
    TOBJCoord = record // Stores X, Y, Z coordinates
        X, Y, Z: Single;
    end;

    function GetCoords(iString: string): TOBJCoord;
    var
        P, P2, P3: Integer;
        fCoord: TOBJCoord;
    begin
        iString := Trim(Copy(iString, 3, Length(iString)));
        P := Pos(' ', iString);
        P2 := PosEx(' ', iString, P + 1);
        P3 := PosEx(' ', iString, P2 + 1);
        if P3 = 0 then
            P3 := 1000;
        iString := StringReplace(iString, '.', FormatSettings.DecimalSeparator,
            [rfReplaceAll]);
        fCoord.X := StrToFloat(Copy(iString, 1, P - 1));
        fCoord.Y := StrToFloat(Copy(iString, P + 1, P2 - P - 1));
        fCoord.Z := StrToFloat(Copy(iString, P2 + 1, P3 - P2 - 1));
        Result := fCoord;
    end;

    procedure LoadDataFromFile;
    var
        F: TextFile;
        S, FileName: string;
        OriginalPoint: p3DVector;
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
    OriginalPoint, RotatedPoint: p3DVector;
    RotX, RotY, RotZ, Matr: TMatrix;
    Vector: T3Vector;
    i: LongInt;
begin
    if FReloadPointCloud then
    begin
        LoadDataFromFile;
        FReloadPointCloud := False;
    end;

    Result := TPointCloud.Create(Alpha, Beta, Gamma);

    RotX := MatrixRotX(DegToRad(Alpha));
    RotY := MatrixRotY(DegToRad(Beta));
    RotZ := MatrixRotZ(DegToRad(Gamma));
    { Computes rotation matrix. }
    Matr := UnitMatrix;
    Mul3DMatrix(RotZ, Matr, Matr);
    Mul3DMatrix(RotY, Matr, Matr);
    Mul3DMatrix(RotX, Matr, Matr);

    { Rotates data point. }
    for i := 0 to PointCloudCache.Count - 1 do
    begin
        New(RotatedPoint);
        OriginalPoint := p3DVector(PointCloudCache.Items[i]);
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
    Point: p3DVector;
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
