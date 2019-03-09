program prometheusclienttests;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  TestPrometheusClient,
  TestCounter,
  TestGauge,
  TestExposition,
  fpcunit,
  fpcunitreport;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  private
    FResultsWritter: TCustomResultsWriter;
    Success: boolean;
  protected
    procedure OnFailure(Sender: TObject; ATest: TTest; AFailure: TTestFailure);
    procedure DoTestRun(ATest: TTest); override;
    function GetResultsWriter: TCustomResultsWriter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Application: TMyTestRunner;

procedure TMyTestRunner.OnFailure(Sender: TObject; ATest: TTest;
  AFailure: TTestFailure);
begin
  Success := False;
end;

procedure TMyTestRunner.DoTestRun(ATest: TTest);
begin
  GetResultsWriter.OnAddFailure := @OnFailure;
  inherited DoTestRun(ATest);
end;

function TMyTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if not Assigned(FResultsWritter) then
    FResultsWritter := inherited GetResultsWriter;

  Result := FResultsWritter;
end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  Success := True;
  inherited Create(AOwner);
end;

destructor TMyTestRunner.Destroy;
begin
  inherited Destroy;
  if not Success then
    Halt(1);
end;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'prometheusclienttests';
  Application.Run;
  Application.Free;
end.
