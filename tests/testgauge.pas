unit TestGauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils, PrometheusClasses,
  PrometheusGauge;

type

  { TTestGauge }

  TTestGauge = class(TTestCase)
  private
    TestGauge: TPrometheusGauge;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCanIncreaseAmount;
    procedure TestCanDecreaseAmount;
    procedure TestCanIncreaseAndDecreaseAmountsForSpecificLabels;
    procedure TestCanSetAmount;
    procedure TestCanSetToCurrentTime;
    procedure TestCanUseTimer;
    procedure TestGaugeIsThreadSafe;
  end;

  { TTestThread }

  TTestThread = class(TThread)
  private
    TestGauge: TPrometheusGauge;
  protected
    procedure Execute; override;
  public
    constructor Create(Gauge: TPrometheusGauge);
  end;

implementation

{ TTestThread }

constructor TTestThread.Create(Gauge: TPrometheusGauge);
begin
  TestGauge := Gauge;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TTestThread.Execute;
var
  I: integer;
begin
  for I := 1 to 5000 do
    TestGauge.Inc;
end;

{ TTestGauge }

procedure TTestGauge.SetUp;
begin
  TestGauge := TPrometheusGauge.Create('test', 'help');
end;

procedure TTestGauge.TearDown;
begin
  TestGauge.Free;
end;

procedure TTestGauge.TestHookUp;
begin
  AssertEquals('test', TestGauge.Name);
end;

procedure TTestGauge.TestCanIncreaseAmount;
begin
  AssertEquals(0, TestGauge.GetMetricAsDouble);
  TestGauge.Inc;
  AssertEquals(1, TestGauge.GetMetricAsDouble);
  TestGauge.Inc(41);
  AssertEquals(42, TestGauge.GetMetricAsDouble);
end;

procedure TTestGauge.TestCanDecreaseAmount;
begin
  AssertEquals(0, TestGauge.GetMetricAsDouble);
  TestGauge.Dec;
  AssertEquals(-1, TestGauge.GetMetricAsDouble);
  TestGauge.Dec(41);
  AssertEquals(-42, TestGauge.GetMetricAsDouble);
end;

procedure TTestGauge.TestCanIncreaseAndDecreaseAmountsForSpecificLabels;
begin
  TestGauge.WithLabels(['foo', 'yes']).Inc(10);
  TestGauge.WithLabels(['bar', 'yes']).Inc(20);
  TestGauge.WithLabels(['foo', 'no', 'bar', 'yes']).Inc(30);

  TestGauge.WithLabels(['foo', 'yes']).Dec(5);
  TestGauge.WithLabels(['bar', 'yes']).Dec(5);
  TestGauge.WithLabels(['foo', 'no', 'bar', 'yes']).Dec(5);

  AssertEquals(5, TestGauge.WithLabels(['foo', 'yes']).GetMetricAsDouble);
  AssertEquals(15, TestGauge.WithLabels(['bar', 'yes']).GetMetricAsDouble);
  AssertEquals(25, TestGauge.WithLabels(['foo', 'no', 'bar', 'yes']).GetMetricAsDouble);
end;

procedure TTestGauge.TestCanSetAmount;
begin
  TestGauge.SetAmount(42);
  AssertEquals(42, TestGauge.GetMetricAsDouble);
  TestGauge.WithLabels(['foo', 'yes']).SetAmount(128);
  AssertEquals(128, TestGauge.WithLabels(['foo', 'yes']).GetMetricAsDouble);
end;

procedure TTestGauge.TestCanSetToCurrentTime;
var
  Timestamp: integer;
begin
  TestGauge.SetToCurrentTime;
  Timestamp := DateTimeToUnix(Now);
  AssertTrue(TestGauge.GetMetricAsDouble > Timestamp - 2);
  AssertTrue(TestGauge.GetMetricAsDouble < Timestamp + 2);
end;

procedure TTestGauge.TestCanUseTimer;
const
  OFFSET = 50;
var
  Children: TPrometheusGaugeChildren;
  StartTime: double;
begin
  StartTime := DateTimeToUnix(Now) - OFFSET;
  Children := TestGauge.WithLabels(['foo', 'bar']);
  Children.StartTimer(StartTime);
  TestGauge.StartTimer(StartTime);
  TestGauge.SetDuration;
  Children.SetDuration;

  AssertTrue('Must be greater than zero',
    TestGauge.GetMetricAsDouble > 0);

  AssertTrue('Must be around OFFSET',
    TestGauge.GetMetricAsDouble - OFFSET < 5);

  AssertTrue('Children must be greater than zero',
    Children.GetMetricAsDouble > 0);

  AssertTrue('Children must be around OFFSET',
    Children.GetMetricAsDouble - OFFSET < 5);
end;

procedure TTestGauge.TestGaugeIsThreadSafe;
var
  I: integer;
  Threads: array[1..16] of TTestThread;
begin
  for I := Low(Threads) to High(Threads) do
    Threads[I] := TTestThread.Create(TestGauge);

  for I := Low(Threads) to High(Threads) do
    Threads[I].WaitFor;

  for I := Low(Threads) to High(Threads) do
    Threads[I].Free;

  AssertEquals(80000, TestGauge.GetMetricAsDouble);
end;

initialization

  RegisterTest(TTestGauge);
end.
