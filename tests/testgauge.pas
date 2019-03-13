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
  end;

implementation

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
var
  Children: TPrometheusGaugeChildren;
begin
  Children := TestGauge.WithLabels(['foo', 'bar']);
  Children.StartTimer;
  Sleep(11);
  TestGauge.StartTimer;
  Sleep(11);
  TestGauge.SetDuration;
  Sleep(11);
  Children.SetDuration;

  AssertTrue('Must be greater or equal to 0.01', TestGauge.GetMetricAsDouble >= 0.01);
  AssertTrue('Must be less than 0.05', TestGauge.GetMetricAsDouble < 0.05);
  AssertTrue('Children must be greater or equal to 0.025', Children.GetMetricAsDouble >= 0.025);
  AssertTrue('Children must be less than 0.05', Children.GetMetricAsDouble < 0.05);
end;

initialization

  RegisterTest(TTestGauge);
end.

