unit TestGauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusClasses, DateUtils;

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
  AssertEquals(0, TestGauge.GetMetric);
  TestGauge.Inc;
  AssertEquals(1, TestGauge.GetMetric);
  TestGauge.Inc(41);
  AssertEquals(42, TestGauge.GetMetric);
end;

procedure TTestGauge.TestCanDecreaseAmount;
begin
  AssertEquals(0, TestGauge.GetMetric);
  TestGauge.Dec;
  AssertEquals(-1, TestGauge.GetMetric);
  TestGauge.Dec(41);
  AssertEquals(-42, TestGauge.GetMetric);
end;

procedure TTestGauge.TestCanIncreaseAndDecreaseAmountsForSpecificLabels;
begin
  TestGauge.WithLabels(['foo', 'yes']).Inc(10);
  TestGauge.WithLabels(['bar', 'yes']).Inc(20);
  TestGauge.WithLabels(['foo', 'no', 'bar', 'yes']).Inc(30);

  TestGauge.WithLabels(['foo', 'yes']).Dec(5);
  TestGauge.WithLabels(['bar', 'yes']).Dec(5);
  TestGauge.WithLabels(['foo', 'no', 'bar', 'yes']).Dec(5);

  AssertEquals(5, TestGauge.WithLabels(['foo', 'yes']).GetMetric);
  AssertEquals(15, TestGauge.WithLabels(['bar', 'yes']).GetMetric);
  AssertEquals(25, TestGauge.WithLabels(['foo', 'no', 'bar', 'yes']).GetMetric);
end;

procedure TTestGauge.TestCanSetAmount;
begin
  TestGauge.SetAmount(42);
  AssertEquals(42, TestGauge.GetMetric);
  TestGauge.WithLabels(['foo', 'yes']).SetAmount(128);
  AssertEquals(128, TestGauge.WithLabels(['foo', 'yes']).GetMetric);
end;

procedure TTestGauge.TestCanSetToCurrentTime;
var
  Timestamp: integer;
begin
  TestGauge.SetToCurrentTime;
  Timestamp := DateTimeToUnix(Now);
  AssertTrue(TestGauge.GetMetric > Timestamp - 2);
  AssertTrue(TestGauge.GetMetric < Timestamp + 2);
end;

procedure TTestGauge.TestCanUseTimer;
var
  Children: TPrometheusGaugeChildren;
begin
  Children := TestGauge.WithLabels(['foo', 'bar']);
  Children.StartTimer;
  Sleep(10);
  TestGauge.StartTimer;
  Sleep(11);
  TestGauge.SetDuration;
  Sleep(10);
  Children.SetDuration;

  AssertTrue('Must be greater or equal to 0.01', TestGauge.GetMetric >= 0.01);
  AssertTrue('Must be less than 0.02', TestGauge.GetMetric < 0.02);
  AssertTrue('Children must be greater or equal to 0.03', Children.GetMetric >= 0.03);
  AssertTrue('Children must be less than 0.04', Children.GetMetric < 0.04);
end;

initialization

  RegisterTest(TTestGauge);
end.

