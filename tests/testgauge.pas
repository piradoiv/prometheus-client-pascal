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
    procedure TestCanCreateWithLabels;
    procedure TestCanIncreaseAmount;
    procedure TestCanDecreaseAmount;
    procedure TestCanIncreaseAndDecreaseAmountsForSpecificLabels;
    procedure TestCanSetAmount;
    procedure TestCanSetToCurrentTime;
  end;

implementation

procedure TTestGauge.SetUp;
begin
  TestGauge := TPrometheusGauge.Create('test', 'help', ['foo', 'bar']);
end;

procedure TTestGauge.TearDown;
begin
  TestGauge.Free;
end;

procedure TTestGauge.TestHookUp;
begin
  AssertEquals('test', TestGauge.Name);
end;

procedure TTestGauge.TestCanCreateWithLabels;
begin
  AssertEquals(2, TestGauge.Labels.Count);
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
  TestGauge.Inc(['foo', 'yes'], 10);
  TestGauge.Inc(['bar', 'yes'], 20);
  TestGauge.Inc(['foo', 'no', 'bar', 'yes'], 30);

  TestGauge.Dec(['foo', 'yes'], 5);
  TestGauge.Dec(['bar', 'yes'], 5);
  TestGauge.Dec(['foo', 'no', 'bar', 'yes'], 5);

  AssertEquals(5, TestGauge.GetMetric(['foo', 'yes']));
  AssertEquals(15, TestGauge.GetMetric(['bar', 'yes']));
  AssertEquals(25, TestGauge.GetMetric(['foo', 'no', 'bar', 'yes']));
end;

procedure TTestGauge.TestCanSetAmount;
begin
  TestGauge.SetAmount(42);
  AssertEquals(42, TestGauge.GetMetric);
  TestGauge.SetAmount(['foo', 'yes'], 128);
  AssertEquals(128, TestGauge.GetMetric(['foo', 'yes']));
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

initialization

  RegisterTest(TTestGauge);
end.

