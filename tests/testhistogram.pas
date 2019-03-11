unit TestHistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusClasses;

type

  { TTestHistogram }

  TTestHistogram = class(TTestCase)
  private
    Histogram: TPrometheusHistogram;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCanObserveResult;
  end;

implementation

procedure TTestHistogram.SetUp;
begin
  Histogram := TPrometheusHistogram.Create('test', 'help');
end;

procedure TTestHistogram.TearDown;
begin
  Histogram.Free;
end;

procedure TTestHistogram.TestHookUp;
begin
  AssertTrue(Assigned(Histogram));
  AssertEquals('test', Histogram.Name);
  AssertEquals('help', Histogram.Description);
end;

procedure TTestHistogram.TestCanObserveResult;
const
  // Default Buckets: (0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10)
  EXPECTED_RESULTS: array[1..11] of integer = (0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2);
var
  Child: TPrometheusHistogramChildren;
  I: integer;
  Message: string;
  UpperBound: double;
  Expected, Actual: integer;
begin
  Child := Histogram.WithLabels(['hello', 'world']);
  Child.Observe(5);
  Child.Observe(5);

  AssertEquals(2, Child.GetMetric.Counter);
  AssertEquals(10, Child.GetMetric.TotalSum);
  AssertEquals(11, Length(Child.GetMetric.BucketCounters));
  for I := Low(Child.GetMetric.BucketCounters) to High(Child.GetMetric.BucketCounters) do
  begin
    Expected := EXPECTED_RESULTS[I + 1];
    UpperBound := Child.GetMetric.BucketCounters[I].UpperInclusiveBound;
    Actual := Child.GetMetric.BucketCounters[I].Counter;
    Message := Format('%f must be %d and it was %d', [UpperBound, Expected, Actual]);
    AssertEquals(Message, Expected, Actual);
  end;
end;

initialization

  RegisterTest(TTestHistogram);
end.
