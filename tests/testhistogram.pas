unit TestHistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusClasses,
  PrometheusHistogram;

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
    procedure TestCanObserveResultWithLabels;
    procedure TestSomeLabelNamesAreBeingForbidded;
    procedure TestCanCreateHistogramsWithSpecificBuckets;
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
  EXPECTED_RESULTS: array[0..10] of integer = (0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2);
var
  I: integer;
  Message: string;
  UpperBound: double;
  Expected, Actual: integer;
begin
  Histogram.Observe(5);
  Histogram.Observe(5);

  AssertEquals(2, Histogram.GetMetric.Counter);
  AssertEquals(10, Histogram.GetMetric.TotalSum);
  AssertEquals(11, Length(Histogram.GetMetric.BucketCounters));

  for I := Low(Histogram.GetMetric.BucketCounters)
    to High(Histogram.GetMetric.BucketCounters) do
  begin
    Expected := EXPECTED_RESULTS[I];
    UpperBound := Histogram.GetMetric.BucketCounters[I].UpperInclusiveBound;
    Actual := Histogram.GetMetric.BucketCounters[I].Counter;
    Message := Format('%f must be %d and it was %d', [UpperBound, Expected, Actual]);
    AssertEquals(Message, Expected, Actual);
  end;
end;

procedure TTestHistogram.TestCanObserveResultWithLabels;
var
  Child: TPrometheusHistogramChildren;
  I: integer;
begin
  Child := Histogram.WithLabels(['foo', 'bar']);
  Child.Observe(1);
  for I := Low(Child.GetMetric.BucketCounters)
    to High(Child.GetMetric.BucketCounters) do
  begin
    if Child.GetMetric.BucketCounters[I].UpperInclusiveBound >= 1 then
      AssertEquals(1, Child.GetMetric.BucketCounters[I].Counter)
    else
      AssertEquals(0, Child.GetMetric.BucketCounters[I].Counter);
  end;
end;

procedure TTestHistogram.TestSomeLabelNamesAreBeingForbidded;
begin
  Self.ExpectException('Exception must be thrown', Exception,
    'Label with name "le" is not permitted');
  Histogram.WithLabels(['le', 'nope']);
end;

procedure TTestHistogram.TestCanCreateHistogramsWithSpecificBuckets;
var
  I: integer;
  CustomBuckets: array[0..4] of double = (5, 10, 15, 20, 25);
begin
  with TPrometheusHistogram.Create('foo', 'bar') do
  begin
    SetBuckets(CustomBuckets);
    for I := Low(GetMetric.BucketCounters) to High(GetMetric.BucketCounters) do
      AssertEquals(CustomBuckets[I], GetMetric.BucketCounters[I].UpperInclusiveBound);
    Free;
  end;
end;

initialization

  RegisterTest(TTestHistogram);
end.
