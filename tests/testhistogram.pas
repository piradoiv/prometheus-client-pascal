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
var
  Child: TPrometheusHistogramChildren;
  I: integer;
begin
  Child := Histogram.WithLabels(['hello', 'world']);
  Child.Observe(5);
  Child.Observe(5);

  AssertEquals(2, Child.GetMetric.Counter);
  AssertEquals(10, Child.GetMetric.TotalSum);
  AssertEquals(11, Length(Child.GetMetric.BucketCounters));
  AssertEquals(0, Child.GetMetric.BucketCounters[8].Counter);
  AssertEquals(2, Child.GetMetric.BucketCounters[9].Counter);
  AssertEquals(2, Child.GetMetric.BucketCounters[10].Counter);
end;

initialization

  RegisterTest(TTestHistogram);
end.

