unit TestExposition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusRegistry,
  PrometheusClasses, PrometheusCounter, PrometheusGauge, PrometheusHistogram;

type

  { TTestExposition }

  TTestExposition = class(TTestCase)
  private
    Prometheus: TPrometheusRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCanExposeCounters;
    procedure TestCanExposeGauges;
    procedure TestCanExposeHistograms;
    procedure TestCanExposeEverything;
  end;

implementation

procedure TTestExposition.SetUp;
begin
  Prometheus := TPrometheusRegistry.Create;
end;

procedure TTestExposition.TearDown;
begin
  Prometheus.Free;
end;

procedure TTestExposition.TestHookUp;
begin
  AssertNotNull(Prometheus);
end;

procedure TTestExposition.TestCanExposeCounters;
var
  Counter: TPrometheusCounter;
  ExpectedList, ActualList: TStringList;
  Expected, Actual: string;
begin
  Counter := Prometheus.Counter('http_requests_total',
    'The total number of HTTP requests');
  Counter.Inc(1030);
  Counter.WithLabels(['method', 'post', 'code', '200']).Inc(1027);
  Counter.WithLabels(['method', 'post', 'code', '400']).Inc(3);

  ExpectedList := TStringList.Create;
  with ExpectedList do
  begin
    Add('# TYPE http_requests_total counter');
    Add('# HELP http_requests_total The total number of HTTP requests');
    Add('http_requests_total 1030');
    Add('http_requests_total{code="200", method="post"} 1027');
    Add('http_requests_total{code="400", method="post"} 3');
  end;

  ActualList := TStringList.Create;
  ActualList.Text := Counter.Expose;

  Expected := ExpectedList.Text;
  Actual := ActualList.Text;
  ExpectedList.Free;
  ActualList.Free;

  AssertEquals(Expected, Actual);
end;

procedure TTestExposition.TestCanExposeGauges;
var
  Gauge: TPrometheusGauge;
  ExpectedList, ActualList: TStringList;
  Expected, Actual: string;
begin
  Gauge := Prometheus.Gauge('active_users', 'The current number of active users');
  Gauge.Inc(600);
  Gauge.WithLabels(['type', 'anonymous']).Inc(300);
  Gauge.WithLabels(['type', 'users']).Inc(200);
  Gauge.WithLabels(['type', 'administrators']).Inc(10);
  Gauge.WithLabels(['type', 'anonymous']).Dec(10);
  Gauge.SetAmount(590);

  ExpectedList := TStringList.Create;
  with ExpectedList do
  begin
    Add('# TYPE active_users gauge');
    Add('# HELP active_users The current number of active users');
    Add('active_users 590');
    Add('active_users{type="anonymous"} 290');
    Add('active_users{type="users"} 200');
    Add('active_users{type="administrators"} 10');
  end;

  ActualList := TStringList.Create;
  ActualList.Text := Gauge.Expose;

  Expected := ExpectedList.Text;
  Actual := ActualList.Text;
  ExpectedList.Free;
  ActualList.Free;

  AssertEquals(Expected, Actual);
end;

procedure TTestExposition.TestCanExposeHistograms;
var
  Histogram: TPrometheusHistogram;
  Child: TPrometheusHistogramChildren;
  ExpectedList, ActualList: TStringList;
  Expected, Actual: string;
begin
  Histogram := TPrometheusHistogram.Create('foo', 'bar');
  Child := Histogram.WithLabels(['hello', 'world']);
  Child.Observe(5);
  Child.Observe(5);

  ExpectedList := TStringList.Create;
  with ExpectedList do
  begin
    Add('# TYPE foo histogram');
    Add('# HELP foo bar');
    Add('foo{hello="world", le="0.01"} 0');
    Add('foo{hello="world", le="0.03"} 0');
    Add('foo{hello="world", le="0.05"} 0');
    Add('foo{hello="world", le="0.10"} 0');
    Add('foo{hello="world", le="0.25"} 0');
    Add('foo{hello="world", le="0.50"} 0');
    Add('foo{hello="world", le="1.00"} 0');
    Add('foo{hello="world", le="2.50"} 0');
    Add('foo{hello="world", le="5.00"} 2');
    Add('foo{hello="world", le="10.00"} 2');
    Add('foo{hello="world", le="+Inf"} 2');
    Add('foo_sum{hello="world"} 10.00');
    Add('foo_count{hello="world"} 2');
  end;

  ActualList := TStringList.Create;
  ActualList.Text := Histogram.Expose;

  Expected := ExpectedList.Text;
  Actual := ActualList.Text;
  ExpectedList.Free;
  ActualList.Free;
  Histogram.Free;

  AssertEquals(Expected, Actual);
end;

procedure TTestExposition.TestCanExposeEverything;
var
  Counter1, Counter2: TPrometheusCounter;
  ExpectedList, ActualList: TStringList;
  Expected, Actual: string;
begin
  Counter1 := Prometheus.Counter('counter1');
  Counter2 := Prometheus.Counter('counter2');

  Counter1.Inc(10);
  Counter2.Inc(100);

  ExpectedList := TStringList.Create;
  ActualList := TStringList.Create;

  with ExpectedList do
  begin
    Add('# TYPE counter1 counter');
    Add('counter1 10');
    Add('');
    Add('# TYPE counter2 counter');
    Add('counter2 100');
    Add('');
  end;

  ActualList.Text := Prometheus.Expose;

  Expected := ExpectedList.Text;
  Actual := ActualList.Text;
  ExpectedList.Free;
  ActualList.Free;

  AssertEquals(Expected, Actual);
end;

initialization

  RegisterTest(TTestExposition);
end.
