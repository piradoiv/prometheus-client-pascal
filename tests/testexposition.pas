unit TestExposition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusRegistry, PrometheusClasses;

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
  Counter.Inc(['method', 'post', 'code', '200'], 1027);
  Counter.Inc(['method', 'post', 'code', '400'], 3);

  ExpectedList := TStringList.Create;
  with ExpectedList do
  begin
    Add('# TYPE http_requests_total counter');
    Add('# HELP http_requests_total The total number of HTTP requests');
    Add('http_requests_total 1030');
    Add('http_requests_total{code="200",method="post"} 1027');
    Add('http_requests_total{code="400",method="post"} 3');
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
  Gauge.Inc(['type', 'anonymous'], 300);
  Gauge.Inc(['type', 'users'], 200);
  Gauge.Inc(['type', 'administrators'], 10);
  Gauge.Dec(['type', 'anonymous'], 10);
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
