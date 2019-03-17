unit TestPrometheusClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusRegistry,
  PrometheusClasses, PrometheusCounter, PrometheusGauge, PrometheusHistogram;

type

  { TTestPrometheusClient }

  TTestPrometheusClient = class(TTestCase)
  private
    Registry: TPrometheusRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCanRegisterCollector;
    procedure TestCanCheckIfCollectorExists;
    procedure TestCanUnregisterCollector;
    procedure TestCanGetCollector;
    procedure TestCanCreateAndRegisterCounter;
    procedure TestThreadSafety;
  end;

  { TTestThread }

  TTestThread = class(TThread)
  private
    TestRegistry: TPrometheusRegistry;
  protected
    procedure Execute; override;
  public
    constructor Create(Registry: TPrometheusRegistry);
  end;

implementation

{ TTestThread }

constructor TTestThread.Create(Registry: TPrometheusRegistry);
begin
  TestRegistry := Registry;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TTestThread.Execute;
var
  Counter: TPrometheusCounter;
  Gauge: TPrometheusGauge;
  Histogram: TPrometheusHistogram;
  I: integer;
begin
  Counter := TPrometheusCounter(TestRegistry.Get('test_counter'));
  Gauge := TPrometheusGauge(TestRegistry.Get('test_gauge'));
  Histogram := TPrometheusHistogram(TestRegistry.Get('test_histogram'));

  for I := 1 to 100 do
  begin
    Counter.Inc;
    Gauge.Inc;
    Histogram.Observe(4.2);
    Sleep(Random(2));
  end;
end;

procedure TTestPrometheusClient.SetUp;
begin
  Registry := TPrometheusRegistry.Create;
end;

procedure TTestPrometheusClient.TearDown;
begin
  Registry.Free;
end;

procedure TTestPrometheusClient.TestHookUp;
begin
  AssertNotNull(Registry);
end;

procedure TTestPrometheusClient.TestCanRegisterCollector;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', 'help');
  Registry.Register(Counter);
  AssertEquals(Counter.Name, TPrometheusCounter(Registry.Get(Counter.Name)).Name);
end;

procedure TTestPrometheusClient.TestCanCheckIfCollectorExists;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', '');
  AssertFalse(Registry.Exists(Counter.Name));
  Registry.Register(Counter);
  AssertTrue(Registry.Exists(Counter.Name));
end;

procedure TTestPrometheusClient.TestCanUnregisterCollector;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', '');
  Registry.Register(Counter);
  AssertTrue(Registry.Exists(Counter.Name));
  Registry.Unregister(Counter.Name);
  AssertFalse(Registry.Exists(Counter.Name));
  Counter.Free;
end;

procedure TTestPrometheusClient.TestCanGetCollector;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', '');
  Counter.Inc(42);
  Registry.Register(Counter);
  Counter := TPrometheusCounter(Registry.Get(Counter.Name));
  AssertEquals(42, Counter.GetMetricAsDouble);
end;

procedure TTestPrometheusClient.TestCanCreateAndRegisterCounter;
var
  Counter: TPrometheusCounter;
begin
  Counter := Registry.Counter('test', 'help');
  Counter.Inc(42);
  AssertEquals(42, Counter.GetMetricAsDouble);
end;

procedure TTestPrometheusClient.TestThreadSafety;
var
  Counter: TPrometheusCounter;
  Gauge: TPrometheusGauge;
  Histogram: TPrometheusHistogram;
  I: integer;
  Threads: array[1..16] of TTestThread;
begin
  Counter := Registry.Counter('test_counter', 'Test Counter');
  Gauge := Registry.Gauge('test_gauge', 'Test Gauge');
  Histogram := Registry.Histogram('test_histogram', 'Test Histogram');

  for I := Low(Threads) to High(Threads) do
    Threads[I] := TTestThread.Create(Registry);

  for I := Low(Threads) to High(Threads) do
    Threads[I].WaitFor;

  for I := Low(Threads) to High(Threads) do
    Threads[I].Free;

  AssertEquals(1600, Counter.GetMetricAsDouble);
  AssertEquals(1600, Gauge.GetMetricAsDouble);
  AssertEquals(1600, Histogram.GetMetric.Counter);
end;

initialization

  RegisterTest(TTestPrometheusClient);
end.

