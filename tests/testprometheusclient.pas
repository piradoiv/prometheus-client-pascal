unit TestPrometheusClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusRegistry, PrometheusClasses;

type

  TTestPrometheusClient = class(TTestCase)
  private
    Registry: TPrometheusRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCanRegisterMetric;
    procedure TestCanCheckIfMetricExists;
    procedure TestCanUnregisterMetric;
    procedure TestCanGetMetric;
    procedure TestCanCreateAndRegisterCounter;
  end;

implementation

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

procedure TTestPrometheusClient.TestCanRegisterMetric;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', 'help');
  Registry.Register(Counter);
  AssertEquals(Counter.Name, TPrometheusCounter(Registry.Get(Counter.Name)).Name);
end;

procedure TTestPrometheusClient.TestCanCheckIfMetricExists;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', '');
  AssertFalse(Registry.Exists(Counter.Name));
  Registry.Register(Counter);
  AssertTrue(Registry.Exists(Counter.Name));
end;

procedure TTestPrometheusClient.TestCanUnregisterMetric;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', '');
  Registry.Register(Counter);
  AssertTrue(Registry.Exists(Counter.Name));
  Registry.Unregister(Counter.Name);
  AssertFalse(Registry.Exists(Counter.Name));
end;

procedure TTestPrometheusClient.TestCanGetMetric;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', '');
  Counter.Inc(42);
  Registry.Register(Counter);
  Counter := TPrometheusCounter(Registry.Get(Counter.Name));
  AssertEquals(42, Counter.GetMetric);
end;

procedure TTestPrometheusClient.TestCanCreateAndRegisterCounter;
var
  Counter: TPrometheusCounter;
begin
  Counter := Registry.Counter('test', 'help');
  Counter.Inc(42);
  AssertEquals(42, Counter.GetMetric);
end;

initialization

  RegisterTest(TTestPrometheusClient);
end.

