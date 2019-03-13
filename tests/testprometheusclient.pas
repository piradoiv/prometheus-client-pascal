unit TestPrometheusClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusRegistry,
  PrometheusClasses, PrometheusCounter;

type

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

initialization

  RegisterTest(TTestPrometheusClient);
end.

