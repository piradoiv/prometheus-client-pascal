program helloworld;

uses
  SysUtils,
  PrometheusRegistry,
  PrometheusClasses;

var
  Registry: TPrometheusRegistry;
  Counter: TPrometheusCounter;
  CounterWithLabels: TPrometheusCounterChildren;
  Gauge: TPrometheusGauge;
  I: integer;
begin
  Registry := TPrometheusRegistry.Create;
  Counter := Registry.Counter('hello_world', 'Hello from Prometheus client!');
  for I := 1 to 10 do
    Counter.Inc;

  CounterWithLabels := Counter.WithLabels(['datacenter', 'dc1', 'foo', 'bar']);
  CounterWithLabels.Inc(42);

  Gauge := Registry.Gauge('hello_gauge',
    'A gauge represents a numerical value that can go up and down');
  Gauge.Inc(10);
  Gauge.Dec;
  Gauge.SetAmount(42);
  Gauge.SetToCurrentTime;

  Writeln(Registry.Expose);
  Registry.Free;
end.
