unit TestPrometheusClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusRegistry;

type

  TTestPrometheusClient = class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestPrometheusClient.TestHookUp;
var
  Registry: TPrometheusRegistry;
begin
  Registry := TPrometheusRegistry.Create;
  AssertNotNull(Registry);
  Registry.Free;
end;

initialization

  RegisterTest(TTestPrometheusClient);
end.

