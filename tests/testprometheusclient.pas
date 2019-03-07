unit TestPrometheusClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, PrometheusRegistry;

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
end;



initialization

  RegisterTest(TTestPrometheusClient);
end.

