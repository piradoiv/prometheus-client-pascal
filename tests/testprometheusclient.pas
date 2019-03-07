unit TestPrometheusClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestPrometheusClient = class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestPrometheusClient.TestHookUp;
begin
  Fail('Write your own test');
end;



initialization

  RegisterTest(TTestPrometheusClient);
end.

