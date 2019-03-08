program prometheusclienttests;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  TestPrometheusClient,
  TestCounter,
  TestGauge,
  TestExposition;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
    // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'prometheusclienttests';
  Application.Run;
  Application.Free;
end.
