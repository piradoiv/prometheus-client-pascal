unit TestCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, PrometheusClasses;

type

  { TTestCounter }

  TTestCounter = class(TTestCase)
  private
    TestCounter: TPrometheusCounter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestCanGetOptions;
    procedure TestSetCounterAmount;
    procedure TestCanIncreaseAmount;
    procedure TestDecreasingCounterRaisesAnException;
  end;

implementation

procedure TTestCounter.SetUp;
begin
  TestCounter := TPrometheusCounter.Create('test', 'help');
end;

procedure TTestCounter.TearDown;
begin
  TestCounter.Free;
end;

procedure TTestCounter.TestHookUp;
begin
  AssertNotNull(TestCounter);
end;

procedure TTestCounter.TestCanGetOptions;
begin
  AssertEquals('test', TestCounter.Name);
  AssertEquals('help', Testcounter.Help);
end;

procedure TTestCounter.TestSetCounterAmount;
begin
  AssertEquals(0, TestCounter.Counter);
  TestCounter.Counter := 42;
  AssertEquals(42, TestCounter.Counter);
end;

procedure TTestCounter.TestCanIncreaseAmount;
begin
  AssertEquals(0, TestCounter.Counter);
  TestCounter.Inc;
  AssertEquals(1, TestCounter.Counter);
  TestCounter.Inc(41);
  AssertEquals(42, TestCounter.Counter);
end;

procedure TTestCounter.TestDecreasingCounterRaisesAnException;
var
  ExceptionFired: boolean;
  ExceptionMessage: string;
begin
  ExceptionFired := False;
  try
    TestCounter.Counter := 42;
    TestCounter.Counter := 41;
  except
    on E: Exception do
    begin
      ExceptionFired := True;
      ExceptionMessage := E.Message;
    end;
  end;

  AssertTrue(ExceptionFired);
  AssertEquals('Counters can''t be decreased', ExceptionMessage);
end;

initialization

  RegisterTest(TTestCounter);
end.

