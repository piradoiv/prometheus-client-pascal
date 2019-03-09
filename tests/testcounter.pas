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
    procedure TestCanCreateCounterWithLabels;
    procedure TestCanIncreaseAmountForSpecificLabels;
    procedure TestShouldThrowAnExceptionWithNegativeArguments;
    procedure TestCanGetWithLabelsChildren;
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
  AssertEquals('help', Testcounter.Description);
end;

procedure TTestCounter.TestSetCounterAmount;
begin
  AssertEquals(0, TestCounter.GetMetric);
  TestCounter.Inc(42.0);
  AssertEquals(42, TestCounter.GetMetric);
end;

procedure TTestCounter.TestCanIncreaseAmount;
begin
  AssertEquals(0, TestCounter.GetMetric);
  TestCounter.Inc;
  AssertEquals(1, TestCounter.GetMetric);
  TestCounter.Inc(41.42);
  AssertEquals(42.42, TestCounter.GetMetric);
end;

procedure TTestCounter.TestCanCreateCounterWithLabels;
begin
  TestCounter := TPrometheusCounter.Create('test', 'Test counter',
    ['foo', 'bar', 'baz']);

  AssertEquals('test', TestCounter.Name);
  AssertEquals('Test counter', TestCounter.Description);
  AssertEquals(3, TestCounter.Labels.Count);
end;

procedure TTestCounter.TestCanIncreaseAmountForSpecificLabels;
begin
  TestCounter := TPrometheusCounter.Create('test', ['datacenter', 'job']);
  TestCounter.Inc(['job', 'test', 'datacenter', 'eu1'], 10);
  TestCounter.Inc(['datacenter', 'eu2', 'job', 'test'], 5);

  AssertEquals(10, TestCounter.GetMetric(['datacenter', 'eu1', 'job', 'test']));
  AssertEquals(5, TestCounter.GetMetric(['datacenter', 'eu2', 'job', 'test']));
end;

procedure TTestCounter.TestShouldThrowAnExceptionWithNegativeArguments;
var
  ExceptionThrown: boolean;
  ExceptionMessage: string;
begin
  ExceptionThrown := False;
  try
    TestCounter.Inc(-1);
  except
    on E: Exception do
    begin
      ExceptionThrown := True;
      ExceptionMessage := E.Message;
    end;
  end;
  AssertTrue(ExceptionThrown);
  AssertEquals('Increment must be a non-negative number', ExceptionMessage);
end;

procedure TTestCounter.TestCanGetWithLabelsChildren;
var
  Children: TPrometheusCounterChildren;
begin
  Children := TestCounter.WithLabels(['foo', 'bar']);
  AssertEquals('foo:bar', Children.Key);
end;

initialization

  RegisterTest(TTestCounter);
end.

