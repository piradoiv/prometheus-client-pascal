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
    procedure TestCanIncreaseAmountOnChildren;
    procedure TestLabelNamesAreBeingVerified;
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
  TestCounter.Inc(42);
  AssertEquals(42, TestCounter.GetMetric);
end;

procedure TTestCounter.TestCanIncreaseAmount;
begin
  AssertEquals(0, TestCounter.GetMetric);
  TestCounter.Inc;
  AssertEquals(1, TestCounter.GetMetric);
  TestCounter.Inc(5);
  AssertEquals(6, TestCounter.GetMetric);
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
  TestCounter.WithLabels(['job', 'test', 'datacenter', 'eu1']).Inc(10);
  TestCounter.WithLabels(['datacenter', 'eu2', 'job', 'test']).Inc(5);

  AssertEquals(10, TestCounter.WithLabels(['datacenter', 'eu1', 'job', 'test']).GetMetric);
  AssertEquals(5, TestCounter.WithLabels(['datacenter', 'eu2', 'job', 'test']).GetMetric);
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

procedure TTestCounter.TestCanIncreaseAmountOnChildren;
var
  Children: TPrometheusCounterChildren;
begin
  Children := TestCounter.WithLabels(['foo', 'bar']);
  TestCounter.WithLabels(['foo', 'bar']).Inc(1);
  TestCounter.WithLabels(['foo', 'bar']).Inc(1);
  Children.Inc(40);

  AssertEquals(42, TestCounter.WithLabels(['foo', 'bar']).GetMetric);
  AssertEquals(42, Children.GetMetric);
  AssertEquals(0, TestCounter.WithLabels(['foo', 'nope']).GetMetric);
end;

initialization

  RegisterTest(TTestCounter);
end.

