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
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', 'Test counter',
    ['foo', 'bar', 'baz']);

  AssertEquals('test', Counter.Name);
  AssertEquals('Test counter', Counter.Description);
  Counter.Free;
end;

procedure TTestCounter.TestCanIncreaseAmountForSpecificLabels;
var
  Counter: TPrometheusCounter;
begin
  Counter := TPrometheusCounter.Create('test', ['datacenter', 'job']);
  Counter.WithLabels(['job', 'test', 'datacenter', 'eu1']).Inc(10);
  Counter.WithLabels(['datacenter', 'eu2', 'job', 'test']).Inc(5);

  AssertEquals(10, Counter.WithLabels(['datacenter', 'eu1', 'job', 'test']).GetMetric);
  AssertEquals(5, Counter.WithLabels(['datacenter', 'eu2', 'job', 'test']).GetMetric);
  Counter.Free;
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

{******************************************************************************
Label names may contain ASCII letters, numbers, as well as underscores.
They must match the regex [a-zA-Z_][a-zA-Z0-9_]*. Label names beginning
with __ are reserved for internal use.
*******************************************************************************}
procedure TTestCounter.TestLabelNamesAreBeingVerified;
var
  ValidLabels, InvalidLabels: TStringList;
  I: integer;
begin
  ValidLabels := TStringList.Create;
  with ValidLabels do
  begin
    Add('IAMVALID');
    Add('abc_DEF_ghi_1234');
    Add('I_AM_VALID');
    Add('i_am_valid');
  end;

  for I := 0 to ValidLabels.Count - 1 do
  begin
    try
      TestCounter.WithLabels([ValidLabels.Strings[I], 'true']);
    except
      Fail(Format('''%s'' should be a valid string', [ValidLabels.Strings[I]]));
    end;
  end;

  InvalidLabels := TStringList.Create;
  with InvalidLabels do
  begin
    Add('i n v a l i d');
    Add('__invalid');
    Add('ñot-valid');
    Add('not-VALID');
  end;

  for I := 0 to InvalidLabels.Count - 1 do
  begin
    try
      TestCounter.WithLabels([InvalidLabels.Strings[I], 'true']);
      Fail(Format('''%s'' should not be a valid string', [InvalidLabels.Strings[I]]));
    except
      on E: Exception do
        AssertEquals('Invalid label name found', E.Message);
    end;
  end;

  ValidLabels.Free;
  InvalidLabels.Free;
end;

initialization

  RegisterTest(TTestCounter);
end.
