unit PrometheusCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, PrometheusClasses;

type
  { TPrometheusCounterChildren }

  TPrometheusCounterChildren = class(TPrometheusCustomCollector)
  private
    Value: double;
  public
    Key: string;
    procedure Inc(Amount: double = 1);
    function GetMetricAsDouble: double;
  end;

  { TPrometheusCounter }

  TPrometheusCounter = class(TPrometheusCounterChildren)
  public
    procedure Inc(Amount: double = 1);
    function GetMetricAsDouble: double;
    function WithLabels(LabelArray: array of const): TPrometheusCounterChildren;
    function Expose: string; override;
  end;

implementation

{ TPrometheusCounter }

procedure TPrometheusCounter.Inc(Amount: double);
begin
  WithLabels([]).Inc(Amount);
end;

function TPrometheusCounter.GetMetricAsDouble: double;
begin
  Result := WithLabels([]).GetMetricAsDouble;
end;

function TPrometheusCounter.WithLabels(LabelArray: array of
  const): TPrometheusCounterChildren;
var
  AKey: string;
  Index: integer;
begin
  AKey := GetKeyFromLabels(LabelArray);
  Index := FStorage.FindIndexOf(AKey);

  if Index < 0 then
  begin
    Result := TPrometheusCounterChildren.Create(Name, Description);
    Result.Key := AKey;
    Result.Inc(0);
    Index := FStorage.Add(AKey, Result);
  end;

  Result := TPrometheusCounterChildren(FStorage.Items[Index]);
end;

{ TPrometheusCounterChildren }

procedure TPrometheusCounterChildren.Inc(Amount: double);
begin
  if Amount < 0 then
    raise Exception.Create('Increment must be a non-negative number');
  Value := Value + Amount;
end;

function TPrometheusCounterChildren.GetMetricAsDouble: double;
begin
  Result := Value;
end;

function TPrometheusCounter.Expose: string;
var
  Lines: TStringList;
  MetricName: string;
  I: integer;
  Children: TPrometheusCounterChildren;
  Amount: string;
begin
  Lines := TStringList.Create;
  Lines.Add(Format('# TYPE %s %s', [Name, GetMetricType]));
  if Description <> '' then
    Lines.Add(Format('# HELP %s %s', [Name, Description]));

  for I := 0 to FStorage.Count - 1 do
  begin
    MetricName := GetMetricName(FStorage.NameOfIndex(I));
    Children := TPrometheusCounterChildren(FStorage.Items[I]);
    Amount := Format('%f', [Children.GetMetricAsDouble]);
    if AnsiEndsStr('.00', Amount) then
      Amount := IntToStr(Round(Children.GetMetricAsDouble));
    Lines.Add(Format('%s %s', [MetricName, Amount]));
  end;

  Result := Lines.Text;
  Lines.Free;
end;

end.

