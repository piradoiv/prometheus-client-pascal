unit PrometheusHistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PrometheusClasses;

type
  { TPrometheusHistogramChildren }

  TPrometheusHistogramChildren = class(TPrometheusCustomCollector)
  private
    Value: TPrometheusBucketResult;
  public
    Key: string;
    procedure Observe(Amount: double);
    function GetMetric: TPrometheusBucketResult;
    function Expose: string; override;
  end;

  { TPrometheusHistogram }

  TPrometheusHistogram = class(TPrometheusHistogramChildren)
  const
    DEFAULT_BUCKETS: array[0..10] of double =
      (0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10);
  protected
    Buckets: array of double;
  public
    constructor Create(Options: TPrometheusOptions);
    constructor Create(AName: string; ADescription: string);
    procedure Observe(Amount: double);
    procedure SetBuckets(CustomBuckets: TPrometheusCustomBuckets);
    function GetMetric: TPrometheusBucketResult;
    function WithLabels(LabelArray: array of const): TPrometheusHistogramChildren;
    function Expose: string; override;
  end;

implementation

{ TPrometheusHistogram }

constructor TPrometheusHistogram.Create(Options: TPrometheusOptions);
begin
  inherited Create(Options);
end;

constructor TPrometheusHistogram.Create(AName: string; ADescription: string);
var
  Options: TPrometheusOptions;
begin
  Options.Name := AName;
  Options.Description := ADescription;
  Create(Options);
end;

procedure TPrometheusHistogram.Observe(Amount: double);
begin
  if not Assigned(Buckets) then
    SetBuckets(DEFAULT_BUCKETS);

  WithLabels([]).Observe(Amount);
end;

procedure TPrometheusHistogram.SetBuckets(CustomBuckets: TPrometheusCustomBuckets);
begin
  if Assigned(Buckets) then
    raise Exception.Create('Can not modify buckets if they''re already set');

  Buckets := CustomBuckets;
end;

function TPrometheusHistogram.GetMetric: TPrometheusBucketResult;
begin
  Result := WithLabels([]).GetMetric;
end;

function TPrometheusHistogram.WithLabels(LabelArray: array of const):
TPrometheusHistogramChildren;
var
  AKey: string;
  I, Index: integer;
begin
  if not Assigned(Buckets) then
    SetBuckets(DEFAULT_BUCKETS);

  AKey := GetKeyFromLabels(LabelArray);
  Index := FStorage.FindIndexOf(AKey);

  if Index < 0 then
  begin
    Result := TPrometheusHistogramChildren.Create(Name, Description);
    Result.Key := AKey;
    Result.Value.TotalSum := 0;
    Result.Value.Counter := 0;

    SetLength(Result.Value.BucketCounters, Length(Buckets));
    for I := Low(Result.Value.BucketCounters) to High(Result.Value.BucketCounters) do
    begin
      Result.Value.BucketCounters[I].UpperInclusiveBound := Buckets[I];
      Result.Value.BucketCounters[I].Counter := 0;
    end;

    Index := FStorage.Add(AKey, Result);
  end;

  Result := TPrometheusHistogramChildren(FStorage.Items[Index]);
end;

function TPrometheusHistogram.Expose: string;
var
  Lines: TStringList;
  MetricName: string;
  I: integer;
  Children: TPrometheusHistogramChildren;
begin
  Lines := TStringList.Create;
  Lines.Add(Format('# TYPE %s %s', [Name, GetMetricType]));
  if Description <> '' then
    Lines.Add(Format('# HELP %s %s', [Name, Description]));

  for I := 0 to FStorage.Count - 1 do
  begin
    Children := TPrometheusHistogramChildren(FStorage.Items[I]);
    Lines.Add(Children.Expose);
    MetricName := GetMetricName(FStorage.NameOfIndex(I));
    MetricName := StringReplace(MetricName, '}', ', le="+Inf"}', []);
    Lines.Add(Format('%s %d', [MetricName, Children.GetMetric.Counter]));
    MetricName := GetMetricName(FStorage.NameOfIndex(I));
    Lines.Add(Format('%s %f', [StringReplace(MetricName, '{', '_sum{', []),
      Children.GetMetric.TotalSum]));
    Lines.Add(Format('%s %d', [StringReplace(MetricName, '{', '_count{', []),
      Children.GetMetric.Counter]));
  end;

  Result := Lines.Text;
  Lines.Free;
end;

{ TPrometheusHistogramChildren }

procedure TPrometheusHistogramChildren.Observe(Amount: double);
var
  I: integer;
begin
  Inc(Value.Counter);
  Value.TotalSum := Value.TotalSum + Amount;
  for I := Low(Value.BucketCounters) to High(Value.BucketCounters) do
    if Amount <= Value.BucketCounters[I].UpperInclusiveBound then
      Inc(Value.BucketCounters[I].Counter);
end;

function TPrometheusHistogramChildren.GetMetric: TPrometheusBucketResult;
begin
  Result := Value;
end;

function TPrometheusHistogramChildren.Expose: string;
var
  MetricName: string;
  J: integer;
  BucketName: string;
  BucketCounter: integer;
begin
  Result := '';
  for J := 1 to High(GetMetric.BucketCounters) do
  begin
    BucketName := Format('%f', [GetMetric.BucketCounters[J].UpperInclusiveBound]);
    BucketCounter := GetMetric.BucketCounters[J].Counter;
    MetricName := GetMetricName(Key);
    MetricName := StringReplace(MetricName, '}', Format(', le="%s"}', [BucketName]), []);
    Result := Concat(Result, Format('%s %d', [MetricName, BucketCounter]), #13#10);
  end;
  Result := Trim(Result);
end;

end.
