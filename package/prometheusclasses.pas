unit PrometheusClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, StrUtils, contnrs;

type
  TPrometheusOpts = packed record
    Name: string;
    Description: string;
    Labels: TStringList;
  end;

  { TPrometheusCollector }

  TPrometheusCollector = class
  protected
    Opts: TPrometheusOpts;
    FLabels: TStringList;
    Storage: TFPHashObjectList;
    function GetKeyFromLabels(LabelArray: array of const): string;
    function GetMetricName(LabelString: string): string;
    function GetMetricType: string;
  public
    constructor Create(Options: TPrometheusOpts);
    constructor Create(Name: string; Description: string);
    constructor Create(Name: string; Description: string; Labels: array of const);
    constructor Create(Name: string; Labels: array of const);
    destructor Destroy; override;
    function Expose: string;
  published
    property Name: string read Opts.Name;
    property Description: string read Opts.Description;
    property Labels: TStringList read Opts.Labels;
  end;

  { TPrometheusCounterChildren }

  TPrometheusCounterChildren = class
  private
    Value: double;
  public
    Key: string;
    procedure Inc(Amount: double = 1);
    function GetMetric: double;
  end;

  { TPrometheusCounter }

  TPrometheusCounter = class(TPrometheusCollector)
  public
    procedure Inc(Amount: double = 1);
    function GetMetric: double;
    function WithLabels(LabelArray: array of const): TPrometheusCounterChildren;
  end;

  { TPrometheusGaugeChildren }

  TPrometheusGaugeChildren = class
  private
    Value: double;
  public
    Key: string;
    procedure Inc(Amount: double = 1);
    procedure Dec(Amount: double = 1);
    procedure SetAmount(Amount: double = 1);
    procedure SetToCurrentTime;
    function GetMetric: double;
  end;

  { TPrometheusGauge }

  TPrometheusGauge = class(TPrometheusCollector)
  public
    procedure Inc(Amount: double = 1);
    procedure Dec(Amount: double = 1);
    procedure SetAmount(Amount: double = 1);
    procedure SetToCurrentTime;
    function GetMetric: double;
    function WithLabels(LabelArray: array of const): TPrometheusGaugeChildren;
  end;

implementation

{ TPrometheusGaugeChildren }

procedure TPrometheusGaugeChildren.Inc(Amount: double);
begin
  Value := Value + Amount;
end;

procedure TPrometheusGaugeChildren.Dec(Amount: double);
begin
  Value := Value - Amount;
end;

procedure TPrometheusGaugeChildren.SetAmount(Amount: double);
begin
  Value := Amount;
end;

procedure TPrometheusGaugeChildren.SetToCurrentTime;
begin
  Value := DateTimeToUnix(Now);
end;

function TPrometheusGaugeChildren.GetMetric: double;
begin
  Result := Value;
end;

{ TPrometheusCounterChildren }

procedure TPrometheusCounterChildren.Inc(Amount: double);
begin
  if Amount < 0 then
    raise Exception.Create('Increment must be a non-negative number');
  Value := Value + Amount;
end;

function TPrometheusCounterChildren.GetMetric: double;
begin
  Result := Value;
end;

{ TPrometheusGauge }

procedure TPrometheusGauge.Inc(Amount: double);
begin
  WithLabels(['___metric', '___default']).Inc(Amount);
end;

procedure TPrometheusGauge.Dec(Amount: double);
begin
  WithLabels(['___metric', '___default']).Dec(Amount);
end;

procedure TPrometheusGauge.SetAmount(Amount: double);
begin
  WithLabels(['___metric', '___default']).SetAmount(Amount);
end;

procedure TPrometheusGauge.SetToCurrentTime;
begin
  WithLabels(['___metric', '___default']).SetToCurrentTime;
end;

function TPrometheusGauge.GetMetric: double;
begin
  Result := Self.WithLabels(['___metric', '___default']).GetMetric;
end;

function TPrometheusGauge.WithLabels(LabelArray: array of const):
TPrometheusGaugeChildren;
var
  Key: string;
  Index: integer;
begin
  Key := GetKeyFromLabels(LabelArray);
  Index := Storage.FindIndexOf(Key);

  if Index < 0 then
  begin
    Result := TPrometheusGaugeChildren.Create;
    Result.Key := Key;
    Result.Inc(0);
    Index := Storage.Add(Key, Result);
  end;

  Result := TPrometheusGaugeChildren(Storage.Items[Index]);
end;

{ TPrometheusCounter }

procedure TPrometheusCounter.Inc(Amount: double);
begin
  WithLabels(['___metric', '___default']).Inc(Amount);
end;

function TPrometheusCounter.GetMetric: double;
begin
  Result := WithLabels(['___metric', '___default']).GetMetric;
end;

function TPrometheusCounter.WithLabels(LabelArray: array of
  const): TPrometheusCounterChildren;
var
  Key: string;
  Index: integer;
begin
  Key := GetKeyFromLabels(LabelArray);
  Index := Storage.FindIndexOf(Key);

  if Index < 0 then
  begin
    Result := TPrometheusCounterChildren.Create;
    Result.Key := Key;
    Result.Inc(0);
    Index := Storage.Add(Key, Result);
  end;

  Result := TPrometheusCounterChildren(Storage.Items[Index]);
end;

{ TPrometheusCollector }

function TPrometheusCollector.GetKeyFromLabels(LabelArray: array of const): string;
var
  LabelList: TStringList;
  I: integer;
begin
  LabelList := TStringList.Create;
  LabelList.Sorted := True;
  LabelList.Delimiter := '|';
  LabelList.NameValueSeparator := ':';

  for I := Low(LabelArray) to High(LabelArray) do
  begin
    if I mod 2 <> 0 then
      Continue;
    LabelList.Values[ansistring(LabelArray[I].VAnsiString)] :=
      ansistring(LabelArray[I + 1].VAnsiString);
  end;
  Result := LabelList.DelimitedText;
  LabelList.Free;
end;

function TPrometheusCollector.GetMetricName(LabelString: string): string;
var
  LabelList, ConvertedList: TStringList;
  I: integer;
begin
  LabelList := TStringList.Create;
  LabelList.Sorted := True;
  LabelList.Delimiter := '|';
  LabelList.NameValueSeparator := ':';
  LabelList.DelimitedText := LabelString;

  ConvertedList := TStringList.Create;
  ConvertedList.Sorted := True;
  ConvertedList.QuoteChar := #0;
  for I := 0 to LabelList.Count - 1 do
    ConvertedList.Values[LabelList.Names[I]] :=
      Format('"%s"', [LabelList.ValueFromIndex[I]]);

  Result := Format('%s{%s}', [Name, ConvertedList.DelimitedText]);
  if ConvertedList.DelimitedText = '___metric="___default"' then
    Result := Name;
end;

function TPrometheusCollector.GetMetricType: string;
begin
  case Self.ToString of
    'TPrometheusCounter': Result := 'counter';
    'TPrometheusGauge': Result := 'gauge';
  end;
end;

constructor TPrometheusCollector.Create(Options: TPrometheusOpts);
begin
  if not Assigned(Options.Labels) then
    Options.Labels := TStringList.Create;
  Opts := Options;
  FLabels := TStringList.Create;
  Storage := TFPHashObjectList.Create;
  Storage.OwnsObjects := True;
end;

constructor TPrometheusCollector.Create(Name: string; Description: string);
var
  Options: TPrometheusOpts;
begin
  Options.Name := Name;
  Options.Description := Description;
  Create(Options);
end;

constructor TPrometheusCollector.Create(Name: string; Description: string;
  Labels: array of const);
var
  Options: TPrometheusOpts;
  I: integer;
begin
  Options.Name := Name;
  Options.Description := Description;
  Options.Labels := TStringList.Create;
  for I := Low(Labels) to High(Labels) do
    Options.Labels.Add(ansistring(Labels[I].VAnsiString));

  Create(Options);
end;

constructor TPrometheusCollector.Create(Name: string; Labels: array of const);
begin
  Create(Name, '', Labels);
end;

destructor TPrometheusCollector.Destroy;
begin
  FLabels.Free;
  Storage.Free;
  inherited Destroy;
end;

function TPrometheusCollector.Expose: string;
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

  for I := 0 to Storage.Count - 1 do
  begin
    MetricName := GetMetricName(Storage.NameOfIndex(I));
    Children := TPrometheusCounterChildren(Storage.Items[I]);
    Amount := Format('%f', [Children.GetMetric]);
    if AnsiEndsStr('.00', Amount) then
      Amount := IntToStr(Round(Children.GetMetric));
    Lines.Add(Format('%s %s', [MetricName, Amount]));
  end;

  Result := Lines.Text;
  Lines.Free;
end;

end.
