unit PrometheusClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, StrUtils, contnrs, Regexpr;

type
  TPrometheusOpts = packed record
    Name: string;
    Description: string;
  end;

  { TPrometheusCollector }

  TPrometheusCollector = class
  protected
    Opts: TPrometheusOpts;
    Storage: TFPHashObjectList;
    procedure ValidateMetricName(Name: string);
    procedure ValidateLabelName(Name: string);
    procedure BuildStorage;
    function GetKeyFromLabels(LabelArray: array of const): string;
    function GetMetricName(LabelString: string): string;
    function GetMetricType: string;
  public
    constructor Create(Options: TPrometheusOpts);
    constructor Create(Name: string; Description: string);
    destructor Destroy; override;
    function Expose: string;
  published
    property Name: string read Opts.Name;
    property Description: string read Opts.Description;
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
    TimerStartTime: double;
  public
    Key: string;
    procedure Inc(Amount: double = 1);
    procedure Dec(Amount: double = 1);
    procedure SetAmount(Amount: double = 1);
    procedure SetToCurrentTime;
    procedure StartTimer;
    procedure SetDuration;
    function GetMetric: double;
  end;

  { TPrometheusGauge }

  TPrometheusGauge = class(TPrometheusCollector)
  public
    procedure Inc(Amount: double = 1);
    procedure Dec(Amount: double = 1);
    procedure SetAmount(Amount: double = 1);
    procedure SetToCurrentTime;
    procedure StartTimer;
    procedure SetDuration;
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

procedure TPrometheusGaugeChildren.StartTimer;
begin
  TimerStartTime := DateTimeToUnix(Now) + (MillisecondOf(Now) / 1000);
end;

procedure TPrometheusGaugeChildren.SetDuration;
var
  CurrentUnixMilliseconds: double;
begin
  CurrentUnixMilliseconds := DateTimeToUnix(Now) + (MillisecondOf(Now) / 1000);
  SetAmount(CurrentUnixMilliseconds - TimerStartTime);
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
  WithLabels([]).Inc(Amount);
end;

procedure TPrometheusGauge.Dec(Amount: double);
begin
  WithLabels([]).Dec(Amount);
end;

procedure TPrometheusGauge.SetAmount(Amount: double);
begin
  WithLabels([]).SetAmount(Amount);
end;

procedure TPrometheusGauge.SetToCurrentTime;
begin
  WithLabels([]).SetToCurrentTime;
end;

procedure TPrometheusGauge.StartTimer;
begin
  WithLabels([]).StartTimer;
end;

procedure TPrometheusGauge.SetDuration;
begin
  WithLabels([]).SetDuration;
end;

function TPrometheusGauge.GetMetric: double;
begin
  Result := WithLabels([]).GetMetric;
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
  WithLabels([]).Inc(Amount);
end;

function TPrometheusCounter.GetMetric: double;
begin
  Result := WithLabels([]).GetMetric;
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

procedure TPrometheusCollector.ValidateMetricName(Name: string);
var
  Regex: TRegExpr;
  Valid: boolean;
begin
  if AnsiStartsStr('__', Name) then
    raise Exception.Create('Invalid metric name found');

  Regex := TRegExpr.Create;
  Regex.Expression := '^[a-zA-Z_:][a-zA-Z0-9_:]*$';
  Valid := Regex.Exec(Name);
  Regex.Free;
  if not Valid then
    raise Exception.Create('Invalid metric name found');
end;

procedure TPrometheusCollector.ValidateLabelName(Name: string);
var
  Found: boolean;
  Regex: TRegExpr;
begin
  if AnsiStartsStr('__', Name) then
    raise Exception.Create('Invalid label name found');

  Regex := TRegExpr.Create;
  Regex.Expression := '^[a-zA-Z_][a-zA-Z0-9_]*$';
  Found := Regex.Exec(Name);
  Regex.Free;
  if not Found then
    raise Exception.Create('Invalid label name found');
end;

procedure TPrometheusCollector.BuildStorage;
begin
  Storage := TFPHashObjectList.Create(True);
end;

function TPrometheusCollector.GetKeyFromLabels(LabelArray: array of const): string;
var
  LabelName: string;
  LabelList: TStringList;
  I: integer;
begin
  LabelList := TStringList.Create;
  try
    LabelList.Sorted := True;
    LabelList.Delimiter := '|';
    LabelList.NameValueSeparator := ':';

    for I := Low(LabelArray) to High(LabelArray) do
    begin
      if I mod 2 <> 0 then
        Continue;
      LabelName := ansistring(LabelArray[I].VAnsiString);
      ValidateLabelName(LabelName);
      LabelList.Values[LabelName] := ansistring(LabelArray[I + 1].VAnsiString);
    end;
    Result := LabelList.DelimitedText;
  finally
    LabelList.Free;
  end;
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
  if ConvertedList.DelimitedText = '' then
    Result := Name;

  LabelList.Free;
  ConvertedList.Free;
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
  ValidateMetricName(Options.Name);
  Opts := Options;
  BuildStorage;
end;

constructor TPrometheusCollector.Create(Name: string; Description: string);
var
  Options: TPrometheusOpts;
begin
  Options.Name := Name;
  Options.Description := Description;
  Create(Options);
end;

destructor TPrometheusCollector.Destroy;
begin
  if Assigned(Storage) then
  begin
    Storage.Clear;
    Storage.Free;
  end;
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
