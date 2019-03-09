unit PrometheusClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, contnrs;

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
    Storage: TFPHashList;
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

  TPrometheusCounterChildren = object
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
    procedure Inc(LabelArray: array of const; Amount: double = 1);
    function GetMetric: double;
    function GetMetric(LabelArray: array of const): double;
    function WithLabels(LabelArray: array of const): TPrometheusCounterChildren;
  end;

  { TPrometheusGauge }

  TPrometheusGauge = class(TPrometheusCollector)
  public
    procedure Inc(Amount: double = 1);
    procedure Inc(LabelArray: array of const; Amount: double = 1);
    procedure Dec(Amount: double = 1);
    procedure Dec(LabelArray: array of const; Amount: double = 1);
    procedure SetAmount(Amount: double = 1);
    procedure SetAmount(LabelArray: array of const; Amount: double = 1);
    procedure SetToCurrentTime;
    procedure SetToCurrentTime(LabelArray: array of const);
    function GetMetric: double;
    function GetMetric(LabelArray: array of const): double;
  end;

implementation

{ TPrometheusCounterChildren }

procedure TPrometheusCounterChildren.Inc(Amount: double);
begin
  Value := Value + Amount;
end;

function TPrometheusCounterChildren.GetMetric: double;
begin
  Result := Value;
end;

{ TPrometheusGauge }

procedure TPrometheusGauge.Inc(Amount: double);
begin
  Self.Inc(['___metric', '___default'], Amount);
end;

procedure TPrometheusGauge.Inc(LabelArray: array of const; Amount: double);
var
  Key: string;
begin
  Key := GetKeyFromLabels(LabelArray);
  FLabels.Values[Key] :=
    FloatToStr(StrToFloatDef(FLabels.Values[Key], 0) + Amount);
end;

procedure TPrometheusGauge.Dec(Amount: double);
begin
  Self.Dec(['___metric', '___default'], Amount);
end;

procedure TPrometheusGauge.Dec(LabelArray: array of const; Amount: double);
var
  Key: string;
begin
  Key := GetKeyFromLabels(LabelArray);
  FLabels.Values[Key] :=
    FloatToStr(StrToFloatDef(FLabels.Values[Key], 0) - Amount);
end;

procedure TPrometheusGauge.SetAmount(Amount: double);
begin
  SetAmount(['___metric', '___default'], Amount);
end;

procedure TPrometheusGauge.SetAmount(LabelArray: array of const; Amount: double);
var
  Key: string;
begin
  Key := GetKeyFromLabels(LabelArray);
  FLabels.Values[Key] := FloatToStr(Amount);
end;

procedure TPrometheusGauge.SetToCurrentTime;
begin
  SetToCurrentTime(['___metric', '___default']);
end;

procedure TPrometheusGauge.SetToCurrentTime(LabelArray: array of const);
var
  Key: string;
begin
  Key := GetKeyFromLabels(LabelArray);
  FLabels.Values[Key] := IntToStr(DateTimeToUnix(Now));
end;

function TPrometheusGauge.GetMetric: double;
begin
  Result := Self.GetMetric(['___metric', '___default']);
end;

function TPrometheusGauge.GetMetric(LabelArray: array of const): double;
begin
  Result := StrToFloatDef(FLabels.Values[GetKeyFromLabels(LabelArray)], 0);
end;

{ TPrometheusCounter }

procedure TPrometheusCounter.Inc(Amount: double);
begin
  Inc(['___metric', '___default'], Amount);
end;

procedure TPrometheusCounter.Inc(LabelArray: array of const; Amount: double);
var
  Key: string;
begin
  if Amount < 0 then
    raise Exception.Create('Increment must be a non-negative number');

  Key := GetKeyFromLabels(LabelArray);
  FLabels.Values[Key] :=
    FloatToStr(StrToFloatDef(FLabels.Values[Key], 0) + Amount);
end;

function TPrometheusCounter.GetMetric: double;
begin
  Result := GetMetric(['___metric', '___default']);
end;

function TPrometheusCounter.GetMetric(LabelArray: array of const): double;
begin
  Result := StrToFloatDef(FLabels.Values[GetKeyFromLabels(LabelArray)], 0);
end;

function TPrometheusCounter.WithLabels(LabelArray: array of const):
TPrometheusCounterChildren;
var
  Key: string;
  Index: integer;
begin
  Key := GetKeyFromLabels(LabelArray);
  Index := Storage.FindIndexOf(Key);
  Result.Key := Key;
  if Index < 0 then
    Storage.Add(Key, @Result);

  Result := TPrometheusCounterChildren(Storage.Find(Key)^);
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
  Storage := TFPHashList.Create;
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
begin
  Lines := TStringList.Create;
  Lines.Add(Format('# TYPE %s %s', [Name, GetMetricType]));
  if Description <> '' then
    Lines.Add(Format('# HELP %s %s', [Name, Description]));

  for I := 0 to FLabels.Count - 1 do
  begin
    MetricName := GetMetricName(FLabels.Names[I]);
    Lines.Add(Format('%s %s', [MetricName, FLabels.ValueFromIndex[I]]));
  end;

  Result := Lines.Text;
  Lines.Free;
end;

end.
