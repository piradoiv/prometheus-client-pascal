unit PrometheusClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TPrometheusOpts = packed record
    Name: string;
    Help: string;
    Labels: TStringList;
  end;

  { TPrometheusMetric }

  TPrometheusMetric = class
  protected
    Opts: TPrometheusOpts;
    FLabels: TStringList;
    function GetKeyFromLabels(LabelArray: array of const): string;
    function GetMetricName(LabelString: string): string;
  public
    constructor Create(Options: TPrometheusOpts);
    constructor Create(Name: string; Help: string = '');
    constructor Create(Name: string; Help: string; Labels: array of const);
    constructor Create(Name: string; Labels: array of const);
    function Expose: string; virtual; abstract;
  published
    property Name: string read Opts.Name;
    property Help: string read Opts.Help;
    property Labels: TStringList read Opts.Labels;
  end;

  { TPrometheusCounter }

  TPrometheusCounter = class(TPrometheusMetric)
  public
    procedure Inc(Amount: double = 1);
    procedure Inc(LabelArray: array of const; Amount: double = 1);
    function GetMetric: double;
    function GetMetric(LabelArray: array of const): double;
    function Expose: string; override;
  end;

  { TPrometheusGauge }

  TPrometheusGauge = class(TPrometheusMetric)
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
    function Expose: string; override;
  end;

implementation

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

function TPrometheusGauge.Expose: string;
var
  Lines: TStringList;
  MetricName: string;
  I: integer;
begin
  Lines := TStringList.Create;
  Lines.Add(Format('# TYPE %s gauge', [Name]));
  if Help <> '' then
    Lines.Add(Format('# HELP %s %s', [Name, Help]));

  for I := 0 to FLabels.Count - 1 do
  begin
    MetricName := GetMetricName(FLabels.Names[I]);
    Lines.Add(Format('%s %s', [MetricName, FLabels.ValueFromIndex[I]]));
  end;

  Result := Lines.Text;
  Lines.Free;
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

function TPrometheusCounter.Expose: string;
var
  Lines: TStringList;
  MetricName: string;
  I: integer;
begin
  Lines := TStringList.Create;
  Lines.Add(Format('# TYPE %s counter', [Name]));
  if Help <> '' then
    Lines.Add(Format('# HELP %s %s', [Name, Help]));

  for I := 0 to FLabels.Count - 1 do
  begin
    MetricName := GetMetricName(FLabels.Names[I]);
    Lines.Add(Format('%s %s', [MetricName, FLabels.ValueFromIndex[I]]));
  end;

  Result := Lines.Text;
  Lines.Free;
end;

{ TPrometheusMetric }

function TPrometheusMetric.GetKeyFromLabels(LabelArray: array of const): string;
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

function TPrometheusMetric.GetMetricName(LabelString: string): string;
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

constructor TPrometheusMetric.Create(Options: TPrometheusOpts);
begin
  if not Assigned(Options.Labels) then
    Options.Labels := TStringList.Create;
  Opts := Options;
  FLabels := TStringList.Create;
end;

constructor TPrometheusMetric.Create(Name: string; Help: string);
var
  Options: TPrometheusOpts;
begin
  Options.Name := Name;
  Options.Help := Help;
  Create(Options);
end;

constructor TPrometheusMetric.Create(Name: string; Help: string;
  Labels: array of const);
var
  Options: TPrometheusOpts;
  I: integer;
begin
  Options.Name := Name;
  Options.Help := Help;
  Options.Labels := TStringList.Create;
  for I := Low(Labels) to High(Labels) do
    Options.Labels.Add(ansistring(Labels[I].VAnsiString));

  Create(Options);
end;

constructor TPrometheusMetric.Create(Name: string; Labels: array of const);
begin
  Create(Name, '', Labels);
end;

end.
