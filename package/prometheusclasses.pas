unit PrometheusClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, contnrs, Regexpr;

type
  TPrometheusOptions = packed record
    Name: string;
    Description: string;
  end;

  { TPrometheusCustomCollector }

  TPrometheusCustomCollector = class(TObject)
  protected
    FOptions: TPrometheusOptions;
    FStorage: TFPHashObjectList;
    procedure ValidateMetricName(Name: string);
    procedure ValidateLabelName(Name: string);
    procedure BuildStorage;
    function GetKeyFromLabels(LabelArray: array of const): string;
    function GetMetricName(LabelString: string): string;
    function GetMetricType: string;
  public
    constructor Create(Options: TPrometheusOptions);
    constructor Create(Name: string; Description: string);
    destructor Destroy; override;
    function Expose: string; virtual; abstract;
  published
    property Name: string read FOptions.Name;
    property Description: string read FOptions.Description;
  end;

  { TPrometheusBucket }

  TPrometheusBucket = packed record
    UpperInclusiveBound: double;
    Counter: integer;
  end;

  { TPrometheusBucketResult }

  TPrometheusBucketResult = packed record
    BucketCounters: array of TPrometheusBucket;
    TotalSum: double;
    Counter: integer;
  end;

implementation

{ TPrometheusCustomCollector }

procedure TPrometheusCustomCollector.ValidateMetricName(Name: string);
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

procedure TPrometheusCustomCollector.ValidateLabelName(Name: string);
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

procedure TPrometheusCustomCollector.BuildStorage;
begin
  FStorage := TFPHashObjectList.Create(True);
end;

function TPrometheusCustomCollector.GetKeyFromLabels(LabelArray: array of const): string;
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

function TPrometheusCustomCollector.GetMetricName(LabelString: string): string;
var
  LabelList, ConvertedList: TStringList;
  I: integer;
  Labels: string;
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

  Labels := StringReplace(ConvertedList.DelimitedText, ',', ', ', [rfReplaceAll]);
  Result := Format('%s{%s}', [Name, Labels]);
  if ConvertedList.DelimitedText = '' then
    Result := Name;

  LabelList.Free;
  ConvertedList.Free;
end;

function TPrometheusCustomCollector.GetMetricType: string;
begin
  case Self.ToString of
    'TPrometheusCounter': Result := 'counter';
    'TPrometheusGauge': Result := 'gauge';
    'TPrometheusHistogram': Result := 'histogram';
  end;
end;

constructor TPrometheusCustomCollector.Create(Options: TPrometheusOptions);
begin
  ValidateMetricName(Options.Name);
  FOptions := Options;
  BuildStorage;
end;

constructor TPrometheusCustomCollector.Create(Name: string; Description: string);
var
  Options: TPrometheusOptions;
begin
  Options.Name := Name;
  Options.Description := Description;
  Create(Options);
end;

destructor TPrometheusCustomCollector.Destroy;
begin
  if Assigned(FStorage) then
  begin
    FStorage.Clear;
    FStorage.Free;
  end;
  inherited Destroy;
end;

end.
