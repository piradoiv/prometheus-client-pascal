unit PrometheusClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPrometheusOpts = packed record
    Name: string;
    Help: string;
  end;

  { TPrometheusMetric }

  TPrometheusMetric = class
  protected
    Opts: TPrometheusOpts;
  public
    constructor Create(Options: TPrometheusOpts);
    constructor Create(Name: string; Help: string = '');
  published
    property Name: string read Opts.Name;
    property Help: string read Opts.Help;
  end;

  { TPrometheusCounter }

  TPrometheusCounter = class(TPrometheusMetric)
  private
    FCounter: integer;
    function GetCounter: integer;
    procedure SetCounter(Amount: integer);
  public
    procedure Inc(Amount: integer = 1);
  published
    property Counter: integer read GetCounter write SetCounter;
  end;

implementation

{ TPrometheusCounter }

function TPrometheusCounter.GetCounter: integer;
begin
  Result := FCounter;
end;

procedure TPrometheusCounter.SetCounter(Amount: integer);
begin
  if Amount < FCounter then
    raise Exception.Create('Counters can''t be decreased');

  FCounter := Amount;
end;

procedure TPrometheusCounter.Inc(Amount: integer);
begin
  FCounter := FCounter + Amount;
end;

{ TPrometheusMetric }

constructor TPrometheusMetric.Create(Options: TPrometheusOpts);
begin
  Opts := Options;
end;

constructor TPrometheusMetric.Create(Name: string; Help: string);
var
  Options: TPrometheusOpts;
begin
  Options.Name := Name;
  Options.Help := Help;
  Create(Options);
end;

end.

