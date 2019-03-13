unit PrometheusRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PrometheusClasses, PrometheusCounter, PrometheusGauge,
  PrometheusHistogram;

type

  { TPrometheusRegistry }

  TPrometheusRegistry = class
  private
    Storage: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(Metric: TPrometheusCustomCollector);
    procedure Unregister(Name: string);
    function Counter(Name: string; Help: string): TPrometheusCounter;
    function Gauge(Name: string; Help: string): TPrometheusGauge;
    function Histogram(Name: string; Help: string): TPrometheusHistogram;
    function Exists(Name: string): boolean;
    function Get(Name: string): TPrometheusCustomCollector;
    function Expose: string;
  end;

implementation

constructor TPrometheusRegistry.Create;
begin
  Storage := TStringList.Create;
  Storage.Sorted := True;
  Storage.OwnsObjects := False;
end;

destructor TPrometheusRegistry.Destroy;
var
  I: integer;
begin
  for I := Storage.Count - 1 downto 0 do
    Storage.Objects[I].Free;
  Storage.Free;
  inherited Destroy;
end;

procedure TPrometheusRegistry.Register(Metric: TPrometheusCustomCollector);
begin
  if Exists(Metric.Name) then
    raise Exception.Create(Format('%s has been already registered', [Metric.Name]));

  Storage.AddObject(Metric.Name, Metric);
end;

procedure TPrometheusRegistry.Unregister(Name: string);
var
  Index: integer;
begin
  if Storage.Find(Name, Index) then
  begin
    if Assigned(Storage.Objects[Index]) then
      Storage.Delete(Index);
  end;
end;

function TPrometheusRegistry.Counter(Name: string; Help: string): TPrometheusCounter;
begin
  Result := TPrometheusCounter.Create(Name, Help);
  Register(Result);
end;

function TPrometheusRegistry.Gauge(Name: string; Help: string): TPrometheusGauge;
begin
  Result := TPrometheusGauge.Create(Name, Help);
  Register(Result);
end;

function TPrometheusRegistry.Histogram(Name: string; Help: string): TPrometheusHistogram;
begin
  Result := TPrometheusHistogram.Create(Name, Help);
  Register(Result);
end;

function TPrometheusRegistry.Exists(Name: string): boolean;
var
  Index: integer;
begin
  Result := Storage.Find(Name, Index);
end;

function TPrometheusRegistry.Get(Name: string): TPrometheusCustomCollector;
var
  Index: integer;
begin
  if Storage.Find(Name, Index) then
    Result := TPrometheusCustomCollector(Storage.Objects[Index]);
end;

function TPrometheusRegistry.Expose: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Storage.Count - 1 do
    Result := Concat(Result, TPrometheusCustomCollector(Storage.Objects[I]).Expose, #13#10);
end;

end.


