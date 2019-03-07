unit PrometheusRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PrometheusClasses;

type

  TPrometheusRegistry = class
  private
    Storage: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(Metric: TPrometheusMetric);
    procedure Unregister(Name: string; FreeMetric: boolean = True);
    function Counter(Name: string; Help: string = ''): TPrometheusCounter;
    function Exists(Name: string): boolean;
    function Get(Name: string): TPrometheusMetric;
  end;

implementation

constructor TPrometheusRegistry.Create;
begin
  Storage := TStringList.Create;
  Storage.Sorted := True;
end;

destructor TPrometheusRegistry.Destroy;
begin
  inherited Destroy;
  Storage.Free;
end;

procedure TPrometheusRegistry.Register(Metric: TPrometheusMetric);
var
  Index: integer;
begin
  if Storage.Find(Metric.Name, Index) then
    raise Exception.Create(Format('%s has been already registered', [Metric.Name]));

  Storage.AddObject(Metric.Name, Metric);
end;

procedure TPrometheusRegistry.Unregister(Name: string; FreeMetric: boolean);
var
  Index: integer;
begin
  if Storage.Find(Name, Index) then
  begin
    if FreeMetric then
      Storage.Objects[Index].Free;

    Storage.Delete(Index);
  end;
end;

function TPrometheusRegistry.Counter(Name: string; Help: string): TPrometheusCounter;
begin
  Result := TPrometheusCounter.Create(Name, Help);
  Self.Register(Result);
end;

function TPrometheusRegistry.Exists(Name: string): boolean;
var
  Index: integer;
begin
  Result := Storage.Find(Name, Index);
end;

function TPrometheusRegistry.Get(Name: string): TPrometheusMetric;
var
  Index: integer;
begin
  if Storage.Find(Name, Index) then
    Result := TPrometheusMetric(Storage.Objects[Index]);
end;

end.


