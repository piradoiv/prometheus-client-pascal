unit PrometheusRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PrometheusClasses;

type

  { TPrometheusRegistry }

  TPrometheusRegistry = class
  private
    Storage: TStringList;
    Mutex: TRTLCriticalSection;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(Metric: TPrometheusMetric);
    procedure Unregister(Name: string);
    function Counter(Name: string; Help: string = ''): TPrometheusCounter;
    function Gauge(Name: string; Help: string = ''): TPrometheusGauge;
    function Exists(Name: string): boolean;
    function Get(Name: string): TPrometheusMetric;
    function Expose: string;
  end;

implementation

procedure TPrometheusRegistry.Lock;
begin
  EnterCriticalSection(Mutex);
end;

procedure TPrometheusRegistry.Unlock;
begin
  LeaveCriticalSection(Mutex);
end;

constructor TPrometheusRegistry.Create;
begin
  InitCriticalSection(Mutex);
  Storage := TStringList.Create;
  Storage.Sorted := True;
  Storage.OwnsObjects := True;
end;

destructor TPrometheusRegistry.Destroy;
begin
  Storage.Free;
  DoneCriticalSection(Mutex);
  inherited Destroy;
end;

procedure TPrometheusRegistry.Register(Metric: TPrometheusMetric);
begin
  if Exists(Metric.Name) then
    raise Exception.Create(Format('%s has been already registered', [Metric.Name]));

  Lock;
  try
    Storage.AddObject(Metric.Name, Metric);
  finally
    Unlock;
  end;
end;

procedure TPrometheusRegistry.Unregister(Name: string);
var
  Index: integer;
begin
  if Storage.Find(Name, Index) then
  begin
    Lock;
    try
      Storage.Delete(Index);
    finally
      Unlock;
    end;
  end;
end;

function TPrometheusRegistry.Counter(Name: string; Help: string): TPrometheusCounter;
begin
  Result := TPrometheusCounter.Create(Name, Help);
  Self.Register(Result);
end;

function TPrometheusRegistry.Gauge(Name: string; Help: string): TPrometheusGauge;
begin
  Result := TPrometheusGauge.Create(Name, Help);
  Self.Register(Result);
end;

function TPrometheusRegistry.Exists(Name: string): boolean;
var
  Index: integer;
begin
  Lock;
  try
    Result := Storage.Find(Name, Index);
  finally
    Unlock;
  end;
end;

function TPrometheusRegistry.Get(Name: string): TPrometheusMetric;
var
  Index: integer;
begin
  if Storage.Find(Name, Index) then
    Result := TPrometheusMetric(Storage.Objects[Index]);
end;

function TPrometheusRegistry.Expose: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Storage.Count - 1 do
    Result := Concat(Result, TPrometheusMetric(Storage.Objects[I]).Expose, #13#10);
end;

end.


