unit PrometheusGauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, StrUtils, PrometheusClasses;

type
  { TPrometheusGaugeChildren }

  TPrometheusGaugeChildren = class(TPrometheusCustomCollector)
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
    procedure StartTimer(StartTime: double);
    procedure SetDuration;
    function GetMetricAsDouble: double;
  end;

  { TPrometheusGauge }

  TPrometheusGauge = class(TPrometheusGaugeChildren)
  public
    procedure Inc(Amount: double = 1);
    procedure Dec(Amount: double = 1);
    procedure SetAmount(Amount: double = 1);
    procedure SetToCurrentTime;
    procedure StartTimer;
    procedure StartTimer(StartTime: double);
    procedure SetDuration;
    function GetMetricAsDouble: double;
    function WithLabels(LabelArray: array of const): TPrometheusGaugeChildren;
    function Expose: string; override;
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
  StartTimer(DateTimeToUnix(Now) + (MillisecondOf(Now) / 1000));
end;

procedure TPrometheusGaugeChildren.StartTimer(StartTime: double);
begin
  TimerStartTime := StartTime;
end;

procedure TPrometheusGaugeChildren.SetDuration;
var
  CurrentUnixMilliseconds: double;
begin
  CurrentUnixMilliseconds := DateTimeToUnix(Now) + (MillisecondOf(Now) / 1000);
  SetAmount(CurrentUnixMilliseconds - TimerStartTime);
end;

function TPrometheusGaugeChildren.GetMetricAsDouble: double;
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

procedure TPrometheusGauge.StartTimer(StartTime: double);
begin
  WithLabels([]).StartTimer(StartTime);
end;

procedure TPrometheusGauge.SetDuration;
begin
  WithLabels([]).SetDuration;
end;

function TPrometheusGauge.GetMetricAsDouble: double;
begin
  Result := WithLabels([]).GetMetricAsDouble;
end;

function TPrometheusGauge.WithLabels(LabelArray: array of const):
TPrometheusGaugeChildren;
var
  AKey: string;
  Index: integer;
begin
  AKey := GetKeyFromLabels(LabelArray);
  Index := FStorage.FindIndexOf(AKey);

  if Index < 0 then
  begin
    Result := TPrometheusGaugeChildren.Create(Name, Description);
    Result.Key := AKey;
    Result.Inc(0);
    Index := FStorage.Add(AKey, Result);
  end;

  Result := TPrometheusGaugeChildren(FStorage.Items[Index]);
end;

function TPrometheusGauge.Expose: string;
var
  Lines: TStringList;
  MetricName: string;
  I: integer;
  Children: TPrometheusGaugeChildren;
  Amount: string;
begin
  Lines := TStringList.Create;
  Lines.Add(Format('# TYPE %s %s', [Name, GetMetricType]));
  if Description <> '' then
    Lines.Add(Format('# HELP %s %s', [Name, Description]));

  for I := 0 to FStorage.Count - 1 do
  begin
    MetricName := GetMetricName(FStorage.NameOfIndex(I));
    Children := TPrometheusGaugeChildren(FStorage.Items[I]);
    Amount := Format('%f', [Children.GetMetricAsDouble]);
    if AnsiEndsStr('.00', Amount) then
      Amount := IntToStr(Round(Children.GetMetricAsDouble));
    Lines.Add(Format('%s %s', [MetricName, Amount]));
  end;

  Result := Lines.Text;
  Lines.Free;
end;

end.

