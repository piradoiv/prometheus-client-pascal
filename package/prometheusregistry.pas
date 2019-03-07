unit PrometheusRegistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TPrometheusRegistry = class
  private
    Storage: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TPrometheusRegistry.Create;
begin
  Storage := TStringList.Create;
end;

destructor TPrometheusRegistry.Destroy;
begin
  inherited Destroy;
  Storage.Free;
end;

end.

