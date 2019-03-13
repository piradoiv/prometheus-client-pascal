program simplewebserver;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  FPHttpServer,
  PrometheusRegistry,
  PrometheusCounter,
  PrometheusGauge;

type

  TExampleServer = class(TFpHttpServer)
  protected
    Registry: TPrometheusRegistry;
    TotalRequests: TPrometheusCounter;
    PendingRequests: TPrometheusGauge;
    procedure OnRequestHandler(Sender: TObject; var Req: TFPHTTPConnectionRequest;
      var Res: TFPHTTPConnectionResponse);
    procedure OnExceptionHandler(Sender: TObject; E: Exception);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  constructor TExampleServer.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    OnRequest := @OnRequestHandler;
    OnRequestError := @OnExceptionHandler;
    Registry := TPrometheusRegistry.Create;
    TotalRequests := Registry.Counter('http_requests_total',
      'Amount of requests received');
    PendingRequests := Registry.Gauge('pending_requests_total',
      'Amount of requests pending to be processed');
  end;

  procedure TExampleServer.OnRequestHandler(Sender: TObject;
  var Req: TFPHTTPConnectionRequest; var Res: TFPHTTPConnectionResponse);
  begin
    TotalRequests.WithLabels(['method', Req.Method, 'handler', Req.PathInfo]).Inc;
    PendingRequests.Inc;

    case Req.PathInfo of
      '':
      begin
        TotalRequests.WithLabels(['status_code', '200']).Inc;
        Res.Content := 'Hello, World!, check the <a href="/metrics">metrics</a>.';
      end;
      '/metrics':
      begin
        TotalRequests.WithLabels(['status_code', '200']).Inc;
        Res.ContentType := 'text/plain; version=0.0.4';
        Res.Content := Registry.Expose;
      end
      else
      begin
        TotalRequests.WithLabels(['status_code', '404']).Inc;
        Res.Content := 'Not found';
      end
    end;

    PendingRequests.Dec;
  end;

  procedure TExampleServer.OnExceptionHandler(Sender: TObject; E: Exception);
  begin
    Writeln(E.Message);
  end;

var
  Server: TExampleServer;
begin
  Server := TExampleServer.Create(nil);
  Server.Port := 4242;

  Writeln(Format('Starting the server, check http://localhost:%d/', [Server.Port]));
  Writeln('Press CTRL+C to exit');
  Server.Active := True;
end.
