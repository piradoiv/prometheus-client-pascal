**Home** | [Examples](/examples/)

# Prometheus Client for FreePascal [![Build Status](https://travis-ci.org/piradoiv/prometheus-client-pascal.svg?branch=master)](https://travis-ci.org/piradoiv/prometheus-client-pascal)

This is the [FreePascal](https://www.freepascal.org/) client library for [Prometheus](https://prometheus.io/), an open-source systems monitoring and alerting toolkit. Combined with [Grafana](https://grafana.com/), you'll be able to create dashboards and see what's happening inside your application.

As of 2016, Prometheus lives under the umbrella of [Cloud Native Computing Foundation](https://www.cncf.io/).

![Prometheus with Grafana](https://github.com/piradoiv/prometheus-client-pascal/blob/master/img/grafana_prometheus.png?raw=true)

This library just collects data, you'll need a running Prometheus server to extract, query and monitor it.

Usage examples can be found under [examples](./examples) folder.

## Implementation status

This library is in alpha stage, and is not intended for production use yet. A roadmap is available through the [milestones](https://github.com/piradoiv/prometheus-client-pascal/milestones) section of this repository.

### Metrics

- [Counter](https://prometheus.io/docs/concepts/metric_types/#counter) ✅ Working
- [Gauge](https://prometheus.io/docs/concepts/metric_types/#gauge) ✅ Working
- [Histogram](https://prometheus.io/docs/concepts/metric_types/#histogram) ✅ Working
- [Summary](https://prometheus.io/docs/concepts/metric_types/#summary) ❌ Not implemented yet

