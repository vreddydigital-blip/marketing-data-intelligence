# Deployment Guide

The app is a standard R Shiny application; any Shiny host works. Two
recommended paths, no paid APIs required. This repository ships ready-made
deployment artifacts:

- `deploy.R` — publishes to shinyapps.io via rsconnect (credentials from env vars)
- `Dockerfile` + `.dockerignore` — production container image for self-hosting

Free-tier facts (checked at development time — allowances can change, verify
current terms before relying on them): the shinyapps.io free plan provides
5 deployed apps, **25 active hours per month** (uptime only while the app is
actively serving; idle apps sleep), instances with 1 GB RAM, and a 1 GB bundle
limit. Authentication features and larger instances are paid-tier only, and
the free plan does not require a credit card. Do not assume the free tier is
permanent.

## Option A: shinyapps.io (managed, has a free tier)

1. Create a free account at https://www.shinyapps.io (free tier exists but is
   subject to provider change — check current allowances; do not assume it
   remains free forever).
2. In R (or simply `Rscript deploy.R` from the project root):
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name = "<your-name>",
                             token = Sys.getenv("SHINYAPPS_TOKEN"),        # from env var, do not hardcode
                             secret = Sys.getenv("SHINYAPPS_SECRET"))
   deployApp(appDir = ".", appName = "marketing-data-intelligence")
   ```
3. Notes:
   - Set `MDI_DATA_DIR` writable storage considerations: shinyapps.io rebuilds
     the instance, so SQLite/data files are **ephemeral**. Use the explicit
     "Save session" flow knowing data resets between deploys.
   - The 2-core/1-GB free instances are suitable for demo datasets, not bulk ETL.
   - Startup repos the renv.lock file: include it so deployment restores tested versions.

## Option B: ShinyProxy in Docker (self-hosted, free software)

1. Build and run the shipped image:
   ```bash
   docker build -t marketing-data-intelligence .
   docker run -p 3838:3838 -v mdi-data:/data marketing-data-intelligence
   ```
   The image is based on `rocker/shiny-verse:4.4.1` (Shiny, tidyverse, and
   pandoc included), installs all remaining R packages, and stores SQLite,
   logs, and reports under the mounted `/data` volume.
2. Run ShinyProxy (open source) on a VM; it provides auth (LDAP/OAuth) and
   container-per-user isolation — recommended for anything user-facing.
3. Database: the `/data` volume persists SQLite across restarts, or set
   `MDI_DB_ENGINE` and migrate to PostgreSQL for production use.

## Environment configuration

| Variable | Default | Meaning |
| --- | --- | --- |
| `MDI_MAX_UPLOAD_MB` | 50 | upload limit |
| `MDI_DATA_DIR` | `data` | SQLite, logs, reports |
| `MDI_LOG_LEVEL` | INFO | DEBUG/INFO/WARN/ERROR |
| `MDI_DB_ENGINE` | sqlite | database backend |
| `MDI_PORT` / `MDI_HOST` | 3838 / 127.0.0.1 | listener (run_app.R) |
| `SHINYAPPS_NAME` / `SHINYAPPS_TOKEN` / `SHINYAPPS_SECRET` | — | shinyapps.io credentials (deploy.R only) |

## Restart behaviour

- Shiny apps restart on idle (shinyapps.io) or container restart (ShinyProxy).
  Ephemeral state (in-memory dataset) is lost; saved sessions in SQLite
  survive when the data directory is on persistent storage.
- Logs append to `data/logs/app.log`; rotate or ship them with the platform.

## Option B quick start (self-hosted)

On any machine or VM with Docker:

```bash
git clone https://github.com/vreddydigital-blip/marketing-data-intelligence.git
cd marketing-data-intelligence
./start_docker.sh        # builds the image, starts the container, waits for HTTP 200
```

`docker-compose.yml` mounts a named volume `mdi-data` at `/data` so the SQLite
database, logs, and generated reports survive container restarts and rebuilds.
The container listens on port 3838 (change the host-side mapping in
`docker-compose.yml` if that port is taken).

### Putting ShinyProxy in front (authentication, multi-user)

For anything user-facing, run the image behind ShinyProxy instead of exposing
the app directly. Minimal `/etc/shinyproxy/application.yml`:

```yaml
proxy:
  title: Marketing Data Intelligence
  port: 8080
  authentication: simple        # or ldap / openid (OAuth2) in production
  users:
    - name: analyst
      password: change-me       # source from a secret manager, not git
  docker:
    internal-networking: true
  specs:
    - id: mdi
      display-name: Marketing Data Intelligence
      container-image: marketing-data-intelligence:latest
      container-volumes: [ "/var/lib/shinyproxy/mdi:/data" ]
      port: 3838
```

Build the image on the VM (`docker build -t marketing-data-intelligence .`),
run ShinyProxy itself as a container, and create the host directory for the
volume. Each user session gets an isolated container; the `/data` bind mount
persists SQLite sessions across restarts.

### Resource sizing

- Demo/portfolio use: 2 vCPU, 2 GB RAM is comfortable.
- Regular analyst use with ranger models on ~100k-row data: 4 vCPU, 4-8 GB RAM.
- The MDI_MAX_UPLOAD_MB limit and model complexity drive peak memory; monitor
  with `docker stats`.

### TLS / reverse proxy

Terminate TLS with nginx, Caddy, or Traefik in front of port 3838
(direct RShiny) or ShinyProxy's port 8080. For websocket-based Shiny traffic a
reverse proxy must support the Upgrade header; nginx example:

```nginx
location / {
  proxy_pass http://127.0.0.1:3838;
  proxy_http_version 1.1;
  proxy_set_header Upgrade $http_upgrade;
  proxy_set_header Connection "upgrade";
  proxy_set_header Host $host;
}
```
