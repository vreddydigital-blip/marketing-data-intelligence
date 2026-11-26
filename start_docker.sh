#!/usr/bin/env bash
# First-run script for self-hosting Marketing Data Intelligence.
# Requires: Docker (with the compose plugin) and an open port 3838.
set -euo pipefail
cd "$(dirname "$0")"

docker compose build
docker compose up -d
echo "Waiting for the application to start ..."
for i in $(seq 1 30); do
  if curl -fsS -o /dev/null http://127.0.0.1:3838/; then
    echo "Ready: http://127.0.0.1:3838 (data persists in the mdi-data volume)"
    exit 0
  fi
  sleep 2
done
echo "The application did not answer within 60 s; check: docker compose logs"
exit 1
