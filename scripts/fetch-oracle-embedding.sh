#!/bin/sh
# Download the oracle embedding (Kobayashi, Daido and Bekki 2026) into $LIGHTBLUE/data/.
set -eu
: "${LIGHTBLUE:?set LIGHTBLUE to the lightblue directory}"
REL=https://github.com/DaisukeBekki/lightblue/releases/download/oracle-embedding-v1
mkdir -p "$LIGHTBLUE/data" && cd "$LIGHTBLUE/data"
curl -fsSL "$REL/Embedding.csv.gz" -o Embedding.csv.gz
curl -fsSL "$REL/SHA256SUMS" -o SHA256SUMS
gunzip -kf Embedding.csv.gz
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c SHA256SUMS
else
  shasum -a 256 -c SHA256SUMS
fi
