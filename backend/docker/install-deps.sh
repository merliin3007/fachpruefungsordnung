#!/bin/bash

# strict bash mode
set -euo pipefail

# apt-get without manual input
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get -y upgrade

# installer newer postgres version, since we use an old image
# when upgrading to debian bullseye, this might become obsolete
apt-get install -y build-essential
apt-get install -y wget gnupg lsb-release
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
  > /etc/apt/sources.list.d/pgdg.list
apt-get update
apt-get install -y postgresql-15 libpq-dev
