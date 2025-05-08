#!/usr/bin/env sh
set -eu

TEMPLATE=/etc/nginx/templates/default.conf.template

if [ -e "$TEMPLATE" ]; then
    envsubst '${HOST_URL}' < "$TEMPLATE" > /etc/nginx/conf.d/default.conf
fi

exec "$@"
