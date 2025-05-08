#!/usr/bin/env sh
set -eu

TEMPLATE=/etc/nginx/conf.d/default-ssl.conf.template

if [ -e "$TEMPLATE" ]; then
    envsubst '${HOST_URL}' < "$TEMPLATE" > /etc/nginx/conf.d/default.conf
fi

exec "$@"
