# +------------------------------+
# |            BUILD             |
# +------------------------------+

FROM debian:bookworm-slim AS build

# install dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends locales git npm nodejs
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
RUN locale-gen de_DE.UTF-8 && update-locale

# install required npm packages
RUN npm install -g spago@next
RUN npm install -g esbuild
RUN npm install -g purescript

WORKDIR /build

# install project dependencies
COPY package.json package.json
COPY package-lock.json package-lock.json
COPY spago.yaml spago.yaml
COPY spago.lock spago.lock

RUN npm install
RUN spago install

# build
COPY src src
COPY test test

RUN spago build
RUN spago bundle --bundle-type app

# +------------------------------+
# |            FINAL             |
# +------------------------------+

FROM nginx:alpine AS final

WORKDIR /home

COPY index.html index.html
COPY --from=build /build/index.js index.js

COPY docker-entrypoint.sh docker-entrypoint.sh
RUN chmod +x docker-entrypoint.sh

ENTRYPOINT ["./docker-entrypoint.sh"]

CMD ["nginx", "-g", "daemon off;"]
