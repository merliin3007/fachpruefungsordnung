FROM haskell:9.8.4

WORKDIR /app

# install dependencies
COPY docker docker
RUN docker/install-deps.sh

# copy project files
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY package.yaml package.yaml
COPY *.cabal ./

RUN --mount=type=cache,target=/root/.stack/ \
    --mount=type=cache,target=/app/.stack-work/ \
    stack setup && stack build --only-dependencies \
    --extra-include-dirs=/usr/include/postgresql \
    --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# copy source files
COPY src/ src/
COPY app/ app/

# build project
RUN --mount=type=cache,target=/root/.stack/ \
    --mount=type=cache,target=/app/.stack-work/ \
    stack build \
    --extra-include-dirs=/usr/include/postgresql \
    --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

CMD ["stack", "run"]
