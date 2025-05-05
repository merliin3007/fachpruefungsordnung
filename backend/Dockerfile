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

RUN stack setup && stack build --only-dependencies \
    --extra-include-dirs=/usr/include/postgresql \
    --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# copy source files
COPY src/ src/
COPY app/ app/

# build project
RUN stack build \
    --extra-include-dirs=/usr/include/postgresql \
    --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# Gute Nacht :)
CMD ["/app/.stack-work/dist/x86_64-linux/ghc-9.8.4/build/backend-exe/backend-exe"]
