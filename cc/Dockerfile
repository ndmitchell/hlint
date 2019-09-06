#
#
#############
#
# Please be aware of the specification as of: 
# https://github.com/codeclimate/spec/blob/master/SPEC.md
#
############
#
# Build stage
#
FROM haskell as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG C.UTF-8

RUN mkdir -p /src
WORKDIR /src

# Initialize package
COPY hlint.cabal /src/
RUN cabal v2-update

# Install HLint
RUN cabal v2-build --dependencies-only
COPY ./src /src/src
COPY ./data /src/data
COPY ./LICENSE /src/LICENSE
COPY ./README.md /src/README.md
COPY ./CHANGES.txt /src/CHANGES.txt
RUN cabal v2-install exe:hlint

# Install CC/Engine executable
RUN mkdir -p /cc
WORKDIR /cc
COPY ./cc/engine.cabal /cc/engine.cabal
RUN cabal v2-build --dependencies-only
COPY ./cc/Engine.hs /cc/Engine.hs
RUN cabal v2-install exe:engine

#
# Runtime
#
FROM debian:stretch
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends libgmp-dev

# Executables from build stage
COPY --from=builder /root/.cabal/bin/hlint /usr/bin/hlint
COPY --from=builder /root/.cabal/bin/engine /usr/bin/engine
COPY --from=builder /src/data /opt/hlint

RUN mkdir -p /code
VOLUME /code
WORKDIR /code

RUN useradd app --uid 9000
USER app

CMD ["/usr/bin/engine"]
