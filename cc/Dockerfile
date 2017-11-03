#
# Build stage
#
FROM fpco/stack-build:lts as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH

RUN mkdir -p /src
WORKDIR /src

# Install appropriate GHC
COPY stack.yaml /src/
RUN stack setup

# Install HLint
COPY hlint.cabal /src/
RUN stack install --dependencies-only
COPY ./src /src/src
COPY ./data /src/data
COPY ./LICENSE /src/LICENSE
RUN stack install

# Install CC/Engine executable
RUN mkdir -p /cc
WORKDIR /cc
COPY ./cc/engine.cabal /cc/engine.cabal
RUN stack init && stack install --dependencies-only
COPY ./cc/Engine.hs /cc/Engine.hs
RUN stack install

#
# Runtime
#
FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8

# Executables from build stage
COPY --from=builder /root/.local/bin/hlint /usr/bin/hlint
COPY --from=builder /root/.local/bin/engine /usr/bin/engine
COPY --from=builder /src/data /opt/hlint

RUN mkdir -p /code
VOLUME /code
WORKDIR /code

RUN useradd app --uid 9000
USER app

# Reset fpco/stack-run's dumb ENTRYPOINT
ENTRYPOINT []
CMD ["/usr/bin/engine"]
