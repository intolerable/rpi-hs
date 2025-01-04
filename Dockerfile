FROM ubuntu

RUN apt update -y && apt install -y --no-install-recommends \
  build-essential \
  ca-certificates \
  curl \
  device-tree-compiler \
  libgmp-dev \
  libgpiod-dev \
  locales \
  kmod \
  pkg-config

ARG ARCH=aarch64

RUN \
  curl https://downloads.haskell.org/~ghcup/${ARCH}-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup

ARG GHC=9.6.6
ARG CABAL=latest

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

RUN ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \
    cabal update && \
    cabal install ghcid --installdir=/usr/local/bin

RUN curl https://sh.rustup.rs -sSf | bash -s -- -y

ENV PATH="/root/.cargo/bin:${PATH}"
