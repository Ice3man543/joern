# ── Stage 1: Build Joern fork from source ─────────────────────────────────────
FROM --platform=linux/amd64 ubuntu:24.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    openjdk-21-jdk curl gnupg git \
    && rm -rf /var/lib/apt/lists/*

# Install sbt
ENV SBT_VERSION=1.12.1
ENV SBT_HOME=/usr/local/sbt
ENV PATH="${SBT_HOME}/bin:${PATH}"
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
    | tar -xz -C /usr/local

WORKDIR /build
COPY . .
RUN sbt stage

# ── Stage 2: Runtime image ────────────────────────────────────────────────────
# Drop-in replacement for ghcr.io/joernio/joern:nightly
FROM --platform=linux/amd64 ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    openjdk-21-jre-headless python3 git curl ca-certificates \
    php-cli \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf /usr/bin/python3 /usr/bin/python

RUN useradd -m -s /bin/bash joern

# Copy built Joern — same layout as ghcr.io/joernio/joern:nightly (/opt/joern/joern-cli/)
COPY --from=builder /build/joern-cli/target/universal/stage /opt/joern/joern-cli
RUN chmod +x /opt/joern/joern-cli/bin/* 2>/dev/null || true

ENV PATH="/opt/joern/joern-cli/bin:${PATH}"

USER joern
WORKDIR /home/joern
