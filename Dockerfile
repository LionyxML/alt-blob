# syntax=docker/dockerfile:1

# ---- Build stage: compile the standalone binary with SBCL + Quicklisp ----
# clfoundation/sbcl ships an SBCL built with core compression (needed for the
# :compression 9 in build.lisp).
FROM docker.io/clfoundation/sbcl:latest AS build

# curl + CA certs to bootstrap Quicklisp.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      curl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Bootstrap Quicklisp into /root/quicklisp (build.lisp loads ~/quicklisp/setup.lisp).
RUN curl -fsSL https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp \
 && sbcl --non-interactive \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
 && rm -f /tmp/quicklisp.lisp

# Build at /app so asdf:system-relative-pathname bakes /app/templates and
# /app/www into the saved image; the runtime stage mirrors that path.
WORKDIR /app
COPY . .
RUN make build

# ---- Runtime stage: slim image with just the binary + assets ----
# Must match the builder's Debian release (clfoundation/sbcl:latest = trixie) so
# the binary's glibc requirement is satisfied. Bump both together if that moves.
FROM docker.io/library/debian:trixie-slim AS runtime

# libzstd1: decompress the compressed core.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libzstd1 ca-certificates \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir -p /data

WORKDIR /app
COPY --from=build /app/alt /app/alt
COPY --from=build /app/templates /app/templates
COPY --from=build /app/www /app/www

# Sensible container defaults; compose overrides the rest from .env.
ENV PORT=3900 \
    DATA_DIR=/data

EXPOSE 3900
VOLUME ["/data"]

ENTRYPOINT ["/app/alt"]
