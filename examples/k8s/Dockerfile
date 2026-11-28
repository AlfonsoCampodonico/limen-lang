# The site operator (examples/site_operator.lmn) as an image for a cluster.
# The first stage builds Limen from this repository, then the operator as a
# native binary; the second holds only that binary and OpenSSL. Build from
# the repository's root:
#   docker build -f examples/k8s/Dockerfile -t limen-site-operator:dev .
FROM debian:trixie AS build
RUN apt-get update && apt-get install -y --no-install-recommends \
      ocaml-nox ocaml-dune ocaml-findlib gcc libc6-dev make \
      libssl-dev libsqlite3-dev ca-certificates \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY . .
RUN dune build ./bin/main.exe
RUN LIMEN_CACHE=off ./_build/default/bin/main.exe build examples/site_operator.lmn -o /site-operator

FROM debian:trixie-slim
RUN apt-get update && apt-get install -y --no-install-recommends libssl3 \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /site-operator /usr/local/bin/site-operator
USER 65532:65532
ENTRYPOINT ["/usr/local/bin/site-operator"]
