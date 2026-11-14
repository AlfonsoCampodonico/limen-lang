# A Linux box for building and testing Tessera: the compiler, both backends
# and the full test suite. Works for linux/arm64 and linux/amd64.
#   dev/linux/run.sh            # native architecture
#   dev/linux/run.sh amd64      # x86-64 (emulated on an arm64 Mac)
FROM debian:trixie
RUN apt-get update && apt-get install -y --no-install-recommends \
      ocaml-nox ocaml-dune ocaml-findlib gcc libc6-dev make \
      libssl-dev libsqlite3-dev openssl ca-certificates \
      python3 git curl procps tzdata \
    && rm -rf /var/lib/apt/lists/*
RUN useradd -m tessera
USER tessera
WORKDIR /home/tessera/src
COPY --chown=tessera:tessera . .
RUN dune build 2>&1 | grep -v "Warning\|unused" | tail -20 && dune build
CMD ["sh", "-c", "dune test && bash grammar/check-suite.sh"]
