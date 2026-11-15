# Builds the Linux release: the limen binary and its .deb, on the oldest
# Ubuntu LTS we support, so the result runs on Ubuntu 22.04+ and Debian 12+.
# OCaml 5 comes from opam (the interpreter needs effects).
#   dev/release/linux.sh 0.2.0     # both architectures, into dist-release/
FROM ubuntu:22.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
      opam gcc libc6-dev make m4 pkg-config libssl-dev ca-certificates \
      git curl unzip bubblewrap dpkg-dev \
    && rm -rf /var/lib/apt/lists/*
RUN opam init -y --disable-sandboxing --bare \
 && opam switch create -y default 5.3.0 \
 && opam install -y dune \
 && opam clean -a -c -s --logs
WORKDIR /src
COPY . .
RUN eval $(opam env) && dune build --profile release bin/main.exe
COPY dev/release/deb.sh /deb.sh
CMD ["sh", "/deb.sh"]
