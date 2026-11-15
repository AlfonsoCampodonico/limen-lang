#!/bin/sh
# Inside the release container: package _build's limen as a .deb (and a
# tarball) for this architecture into /out. VERSION comes from the env.
set -e
arch=$(dpkg --print-architecture)
v="${VERSION:?set VERSION}"
root=/tmp/pkg
rm -rf "$root" && mkdir -p "$root/DEBIAN" "$root/usr/bin" "$root/usr/share/doc/limen"
install -m 0755 /src/_build/default/bin/main.exe "$root/usr/bin/limen"
strip "$root/usr/bin/limen" || true
cat > "$root/usr/share/doc/limen/README" <<DOC
Limen $v: a small compiled language where code can only do what it was
handed. The book: https://book.limen-lang.org
DOC
size=$(du -sk "$root/usr" | cut -f1)
cat > "$root/DEBIAN/control" <<CTL
Package: limen
Version: $v
Architecture: $arch
Maintainer: Limen <hello@limen-lang.org>
Installed-Size: $size
Depends: libc6 (>= 2.35), libssl3, gcc | clang, libc6-dev
Recommends: libsqlite3-dev, libssl-dev
Section: devel
Priority: optional
Homepage: https://limen-lang.org
Description: small compiled language where I/O needs a capability
 Limen code can only reach files, the network, the clock and other
 programs through capabilities it was handed. Programs compile through C,
 so a C compiler is needed.
CTL
dpkg-deb --build --root-owner-group "$root" "/out/limen_${v}_${arch}.deb"
mkdir -p "/tmp/limen-$v/bin" && cp "$root/usr/bin/limen" "/tmp/limen-$v/bin/limen"
tar -C /tmp -czf "/out/limen-$v-linux-$arch.tar.gz" "limen-$v"
ls -la /out
