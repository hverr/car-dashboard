#!/bin/sh

set -ex

make setup
make js
cabal install

VERSION=$(awk '$1 == "version:" {print $2}' car-dashboard.cabal)
REVISION=1

DEB_ROOT="dist/car-dashboard_${VERSION}-${REVISION}"
mkdir -p "$DEB_ROOT/DEBIAN/"
mkdir -p "$DEB_ROOT/lib/systemd/system/"
mkdir -p "$DEB_ROOT/usr/bin/"
mkdir -p "$DEB_ROOT/usr/share/car-dashboard/"

cp .cabal-sandbox/bin/car-dashboard "$DEB_ROOT/usr/bin/"
cp car-dashboard.service "$DEB_ROOT/lib/systemd/system/"
rsync -r data "$DEB_ROOT/usr/share/car-dashboard/"
rsync -r node_modules "$DEB_ROOT/usr/share/car-dashboard/"

cat <<EOF > "$DEB_ROOT/DEBIAN/control"
Package: car-dashboard
Version: ${VERSION}-${REVISION}
Section: base
Priority: optional
Architecture: $(dpkg --print-architecture)
Depends: zlib1g-dev, libncurses5-dev
Maintainer: Henri Verroken <henriverroken@gmail.com>
Description: Car Dashboard
EOF

cd dist
dpkg-deb --build "car-dashboard_${VERSION}-${REVISION}"
cd ..

echo "Package is in ./dist/car-dashboard_${VERSION}-${REVISION}"
