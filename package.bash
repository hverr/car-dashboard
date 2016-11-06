#!/bin/sh

set -e

SHOW_HELP=n
BIN=
ARCH=$(dpkg --print-architecture)
PLATFORM="$ARCH"

TEMP=`getopt -o -h --long help,binary:,arch:,platform: -n package.bash -- "$@"`
eval set -- "$TEMP"
while true; do
    case "$1" in
        -h|--help)  SHOW_HELP=y     ; shift   ;;
        --binary)   BIN="$2"        ; shift 2 ;;
        --arch)     ARCH="$2"       ; shift 2 ;;
        --platform) PLATFORM="$2"   ; shift 2 ;;
        --)         shift           ; break   ;;
    esac
done

if [ "$SHOW_HELP" = y ]; then
    echo "usage: $0 [--binary BINARY] [--arch ARCH] [--platform PLAT]"
    exit
fi

echo "Making package with"
echo "    Binary:       $BIN"
echo "    Architecture: $ARCH"
echo "    Platform:     $PLATFORM"

set -x

make setup
make js
if [ -z "$BIN" ]; then
    cabal sandbox init
    cabal install
    BIN=.cabal-sandbox/bin/car-dashboard
fi

VERSION=$(awk '$1 == "version:" {print $2}' car-dashboard.cabal)
REVISION=1

DEB_NAME="car-dashboard_${VERSION}-${REVISION}-${PLATFORM}"
DEB_ROOT="./dist/$DEB_NAME"
rm -rf "$DEB_ROOT"
mkdir -p "$DEB_ROOT/DEBIAN/"
mkdir -p "$DEB_ROOT/lib/systemd/system/"
mkdir -p "$DEB_ROOT/usr/bin/"
mkdir -p "$DEB_ROOT/usr/share/car-dashboard/"

cp "$BIN" "$DEB_ROOT/usr/bin/car-dashboard"
cp car-dashboard.service "$DEB_ROOT/lib/systemd/system/"
rsync -r data "$DEB_ROOT/usr/share/car-dashboard/"
rsync -r node_modules "$DEB_ROOT/usr/share/car-dashboard/"
rsync    car-dashboard-stream-music "$DEB_ROOT/usr/share/car-dashboard/"

cat <<EOF > "$DEB_ROOT/DEBIAN/control"
Package: car-dashboard
Version: ${VERSION}-${REVISION}
Section: base
Priority: optional
Architecture: $ARCH
Depends: zlib1g-dev, libncurses5-dev, libav-tools
Maintainer: Henri Verroken <henriverroken@gmail.com>
Description: Car Dashboard
EOF

cd dist
dpkg-deb --build "car-dashboard_${VERSION}-${REVISION}-${PLATFORM}"
cd ..

echo "Package is in ./dist/$DEB_NAME.deb"
