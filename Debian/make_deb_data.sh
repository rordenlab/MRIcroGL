#!/bin/bash
#Create Debian package for all versions of MRIcroGL

# Fail if anything not planed to go wrong, goes wrong
set -eu
sw=mricrogl-data
source ../vers.inc
ver=$kVers
pkg=${sw}_${ver}_all
#deb=${pkg}.deb
resourcePath=${pkg}/usr/share/mricrogl/


SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd $SCRIPT_DIR

#remove previous package files
rm -rf ${pkg}
#copy all required files
mkdir -p ${pkg}/DEBIAN
#cp ./data-control.txt ${pkg}/DEBIAN/control
sed "s/<ver>/$ver/" ./data-control.txt > ${pkg}/DEBIAN/control

mkdir -p ${resourcePath}
cp -a ../Resources/. ${resourcePath}
#Linux does not need Windows resources python36.dll and python36.zip
rm -rf ${resourcePath}python*.*

dpkg-deb --build --root-owner-group ${pkg}
#remove temporary files
rm -rf ${pkg}
exit 0





