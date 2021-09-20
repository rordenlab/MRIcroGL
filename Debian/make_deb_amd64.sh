#!/bin/bash
# Fail if anything not planed to go wrong, goes wrong
set -eu
sw=mricrogl
source ../vers.inc
ver=$kVers
arch=amd64
#set widgetset set for default(QT5)
wigetset=
sw=${sw}${wigetset}

pkg=${sw}_${ver}_${arch}
#deb=${pkg}.deb
exePath=${pkg}/usr/bin/
appPath=${pkg}/usr/share/applications/
docPath=${pkg}/usr/share/doc/${sw}/
iconPath=${pkg}/usr/share/icons/hicolor/scalable/apps/
manPath=${pkg}/usr/share/man/man1/
man=${sw}.1.gz

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd $SCRIPT_DIR

if [ -z "$wigetset" ]
then
      lazbuild --ws=qt5 ../MRIcroGL.lpi
else
      lazbuild --ws=gtk2 ../MRIcroGL.lpi
fi

#remove previous package files
rm -rf ${pkg}
#copy all required files
#copy control file
mkdir -p ${pkg}/DEBIAN
#cp ./${sw}-control.txt ${pkg}/DEBIAN/control
sed "s/<ver>/$ver/" ./${sw}-control.txt > ${pkg}/DEBIAN/control
#copy executable
mkdir -p ${exePath}
cp ../MRIcroGL ${exePath}${sw}
#copy desktop file
mkdir -p ${appPath}
cp ${sw}.desktop ${appPath}${sw}.desktop
echo cp ${sw}.desktop ${appPath}${sw}.desktop
#copy icon svg
mkdir -p ${iconPath}
cp ${sw}.svg ${iconPath}${sw}.svg

#copy documents
mkdir -p ${docPath}
cp ../license.txt ${docPath}copyright
#copy manual
mkdir -p ${manPath}
cp ${man} ${manPath}${man}

#dpkg-deb --build --root-owner-group ${pkg}
#remove temporary files
#rm -rf ${pkg}

echo Test the package:
echo sudo dpkg -I ${pkg}.deb
echo sudo dpkg -c ${pkg}.deb
echo sudo dpkg -i ${pkg}.deb
exit 0





