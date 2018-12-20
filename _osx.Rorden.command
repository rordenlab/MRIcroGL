#!/bin/sh

#typical users should use the "_osx.command" to build files
# this script builds other tools and special versions of MRIcroGL



find /Users/rorden/tst/MRIcroGL12 -name ‘*.DS_Store’ -type f -delete

#-dDisableLCLGIF -dDisableLCLJPEG -dDisableLCLPNM -dDisableLCLTIFF

#compile dcm2niix
cd ~/dcm2niix/console
#g++ -O3 -dead_strip -I. main_console.cpp nii_foreign.cpp nii_dicom.cpp nifti1_io_core.cpp nii_ortho.cpp nii_dicom_batch.cpp jpg_0XC3.cpp ujpeg.cpp -o dcm2niix  -I/usr/local/lib -I/usr/local/include/openjpeg-2.1 /usr/local/lib/libopenjp2.a
g++-8 -O3 -dead_strip -I. main_console.cpp nii_foreign.cpp nii_dicom.cpp nifti1_io_core.cpp nii_ortho.cpp nii_dicom_batch.cpp jpg_0XC3.cpp ujpeg.cpp -o dcm2niix -DmyDisableOpenJPEG -DmyDisableMiniZ -I~/cloudflarez ~/cloudflarez/libz.a
strip ./dcm2niix
cp dcm2niix /Users/rorden/tst/MRIcroGL12/MRIcroGL.app/Contents/Resources/dcm2niix
cp dcm2niix /Users/rorden/tst/MRIcroGL12/MRIcroMTL.app/Contents/Resources/dcm2niix

cd ~/MRIcroGL12/
#compile MRIcroGL64
~/Lazarus/lazbuild MRIcroGL.lpr --cpu=x86_64 --ws=cocoa
strip ./MRIcroGL
cp MRIcroGL /Users/rorden/tst/MRIcroGL12/MRIcroGL.app/Contents/MacOS/MRIcroGL

./_clean.command
