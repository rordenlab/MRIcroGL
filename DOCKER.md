# Running MRIcroGL from Docker

##### Pre-Compiled

The easiest way to install MRIcroGL is to download a compiled executable:
 - `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_linux.zip`

##### Compiling

You can also compile a copy yourself. Here we will use a Debian-based distribution. I suggest [Lazarus 2.0.4 or later](https://forum.lazarus.freepascal.org/index.php/topic,46721.msg333496/topicseen.html#new) for Debian systems.


docker pull jgoerzen/debian-base-standard
docker run -td --stop-signal=SIGPWR --name=name jgoerzen/debian-base-standard

docker run -it jgoerzen/debian-base-standard /bin/bash
cd ~
wget --no-check-certificate https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.4/lazarus-project_2.0.4-0_amd64.deb
wget --no-check-certificate https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.4/fpc-src_3.0.4-2_amd64.deb
wget --no-check-certificate https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.4/fpc-laz_3.0.4-1_amd64.deb
apt-get update
apt -yq install ./fpc-src_3.0.4-2_amd64.deb
apt -yq install ./fpc-laz_3.0.4-1_amd64.deb 
apt -yq install ./lazarus-project_2.0.4-0_amd64.deb 
apt-get -yq install git
apt-get -yq install libgl1-mesa-dev
apt-get install build-essential 
git clone https://github.com/rordenlab/MRIcroGL12.git
git clone https://github.com/neurolabusc/Metal-Demos
git clone https://github.com/Alexey-T/Python-for-Lazarus.git
lazbuild --add-package ~/Python-for-Lazarus/python4lazarus/python4lazarus_package.lpk  --build-ide=
lazbuild --add-package lazopenglcontext --build-ide=
cd MRIcroGL12
lazbuild -B ./MRIcroGL.lpr
# create distribution
mkdir ~/distro  
mkdir ~/distro/MRIcroGL
cp ./MRIcroGL ~/distro/MRIcroGL/MRIcroGL
cp -a ./Resources/ ~/distro/MRIcroGL


##### Notes

 - MRIcroGL requires OpenGL 3.3 or later, see [troubleshooting page for details](http://www.mccauslandcenter.sc.edu/mricrogl/troubleshooting).
 - While not required, MRIcroGL works best with pigz (`apt -yq install pigz`) and dcm2niix (`apt -yq install dcm2niix`)
 

