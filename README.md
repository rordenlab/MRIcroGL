## About

MRIcroGL is a cross=platform tool for viewing DICOM and NIfTI format images. It provides a drag-and-drop user interface as well as a scripting language. Please see the [wiki page](https://www.nitrc.org/plugins/mwiki/index.php/mricrogl:MainPage) from more details. Note that the wiki page describes version 1.0 of the software, while this Github page is for the upcoming version 1.2. The changes are in general subtle, but the [scripting](PYTHON.md) has changed quite a bit.

## Requirements

MRIcroGL 1.2 requires OpenGL 3.3 (released in 2009) or later. If you only have OpenGL 2.1, you can get [MRIcroGL 1.0](https://github.com/neurolabusc/MRIcroGL/releases) instead. If your computer does not support OpenGL at all, you can try [MRIcron](https://www.nitrc.org/projects/mricron).

## Installation

Go to the [release](https://github.com/neurolabusc/MRIcroGL/releases) page and download the latest release for your operating system (MacOS, Linux, Windows). Extract the archive and run the executable.

## Compiling

It is generally recommended that [download a pre-compiled executable](https://github.com/neurolabusc/MRIcroGL/releases). However, you can compile your own copy from source code.

 - Download and install [Lazarus 2.0 or later](https://www.lazarus-ide.org/).
 - Get the [Metal-Demos repository](https://github.com/neurolabusc/Metal-Demos), for example: `git clone https://github.com/neurolabusc/Metal-Demos`.
 - Get the [MRIcroGL12 repository](https://github.com/neurolabusc/MRIcroGL12), for example: `git clone https://github.com/neurolabusc/MRIcroGL12`.
 - If you are using MacOS and want to build for Apple Metal (instead of OpenGL):
 ⋅⋅* Get the [lazmetalcontrol repository](https://github.com/genericptr/Metal-Framework).
 ⋅⋅* Use the Lazarus Package menu to open and install the lazmetal control.
 ⋅⋅* Open the MRIcroGL project with Lazarus and use the "Project Inspector" to add lazmetalcontrol as a dependency.
 ⋅⋅* Uncomment the line '{$DEFINE METALAPI}' in mainunit.pas.
 - You will need [python4lazarus_package](https://github.com/Alexey-T/Python-for-Lazarus), but hopefully Lazarus will detect and install this for you automatically.
 - Use the "Run" command to compile and run your project.
 - Note you can also compile from the command line - [the MRIcroGL 1.0 web page provides details](https://github.com/neurolabusc/MRIcroGL).

## Scripting and Command Line

You can use all the functions of MRIcroGL using the graphical interface. You can also create [Python scripts](PYTHON.md) to get precise results our automate laborious tasks.

You can also control MRIcroGL from the command line.
 - Launch MRIcroGL and have it automatically run a [Python script](PYTHON.md): `mricrogl myscript.py`. This method also allows you to control MRIcroGL from your preferred programming languages.
 - MacOS uses may find it useful to make an alias to MRIcroGL. This will allow you to simply run `mricrogl` from the command line instead of typing the full path. You can do this by running `open -a TextWrangler ~/.bash_profile` and then [adding an alias](http://osxdaily.com/2007/02/01/how-to-launch-gui-applications-from-the-terminal/), for example adding the line `mricrogl='/Applications/MRIcroGL.app/Contents/MacOS/MRIcroGL'` (assuming the application is in this folder).
 - Reset MRIcroGL to its defaults (forget user preferences): `mricrogl -R`.
 - You can choose the images to load from the command line: `MRIcroGL -std -dr 2000 6000   motor -cm actc -dr 2 4`. In this example, the FSL standard image is loaded as a background with a display range of 2000...6000, and the image 'motor' is loaded as an overlay with the 'actc' colormap and a display range of 2...4. Note that the options are a subset of those available for [fsleyes](https://users.fmrib.ox.ac.uk/~paulmc/fsleyes/userdoc/latest/command_line.html).
   * You can provide an image name: 'motor', 'mni152.nii.gz', etc.
   * If you have FSL installed, you can choose one of the standard images: '-std', '-std1mm', '--standard', '--standard1mm'.
   * You can specify a color map. For example '-cm bone'.
   * You can specify a display range, for example '-dr 3 4'.
   * Note that you can load multiple images, and the color map and display range is applied to the most recently specified image.

## Alternatives

 - [MRIcroGL v1.0](https://github.com/neurolabusc/MRIcroGL/releases) is mature and has similar features.
 - [MRIcron](https://www.nitrc.org/projects/mricron) is similar and does not require OpenGL, but it is unable to generate interactive renderings.
 - [FSLeyes](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FSLeyes) has many similar features, as well as rich support for [FSL](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/). Variants require either OpenGL 1.4 or OpenGL 2.1.
 - [Mango](http://ric.uthscsa.edu/mango/) is a nice viewer.
