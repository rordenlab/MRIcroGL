## About

MRIcroGL is a cross=platform tool for viewing DICOM and NIfTI format images. It provides a drag-and-drop user interface as well as a scripting language. Please see the [wiki page](https://www.nitrc.org/plugins/mwiki/index.php/mricrogl:MainPage) from more details. Note that the wiki page describes version 1.0 of the software, while this Github page is for the upcoming version 1.2. The changes are in general subtle, but the [scripting](PYTHON.md) has changed quite a bit.

## Requirements

MRIcroGL 1.2 requires OpenGL 3.3 (released in 2009) or later. If you only have OpenGL 2.1, you can get [MRIcroGL 1.0](https://github.com/neurolabusc/MRIcroGL/releases) instead. If your computer does not support OpenGL at all, you can try [MRIcron](https://www.nitrc.org/projects/mricron).

## Installation

You can get MRIcroGL using three methods:

 - Download from [NITRC](https://www.nitrc.org/projects/mricrogl/).
 - Download from [Github](https://github.com/neurolabusc/MRIcroGL/releases).
 - Run the following command to get the latest version for Linux, Macintosh or Windows: 
   * `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_linux.zip`
   * `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_macOS.dmg`
   * `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_windows.zip`

  
Once you have downloaded the software, extract the archive and run the executable. Visit the [wiki](https://www.nitrc.org/plugins/mwiki/index.php/mricrogl:MainPage) for a full manual that includes troubleshooting.


## Compiling

It is generally recommended that [download a pre-compiled executable](https://github.com/neurolabusc/MRIcroGL/releases). However, you can compile your own copy from source code.

 - Download and install [Lazarus 2.0 or later](https://www.lazarus-ide.org/).
 - Get the [Metal-Demos repository](https://github.com/neurolabusc/Metal-Demos), for example: `git clone https://github.com/neurolabusc/Metal-Demos`.
 - Get the [MRIcroGL12 repository]( https://github.com/rordenlab/MRIcroGL12.git), for example: `git clone https://github.com/neurolabusc/MRIcroGL12`.
 - If you are using MacOS and want to build for Apple Metal (instead of OpenGL):
   * Get the [lazmetalcontrol repository](https://github.com/genericptr/Metal-Framework).
   * Use the Lazarus Package menu to open and install the lazmetal control.
   * Open the MRIcroGL project with Lazarus and use the "Project Inspector" to add lazmetalcontrol as a dependency.
   * Uncomment the line '{$DEFINE METALAPI}' in mainunit.pas.
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

## Supported Image Data

MRIcroGL uses NIfTI as its native format. However, you can drag-and-drop files of various formats and the software should automatically detect and load these images.

 - [AFNI Brik](https://afni.nimh.nih.gov/pub/dist/doc/program_help/README.attributes.html)(.head).
 - [Analyze](http://imaging.mrc-cbu.cam.ac.uk/imaging/FormatAnalyze)(.hdr).
 - [Bio-Rad PIC](https://docs.openmicroscopy.org/bio-formats/5.8.2/formats/bio-rad-pic.html)(.pic).
 - [Blender Voxel data](http://pythology.blogspot.com/2014/08/you-can-do-cool-stuff-with-manual.html)(.bvox).
 - [BrainVoyager VMR](https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/343-developer-guide-2-6-the-format-of-vmr-files)(.vmr, .v16).
 - [DeltaVision](https://docs.openmicroscopy.org/bio-formats/5.8.2/formats/deltavision.html)(.dv).
 - [DICOM](http://people.cas.sc.edu/rorden/dicom/index.html)(varies).
 - [ECAT](http://nipy.org/nibabel/reference/nibabel.ecat.html)(.v).
 - [FreeSurfer MGH/MGZ Volume](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat)(.mgh/.mgz).
 - [Guys Image Processing Lab](http://rview.colin-studholme.net/rview/rv9manual/fileform.html#GIPL)(.gipl).
 - [ICS Image Cytometry Standard](https://onlinelibrary.wiley.com/doi/epdf/10.1002/cyto.990110502)(.ics).
 - [ITK MHA/MHD](https://itk.org/Wiki/MetaIO/Documentation)(.mha/.mhd).
 - [MRTrix Volume](https://mrtrix.readthedocs.io/en/latest/getting_started/image_data.html)(.mif/.mih).
 - [NIfTI](https://brainder.org/2012/09/23/the-nifti-file-format/)(.hdr/.nii/.nii.gz/.voi).
 - [NRRD](http://teem.sourceforge.net/nrrd/format.html)(.nhdr/.nrrd).
 - [TIFF via import menu](https://en.wikipedia.org/wiki/TIFF)(.tif/.tiff/varies).
 - [VTK Legacy Voxel Format](https://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf)(.vtk).

## Alternatives

There are many terrific free tools for viewing medical imaging data. Since they are free, consider downloading a few and using the best tool for the task at hand. Below are a couple of my personal favorites.

 - [MRIcroGL v1.0](https://github.com/neurolabusc/MRIcroGL/releases) is mature and has similar features.
 - [MRIcron](https://www.nitrc.org/projects/mricron) is similar and does not require OpenGL, but it is unable to generate interactive renderings.
 - [FSLeyes](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FSLeyes) has many similar features, as well as rich support for [FSL](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/). Variants require either OpenGL 1.4 or OpenGL 2.1.
 - [Mango](http://ric.uthscsa.edu/mango/) is a nice viewer.
