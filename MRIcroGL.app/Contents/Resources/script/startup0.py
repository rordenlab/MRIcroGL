import gl
gl.resetdefaults()
pth = '~/afni/'
#the desai atlas comes with AFNI
# if this fails, use File/OpenAFNI to set AFNI folder
gl.loadimage('TT_desai_dd_mpm+tlrc.HEAD')
#set atlas saturation to 20%
gl.minmax(0, 0, 20)
gl.overlayload(pth+'ttest.wav.t0.p3.RT_REML.L-A.allmask+tlrc.HEAD')
gl.minmax(1, 2, 5)
gl.colorname(1,'Plasma')
#make overlay translucent
gl.opacity(1, 75)
#find clusters
#the 'tomography' shader works well with discrete atlases
gl.shadername('Tomography')
gl.cutout(0,1,0.5,0.5,0.5,1)
gl.azimuthelevation(140,15)
