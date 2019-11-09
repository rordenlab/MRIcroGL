import gl
gl.resetdefaults()
pth = '~/fdr/'
#the desai atlas comes with AFNI
# if this fails, use File/OpenAFNI to set AFNI folder
# gl.loadimage(pth+'stats.FT+orig.HEAD')
#gl.loadimage('TTatlas+tlrc.HEAD')
gl.loadimage(pth+'p05n00+tlrc.HEAD')
gl.orthoviewmm(0,0,0);