import gl
gl.resetdefaults()
#the desai atlas comes with AFNI
# if this fails, use File/OpenAFNI to set AFNI folder
# gl.loadimage(pth+'stats.FT+orig.HEAD')
#gl.loadimage('TTatlas+tlrc.HEAD')
gl.resetdefaults()
gl.orthoviewmm(0,0,0)
#gl.loadimage('TTatlas+tlrc')
gl.loadimage('/Users/chris/src/AICHAlr')

#gl.generateclusters(0)
#gl.generateclusters(0, 0.5, 32, 1, 0)