import gl
gl.resetdefaults()
gl.loadimage('brT1s005+tlrc.HEAD')
gl.overlayload('stats.s1+tlrc.HEAD')
#gl.minmax(1, 5, 5)
gl.generateclusters(1)
gl.orthoviewmm(0,0,0)