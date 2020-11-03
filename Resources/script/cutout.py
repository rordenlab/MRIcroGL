import gl
gl.resetdefaults()
#open background image
gl.loadimage('spm152')
gl.overlayloadsmooth(1)
#open overlay: show positive regions
gl.overlayload('spmMotor')
gl.minmax(1, -2,-5)
gl.colorname(1,'8redyell')
gl.colorfromzero(1,1)
gl.overlayload('spmMotor')
gl.minmax(2, 2,5)
gl.colorname(2,'5winter')
gl.colorfromzero(2,1)
gl.hiddenbycutout(1,1)
gl.cutout(0.5,0.5,0.5,0,1,1)
gl.shadername('OverlaySurface')
gl.backcolor(255,255,255)