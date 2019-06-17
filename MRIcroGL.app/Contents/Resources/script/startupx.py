import gl
gl.resetdefaults()
gl.view(16)
#open background image
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayload('spmMotor')
gl.minmax(1, -4, 4)
gl.colorname(1,'actc')
gl.opacity(1,50)