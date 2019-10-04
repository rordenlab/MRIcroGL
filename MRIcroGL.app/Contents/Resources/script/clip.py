import gl
gl.resetdefaults()
#open background image
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayload('spmMotor')
gl.minmax(1, 4, 4)
gl.opacity(1,50)
gl.clipazimuthelevation(0.4, 0, 160)