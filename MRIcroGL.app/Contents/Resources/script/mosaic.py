import gl
gl.resetdefaults()
#open background image
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayload('spmMotor')
gl.minmax(1, 4, 4)
gl.opacity(1,50)
#open overlay: show negative regions
gl.overlayload('spmMotor')
gl.minmax(2, -4, -4)
gl.colorname (2,"3blue")
gl.mosaic("A L+ H -0.2 -24 -16 16 40; 48 56 S X R 0");