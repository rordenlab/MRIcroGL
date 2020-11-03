import gl
gl.resetdefaults()
#open background image
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayloadsmooth(1)
gl.overlayload('spmMotor')
gl.minmax(1, 4, 4)
gl.opacity(1,50)
#open overlay: show negative regions
gl.overlayloadsmooth(0)
gl.overlayload('spmMotor')
gl.minmax(2, -4, -4)
gl.colorname (2,"3blue")
gl.overlayloadsmooth(1)
print("n.b. Blue surface is jagged (smoothing off)");
