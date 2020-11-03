import gl
gl.resetdefaults()
gl.overlayloadsmooth(1)
gl.loadimage('spm152')
gl.overlayload('spmMotor')
gl.removesmallclusters(1, 3.5, 800)
gl.minmax(1, 3.5, 3.5)
gl.overlayload('spmMotor')
gl.removesmallclusters(2, -2.5, 800)
gl.minmax(2, -2.5, -2.5)
print("overlays only have large clusters");