import gl
gl.resetdefaults()
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayload('aal')
gl.scriptformvisible(1)
gl.overlayload('spmMotor')
gl.minmax(2, 4, 4)
#gl.orthoviewmm(37,-14,47)