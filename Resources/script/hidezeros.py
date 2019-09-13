import gl
gl.resetdefaults()
gl.view(16)
#open background image
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayload('spmMotor')
gl.minmax(1, -4, 4)
gl.colorname(1,'blue2red')
gl.opacity(1,80)
gl.orthoviewmm(0,0,60)
gl.zerointensityinvisible(1,1)