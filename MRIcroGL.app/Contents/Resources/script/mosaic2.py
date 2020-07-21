import gl
gl.resetdefaults()
gl.backcolor(255, 255, 255)
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
gl.colorbarposition(0)
#"a"xial, "c"oronal and "s"agittal "r"enderings
gl.mosaic("A R 0 C R 0 S R 0; A R -0 C R -0 S R -0");