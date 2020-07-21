import gl
gl.resetdefaults()
gl.loadimage('chris_t2')
gl.orthoviewmm(0,50,0)
gl.minmax(0, 2, 155)
gl.backcolor(255, 255,255)
gl.invertcolor(0,1)
#This MatCap looks nice for Rendering
#gl.shadermatcap('Cortex')
#gl.clipazimuthelevation(0.5, 0, 90)
