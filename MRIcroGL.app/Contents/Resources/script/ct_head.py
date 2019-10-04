import gl
gl.resetdefaults()
gl.loadimage('CT_Philips')
gl.colorname(0,'CT_Kidneys')
gl.shadername('Tomography')
gl.azimuthelevation(150,10)
gl.clipazimuthelevation(0.25, 180, 60)
