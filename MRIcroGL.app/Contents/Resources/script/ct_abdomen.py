import gl
gl.resetdefaults()
ktime= 30
ksteps= 72
gl.resetdefaults()
gl.loadimage('CT_Abdo')
#adjust contrast to show bones
gl.minmax(0, -1024, 640)
for x in range(1, ksteps):
  gl.azimuthelevation(160+(x*5),30)
  gl.wait(ktime)
#adjust color scheme to show kidneys
gl.colorname(0,'CT_Kidneys')
gl.shadername('Tomography')
#gl.shaderadjust('brighten', 1.2)
for x in range(1, ksteps):
  gl.azimuthelevation(160+(x*5),30)
  gl.wait(ktime)

