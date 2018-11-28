import gl
gl.resetdefaults()
ktime= 15
ksteps= 36
gl.resetdefaults()
gl.loadimage('abdo256')
#adjust contrast to show bones
gl.minmax(0, 300, 1200)
for x in range(1, ksteps):
  gl.azimuthelevation(x*10,30)
  gl.wait(ktime)
#adjust color scheme to show kidneys
gl.colorname(0,'CT_Kidneys')
for x in range(1, ksteps):
  gl.azimuthelevation(x*10,30)
  gl.wait(ktime)
