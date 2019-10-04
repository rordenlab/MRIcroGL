import gl
ksteps = 12
ktime= 100
thick = 1.0/ksteps
gl.resetdefaults()
gl.loadimage('spm152')
gl.overlayload('spmMotor')
gl.minmax(1, 4, 4)
gl.shaderadjust('boundThresh', 0.35)
gl.shaderadjust('edgeThresh', 0.42)
gl.shaderadjust('edgeBoundMix',0.05)
gl.shaderadjust('colorTemp', 0.8)
gl.backcolor(255, 255,255)
gl.clipazimuthelevation(0.36, 0, 180)
gl.clipthick(thick)
gl.shaderadjust('overlayClip', 0)
for x in range(1, ksteps):
  gl.clipazimuthelevation(0.001+((x-1)*thick), 0, 180);
  gl.wait(ktime)
  #gl.savebmp(str(x)+'.png')