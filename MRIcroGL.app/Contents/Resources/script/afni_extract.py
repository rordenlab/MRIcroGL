import gl
gl.resetdefaults()
gl.backcolor(255,255,255)
pth = '~/afni/'
gl.loadimage(pth+'FT_anat+orig.HEAD')
gl.overlayload('TSNR.FT+orig.HEAD')
gl.opacity(1, 50)
gl.extract(1,1,3)
gl.clipazimuthelevation(0.3, 0, 120)