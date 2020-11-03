import gl
gl.resetdefaults()
gl.loadimage('spm152')
#open overlay: show only 3 regions
gl.overlayload('aal')
gl.atlasshow(1, (22, 23, 17))
#gl.atlashide(1)
#gl.atlasshow(1, 17)
#gl.atlashide(1, (22, 23, 17))
