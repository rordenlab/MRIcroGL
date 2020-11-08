import gl
gl.resetdefaults()
gl.loadimage('spm152')
#open overlay: show positive regions
gl.overlayload('aal')
gl.scriptformvisible(1)
#gl.atlashide(1)
#gl.atlasshow(1, 17)
gl.atlasshow(1, (22, 23, 17))
mx = gl.atlasmaxindex(1)
print(mx)
#gl.opacity(1,50)
#
#  if (layer > 0) then
#     Vol1.UpdateOverlays(vols); 
#str = gl.atlaslabels(1)
#print(str)
#gl.minmax(1, 4, 100)


#gl.overlayload('aal')
#

#gl.generateclusters(0)
#gl.generateclusters(0, 0.5, 32, 1, 0)