import gl
gl.resetdefaults()
gl.orthoviewmm(0,0,0)
gl.loadimage('/Users/chris/V1.nii.gz')
#gl.loadimage('/Users/chris/FA.nii.gz')
#open overlay: show positive regions
#gl.overlayload('/Users/chris/V1.nii.gz')
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