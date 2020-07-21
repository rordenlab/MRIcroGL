import gl
gl.resetdefaults()
gl.loadimage('/Users/chris/Neuro/MRIcroGLold/templates/inia19-NeuroMaps.nii.gz')
gl.scriptformvisible(1)
gl.atlashide(0)
gl.atlasshow(0, 151,153,129)
gl.atlasshow(0, 1215,1,3,7)
print('Regions '+ str(gl.atlasmaxindex(0)) )
#print(gl.atlaslabels(0))