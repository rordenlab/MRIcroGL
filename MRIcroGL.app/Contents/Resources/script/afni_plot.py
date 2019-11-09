import gl
gl.resetdefaults()
pth = '~/afni/'
gl.loadimage(pth+'fitts.FT+orig.HEAD')
gl.overlayload(pth+'stats.FT+orig.HEAD')
gl.loadgraph(pth+'ideal_Arel.1D', 0)
gl.loadgraph(pth+'ideal_Vrel.1D', 1)
gl.graphscaling(3)
gl.volume(1,8)
gl.minmax(1, 6, 12)
gl.colorname(1,'Viridis')
gl.generateclusters(1)
gl.orthoviewmm(0,0,0)