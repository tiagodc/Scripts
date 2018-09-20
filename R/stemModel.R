require(TreeLS)

plotCylinder = function(xCenter = 0, yCenter = 0, hBase = 0, hTop = 1, radius = 0.5, col = 'green'){
  
  axis = matrix(c(
    rep(xCenter, 2),
    rep(yCenter, 2),
    seq(hBase, hTop, length.out = 2)
  ), ncol = 3, byrow = F)
  
  cyl = cylinder3d(axis, radius = radius)
  
  mesh = shade3d(addNormals(subdivision3d(cyl, depth = 0)), col = col)
  # mesh = shade3d(cyl, col=col)
}

stemModel = function(modelStack, solidColor='green', pointsColor = 'darkred', bg = 'black'){
  
  stats = modelStack$circles
  
  bg3d(bg)
  apply(stats, 1, function(row){
    
    plotCylinder(row['x'], row['y'], row['z1'], row['z2'], row['r'], solidColor)
    
  })
  
  rgl.points(modelStack$stem.points, size=.5, color = pointsColor)
  
}

isolate = pref_HT(pine)
modelStack = fit_RANSAC_circle(isolate)

stemModel(modelStack)
rgl.points(pine, size=.5, col='white')