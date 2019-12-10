require(TreeLS)
require(optimx)

rotateCloud = function(pars, cb){

  cb = cb %*% rotationMatrix(pars[1], pars[2], pars[3])
  cb[,1] = cb[,1] + pars[4]
  cb[,2] = cb[,2] + pars[5]
  cb[,3] = cb[,3] + pars[6]

  return(cb)
}

sumMinDists = function(pars, ca, cb){

  cb = rotateCloud(pars, cb)

  d1 = nrow(ca)
  d2 = nrow(cb)

  distMat = dist(rbind(ca, cb),
                 method = 'euclidean',
                 diag = T, upper = T)

  distMat = as.matrix(distMat)
  distMat = distMat[1:d1, (d1+1):(d1+d2)]
  minDists = apply(distMat, 1, min)

  return(sum(minDists))
}

init = rep(0,6)
cb = tlsSample(newrawFrame, randomize(.05))@data[,1:3] %>% as.matrix
ca = tlsSample(regFrame, randomize(.05))@data[,1:3] %>% as.matrix
opt = optimx(init, sumMinDists, ca=ca, cb=cb)

clear3d()
bg3d('black')
rgl.points(ca)
rgl.points(cb, col='red')

pars = as.double(opt[2,1:6])
cc = rotateCloud(pars, cb)
spheres3d(as.data.frame(cc), radius = .2, col='blue', size=2)

