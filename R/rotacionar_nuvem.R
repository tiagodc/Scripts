require(TreeLS)
require(lidR)
require(magrittr)

anguloX = function(XYZplane, eixo='z', stat_mat=cov){
  e = eigen(stat_mat(XYZplane))
  #if(e$vectors[3,3] < 0) e$vectors = -e$vectors
  if(eixo != 'z') e$vectors[3,3] = 0
  
  vetor_eixo = if(eixo=='z') c(0,0,1) else if(eixo=='x') c(1,0,0) else c(0,1,0)
  ang = ( e$vectors[,3] %*% vetor_eixo ) / ( sqrt(sum(e$vectors[,3]^2)) * sqrt(sum(vetor_eixo^2)) )
  ang = ang[,,drop=T]
  degs = acos(ang)*180/pi
  return(degs)
}

rotationMatrix = function (ax, az, ax2){
  ax = ax * pi/180
  Rx = matrix(c(1, 0, 0, 0, cos(ax), sin(ax), 0, -sin(ax), 
                cos(ax)), ncol = 3, byrow = T)
  az = az * pi/180
  Rz = matrix(c(cos(az), 0, -sin(az), 0, 1, 0, sin(az), 0, 
                cos(az)), ncol = 3, byrow = T)
  ax2 = ax2 * pi/180
  Rx2 = matrix(c(cos(ax2), sin(ax2), 0, -sin(ax2), cos(ax2), 
                 0, 0, 0, 1), ncol = 3, byrow = T)
  ro.mat = Rx2 %*% Rz %*% Rx
  return(ro.mat)
}

rotateCloud = function(file, lasDir){
  
  cmd = paste0('wine ', lasDir, 'lasground.exe -i ', file, 
               ' -odix _temp -olaz -no_bulge -no_stddev -wilderness')
  system(cmd)
  
  file = sub('\\.laz','_temp.laz',file)
  cloud = readLAS(file)
  unlink(file, force = T)
  
  chao = lasfilter(cloud, Classification == 2) 
  cen = apply(chao@data[,1:2], 2, mean) %>% as.double
  chao_clip = lasclipCircle(chao, cen[1], cen[2], 10)
  
  az = anguloX(chao_clip@data[,1:3], 'z', cov)
  ax = anguloX(chao_clip@data[,1:3], 'x', cov)
  ay = anguloX(chao_clip@data[,1:3], 'y', cov)
  
  rz = ifelse(az > 90, 180-az, -az)
  rx = ifelse(ay < 90, -ax, ax)   
  
  rot = rotationMatrix(0, rz, rx)
  
  cloud_norm = ( as.matrix(cloud@data[,1:3]) %*% as.matrix(rot) ) %>% as.data.frame
  
  cloud@data[,1:3] = cloud_norm
  
  return(lasfilter(cloud, Classification != 2))
}


