require(TreeLS)
require(rlas)
require(lidR)
require(stringr)

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

treatCloud = c(#"klabin_p1180_parado_90.laz", #done 
               #"klabin_p224_central_rambo.laz", #done
               #"klabin_p228_transecto_45.laz", #done
               #klabin_p228_transecto.laz", #done
               #"klabin_p5830_girando.laz", #done
               # "klabin_p5830_girando_rotacionado.laz",
               # "klabin_p232_transecto_45.laz",
               # "klabin_p228_aleatorio.laz",
               # "labin_p228_aleatorio2.laz",
               # "klabin_p224_u.laz",
               # "klabin_p224_transecto_90.laz",
               # "klabin_p224_transecto_45_rotacionado.laz",
               # "klabin_p224_transecto_45.laz",
               # "klabin_p224_central_rambo45.laz",
               # "duratex_p1480_transecto_giro.laz",
               # "duratex_p1464_transecto_giro.laz",
               # "duratex_p1462_transecto_45.laz",
  'klabin_p5830_girando_rotacionado.laz',
  'klabin_p228_aleatorio.laz',
  'klabin_p224_transecto_90.laz'
               )

i = 5
cloud = readLAS(treatCloud[i], XYZonly = T)

vec = eigen(cov(cloud@data))
vec = vec[[2]][,3]

ax = angle(c(0,0,1), vec)
az = angle(c(1,0), vec[-3])
ax2 = angle(c(0,1), vec[-3])

cloud2 = as.matrix(cloud@data) #%*% xyz.rotation.matrix(-ax,-az,0)
cloud2 = as.data.frame(cloud2)
names(cloud2) = c('X', 'Y', 'Z')

clear3d(); rgl.points(cloud2, size=.5) ; axes3d(col='red')
apply(cloud2, 2, range)
cloud2[,3] = -cloud2[,3]

writeLAS(LAS(cloud2),treatCloud[i])

lasground = paste('wine ../lasground -i',  'temp/cloud.laz', '-no_bulge -no_stddev -wilderness -odix "_ground" -olaz')

system(lasground)

cloud3 = readLAS('temp/cloud_ground.laz')
chao = cloud3@data[ cloud3@data$Classification == 2 ,]

clear3d(); rgl.points(chao, size=2, col='red') ; rgl.points(cloud3@data, size=.5) ; axes3d(col='red')

files = dir('normalized', pattern = '\\.laz', full.names = T)

for(i in files){
  laz = readLAS(i, XYZonly = T)
  
  stn = sub('\\_n.laz', '_stems.laz', i)
  stn = sub('normalized', 'stems', stn)
  stems = readLAS(stn, XYZonly = T)
  
  rgl.open()
  bg3d('black')
  rgl.points(stems@data, col='green')  
  rgl.points(laz@data, col='white', size=.5)
  axes3d(col = 'red')
  
}

nomeNuvem = function(rglNumber){
  return(files[ rglNumber-49 ])
}

nomeNuvem(54)
