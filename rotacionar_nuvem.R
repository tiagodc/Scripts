require(TreeLS)
require(lidR)
require(rlas)

salvaLAZ = function(x, y, z, nome){
  nuv = data.frame(x=x,y=y,z=z)
  lash = list()
  lash$`File Signature` = 'LASF'
  lash$`File Source ID` = 0
  lash$`Global Encoding`= 0
  lash$`Project ID - GUID` = 0
  lash$`Version Major` = 1
  lash$`Version Minor` = 2
  lash$`System Identifier` = ''
  lash$`Generating Software` = 'R'
  lash$`File Creation Day of Year` = 0
  lash$`File Creation Year` = 2017
  lash$`Header Size` = 227
  lash$`Offset to point data` = 227
  lash$`Number of variable length records` = 0
  lash$`Point Data Format ID` = 2
  lash$`Point Data Record Length` = 26
  lash$`Number of point records` = nrow(nuv)
  lash$`Number of 1st return` = 0
  lash$`Number of 2nd return` = 0
  lash$`Number of 3rd return` = 0
  lash$`Number of 4th return` = 0
  lash$`Number of 5th return` = 0
  lash$`X scale factor` = 0.01
  lash$`Y scale factor` = 0.01
  lash$`Z scale factor` = 0.01
  lash$`X offset` = 0
  lash$`Y offset` = 0
  lash$`Z offset` = 0
  lash$`Max X` = max(nuv$x)
  lash$`Min X` = min(nuv$x)
  lash$`Max Y` = max(nuv$y)
  lash$`Min Y` = min(nuv$y)
  lash$`Max Z` = max(nuv$z)
  lash$`Min Z` = min(nuv$z)
  lash$`Variable Length Records` = list()
  
  rlas::writelas(nome, 
                 header = lash, X = nuv$x, Y = nuv$y, Z=nuv$z)
  
  return(nuv)
  
}

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

setwd('~/ProLiDAR/Pilotos/TLS/LAZ')
lazFiles = c('duratex_p1462_girando.laz', 'duratex_p1462_transecto_45.laz', 'duratex_p1464_girando.laz', 'duratex_p1464_transecto_giro.laz', 'duratex_p1480_girando.laz', 'duratex_p1480_transecto_giro.laz', 'klabin_p224_central_rambo45.laz', 'klabin_p224_transecto_45.laz', 'klabin_p228_transecto_45.laz', 'klabin_p232_transecto_45.laz')

for(lazFile in lazFiles){
nuvem45 = readLAS(lazFile)
nuvem45 = with(nuvem45@data, data.frame(x=X, y=Y, z=Z))

lasground = paste('wine ../lasground -i',  lazFile, '-no_bulge -no_stddev -wilderness -odix "_ground" -olaz')
system(lasground)

nuvem45 = readLAS(sub('.laz', '_ground.laz', lazFile))
nuvem45 = with(nuvem45@data, data.frame(x=X, y=Y, z=Z, class=Classification))

chao = nuvem45[nuvem45$class == 2,]
floresta = nuvem45[nuvem45$class != 2,]
#rm(nuvem45)

cen = apply(chao[,1:2], 2, mean)
chao_clip = clip.XY(chao, 10, as.double(cen))

#clear3d()
#rgl.points(chao_clip, col='red', size=2)
#rgl.points(floresta, size=.5)
#axes3d()

sm = cov
vetor_normal = eigen(sm(chao_clip[,-4]))$vectors[,3]
med_chao = apply(chao_clip,2,mean)[-4]


#clear3d()
#bg3d('black')
#rgl.points(chao_clip, size=1.5)
#rgl.lines(c(med_chao[1],med_chao[1]+vetor_normal[1]), c(med_chao[2],med_chao[2]+vetor_normal[2]), c(med_chao[3],med_chao[3]+vetor_normal[3]), col='red')
#axes3d()

az = anguloX(chao_clip[,-4], 'z', sm)
ax = anguloX(chao_clip[,-4], 'x', sm)
ay = anguloX(chao_clip[,-4], 'y', sm)

rz = ifelse(az > 90, 180-az, -az)
rx = ifelse(ay < 90, -ax, ax)   

rot = xyz.rotation.matrix(0, rz, rx)

teste = as.double(vetor_normal %*% rot)
axlen = 10

# {
# clear3d()
# bg3d('black')
# rglAXES(xyz = c(axlen,axlen,axlen))
# rgl.lines(axlen*c(0,teste[1]), axlen*c(0,teste[2]), axlen*c(0,teste[3]), col='white', lwd=3)
# rgl.lines(axlen*c(0,vetor_normal[1]), axlen*c(0,vetor_normal[2]), axlen*c(0,vetor_normal[3]), col='purple', lwd=3)
# rgl.lines(axlen*c(0,vetor_normal[1]), axlen*c(0,vetor_normal[2]), axlen*c(0,0), col='orange', lwd=3)
# 
# rgl.lines(c(med_chao[1],med_chao[1]+vetor_normal[1]), c(med_chao[2],med_chao[2]+vetor_normal[2]), c(med_chao[3],med_chao[3]+vetor_normal[3]), col='yellow')
# rgl.points(chao_clip, size=1.5)
# axes3d()
# }

chao_norm = as.matrix(chao_clip[,-4]) %*% as.matrix(rot)
nuvem45_norm = as.matrix(nuvem45[,-4]) %*% as.matrix(rot)

nuvem45_norm = salvaLAZ(nuvem45_norm[,1], nuvem45_norm[,2], nuvem45_norm[,3], sub('.laz', '_rotacionado.laz', lazFile))

rgl.open()
# clear3d()
bg3d('black')
# rgl.points(chao_clip, size=.5)
rgl.points(chao_norm, col='red')
rgl.points(nuvem45_norm, size=.5)
axes3d()
}

lasground = 'wine lasground -i pinus_45_rotacionado.laz  -no_bulge -no_stddev -replace_z -wilderness -odix "_normalized" -olaz'
system(lasground)

pixels = readLAS('parcela_1180/parado_90_rotacionada_normalized_cloud.las')
pixels = with(pixels@data, data.frame(x = X, y = Y, z=Z))

clear3d()
bg3d('black')
#rgl.points(pixels, col='red')
#rgl.points(chao, col='brown')
rgl.points(pinus, size=.5)
spheres3d(relatorio[,c(1,2,7)], radius = relatorio$r_main, col='red')
axes3d()

relatorio = read.table('pinus_45_rotacionado_normalized_result.txt', head=T)
nrow(relatorio)

arvore = clip.XY(pinus, 1, as.double(relatorio[10,1:2]))
nrow(arvore)
arv = salvaLAZ(arvore$x, arvore$y, arvore$z, 'arvore.laz')

fuste = 
  #pref_HT(arvore, .5, min.den = .02)
  readLAS('arvore_cloud.las')
fuste = with(fuste@data, data.frame(x=X, y=Y, z=Z))
nrow(fuste)
perfil = read.table('arvore_result.txt', head=T)
print(perfil)
modelo = fit_RANSAC_circle(fuste, 1)
modelo$circles

{
  clear3d()
  bg3d('black')
  m = stem.model(modelo)
  rgl.points(fuste, col='green')
  rgl.points(arvore, size=1)
  axes3d()
}

?fit_RANSAC_circle
