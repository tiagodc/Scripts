require(TreeLS)
require(magrittr)
require(alphashape3d)
require(cppRouting)

file = '~/Downloads/tropical_tree.las'
# file = system.file("extdata", "pine.laz", package="TreeLS")
las = readTLSLAS(file)

## ---
# PRE-PROCESS AND FILTER POINT CLOUD
las = filter_duplicates(las)
las = nnFilter(las, d=.05, n = 5)
las = tlsSample(las, smp.voxelize(.025))

las = tlsNormalize(las, keep_ground = F)
z = las@data$Z
las = unnormalize_height(las)
las@data$Height = z

las = fastPointMetrics(las, ptm.knn(r = .05), "Anisotropy")
las = filter_poi(las, Anisotropy > .8)

nrow(las@data)
plot(las, color='Anisotropy')

## ---
# APPLY TRIANGLE MESHING WITH ALPHA SHAPES
alpha = .05

ash = ashape3d(TreeLS:::las2xyz(las), alpha = alpha, pert = T, eps = 1e-6)
ashcp = components_ashape3d(ash)
las@data$AlphaComp = ashcp

plot(las, clear_artifacts=F)
plot(ash, byComponents = TRUE, clear=F)
rgl::bg3d('black')

## ---
# FILTER CONNECTED COMPOONENTS
comp_centers = las@data[,.(X=mean(X), Y=mean(Y), Z=mean(Z), H=mean(Height), .N), by=AlphaComp]
comp_centers = comp_centers[order(N, decreasing = T)]

keep_centers = comp_centers[AlphaComp > 0 & N > 5 & H > 1]
keep_centers$ClosestMainPt = 0
main_comp_id = keep_centers$AlphaComp[1]

las@data$MergeComps = las@data$AlphaComp
{
# sec_centers = keep_centers[-1,]
# 
# d = .2
# while(nrow(sec_centers) > 0){
#   print(nrow(sec_centers))
#   knn_dists = nabor::knn(las@data[MergeComps == main_comp_id,.(X,Y,Z)], sec_centers[,.(X,Y,Z)], k = 1)
#   
#   sec_centers$ClosestMainPt = knn_dists$nn.dists %>% as.double
#   merge_ids = sec_centers[ClosestMainPt < d]$AlphaComp
#   
#   if(length(merge_ids) == 0) break
#   
#   sec_centers = sec_centers[ClosestMainPt >= d]
#   las@data[MergeComps %in% merge_ids]$MergeComps = main_comp_id
# }
}

keep = las@data$MergeComps == main_comp_id
keep_ids = (1:nrow(las@data))[keep]

ash$vertex = ash$vertex[ash$vertex[,1] %in% keep_ids,]
ash$edge   = ash$edge[ (ash$edge[,1] %in% keep_ids) & (ash$edge[,2] %in% keep_ids) ,]
ash$triang = ash$triang[ (ash$triang[,1] %in% keep_ids) & (ash$triang[,2] %in% keep_ids) & (ash$triang[,3] %in% keep_ids) ,]
ash$tetra  = ash$tetra[ (ash$tetra[,1] %in% keep_ids) & (ash$tetra[,2] %in% keep_ids) & (ash$tetra[,3] %in% keep_ids) & (ash$tetra[,4] %in% keep_ids) ,]

# ashvol = volume_ashape3d(ash, byComponents = T)

plot(las, clear_artifacts=F)
plot(ash, byComponents = TRUE, clear=F)


## ---
# EXTRACT GRAPH
las@data$Vertex = 1:nrow(las@data)
las_graph = filter_poi(las, MergeComps == main_comp_id)

edges = cbind(ash$edge[ash$edge[,8] > 1,1:2], cost = 0)

add_comps = unique(las_graph@data$AlphaComp)
add_comps = add_comps[add_comps != main_comp_id]

for(i in add_comps){
  main_graph = las_graph@data[AlphaComp == main_comp_id]
  temp = las_graph@data[AlphaComp == i]
  nn_dists = nabor::knn( main_graph[,.(X,Y,Z)], temp[,.(X,Y,Z)], 1)
  
  add_from = main_graph$Vertex[ nn_dists$nn.idx[which.min(nn_dists$nn.dists)] ]
  add_to = temp$Vertex[which.min(nn_dists$nn.dists)]
  
  edges = rbind(edges, c(add_from, add_to, min(nn_dists$nn.dists)))
  las_graph@data[AlphaComp == i]$AlphaComp = main_comp_id
}

from = edges[,1]
to = edges[,2]

edges[,3] = sqrt( (ash$x[from,1] - ash$x[to,1])^2 + (ash$x[from,2] - ash$x[to,2])^2 + (ash$x[from,3] - ash$x[to,3])^2 )
head(edges)

pgr = igraph::graph_from_data_frame(edges[1:150,])
plot(pgr)


## ---
# DETECT MULTIPLE TREES
graph_base = filter_poi(las_graph, Height < 1)
dbnn = dbscan::dbscan(TreeLS:::las2xyz(graph_base), .25)
graph_base@data$cluster = dbnn$cluster
center_base = graph_base@data[,.(X=mean(X), Y=mean(Y)), by=cluster]

pts = c()
for(i in 1:nrow(center_base)){
  cen = center_base[i,]
  dst = sqrt( (graph_base@data$X - cen$X)^2 + (graph_base@data$Y - cen$Y)^2 )
  pts = c(pts, graph_base@data$Vertex[which.min(dst)])
}

nodes = edges[,1:2] %>% as.vector %>% unique
grp = makegraph(edges, F) %>% cpp_simplify


## ---
# SPLIT SINGLE TREE GRAPHS
plot(las_graph, clear_artifacts=F)

first = pts[1]
rgl::spheres3d(ash$x[first,], radius = .05, color='red')

last = pts[2]
rgl::spheres3d(ash$x[last,], radius = .05, color='red')
spath = get_path_pair(grp, first, last, 'Dijkstra')[[1]] %>% as.integer

rgl::lines3d(ash$x[spath,], color='orange', lwd=10)

temp_edges = edges
rm_start = .05

grp = makegraph(temp_edges, F) %>% cpp_simplify
spath = get_path_pair(grp, first, last, 'Dijkstra')[[1]] %>% as.integer
sdist = get_distance_pair(grp, first, last, 'Dijkstra')

spath_pts = las@data[spath,]
top = spath_pts[which.max(Z),]

rm_spheres = data.table()
rm_ids = c()
rm_rad = rm_start
iter = 1
repeat{
  top_row = top[,.(X,Y,Z)]
  top_row$radius = rm_rad
  rm_spheres = rbind(rm_spheres, top_row)
  
  print(iter)
  rm_pts = las_graph@data[sqrt((X - top$X)^2 + (Y - top$Y)^2 + (Z - top$Z)^2) < rm_rad]
  rm_edges = (temp_edges[,1] %in% rm_pts$Vertex) | ((temp_edges[,2] %in% rm_pts$Vertex))
  temp_edges = temp_edges[!rm_edges,]
  
  rm_ids = c(rm_ids, rm_pts$Vertex) %>% unique
  
  igrp = makegraph(temp_edges, F) %>% cpp_simplify
  ispath = get_path_pair(igrp, first, last, 'Dijkstra')[[1]] %>% as.integer
  if(length(ispath) == 0) break
  isdist = get_distance_pair(igrp, first, last, 'Dijkstra')
  
  spath_pts = las@data[ispath,]
  top = spath_pts[which.max(Z),]

  if(abs(isdist - sdist) > rm_rad){
    rm_rad = rm_start
    sdist = isdist
    iter = iter+1
    
    rgl::lines3d(ash$x[ispath,], color='orange', lwd=10)
  }else{
    rm_rad = rm_rad + rm_start
  }
  
}

ash$vertex = ash$vertex[!(ash$vertex[,1] %in% rm_ids),]
ash$edge   = ash$edge[ !(ash$edge[,1] %in% rm_ids) & !(ash$edge[,2] %in% rm_ids) ,]
ash$triang = ash$triang[ !(ash$triang[,1] %in% rm_ids) & !(ash$triang[,2] %in% rm_ids) & !(ash$triang[,3] %in% rm_ids) ,]
ash$tetra  = ash$tetra[ !(ash$tetra[,1] %in% rm_ids) & !(ash$tetra[,2] %in% rm_ids) & !(ash$tetra[,3] %in% rm_ids) & !(ash$tetra[,4] %in% rm_ids) ,]

rm_pts = las@data[rm_ids,.(X,Y,Z)]
rgl.points(rm_pts, size=5, color='red')
spheres3d(rm_spheres[,.(X,Y,Z)], radius = rm_spheres$radius, color='red', alpha=.66)


## ---
# SPLIT TREES INTO SINGLE GRAPHS
ashcp = components_ashape3d(ash)
las@data$AlphaCompFinal = ashcp

comp_pts = las@data[,.N,by=AlphaCompFinal][AlphaCompFinal > 0]
trees = comp_pts[,.(AlphaCompFinal, ratio=N/max(N))][ratio > .8]

plot(las_graph, clear_artifacts=F)
plot(ash, clear=F, byComponents=T)


## ---
# CROWN CONVEX HULL
tree = filter_poi(las, AlphaCompFinal == 2)
# tree = las_graph

plot(tree, clear_artifacts=F)

conv_hull = geometry::convhulln(TreeLS:::las2xyz(tree)) %>% as.vector %>% unique
spheres3d(tree@data[conv_hull,.(X,Y,Z)], radius=.05, color='white')

xtremes = tree@data[conv_hull,.(X,Y,Z)]
xtremes$row = conv_hull
xt_clt = dbscan::dbscan(xtremes[,.(X,Y,Z)], .5, minPts = 1)
xtremes$cluster = xt_clt$cluster
xmeans = xtremes[,.(X=mean(X),Y=mean(Y),Z=mean(Z)),by=cluster]
# spheres3d(xmeans[,.(X,Y,Z)], radius=.075, color='green')

branch_tips = data.table()
for(i in unique(xtremes$cluster)){
  temp = xtremes[cluster == i]
  branch_tips = rbind(branch_tips, temp[sample(1:nrow(temp), 1)])
}

spheres3d(branch_tips[,.(X,Y,Z)], radius=.1, color='red')
ash_tree = ashape3d(TreeLS:::las2xyz(tree), alpha = alpha, pert = T, eps = 1e-6)


## ---
# SHORTEST PATH
plot(tree, clear_artifacts=F)

edges = cbind(ash_tree$edge[ash_tree$edge[,8] > 1,1:2], 0)
from = edges[,1]
to = edges[,2]

edges[,3] = sqrt( (ash_tree$x[from,1] - ash_tree$x[to,1])^2 + (ash_tree$x[from,2] - ash_tree$x[to,2])^2 + (ash_tree$x[from,3] - ash_tree$x[to,3])^2 )

grp = makegraph(edges, F) #%>% cpp_simplify

first = branch_tips[which.min(Z),]$row
rgl::spheres3d(ash_tree$x[first,], radius = .05, color='red')

for(last in branch_tips$row){
  if(last == first) next
  
  rgl::spheres3d(ash_tree$x[last,], radius = .05, color='white')
  spath = get_path_pair(grp, first, last, 'Dijkstra')[[1]] %>% as.integer
  dst = get_distance_pair(grp, first, last, 'Dijkstra')
  print(dst)
  
  rgl::lines3d(ash_tree$x[spath,], color='green', lwd=10)
}



## TREE N.2
## ---
# CROWN CONVEX HULL
tree = filter_poi(las, AlphaCompFinal == 1)
# tree = las_graph

plot(tree, clear_artifacts=F)

conv_hull = geometry::convhulln(TreeLS:::las2xyz(tree)) %>% as.vector %>% unique
spheres3d(tree@data[conv_hull,.(X,Y,Z)], radius=.05, color='white')

xtremes = tree@data[conv_hull,.(X,Y,Z)]
xtremes$row = conv_hull
xt_clt = dbscan::dbscan(xtremes[,.(X,Y,Z)], .5, minPts = 1)
xtremes$cluster = xt_clt$cluster
xmeans = xtremes[,.(X=mean(X),Y=mean(Y),Z=mean(Z)),by=cluster]
# spheres3d(xmeans[,.(X,Y,Z)], radius=.075, color='green')

branch_tips = data.table()
for(i in unique(xtremes$cluster)){
  temp = xtremes[cluster == i]
  branch_tips = rbind(branch_tips, temp[sample(1:nrow(temp), 1)])
}

spheres3d(branch_tips[,.(X,Y,Z)], radius=.1, color='red')
ash_tree = ashape3d(TreeLS:::las2xyz(tree), alpha = alpha, pert = T, eps = 1e-6)


## ---
# SHORTEST PATH
plot(tree, clear_artifacts=F)

edges = cbind(ash_tree$edge[ash_tree$edge[,8] > 1,1:2], 0)
from = edges[,1]
to = edges[,2]

edges[,3] = sqrt( (ash_tree$x[from,1] - ash_tree$x[to,1])^2 + (ash_tree$x[from,2] - ash_tree$x[to,2])^2 + (ash_tree$x[from,3] - ash_tree$x[to,3])^2 )

grp = makegraph(edges, F) #%>% cpp_simplify

first = branch_tips[which.min(Z),]$row
rgl::spheres3d(ash_tree$x[first,], radius = .05, color='red')

for(last in branch_tips$row){
  if(last == first) next
  
  rgl::spheres3d(ash_tree$x[last,], radius = .05, color='white')
  spath = get_path_pair(grp, first, last, 'Dijkstra')[[1]] %>% as.integer
  dst = get_distance_pair(grp, first, last, 'Dijkstra')
  print(dst)
  
  rgl::lines3d(ash_tree$x[spath,], color='green', lwd=10)
}
