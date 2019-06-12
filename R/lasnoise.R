{
  require(magrittr)
  require(lidR)
}

# denoise functions
denoise = function(X, Y, Z, threshold, neighbors){
  
  zMin = order(Z)
  bool = rep(T, length(Z))
  
  bool[Z < 0] = F
  zStep = Z[zMin][-1] - Z[zMin][-length(Z)]
  zStep = which(zStep > threshold*10)
  
  for(zs in zStep)
  {
    if(!bool[zMin[zs]]) next
    
    zval = Z[zMin][zs]
    minDiff = abs(zs - c(1, length(Z))) %>% diff

    if(minDiff < 0)
      bool[ Z > zval ] = F
    else
      bool[ Z <= zval ] = F
  }
  
  for(i in zMin){
    
    if(!bool[i]) next
    
    dist = sqrt( (Z - Z[i])^2 + (Y - Y[i])^2 + (X - X[i])^2)
    dm = dist[ order(dist) ][ 2:(2+neighbors) ] %>% mean
    
    bool[i] = dm < threshold
    
    if(dm < threshold)
      break
  }
  
  for(i in rev(zMin)){

    if(!bool[i]) next
    
    dist = sqrt( (Z - Z[i])^2 + (Y - Y[i])^2 + (X - X[i])^2)
    dm = dist[ order(dist) ][ 2:(2+neighbors) ] %>% mean
    
    bool[i] = dm < threshold
    
    if(dm < threshold)
      break
  }

  return(bool)
}

lasnoise = function(las, threshold=5, neighbors=30){
  las %>% lasfilter( denoise(X,Y,Z,threshold,neighbors) ) %>% return()
}
