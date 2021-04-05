


ptdist <- function(X,Y){
  if(is.vector(Y)){
    Y = t(as.matrix(Y))
  }
  
  XY = as.matrix(Y - X)
  colnames(XY) <- c("x", "y", "z")
  XYdist = apply(Y[,1:2], 1, function(x,y){dist(rbind(x,y))}, y= X[1:2])
  
  return(cbind(XY,XYdist))
}


clkWindow <- function(){
  
  rect <- rgl.select(button = "left", dev = cur3d(), subscene = currentSubscene3d())
  
  clkprj <- rgl.projection(dev = cur3d(),
                           subscene = currentSubscene3d())
  
  wintrictrs <- rgl.user2window(trictrs, projection = clkprj)
  wintri <- rgl.user2window(tri, projection = clkprj)
  
  winvect <- cbind(matrix(rect,ncol = 2, byrow = T), c(0,1))
  objvect <- t(apply(winvect, MARGIN= 1,
                     FUN = function(X,...){ rgl.window2user(x=X[1],
                                                            y=X[2],
                                                            z=X[3],
                                                            projection = clkprj)
                     },
                     dev, subscene)
  )
  
  
  
}


clkPoint <- function(plot = T){
  
  rect <- rgl.select(button = "left", dev = cur3d(), subscene = currentSubscene3d())
  
  clkprj <- rgl.projection(dev = cur3d(),
                           subscene = currentSubscene3d())
  
  wintrictrs <- rgl.user2window(trictrs, projection = clkprj)
  wintri <- rgl.user2window(tri, projection = clkprj)
  
  winvect <- cbind(matrix(rect,ncol = 2, byrow = T), c(0,1))
  objvect <- t(apply(winvect, MARGIN= 1,
                     FUN = function(X,...){ rgl.window2user(x=X[1],
                                                            y=X[2],
                                                            z=X[3],
                                                            projection = clkprj)
                     },
                     dev, subscene)
  )
  
  
  # pt2ctrdists <- ptdist(X = winvect[1,], Y =  wintrictrs) # calculate window XY and Z distances
  # XYdistindex <- order(pt2ctrdists[,4], decreasing = F)[1:50] # get closest centers sorted by window XY distance
  # 
  tri.i <- seq(1, nrow(tri),by = 3 ) # make a dummy triangle index
  verts <- as.vector(sapply(tri.i, FUN = function(X,Y) X + c(0:2))) # triangle vertex index
  
  wintriA <- wintri[verts,]
  objtriA <- tri[verts,]
  rownames(wintriA) <- verts
  
  require(sp)
  
  pt.inside.tri <- sapply(X = tri.i, FUN = function(X,PT,TRI){
    (point.in.polygon(PT[1,1],PT[1,2], TRI[X++c(0:2),1],TRI[X++c(0:2),2])==1)
  }, PT = winvect, TRI = wintri)
  
  
  subtending.tri.ind <- t(sapply(tri.i[pt.inside.tri], FUN = function(X) X+c(0:2)))  
  
  firstTRI <- mean(wintri[t(subtending.tri.ind)[1:3],3]) <  mean(wintri[t(subtending.tri.ind)[4:6],3])
  
  if(firstTRI){
    
    clktri <- tri[t(subtending.tri.ind)[1:3],]
    
  } else{
    
    clktri <- tri[t(subtending.tri.ind)[4:6],]
    
  }
  
  # CALC CLK COORDS
  
  colnames(objvect) <- c("x", "y", "z")
  decomptri<- prcomp(as.matrix(clktri))
  pcvect <- as.data.frame(predict(decomptri,objvect))
  
  a = pcvect[1,]
  b = pcvect[2,]
  
  t = (0 - a[3])/ (b[3] - a[3]) 
  
  ptcoords <- a+unlist(lapply((b-a), FUN = function(X,Y) Y*X, Y=t))
  clkpt <- (as.matrix(ptcoords) %*% t(decomptri$rotation) ) + decomptri$center
  
  if(plot){
    points3d(clkpt, col = "green", size = 10)
  }
  return(clkpt)
}
