rm(list = ls())
require(rgl)
require(Rvcg)

spec.fl <- list.files("./alignR/data/turts/input/", pattern = ".off")

i=1


mesh <- vcgImport(file.path("./alignR/data/turts/input/",spec.fl[i]))

tri <- t(mesh$vb)[mesh$it,1:3]


# select point ------------------------------------------------------------
# 


ptdist <- function(X,Y){
  if(is.vector(Y)){
    Y = t(as.matrix(Y))
  }
  
  XY = as.matrix(Y - X)
  colnames(XY) <- c("x", "y", "z")
  XYdist = apply(Y[,1:2], 1, function(x,y){dist(rbind(x,y))}, y= X[1:2])
  
  return(cbind(XY,XYdist))
}


clear3d()
triangles3d(tri, col = "grey")

# clkTriangles <- function() # havent implimented as a function yet. 
# Need to fix calling triangles and meshes from the env. 
  # {
  
  # set up triangle and triangle centers in window and scene coordinates

  # user input click
  rect <- rgl.select(button = "left", dev = cur3d(), subscene = currentSubscene3d())
  dev = cur3d()
  subscene = currentSubscene3d()
  
  clkprj <- rgl.projection(dev = dev,
                           subscene = subscene)
  
  ids = ids3d("shapes")
  scntri <- rgl.attrib(id = ids[which(ids[,2]=="triangles"),1], attrib = "vertices")
  trictrs <- rgl.attrib(id = ids[which(ids[,2]=="triangles"),1], attrib = "centers")
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
  
  

# find nearest triangles in window coords ---------------------------------

  # find nearest triangle centers
  # (can be set to any number, still playing around.)
  # I thought I had solved the nearset surface problem, but 
  # i think i will do that in later steps
  
  ntri <- 20
  a <- ptdist(X = winvect[1,], Y =  wintrictrs) # calculate window XY and Z distances from 
  aprime <- order(a[,4], decreasing = F)[1:ntri] # get closest centers sorted by window XY distance

  
  tri.i <- seq(1, nrow(tri),by = 3 )[aprime] # make a dummy triangle index
  verts <- as.vector(sapply(tri.i, FUN = function(X,Y) X + c(0:2))) # triangle vertex index
  
  wintriA <- wintri[verts,]
  
  # test plot
  # clear3d()
  # triangles3d(wintriA)
  # lines3d(winvect)
  # # axes3d()
  # 

# is vector within triangle? ----------------------------------------------

  # i=1
  require(sp)

  tri.in <- sapply(X = tri.i, FUN = function(X,pt,tri){
    point.in.polygon(pt[1,1],pt[1,2], tri[X++c(0:2),1],tri[X++c(0:2),2])
  }, pt = winvect, tri = wintri)
  
  tri.i[tri.in]
  
  front.tri.ind <- tri.i[order(a[aprime[tri.in!=0],3])[1]]+c(0:2)

  ####
  clktri <- tri[front.tri.ind,]
  ####
  
  # plot to confirm triangle ID
  
  clear3d()
  wire3d(mesh)
  lines3d(objvect, col = "red")
  points3d(trictrs[aprime,], col = "green", size = 5)
  triangles3d(clktri, col = "blue")
  
  

# calc coordinate ---------------------------------------------------------

  
  
  decomptri<- prcomp(clktri)
  pcvect <- as.data.frame(predict(decomptri,winvect))
  
  pctri <- predict(decomptri,wintri[-(i+c(0:2)),])
  
  
# pcvectdir<- pcvect[1,]-pcvect[2,]
  
  a = pcvect[1,]
  b = pcvect[2,]
  
  t = (0 - a[3])/ (b[3] - a[3]) 
  
  ptcoords <- a+unlist(lapply((b-a), FUN = function(X,Y) Y*X, Y=t))
  
  clkpt <- ( as.matrix(ptcoords)-decomptri$center ) %*% decomptri$rotation
  
  tri
  
  points3d(clkpt, col = "green", size = 5)
  

# next --------------------------------------------------------------------

  
  
  # convert window click coords to scene coords
  winvect <- cbind(matrix(rect,ncol = 2, byrow = T), c(0,1))
  objvect <- t(apply(winvect, MARGIN= 1,
                     FUN = function(X,...){ rgl.window2user(x=X[1],
                                                            y=X[2],
                                                            z=X[3],
                                                            projection = clkprj)
                     },
                     dev, subscene)
              )
  
                                    # user click to triangle centres
  
  aprime <- order(a[,4], decreasing = F)[1:ntri] # get closest centers sorted by window XY distance
  
  tri.i <- seq(1, nrow(tri),by = 3 )[aprime] # make a dummy triangle index
  verts <- as.vector(sapply(tri.i, FUN = function(X,Y) X + c(0:2))) # triangle vertex index
  
  trictrs[aprime,] # these are the scene coords of the nearest triangle centers
  plttri <- scntri[verts,] # these are the scene coords of the triangle vertices
  
  # a2 <- a[aprime,] # this is for later maybe
  
  
  
  # replot to mesh to visualise click vector, triangle centres and triangles. 
  # This is still a mess in terms of enviroment objects. Want to find a way 
  # to replot from just the scene pars
  clear3d()
  
  wire3d(mesh)
  lines3d(objvect, col = "red")
  points3d( trictrs[aprime,], col = "green", size = 5)
  triangles3d(plttri, col = "blue")
  
  # }
  

# do triangles subtend line? ----------------------------------------------

  objvect
  plttri
  
  
  winvect[1,]
  
  wintri <- rgl.user2window(plttri, projection = clkprj)
  
  triangles3d(wintri, col = "blue")
  lines3d(winvect)
  
  triseq <- seq(1, nrow(wintri), by = 3)
  
  i <- triseq[1]
  
  for(i in triseq){
    
    tsttri <- wintri[i+c(0:2),]
    
    decomptri<- prcomp(tsttri)
    pcvect <- as.data.frame(predict(decomptri,winvect))
    
    pctri <- predict(decomptri,wintri[-(i+c(0:2)),])
    clear3d()
    
    # pcvectdir<- pcvect[1,]-pcvect[2,]
  
    a = pcvect[1,]
    b = pcvect[2,]
    
    t = (0 - a[3])/ (b[3] - a[3]) 
    
    ptcoords <- a+unlist(lapply((b-a), FUN = function(X,Y) Y*X, Y=t))
    
    
    # ln3d <- lines3d(pcvect)
    # predict(line(pcvect))
    clear3d()
    triangles3d(decomptri$x, col = "red")
    triangles3d(pctri, col = "blue")
    lines3d(pcvect, col= "red")
    axes3d()
    points3d(ptcoords, col = "black", size= 10)
  }
  
  
  