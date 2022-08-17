rm(list = ls())
require(rgl)
require(Rvcg)

spec.fl <- list.files("./test data/turts/input/", pattern = ".off")

i=1

head(tri)

spec$it[,1]
t(spec$vb)[,1]

dim(sp_list[[1]]$material$color)
dim(sp_list[[1]]$vb)
dim(t(sp_list[[1]]$vb)[sp_list[[1]]$it,1:3])

spec <- sp_list[[1]]

tri <- t(spec$vb)[spec$it,1:3]

specimen <- scale(as.matrix(t(spec$vb)[, -4]), scale = FALSE)
spec$vb <- rbind(t(specimen), 1)

mesh <- spec
if (is.null(mesh$material))
  mesh$material$color <- "gray"
if (is.null(mesh$material$color))
  mesh$material$color <- "gray"


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


open3d()
plot3d(specimen[, 1], specimen[, 2], specimen[, 3], size = 1,
                     aspect = FALSE, box = FALSE, axes = FALSE,
                     xlab = "",  ylab = "",  zlab = "")
shade3d(mesh, meshColor = "vertices", add = TRUE)

triangles3d(tri, col = "grey")

# clkTriangles <- function() # havent implimented as a function yet.
# Need to fix calling triangles and meshes from the env.
  {

  # set up triangle and triangle centers in window and scene coordinates
  ids = ids3d("shapes")
  scntri <- rgl.attrib(id = ids[which(ids[,2]=="triangles"),1], attrib = "vertices")
  trictrs <- rgl.attrib(id = ids[which(ids[,2]=="triangles"),1], attrib = "centers")


  # user input click
  rect <- rgl.select(button = "left", dev = cur3d(), subscene = currentSubscene3d())
  dev = cur3d()
  subscene = currentSubscene3d()

  tmp_par <- par3d(c("scale", "modelMatrix","projMatrix", "viewport", "userMatrix","userProjection","mouseMode","windowRect","activeSubscene"), subscene = subscene)

  tmp_par$modelMatrix
  tmp_par$userMatrix

  tmp_par$projMatrix
  tmp_par$userProjection

  clkprj <- rgl.projection(dev = dev,
                           subscene = subscene)

  clkprj_user <- clkprj
  clkprj_user$proj <- tmp_par$userProjection

  wintrictrs <- rgl.user2window(trictrs, projection = clkprj_user)

  # convert window click coords to scene coords
  winvect <- cbind(matrix(rect,ncol = 2, byrow = T), c(0,1))
  objvect <- t(apply(winvect, MARGIN= 1,
                     FUN = function(X,...){ rgl.window2user(x=X[1],
                                                            y=X[2],
                                                            z=X[3],
                                                            projection = clkprj_user)
                     })
              )

  objvect <- rgl.window2user(x=winvect, projection = clkprj_user)
  test_matrix <- par3d("userMatrix")
  test_view <- rgl.viewpoint(userMatrix = test_matrix)

  # find nearest triangle centers
  # (can be set to any number, still playing around.)
  # I thought I had solved the nearset surface problem, but
  # i think i will do that in later steps

  ntri <- 20
  a <- ptdist(X = winvect[1,], Y =  wintrictrs) # calculate window XY and Z distances from
                                                # user click to triangle centres

  aprime <- order(a[,4], decreasing = F)[1:ntri] # get closest centers sorted by window XY distance

  tri.aprime <- seq(1, nrow(tri),by = 3 )[aprime] # make a dummy triangle index
  verts <- as.vector(sapply(tri.aprime, FUN = function(X,Y) X + c(0:2))) # triangle vertex index

  win_pts <- trictrs[aprime,] # these are the scene coords of the nearest triangle centers
  plttri <- scntri[verts,] # these are the scene coords of the triangle vertices

  # a2 <- a[aprime,] # this is for later maybe



  # replot to mesh to visualise click vector, triangle centres and triangles.
  # This is still a mess in terms of enviroment objects. Want to find a way
  # to replot from just the scene pars
  clear3d()

  wire3d(mesh)
  lines3d(objvect, col = "red")
  points3d(trictrs[aprime,], col = "green", size = 5)
  triangles3d(plttri, col = "blue")

  }

win_pts
objvect / 5

tmp_par



test_verts <- matrix(c(-3.897204,-1.878483,2.990001,
                       -3.651888,-2.001883,3.074201,
                       -3.528921,-1.881816,3.169435,
                       -3.651395,-1.637750,3.150368,
                       -4.020219,-1.999183,2.916635,
                       -4.019273,-1.635216,3.012535,
                       -3.896431,-1.514550,3.077668,
                       -3.898038,-2.242716,2.896168,
                       -3.528629,-1.517583,3.231435,
                       -3.529435,-2.245883,3.084468,
                       -4.264594,-1.877816,2.920468,
                       -3.652570,-2.366016,2.979801,
                       -3.283831,-2.005450,3.267801,
                       -4.386839,-2.000050,2.910901,
                       -3.283604,-1.641516,3.337735,
                       -4.021184,-2.363550,2.818335,
                       -4.265610,-2.242249,2.829235,
                       -4.263305,-1.514283,3.028968,
                       -4.385611,-1.636183,3.010268,
                       -4.749282,-2.006416,3.143435),
                     ncol=3, byrow = T)

test_verts[,1] <- test_verts[,1] - 10

