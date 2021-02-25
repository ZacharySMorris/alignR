#### Breaking down the selectpoints3d function to understand how to implement it in shiny rglWidgets ####

function (objects = ids3d()$id, value = TRUE, closest = TRUE,
          multiple = FALSE, ...)
  #objects is a vector of object id values to use for the search

{
  if (value)
    result <- cbind(x = numeric(0), y = numeric(0), z = numeric(0))
  else result <- cbind(id = integer(0), index = integer(0))
  rdist <- I
  first <- TRUE
  while (first || is.function(multiple) || multiple) {
    f <- select3d(...) #name a function f that does select3d
    if (is.null(f))
      break
    e <- environment(f) #save the environment of selection, used to save the window projection and window llx,lly,urx,ury values
    dist <- Inf #set distribution to infinity...why?
    prev <- nrow(result) #I think this is only used when selecting multiple points, which we won't use
    for (id in objects) { #runs a loop for each shape in the rgl object
      verts <- rgl.attrib(id, "vertices") #extracts the vertices of the specific shape, could also grab the normals as well!
      hits <- f(verts) #runs select3d function to see if the vertices of the selected object are within the user selected region...I think...
      if (any(hits))
        dist <- 0
      else if (closest && dist > 0 && nrow(verts)) { #a check that I don't fully understand, but seems to check whether closest is true and the dist matrix is big enough for more data?
        wincoords <- rgl.user2window(verts, projection = e$proj) #extracts the window? coordinates that correspond to the vertices of the shape given the projection
        wz <- wincoords[, 3] #grab the z values of the window? coordinates
        keep <- (0 <= wz) && (wz <= 1) #keep only those z coords b/w zero and one
        wincoords <- wincoords[keep, , drop = FALSE] #keep the rows with acceptable z coords while maintaining it as a data.frame (drop = FALSE)
        if (!nrow(wincoords)) #check whether there are any rows left in wincoords, if not then skips to next shape
          next
        wx <- wincoords[, 1] #grab the x values of the window? coordinates
        xdist <- ifelse(wx < e$llx, #text whether x coords are less than the lower left x coordinate of the selected region in the environment
                        (wx - e$llx)^2, #if yes, then take the squared difference
                        ifelse(wx < e$urx, #if no, then test whether the x coords are less than the upper right x coordinate
                               0, #if yes, return zero
                               (wx - e$urx)^2) #if no, return the squared difference
                        )

        wy <- wincoords[, 2] #grab the y values of the window? coordinates
        #perform an ifelse check for whether the y coords are less than the lower and upper y coordinates of the selected region in the environment
        #if outside the lower and upper bounds, return the squared difference in values
        #if above the lower, but below the upper, then return zero
        #returns a vector of the length of y coordinates with these values
        ydist <- ifelse(wy < e$lly, (wy - e$lly)^2, ifelse(wy < e$ury, 0, (wy - e$ury)^2))

        dists <- xdist + ydist #adds the x and y distributions together, so only vertices with both x & y within the region will remain zero
        hits <- (dists < dist) & (dists == min(dists)) #checks which rows are less than infinity?? and also equal to the min value (which should be zero unless no vertex falls in selected area)
        dist <- min(c(dist, dists)) #if a vetex falls in selected region will be zero, if not then the distance to the nearest vertex
      }
      if (!any(hits)) #if there are no hits, skip to next shape
        next
      if (prev && nrow(result) > prev && rdist > dist) #check of some kind...presumably if it finds that there are more previously saved results than in the current run of the loop??
        result <- result[seq_len(prev), , drop = FALSE] #finds the number of previously filled rows of the results table and saves those as results
      if (value) #if saving the values instead of indices
        result <- rbind(result, verts[hits, ]) #then append the coordinates of verticies that were hits to existing results
      else result <- rbind(result, cbind(id, which(hits))) #if saving indices, then append the shape ID ("id") and vertex indices that were hits to existing results
      if (is.function(multiple) && nrow(result) > prev && #check for if doing multiple points and breaks when end is reached
          !multiple(result[(prev + 1):nrow(result), , drop = FALSE]))
        break
      rdist <- dist #save current loop dist and rdist for next loop
      first <- FALSE #change so function knows this is no longer first loop
    }
    if (value) #if returning coordinate values instead of indices
      result <- unique(result) #then ensure duplicates are dropped before output
  }
  result #output results
}

