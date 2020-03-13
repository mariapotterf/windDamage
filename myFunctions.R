

# My functions:

# LIst my functions:
findOpenEdge_sf <- function(sf, treeHeight, distance = 40, pixel.width = 16, ...) {
  
  # loop through the dataframe
  sf$open_edge <- FALSE
  
  for (i in 1:nrow(sf)) {
    
    # define stands and leftover forest
    one  = sf[i, ]
    left = sf[-i,]
    
    # Create buffer and intersectb buffer with neighbors: evalues if any are left?
    buff = st_buffer(one, distance) # distance
    
    # Subset the polygons that overlaps with the buffer
    nbrs.buff <- left[st_intersects(buff, left, sparse =  FALSE),]
    
    # If conditions to determine if the forest has open edge or not
    if (nrow(nbrs.buff) == 0) {
      sf[i,]$open_edge <- TRUE  
      
    } else {  # neighbors are smaller than the stands
      
      # Compare the height of the stands: 
      height.one  = rep(one$treeHeight, nrow(nbrs.buff))
      height.nbrs = nbrs.buff$treeHeight
      
      # Get the differences between the neighbouring stands
      difference = height.one - height.nbrs
      
      # compare here the tree heights of stands
      if(any(difference > 5)) {
        sf[i,]$open_edge <- TRUE
        
        # Check if there is a big gap in neighborhood    
      } else {                     
        
        # Get the difference between two shapefiles???
        # Add `one` to `neighbors` and dissolve (union) inner boundaries  
        u <- st_union(rbind(nbrs.buff, one))
        
        # Erase existing stands from the buffer
        int.buff.one = st_difference(st_geometry(buff), st_geometry(u)) 
        
        # check if gaps exists 
        if (length(int.buff.one) > 0 ) {
          
          # Calculate area of intersected data
          int.buff.one.area = st_area(int.buff.one)
          
          if (as.numeric(int.buff.one.area) > pixel.width^2)  {
            sf[i,]$open_edge <- TRUE
          }
        }
      }
    }
  }
  return(sf) 
} 
