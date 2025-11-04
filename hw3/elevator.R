# going up first btw
elevator = function(x) {
  floorTop = max(x[, 2:3])
  floor = 1
  time = 0
  direction = "up"
  x = matrix(x, ncol = 3)
  onboard = matrix(ncol = 3, nrow = 0)
  
  x = x[order(x[, 1]), , drop = FALSE]
  
  start = function(x, onboard, time) {
    if (nrow(x) > 0 && any(time >= x[,1])) {
      return(TRUE)
      }
    if (nrow(onboard) > 0) {
      return(TRUE)
      }
    {return(FALSE)}
  }
  
  drop = function(floor, time, onboard) {
    served = which(floor == onboard[, 3] & time >= onboard[, 1])
    if (length(served) > 0) {
      onboard = onboard[-served, , drop = FALSE]
      return(list(
        dropped = 1,
        onboard = onboard
      ))
    }
    else {
      return(list(
        dropped = 0, 
        onboard = onboard))
    }
  }
  
  pick = function(x, floor, time, onboard) {
    picked <- which(floor == x[, 2] & time >= x[, 1])
    if (length(picked) > 0) {
      onboard = rbind(onboard, x[picked, ])
      x = x[-picked, , drop = FALSE]
      return(list(
        picked = 1,
        onboard = onboard,
        x = x))
    } else {
      return(list(picked = 0,
                  onboard = onboard,
                  x = x))
    }
  }
  
  while (nrow(x) > 0 || nrow(onboard) > 0) {
  
  d = drop(floor, time, onboard)  
  onboard = d$onboard
    
  p = pick(x, floor, time, onboard)
  x = p$x
  onboard = p$onboard
  
  if (p$picked || d$dropped) {
    time = time + 1
    next
  }
  
  if (direction == "up" & floor == floorTop) {
    direction = "down"
  } else if (direction == "down" & floor == 1) {
    direction = "up"
  }
  
  if(nrow(x) == 0) x = matrix(ncol = 3, nrow = 0)
  if(nrow(onboard) == 0) onboard = matrix(ncol = 3, nrow = 0)

  if (start(x, onboard, time)) {
    if (direction == "up") {
      floor = floor + 1
    } else {
      floor = floor - 1
    }
    time = time + 1
  } else {
    time = time + 1 
  }
  
  }
  
  return(time)
}


