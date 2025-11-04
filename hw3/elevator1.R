# going up first btw
elevator = function(x) {
  floorTop = max(x[, 2:3])
  floor = 1
  time = 0
  direction = "up"
  x = matrix(x, ncol = 3)
  onboard = matrix(ncol = 3, nrow = 0)
  queue = onboard
  
  x = x[order(x[, 1]), , drop = FALSE]
  
  start = function(x, onboard, queue, time) {
    if (nrow(x) > 0 && any(time >= x[, 1])) {
      queued = which(x[, 1] <= time)
      queue = rbind(queue, x[queued, , drop = FALSE])
      x = x[-queued, , drop = FALSE]  # remove queued passengers from x
    }
    return(list(
      x = x,
      queue = queue,
      started = (nrow(onboard) > 0 | nrow(queue) > 0)
    ))
  }
  
  
  drop = function(floor, time, onboard) {
    served = which(floor == onboard[, 3] & time >= onboard[, 1])
    if (length(served) > 0) {
      onboard = onboard[-served, , drop = FALSE]
      return(list(dropped = 1, onboard = onboard))
    }
    else {
      return(list(dropped = 0, onboard = onboard))
    }
  }
  
  pick = function(queue, floor, time, onboard) {
    picked <- which(floor == queue[, 2])
    if (length(picked) > 0) {
      onboard = rbind(onboard, queue[picked, ])
      queue = queue[-picked, , drop = FALSE]
      return(list(
        picked = 1,
        onboard = onboard,
        queue = queue
      ))
    } else {
      return(list(
        picked = 0,
        onboard = onboard,
        queue = queue
      ))
    }
  }
  
  while (nrow(x) > 0 || nrow(onboard) > 0 || nrow(queue) > 0) {
    s = start(x, onboard, queue, time)
    x = s$x
    queue = s$queue
    if (!s$started){
      time = time + 1
      next
      }
    
    
    d = drop(floor, time, onboard)
    onboard = d$onboard
    
    p = pick(queue, floor, time, onboard)
    queue = p$queue
    onboard = p$onboard
    
    if (p$picked || d$dropped) {
      time = time + 1
      next
    }
    
    if (nrow(x) == 0)
      x = matrix(ncol = 3, nrow = 0)
    if (nrow(queue) == 0)
      queue = matrix(ncol = 3, nrow = 0)
    if (nrow(onboard) == 0)
      onboard = matrix(ncol = 3, nrow = 0)
    
    if (direction == "up" & floor == floorTop) {
      direction = "down"
    } else if (direction == "down" & floor == 1) {
      direction = "up"
    }
    
    if (direction == "up") {
      floor = floor + 1
    } else {
      floor = floor - 1
    }
    
    time = time + 1
  
  
  }
  return(time)
}

