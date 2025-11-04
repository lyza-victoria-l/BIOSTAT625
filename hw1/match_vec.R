match_vec = function(x, y) {
  k = as.numeric(length(x))
  n = as.numeric(length(y))
  if (k > n) {
    return(0)
  }
  m = which(x[1] == y)
  r = sapply(m, function(index)
    index:(index + k - 1))
  result = apply(
    r,
    MARGIN = 2,
    FUN = function(t)
      all(y[t] == x)
  )
  return(sum(result))
}


