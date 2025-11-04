quadratic = function(a, b, c){
  d = b^2 - 4 * a * c
  if(d < 0){
    return(NA)
    }
  return((-b - sqrt(d))/(2*a))
}
