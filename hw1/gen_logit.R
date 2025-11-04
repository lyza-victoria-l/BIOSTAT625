gen_logit = function(b, x) {
  logits = b %*% x
  ref = 1 / (sum(exp(logits)) + 1)
  Y = c(ref, exp(logits)*ref)
  return(Y)
}




