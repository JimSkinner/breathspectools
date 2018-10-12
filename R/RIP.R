RIP <- function(bsmat) {
  #as.numeric(names(apply(bsmat, 1, function(row) {
  #  max(row)
  #})))
  rip <- vapply(1:nrow(bsmat), function(i) {
    as.numeric(colnames(bsmat)[which.max(bsmat[i,])])
  }, numeric(1))
  names(rip) <- rownames(bsmat)
  return(rip)
}
