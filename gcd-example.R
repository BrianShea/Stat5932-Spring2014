gcd <- function {
  # Domain conditions
  if(length(n) != 1 || length(m) != 1)
    stop("arguments must be length one")
  if(!is.numeric(c(n, m)))
    stop("arguments must be numeric")
  if(any(is.nan(c(n, m))))
    return(NaN)
  if(any(is.na(c(n, m))))
    return(NA)
  if(any(is.infinite(c(n, m))))
    return(Inf)
  if(!isTRUE(all.equal(round(c(n, m)), c(n, m))))
    stop("arguments must be integer-valued")
  else {
    n <- abs(round(n))
    m <- abs(round(m))
  }
  ## The algorithm
  if(m == 0)
    n
  else
    gcd(m, n %% m)
}
