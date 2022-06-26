#' @export
#' @keywords internal
mgrepl <- function(patterns, text, log.fun = all, na.replace = FALSE,
                   use.which = FALSE, cores = 1, ...) {
  if (!is.atomic(na.replace) || length(na.replace) != 1)
    stop("use a single value for na.replace")
  f <- match.fun(log.fun)
  ina <- which(is.na(text))
  patterns <- as.list(as.character(unlist(patterns)))
  i <- parallel::mclapply(patterns, function (y) grepl(y, text, ...), mc.cores = cores)
  i <- do.call(cbind, i)
  i <- apply(i, 1, f)
  if (use.which) {
    .Deprecated(msg = "use.which will be deprecated, use which(mgrepl(...)) instead")
    if (is.atomic(i)) return(which(i))
  }
  if (length(ina)) {
    if (is.matrix(i)) {
      i[,ina] <- na.replace
    } else {
      i[ina] <- na.replace
    }
  }
  return(i)

}


