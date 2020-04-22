# Down sample based on caret::downsample
# Reviewed 2020 - no change

down_sample <- function (x, y, list = FALSE, yname = "Class") {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = TRUE)
  }
  if (!is.factor(y)) {
    stop("Down-sampling requires a factor variable as the response")
  }
  minClass <- min(table(y))
  x$.outcome <- y
  x <- plyr::ddply(x, ~.outcome, function(dat, n) dat[sample(seq(along = dat$.outcome), 
                                                  n), , drop = FALSE], n = minClass)
  y <- x$.outcome
  x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
  if (list) {
    if (inherits(x, "matrix")) {
      x <- as.matrix(x)
    }
    out <- list(x = x, y = y)
  }
  else {
    out <- cbind(x, y)
    colnames(out)[ncol(out)] <- yname
  }
  out
}