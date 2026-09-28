# A synthetic "full" dataset for exercising post-reveal code before the tag: the lower
# triangle is the deterministic chain ladder completion (all-year factors, no tail) of the
# upper triangle. It contains no real 2008-2016 values.
synthetic_full <- function(codes) {
  up <- load_upper()
  up <- up[up$grcode %in% codes, ]
  rows <- list(up)
  x <- as_at(up, 2007)
  for (code in codes) {
    tr <- insurer_triangles(x, code)
    comp <- function(m) {
      f <- ldf_vector(m, "all")
      for (j in 2:10) m[is.na(m[, j]), j] <- m[is.na(m[, j]), j - 1] * f[j - 1]
      m
    }
    P <- comp(tr$paid); I <- comp(tr$inc_booked); CI <- comp(tr$case_inc)
    base <- up[up$grcode == code & up$lag == 1, ]
    for (i in seq_len(nrow(P))) for (j in 1:10) {
      ay <- as.integer(rownames(P)[i])
      if (ay + j - 1 <= 2007) next
      r <- base[base$ay == ay, ]
      r$lag <- j; r$cy <- ay + j - 1
      r$paid <- P[i, j]; r$inc_booked <- I[i, j]; r$case_inc <- CI[i, j]; r$bulk <- r$inc_booked - r$case_inc
      rows[[length(rows) + 1]] <- r
    }
  }
  d <- do.call(rbind, rows)
  as_at(d, max(d$cy))
}
