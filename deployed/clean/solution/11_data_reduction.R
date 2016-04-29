library(data.table)

reduce.freqs = function(freqs) {
  ngram.n = ncol(freqs) - 1

  if (ngram.n <= 1) {
    stop("1-gram reduction is not implemented")
  }

  # this very simple reduction is expected to reduce stored data size
  # sufficiently
  freqs = freqs[freqs$freq > 2, ]

  return(freqs)
}

reduce.token.stats.files = function(token.stats.dir, max.ngram.n, dest.dir) {
  if (max.ngram.n < 2) {
    stop("cannot reduce shorter than 2-grams (not implemented)")
  }

  for(ngram.n in 2:max.ngram.n) {
    src.file = file.path(token.stats.dir, sprintf("%dgram_freq.csv", ngram.n))
    ngram.freqs = fread(src.file, header = TRUE)
    reduced = reduce.freqs(freqs = ngram.freqs)
    dest.file = file.path(dest.dir, sprintf("%dgram_freq_top_reduced.csv",
                                            ngram.n))

    write.csv(x = reduced, file = dest.file, row.names = FALSE)
  }

  rm(ngram.freqs) # reduce peak memory usage

  src.file = file.path(token.stats.dir, "1_1_assoc_freq.csv")
  assoc.freqs = fread(src.file)
  reduced = reduce.freqs(assoc.freqs)
  dest.file = file.path(dest.dir, sprintf("1_1_assoc_freq_reduced.csv"))
  write.csv(x = reduced, file = dest.file, row.names = FALSE)
}

run.ngram.stats.reduction = function() {
  dirs = matrix(ncol = 2, byrow = TRUE, data = c(
#    "data/dummy_texts/token_stats/", "data/dummy_texts/token_stats/",
    "data/verification/token_stats/", "data/verification/token_stats/",
    "data/cv/token_stats/", "data/cv/token_stats/"))

  cat("data reduction starts...\n")
  apply(X = dirs, MARGIN = 1,
        FUN = function(x)
          reduce.token.stats.files(x[1], 4, x[2]))
  cat("data reduction done.\n")
}

test = function() {
  freqs =
    matrix(ncol = 4, byrow = TRUE, data = c(
      1, 1, 1, 5,
      1, 1, 5, 2,
      2, 1, 3, 6,
      2, 1, 4, 3,
      2, 1, 1, 8,
      2, 1, 3, 3,
      2, 1, 5, 1,
      2, 1, 7, 2
    ))

  colnames(freqs) <- c("word_1", "word_2", "word_3", "freq")
  freqs = as.data.table(freqs)
  print(freqs)
  print(reduce.ngram.freqs(freqs, 3))
}

# TODO: test nicely
# test()

# run.ngram.stats.reduction()