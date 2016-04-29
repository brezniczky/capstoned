source("clean/solution/14_term_filtering.R")
library(Rcpp)
sourceCpp("clean/solution/10_ngram_cpp.cpp")

# TODO: move this into a common utility function
log.print = function(msg) {
  cat(sprintf("%s: %s\n", as.character(date()), msg))
}

create.ngram.list = function(source.file, target.freq.file,
                             target.dict.file = "", ngram.n,
                             enabled.words) {

  source.text = readLines(source.file)
  source.text = paste0(unlist(source.text), collapse = "\n")

  get_ngrams_fast(text = source.text, excluded_words = "",
                  ngram_n = ngram.n, enabled_words = enabled.words,
                  csv_filenames = c(target.dict.file, target.freq.file))
}

create.ngram.lists = function(text.filename, target.dir,
                              word.coverage = c(NA, 1, 1, 1),
                              max.ngram.n = 4) {

  # Remarks:
  # word.coverage is recycled, but the first parameter is ignored (1-grams are
  # always fully covered, forming the basis of coverage filtering).

  recycled.get = function(vec, index) {
    # just for fun experiment
    return(vec[((index - 1) %% length(vec)) + 1])
  }

  fp = function(partial.path) {
    return(file.path(target.dir, partial.path))
  }

  target.dict.filename = fp("ngram_dict.csv")
  enabled.words = NULL

  for(ngram.n in 1:max.ngram.n) {
    freq.filename = fp(sprintf("/%dgram_freq.csv", ngram.n))

    if (!file.exists(freq.filename)) {
      act.word.coverage = recycled.get(word.coverage, ngram.n)
      if ((ngram.n > 1) && (act.word.coverage != 1)) {
        log.print(paste0("generating word list with coverage ",
                         act.word.coverage));
        enabled.words =
          get.words.with.coverage(source.dict.filename, ngram.1.freq.filename,
                                  act.word.coverage)
        log.print("removing bad words")
        enabled.words = remove.bad.words(enabled.words)
        log.print("done generating the word list")
      }

      log.print(sprintf("generating %d-grams\n", ngram.n))
      create.ngram.list(source.file = text.filename,
                        target.freq.file = freq.filename,
                        target.dict.file = target.dict.filename,
                        ngram.n = ngram.n,
                        enabled.words = enabled.words)
    }
    else {
      log.print(sprintf("%d-gram files exist, leaving them intact\n", ngram.n))
    }

    if (ngram.n == 1) {
      ngram.1.freq.filename = freq.filename
      source.dict.filename = target.dict.filename
      target.dict.filename = "" # no more dictionaries, they are identical
      # alternatively, for testing only:
      # target.dict.filename = fp(sprintf("ngram_dict_%d.csv", ngram.n + 1))
    }
  }
}

run.ngram.stats.generation = function() {
  log.print("run.ngram.stats.generation() / verification data")
  create.ngram.lists(text.filename = "data/verification/subsampled.txt",
                     target.dir = "data/verification/token_stats/",
                     word.coverage = c(1, 0.95, 0.95, 0.2),
                     max.ngram.n = 4)

  log.print("run.ngram.stats.generation() / CV data")

#   create.ngram.lists(text.filename = "data/cv/training.txt",
#                      target.dir = "data/cv/token_stats",
#                      word.coverage = 0.9)
  create.ngram.lists(text.filename = "data/cv/training.txt",
                     target.dir = "data/cv/token_stats",
                     word.coverage = c(1, 0.95, 0.95, 0.9),
                     max.ngram.n = 4)
  log.print("run.ngram.stats.generation() finished.")
}

test.ngram.stats.generation = function() {

  folder = "data/dummy_texts/"

  fp = function(partial.path) return(file.path(folder, partial.path))

  dict.1.filename = fp("token_stats/ngram_dict_a.csv")
  dict.2.filename = fp("token_stats/ngram_dict.csv")

  create.ngram.list(source.file = fp("verification/subsampled.txt"),
                    target.freq.file = fp("/token_stats/1gram_freq.csv"),
                    target.dict.file = dict.1.filename,
                    ngram.n = 1, enabled.words = NULL)

  create.ngram.list(source.file = fp("/verification/subsampled.txt"),
                    target.freq.file = fp("/token_stats/2gram_freq.csv"),
                    target.dict.file = dict.2.filename,
                    ngram.n = 2, enabled.words = NULL)

  f1 = readBin(dict.1.filename, "raw")
  f2 = readBin(dict.2.filename, "raw")

  if (!identical(f1, f2))
    stop("dictionary output differs in test.ngram.stats.generation()")
}

# test.ngram.stats.generation()

# run.ngram.stats.generation()
# get.words.with.coverage("data/dummy_texts/token_stats/ngram_dict_a.csv",
#                         "data/dummy_texts/token_stats/1gram_freq.csv",
#                         0.5)
