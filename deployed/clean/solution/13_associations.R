library(Rcpp)
sourceCpp("clean/solution/10_ngram_cpp.cpp")
source("clean/solution/14_term_filtering.R")
library(tm) # for the stopwords # TODO: review stopwords

# TODO: move this into a common utility function
# TODO: also: this is a duplicate of stuff in 09_ngrams.R
log.print = function(msg) {
  cat(sprintf("%s: %s\n", as.character(date()), msg))
}

create.associations = function(text.filename, directory,
                               word.coverage) {

  fp = function(partial.path) {
    return(file.path(directory, partial.path))
  }

  excluded.words = stopwords()

  source.dict.filename = fp("token_stats/ngram_dict.csv")
  ngram.1.freq.filename = fp("token_stats/1gram_freq.csv")
  target.dict.file = fp("token_stats/assoc_dict.csv")
  target.freq.file = fp("token_stats/1_1_assoc_freq.csv")

  log.print(paste0("generating word list with coverage ", word.coverage, "\n"))
  enabled.words =
    get.words.with.coverage(dict.filename = source.dict.filename,
                            freq.filename = ngram.1.freq.filename,
                            word.coverage = word.coverage)
  log.print("removing bad words")
  enabled.words = remove.bad.words(enabled.words)
  log.print("done generating the word list")

  log.print("generating 1-1 associations")

  # allow multiple files specified (relevant for at least the "dummy_texts" case
  source.text = unlist(sapply(fp(text.filename), FUN = readLines))
  source.text = paste0(unlist(source.text), collapse = "\n")

  get_associations_cpp(text = source.text,
                       excluded_words = excluded.words,
                       enabled_words = enabled.words,
                       use_enabled_words = TRUE,
                       csv_filenames = c(target.dict.file, target.freq.file));

  log.print("done generating 1-1 associations")
}

run.associations.generation = function() {
  log.print("run.associations.generation() / verification data")
  create.associations(text.filename = "subsampled.txt",
                      directory = "data/verification",
                      word.coverage = c(0.95))

#  stop("bad day good!")
  log.print("run.associations.generation() / CV data")

  create.associations(text.filename = "training.txt",
                      directory = "data/cv",
                      word.coverage = c(0.95))
  log.print("run.associations.generation() finished.")
}

test.associations.generation = function() {
  folder = "data/dummy_texts/"

  fp = function(partial.path) return(file.path(folder, partial.path))

  ref.dict.filename = fp("token_stats/ngram_dict.csv")
  new.dict.filename = fp("token_stats/assoc_dict.csv")

  create.associations(text.filename = "verification/subsampled.txt", #c("text0.txt", "text1.txt"),
                      directory = "data/dummy_texts",
                      word.coverage = 0.9)

  f1 = readBin(ref.dict.filename, "raw")
  f2 = readBin(new.dict.filename, "raw")

  if (!identical(f1, f2))
    stop("dictionary output differs in test.associations.generation()")
}

# test.associations.generation()
