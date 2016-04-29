# The aim is simple for the first version (1.0 - MVP):
# - get hits from the 2gram predictor (which in the end respects linguistic
#   rules) and rank them by their associative relevance

# Further goals:
# - combine the 2gram and associative scores with some simplistic algorithm to
#   get a relevance
#
#   then watch out: if the world is already present, exclude it!

# Combining the scores could actually require some consideration and perhaps
# is doable properly (!(!))
#

# Adding logarithms can be a good ranking estimate anyway

# Actually it seems I can jump development steps and head for the ngram
# predictor + associative predictor combo

# I will possibly run into deployment issues, too.
# However, I don't think I need the 4-grams.

# I've got:
# 2gram_freq 70.8 MB
# maybe
# 3gram_freq 295.5 MB
# and 1_1_assoc_freq 421.7 MB
#
# fits the 1GB thing quite well I'd say
#
# then I could just zip it up
# or try the compressed sqlite3 database
#
# but I believe from the above that the prediction is of far more relevance

# All I want is to put this together, come on ...

source("clean/solution/12_predict_ngram.R")
source("clean/solution/15_predict_associative.R")

combined.predict = function(word_ids, pred.data, prefer.long.words = FALSE) {

  ngram.preds = predict.ngram(word_ids,
                              # TODO: group these into a named list
                              pred.data$top.4.word_ids,
                              pred.data$ngram.tables$ngrams_1,
                              pred.data$ngram.tables$ngrams_2,
                              pred.data$ngram.tables$ngrams_3,
                              pred.data$ngram.tables$ngrams_4,
                              pred.data$total.term.count,
                              pred.data$get.ngram.count.fun)

  assoc.preds = predict.associative(word_ids = word_ids,
                                    pred.data = pred.data$assoc.data)

  # combine the two prediction types: keep word_id, relevance pairs and
  #                                   aggregate relevances together by summation

  # merge, pad with zeroes (outer join)
#   cat("assoc range:", range(assoc.preds$relevance), "\n")
#   cat("ngram range:", range(ngram.preds$relevance), "\n")
#   preds = merge(assoc.preds, ngram.preds, by = "word_id", all.y = TRUE)
#   preds[is.na(preds)] <- 0
# #  preds = preds[, .(relevance = relevance.x / 100000 + relevance.y), word_id]
#   preds = preds[, .(relevance = relevance.x), word_id]
# #  preds = ngram.preds
#   setorder(preds, -relevance)

  # or for now: top 4 ngram, top 8 associative
  setorder(ngram.preds, -relevance)
  setorder(assoc.preds, -relevance)

  preds =
    rbind(
      head(ngram.preds, 4),
      head(assoc.preds, 8))

  return(preds)
}

intialize.combined.pred.data = function() {
  if (exists("cached.combined.pred.data")) {
    return(cached.combined.pred.data)
  }

  # for ease of deployment, an intermediate storage
  cache.filename = "intialize.combined.pred.data.cache.Rdata"

  if (file.exists(cache.filename)) {
    print("loading...")
    load(file = cache.filename)
    print("loaded")
  } else {
    dict = fread("data/cv/token_stats/ngram_dict.csv")
    ngram.tables = load.ngrams()
    assoc_1_1.freq = fread("data/cv/token_stats/1_1_assoc_freq_reduced.csv")
    save(dict, ngram.tables, assoc_1_1.freq, file = cache.filename)
  }

  dict.by.word = cbind(dict, id = 0:(nrow(dict) - 1))
  setkey(dict.by.word, word)
  dict.by.id = copy(dict.by.word)
  setkey(dict.by.id, id)

  top.4.word_ids = get.top.4.word_ids(ngram.tables$ngrams_1)
  total.term.count = sum(ngram.tables$ngrams_1$freq)
  get.ngram.count.fun = create.get.ngram.count.fun(ngram.tables = ngram.tables)

  # the second initialization depends on the first

  get.term.freq = function(word_id) {
    return(ngram.tables$ngrams_1[.(word_id), .(freq = freq)]$freq)
  }
  assoc.pred.data = init.associative.pred.data(term.freq.fun = get.term.freq,
                                               total.term.n =
                                                 sum(ngram.tables$ngrams_1$freq),
                                               dict.by.word = dict.by.word,
                                               dict.by.id = dict.by.id,
                                               ngrams_2 = ngram.tables$ngrams_2,
                                               assoc_1_1.freq = assoc_1_1.freq)

  combined.pred.data =
    list(ngram.tables = ngram.tables,
         top.4.word_ids = top.4.word_ids,
         total.term.count = total.term.count,
         get.ngram.count.fun = get.ngram.count.fun,
         assoc.data = assoc.pred.data,
         dict.by.word = dict.by.word,
         dict.by.id = dict.by.id
    )

  cached.combined.pred.data <<- combined.pred.data

  return(combined.pred.data)
}

test = function() {
  combined.pred.data = intialize.combined.pred.data()

  text =
    "hello"

  words = get_words(text = text, standardize = TRUE)
  ids = words.to.ids(words, combined.pred.data$dict.by.word)

  print(
    system.time({
      preds = combined.predict(word_ids = ids, pred.data = combined.pred.data)
    })
  )


  text =
    "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

  words = get_words(text = text, standardize = TRUE)
  ids = words.to.ids(words, combined.pred.data$dict.by.word)

  print(
    system.time({
      preds = combined.predict(word_ids = ids, pred.data = combined.pred.data)
    })
  )

  print(head(combined.pred.data$dict$word[preds$word_id + 1], 8))

  print(
    system.time({
      preds = combined.predict(word_ids = tail(ids, 4),
                               pred.data = combined.pred.data)
    })
  )

  print(head(combined.pred.data$dict$word[preds$word_id + 1], 8))
}

# test()
