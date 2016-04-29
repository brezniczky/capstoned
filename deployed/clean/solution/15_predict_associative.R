# association based prediction
# the term cooccurrence count matrix forms the basis of the predictive logic
# here

# some addition about the concept (particularly significance verification)
# is documented in clean/exploratory/019...

library(data.table)

get.associative.expected.count = function(n1, n1and2) {
  # TODO: think it over what happens if there were 2 instances of each word -
  #       doesn't it get severely distorted? yes it does
  #
  #       that needs to be fixed.

  return(n1and2 / n1)
}

get.relevance = function(n1, n2, n1and2, n.terms.total) {
  lambda_n2 = as.double(n1) * n2 / n.terms.total
  # use logging as arithmetic problems occur otherwise
  # (many hits get a relevance of 1)
  p_d = ppois(q = n1and2, lambda = lambda_n2, log.p = TRUE)
  return(p_d)
}

is.relevant = function(n1, n2, n1and2, n.terms.total, alpha = 0.05) {
  # experienced number of occurrances of n2
  # versus expected number of occurrances of term 2 over the n1 occurrence of
  # term 1 in case of independence

  p_d = get.relevance(n1, n2, n1andn2, n.terms.total)
  return(p_d > log(1 - alpha))
}

# print(is_relevant(100, 10, 2, 1000))

init.associative.pred.data = function(term.freq.fun, total.term.n,
                                      dict.by.word, dict.by.id, ngrams_2,
                                      assoc_1_1.freq) {
  dt_freq_w1 = setkey(assoc_1_1.freq, word_1)
  dt_freq_w2 = copy(assoc_1_1.freq)
  dt_freq_w2 = setkey(assoc_1_1.freq, word_2)

  dt_ngram_2_by_ids = ngrams_2

  return(list(freq_by_w1 = dt_freq_w1, freq_by_w2 = dt_freq_w2,
              get_term_freq = term.freq.fun,
              n_terms_total  = total.term.n,
              dict_by_id = dict.by.id,
              dict_by_word = dict.by.word,
              ngrams_2 = ngrams_2))
}

# TODO: remove alpha (unless interpretable)
get.associated.word_ids = function(word_id, pred.data, alpha = 0.05,
                                   sentence.word_ids = c()) {

  # The returned data.table
  #
  # * has word_id, relevance, freq columns, and is
  #
  # * ordered by relevance, then frequency (hits are listed from best to last,
  #   most frequently co-occurring with the term given to the least frequent).

  hits1 = pred.data$freq_by_w1[.(word_id), .(word_id = word_2, freq = freq)]
  hits2 = pred.data$freq_by_w2[.(word_id), .(word_id = word_1, freq = freq)]

  hits = rbind(hits1, hits2)

  n1 = pred.data$get_term_freq(word_id)
  n2 = pred.data$get_term_freq(hits$word_id)

  hits$relevance = get.associative.expected.count(n1 = n1, n1and2 = hits$freq)
  setkey(hits, word_id)

  # now filter by the "language system" or by at least as much as we know about
  # it, i.e. allowed bigrams are the limit
  #
  # first, a good sample of the 'linguistically' (typically) allowed
  # continuations

  allowed_ids = pred.data$ngrams_2[.(word_id), ]$word_2
  helper = data.table(word_id = allowed_ids, is_allowed = 1)
  # then match the id's against those with a quick merge
  hits = merge(hits, helper,
               by = "word_id", all.x = TRUE)

  # filter and only keep relevant columns
  hits = hits[is_allowed == 1, ][, c("word_id", "relevance", "freq"),
                                 with = FALSE]

  # then remove everything (with a little complicated reasoning) that has been
  # already used
  if (length(sentence.word_ids) > 0) {
    hits = merge(hits,
                 data.table(word_id = unique(sentence.word_ids),
                            is_redundant = 1),
                 by = "word_id", all.x = TRUE)

    hits[!is.na(hits$is_redundant), ]$relevance =
      hits[!is.na(hits$is_redundant), ]$relevance - 1 # at least one occurrence was there

    # remove the temporary column
    hits = hits[, -c("is_redundant"), with = FALSE]

    hits = hits[hits$relevance > 0, ]
  }

  hits$relevance = log(hits$relevance)
  setorder(hits, -relevance, -freq)

  return(hits)
}

get.associated.words = function(word, pred.data, alpha = 0.05) {
  # Routine for ease of verification mainly, predict.associative() is in the
  # plans as the main associative thing instead.
  #
  # Returned columns:
  # word_id, freq, relevance, word

  dict_by_id = pred.data$dict_by_id
  dict_by_word = pred.data$dict_by_word

  not.the.column.name.word = word # resolve naming conflict :)

  word_id = dict_by_word[.(not.the.column.name.word), ]$id
  ans = get.associated.word_ids(word_id = word_id, pred.data = pred.data,
                                alpha = alpha)

  ans$word = dict_by_id$word[ans$word_id + 1]

  return(ans)
}

predict.associative = function(word_ids, pred.data, alpha = 0.05) {
  # Predicts word_ids coupled with some relevance metric (log prob), given a
  # minimum significance verified against a Poisson model of word count
  # distribution.
  #
  # Log probabilities are used for arithmetic limits preventing the ranking of
  # values near 1.
  #
  # The returned data.table has 2 columns: relevance and word_id.

  cum_associated_data = as.data.table(matrix(nrow = 0, ncol = 3))
  colnames(cum_associated_data) <- c("word_id", "freq", "relevance")

  for(sentence.word_id in word_ids) {
    # TODO: try this with a more allowing alpha value
    associated_data = get.associated.word_ids(sentence.word_id, pred.data,
                                              alpha, word_ids)
    # has (log) relevance and word_id columns
    cum_associated_data = rbind(cum_associated_data, associated_data)
  }

  cum_associated_data =
    cum_associated_data[!is.na(cum_associated_data$relevance), ]

  cum_associated_data = cum_associated_data[, .(relevance = sum(relevance)),
                                            c("word_id")]


  return(cum_associated_data)
}

# ---- Testing -----

init.data.for.assoc.tests = function() {
  if (!exists("d")) {
    freq.1gram = fread("data/cv/token_stats/1gram_freq.csv")
    setkey(freq.1gram, word_1)
    ngrams_2 = fread("data/cv/token_stats/2gram_freq_top_reduced.csv")
    setkey(ngrams_2, word_1, word_2)

    get.term.freq = function(word_id) {
      return(freq.1gram[.(word_id), .(freq = freq)]$freq)
    }

    # should be the same as the ngram_dict.csv, but not right now ...
    # TODO: switch back to ngram_dict
    dict = fread("data/cv/token_stats/assoc_dict.csv")
    dict = cbind(dict, id = 0:(nrow(dict) - 1))

    dict.by.word = dict
    setkey(dict.by.word, word)
    dict.by.id = copy(dict)
    setkey(dict.by.id, id)

    total.term.n = sum(freq.1gram$freq)

    d <<- init.associative.pred.data(get.term.freq, total.term.n,
                                     dict.by.word, dict.by.id,
                                     ngrams_2 = ngrams_2)
  }
}

test.get.associated.words = function(word = "", filter = "") {
  init.data.for.assoc.tests()

  associated_data = get.associated.words(word, d)
#  View(associated_data)
  if (filter != "") {
    print(associated_data[associated_data$word == filter, ])
  }
  else {
    print(associated_data)
  }
}

test.get.from.sentence.all = function(sentence.words, filter = "", view.it) {
  init.data.for.assoc.tests()

  cum_associated_data = c()
  for(sentence.word in sentence.words) {
    # TODO: try this with a more allowing alpha value
    associated_data = get.associated.words(sentence.word, d)
    # has (log) relevance and word columns
    cum_associated_data = rbind(cum_associated_data, associated_data)
  }
  cum_associated_data = cum_associated_data[, .(relevance = sum(relevance)),
                                            c("word")]

  if (!identical(filter, "")) {
    setkey(cum_associated_data, word)
    cum_associated_data = cum_associated_data[.(filter), ]

  }

  setorder(cum_associated_data, -relevance)

  if (!missing(view.it)) {
    View(cum_associated_data)
  }
}

# test.get.associated.words("bag", "arm")
# system.time({test.get.associated.words("bag", "hand")})
#
# test.get.associated.words("bag", "arm")
# system.time({test.get.associated.words("bag", "hand")})

# test.get.from.sentence.all("bag")

test.quiz.questions = function() {
  test.get.from.sentence.all(c("like", "people", "almost", "Adam", "Sandler's"),
                             c("movies", "novels", "stories", "pictures"))

  test.get.from.sentence.all(c("I'd", "just", "like", "all", "of", "these",
                               "questions", "answered", "a", "presentation", "of",
                               "evidence", "and", "a", "jury", "to", "settle",
                               "the"),
                             c("case", "matter", "incident", "account"))

  test.get.from.sentence.all(c("When", "you", "were", "in", "Holland", "you",
                               "were", "like", "1", "inch", "away",
                               "from", "me", "but", "you", "hadn't", "time",
                               "to", "take", "a"),
                             c("look", "walk", "minute", "picture"))

  test.get.from.sentence.all(c("inch", "time", "take"),
                             c("look", "walk", "minute", "picture"))


  test.get.from.sentence.all(
    c(
      "Guy", "at", "my", "table's", "wife", "got", "up", "to", "go", "to", "the",
      "bathroom", "and", "I", "asked", "about", "dessert", "and", "he", "started",
      "telling", "me", "about", "his"
    ),
    c("marital", "horticultural", "spiritual", "financial")
  )

  test.get.from.sentence.all(
    c(
      "wife"
    ),
    c("marital", "horticultural", "spiritual", "financial")
  )
  # I'd give anything to see arctic monkeys this

  test.get.from.sentence.all(
    c(
      "I\'d", "give", "anything", "to", "see", "arctic", "monkeys", "this"
    )#,
  #  c("weekend", "morning", "month", "decade")
  )

  # system.time({test.get.associated.words("bag", "hand")})

  # test.get.associated.words("John")
  #
  # test.get.associated.words("bag")
}

# tasks:
# - word_id sequence -> get next words and relevances  (= predict.associative())
# - sentence -> word_id sequence (existing solution needs a facelift (speed))
#
# - then implement the primitive combined lookup

# - then go for the user interface
# - and deployment options!

# - decision: does stuff before the last "," matter in this?
#   [likely postponed -> becomes a TODO]

# test.quiz.questions()