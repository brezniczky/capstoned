library(data.table)
library(Rcpp)
sourceCpp("clean/solution/10_ngram_cpp.cpp")

create.get.ngram.count.fun = function(ngram.tables) {
  # ngram.tables: list of ngram data.tables

  # make sure the arguments are prepared for fast in-memory lookups
  if (key(ngram.tables[[1]]) != "word_1") stop("invalid key")
  if (any(key(ngram.tables[[2]]) != c("word_1", "word_2"))) stop("invalid key")
  if (any(key(ngram.tables[[3]]) != c("word_1", "word_2", "word_3")))
    stop("invalid key")
  if (any(key(ngram.tables[[4]]) != c("word_1", "word_2", "word_3", "word_4")))
    stop("invalid key")

  get.ngram.count.fun =
    function(word_ids) {

      word_ids = rbind(word_ids) # force vectors to be rows
      ngram_n = ncol(word_ids)
      if (length(word_ids) > 0) {
        ngrams = ngram.tables[[ngram_n]]
        # create an expr. like ngrams[.(word_ids[, 1], word_ids[, 2]), ]
        # this works both as n'th columns for matrices and n'th elements for
        # previously simple vectors
        lookup.expr = paste0("ngrams[.(",
                             paste0(paste0("word_ids[, ", 1:ngram_n, "]"),
                                    collapse = ", "),
                             "), ]")
        ans = eval(parse(text = lookup.expr))$freq
      } else {
        ans = NA
      }
      ans[is.na(ans)] = 0
      return(ans)
    }

  return(get.ngram.count.fun)
}

calc.ngram.probabilities = function(pred.word_ids, word_id,
                                    get.ngram.count.fun, word.count) {
  if (length(pred.word_ids) > 0) {
    n.preceding = get.ngram.count.fun(pred.word_ids)
  } else {
    n.preceding = word.count
  }

  n.rep = length(word_id)
  ngrams.combined =
    cbind(
      matrix(nrow = n.rep, byrow = TRUE,
             data = rep(pred.word_ids, n.rep)),
      word_id)

  n.combined = get.ngram.count.fun(word_ids = ngrams.combined)
  # TODO: is it still necessary to log this prob.?
  return(log(as.double(n.combined) / n.preceding))
}

predict.ngram = function(word_ids, top.4.word_ids, ngrams_1,
                         ngrams_2, ngrams_3, ngrams_4, total.term.count,
                         get.ngram.count.fun, max.hits = 10) {

  # top.4.word_ids: The most common words, i.e. unigrams.
  #
  # TODO: reduce the code

  predict.ngram.unordered = function() {
    l = length(word_ids)
    word_ids[is.na(word_ids)] = -1
    if (l > 0) {
      id.m1 = word_ids[l]
    }
    if (l > 1) {
      id.m2 = word_ids[l - 1]
    }
    if (l > 2) {
      id.m3 = word_ids[l - 2]
    }

    hits = as.data.table(matrix(ncol = 3, nrow = 0))
    colnames(hits) <- c("word_id", "relevance", "reliability")

    expand.cols = function(hits, ngram.n) {
      colnames(hits) <- c("word_id")
      hits$relevance =
        calc.ngram.probabilities(pred.word_ids = tail(word_ids, min(ngram.n - 1, 3)),
                                 word_id = hits$word_id,
                                 get.ngram.count.fun,
                                 word.count = total.term.count)

      # TODO: this could/should also consider how many samples were there to
      #       estimate the probability from (i.e. accuracy of the estimate)
      hits$reliability = ngram.n
      return(hits)
    }

    if (l > 2) {
      filter =
        (ngrams_4$word_3 == id.m1) &
        (ngrams_4$word_2 == id.m2) &
        (ngrams_4$word_1 == id.m3)
      if (sum(filter) > 0) {
        hits = expand.cols(ngrams_4[filter, ][, c("word_4"), with = FALSE], 4)
      }
    }

    if (l > 1) {
      filter =
        (ngrams_3$word_2 == id.m1) &
        (ngrams_3$word_1 == id.m2)
      if (sum(filter) > 0) {
        hits = rbind(hits,
                     expand.cols(ngrams_3[filter, ][, c("word_3"), with = FALSE], 3))
      }
    }

    # TODO: hey, this lookup is not table-key based yet! fix it!
    if (l > 0) {
      filter = (ngrams_2$word_1 == id.m1)
      if (sum(filter) > 0) {
        hits = rbind(hits,
                     expand.cols(ngrams_2[filter, ][, c("word_2"), with = FALSE], 2))
      }
    }

    hits = rbind(hits,
                 expand.cols(ngrams_1[.(top.4.word_ids), ][, c("word_1"), with = FALSE], 1))

    return(hits)
  }

  ans = predict.ngram.unordered()

  ans = ans[,
            .(relevance = relevance[which.max(reliability)]),
            word_id]

  setorder(ans, -relevance)


  return(head(ans, max.hits))
}

words.to.ids = function(words, dict.by.word) {
  ids = dict.by.word[.(words), ]$id
  return(ids)
}

load.ngrams = function() {

  ngram_1_file = "data/cv/token_stats/1gram_freq.csv"

  files =
    c(ngram_1_file,
      sprintf("data/cv/token_stats/%dgram_freq_top_reduced.csv", 2:4))
  ans = list()

  ans =
    sapply(files,
           function(file) {
             fread(file, header = TRUE)
           }
    )

  names(ans) <- paste0("ngrams_", 1:4)

  for(i in 1:4) {
    # allow quick lookups by word_1, word_2, ... by setting an index
    setkeyv(ans[[i]], paste0("word_", 1:i))
  }

  return(ans)
}

# w =
# words.to.indexes(
#   c("al", "jobb", "alap", "Alap", "ba", "jobb", "bal"),
#   dict = data.frame(word = c("alap", "bal", "jobb"))
# )

get.top.4.word_ids = function(ngrams_1) {
  return(
    head(
      order(ngrams_1$freq, decreasing = TRUE), 4)
    - 1) # word_id's are 0-based, array indexes are 1-based
}


test.get.ngram.fun = function() {
  ngram.tables = load.ngrams()
  myfun = create.get.ngram.count.fun(ngram.tables = ngram.tables)

  #  debug(calc.ngram.relevance)
  print(
    calc.ngram.relevance(word_ids = c(0, 1),
                         get.ngram.count.fun = myfun,
                         word.count = sum(ngram.tables$ngrams_1$freq))
  )
  print(
    system.time({
      for(i in (1:100)) {
        calc.ngram.relevance(word_ids = c(0, 1, 589),
                             get.ngram.count.fun = myfun,
                             word.count = sum(ngram.tables$ngrams_1$freq))
      }
    })
  )
}

test = function() {
  dict = fread("data/cv/token_stats/ngram_dict.csv")
  dict$id = 0:(nrow(dict) - 1)
  setkey(dict, word)
  dict.by.id = copy(dict)
  setkey(dict.by.id, id)

  ngrams = load.ngrams()

  print(str(ngrams))

  sentence = "This is why I thank"

  top.4.word_ids = get.top.4.word_ids(ngrams$ngrams_1)
  total.term.count = sum(ngrams$ngrams_1$freq)

  print("top unigrams")
  print(cbind(word_id = top.4.word_ids, term = dict[top.4.word_ids + 1]))

  words = get_words(sentence, TRUE)
  ids = words.to.ids(words, dict)

  print(ids)

  get.ngram.count.fun = create.get.ngram.count.fun(ngram.tables = ngrams)

  next.id.hat =
    predict.ngram(ids, top.4.word_ids,
                  ngrams_1 = ngrams$ngrams_1,
                  ngrams_2 = ngrams$ngrams_2,
                  ngrams_3 = ngrams$ngrams_3,
                  ngrams_4 = ngrams$ngrams_4,
                  total.term.count = total.term.count,
                  get.ngram.count.fun = get.ngram.count.fun)
  setorder(next.id.hat, -relevance)
  next.id.hat = next.id.hat$word_id
  print(dict[next.id.hat + 1])


  cat("Enter * to stop.\n")

  while((sentence = readline("enter sentence:")) != "*") {
    words = get_words(sentence, TRUE)
    ids = words.to.ids(words, dict)

    next.id.hat =
      predict.ngram(ids, top.4.word_ids,
                    ngrams_1 = ngrams$ngrams_1,
                    ngrams_2 = ngrams[[2]],
                    ngrams_3 = ngrams[[3]],
                    ngrams_4 = ngrams[[4]],
                    total.term.count = total.term.count,
                    get.ngram.count.fun = get.ngram.count.fun)
    setorder(next.id.hat, -relevance)

    print(dict.by.id[.(next.id.hat$word_id), ])
  }
}

# test.get.ngram.fun()
# test()
