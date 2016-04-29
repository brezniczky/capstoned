get.words.with.coverage =
  function(dict.filename, freq.filename, word.coverage) {

    library(data.table)
    freqs = fread(freq.filename, header = TRUE)
    dict = fread(dict.filename, header = TRUE)

    freqs = freqs[order(freqs$freq, decreasing = TRUE), ]

    cumsum.freqs = cumsum(freqs$freq)
    covered.instances = round(word.coverage * tail(cumsum.freqs, 1))

    pos = findInterval(covered.instances, cumsum.freqs)

    # have to add 1 as id's are 0-based, and indexes are 1-based
    return(dict[freqs$word_1[1:pos] + 1]$word)
  }

remove.bad.words = function(words) {
  # Simple (and inaccurate) profanity removal.

  remove = function(pattern, words) {
    #    cat("removing ", pattern, "\n")
    return(words[!grepl(pattern, words, ignore.case = TRUE)])
  }

  match.exact.ci = function(word, words, locase.words) {
    return(locase.words == tolower(word))
  }

  list = readLines(
    paste0("public/shutterstock/",
           "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/en"))

  # this is a little overly prohibitive (e.g. smashit could be a word) but
  # surely effective
  words = remove("fuck", words)
  words = remove("suck", words)
  words = remove("shit", words)
  words = remove("pussy", words)
  words = remove("arse", words) # sorry Arsene Lupin
  words = remove("ass", words)
  words = remove("cock", words)

  locase.words = tolower(words)
  is.bad.word = rep(FALSE, length(words))
  for(badness in list) {
    is.bad.word = is.bad.word | match.exact.ci(badness, words, locase.words)
    cat("removed ", badness, "\n")
  }
  words = words[!is.bad.word]

  return(words)
}
