# TODO: generalize functionality and move it outside this unit to enable the
#       creation of alternative samplings of the data (e.g. for bootstrap
#       estimates of things)

# Note: assumed to be run with the project root as the working directory
source("clean/solution/01_text_loading.R")

are.input.variants.ready = function(cv.path, verification.path) {
  cv.files = file.path(cv.path, c("training.txt", "test.txt", "validation.txt"))
  ver.files = file.path(verification.path, "subsampled.txt")
  all.files = c(cv.files, ver.files)

  return(all(file.exists(all.files)))
}

generate.input.variants = function(source.path,
                                   cv.path,
                                   verification.path,
                                   sample.approx.size.MB = 10) {

  write.file = function(path, file, lines) {
    writeLines(text = lines, con = file.path(path, file))
  }

  input = read.files(source.path)
  input = unlist(input)

  if (!is.null(cv.path)) {

    # allocate 60% for training, 20% for validation and 20% for testing

    perc = sample(100, length(input), replace = TRUE)
    is.train = perc <= 60
    is.validation  = (61 <= perc) & (perc <= 80)
    is.test  = perc > 80
    rm(perc)

    write.file(cv.path, "training.txt", input[is.train])
    rm(is.train)
    write.file(cv.path, "validation.txt", input[is.validation])
    rm(is.validation)
    write.file(cv.path, "test.txt", input[is.test])
    rm(is.test)
  }

  if (!is.null(verification.path)) {

    # create small test data

    byte_per_MB = 1024 * 1024
    verification.lines = round(sample.approx.size.MB * byte_per_MB /
                                 object.size(input) * length(input))

    # as much unique lines as before guarantee similarly non-overlapping sets
    # whenever the data is further split for the verification of a CV based
    # algorithm (potential future scenario at the time writing)

    verification.idx = sample(length(input),
                              size = verification.lines,
                              replace = verification.lines > length(input))
    write.file(verification.path, "subsampled.txt", input[verification.idx])
  }
}

run.input.variant.generation = function() {
  source.path = "input/en_US/"
  cv.path = "data/cv/"
  verification.path = "data/verification/"

  if (!are.input.variants.ready(cv.path = cv.path,
                                verification.path = verification.path)) {
    generate.input.variants(source.path, cv.path, verification.path)
  }
}

test.input.variant.generation = function() {
  generate.input.variants("data/dummy_texts/",
                          "data/dummy_texts/cv/",
                          "data/dummy_texts/verification/")
  if (!are.input.variants.ready("data/dummy_texts/cv/",
                                "data/dummy_texts/verification/")) {
    stop("didn't create them")
  }
  cat("test.input.variant.generation() passed\n")
}

test.input.variant.generation()
