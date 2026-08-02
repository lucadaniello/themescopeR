make_small_result <- function() {
  tokens <- make_two_topic_tokens()
  themescope(tokens, vocab_size = 20, threshold_percentile = 0.8,
             min_community_size = 3, seed = 1, concreteness_lexicon = NULL,
             verbose = FALSE)
}

test_that("save_themescope writes a file that read_themescope restores", {
  res <- make_small_result()
  f   <- tempfile(fileext = ".themescope")
  out <- save_themescope(res, f, meta = list(language = "english"))
  expect_true(file.exists(out))

  arc <- read_themescope(f)
  expect_s3_class(arc, "themescope_archive")
  expect_s3_class(arc$result, "themescope")
  expect_equal(names(arc$result$communities), names(res$communities))
  expect_equal(arc$result$psi, res$psi)
  expect_true(all(c("term", "community", "relevance") %in% names(arc$terms)))
  expect_equal(arc$meta$language, "english")
})

test_that("save_themescope stores the words and the collection when given", {
  res    <- make_small_result()
  tokens <- make_two_topic_tokens()
  coll   <- data.frame(doc_id = "doc_1", text = "a1 a2 a3", stringsAsFactors = FALSE)
  f      <- tempfile(fileext = ".themescope")
  save_themescope(res, f, words = tokens, collection = coll)

  arc <- read_themescope(f)
  expect_equal(nrow(arc$words), nrow(tokens))
  expect_equal(arc$collection$text, "a1 a2 a3")
})

test_that("the .themescope extension is added when missing", {
  res <- make_small_result()
  f   <- tempfile()
  out <- save_themescope(res, f)
  expect_match(out, "\\.themescope$")
  expect_true(file.exists(out))
})

test_that("summary describes the corpus, the parameters and the network", {
  res <- make_small_result()
  f   <- tempfile(fileext = ".themescope")
  save_themescope(res, f, words = make_two_topic_tokens(),
                  meta = list(language = "english", lexicon = "none"))
  info <- summary(read_themescope(f))

  expect_s3_class(info, "data.frame")
  expect_identical(names(info), c("field", "value"))
  expect_true(all(c("Documents", "Analysis unit", "Communities",
                    "Similarity measure") %in% info$field))
  expect_equal(info$value[info$field == "Analysis unit"], "lemma")
})

test_that("read_themescope rejects files that are not archives", {
  f <- tempfile(fileext = ".themescope")
  saveRDS(list(something = 1), f)
  expect_error(read_themescope(f), "not a themescope archive")
  expect_error(read_themescope(tempfile()), "not found")
})

test_that("save_themescope validates its inputs", {
  expect_error(save_themescope(list(), tempfile()), "themescope")
  res <- make_small_result()
  expect_error(save_themescope(res, tempfile(), words = "nope"), "data frame")
  expect_error(save_themescope(res, tempfile(), meta = "nope"), "must be a list")
})
