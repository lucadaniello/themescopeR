test_that("themescope runs end-to-end on tokens and returns a valid object", {
  toks <- make_two_topic_tokens()
  lex  <- data.frame(word = unique(toks$lemma),
                     conc.m = stats::runif(length(unique(toks$lemma)), 1, 5))
  res <- themescope(
    toks, concreteness_lexicon = lex,
    vocab_size = 16, threshold_percentile = 0.3,
    min_community_size = 3, seed = 1, verbose = FALSE
  )
  expect_s3_class(res, "themescope")
  expect_true(all(c("graph", "communities", "membership", "psi", "cs",
                    "presence", "network_stats", "vocab", "params") %in% names(res)))
  expect_gte(length(res$communities), 1)
  expect_named(res$psi)
  expect_equal(length(res$psi), length(res$communities))
})

test_that("as.data.frame and top_terms work on a themescope object", {
  toks <- make_two_topic_tokens()
  res <- themescope(toks, concreteness_lexicon = NULL,
                    vocab_size = 16, threshold_percentile = 0.3,
                    min_community_size = 3, seed = 1, verbose = FALSE)

  df <- as.data.frame(res)
  expect_true(all(c("community_id", "size", "psi", "cs", "psi_z", "cs_z",
                    "quadrant") %in% names(df)))
  expect_equal(nrow(df), length(res$communities))

  tt <- top_terms(res, n = 3)
  expect_true(all(c("community", "rank", "term", "relevance", "degree") %in% names(tt)))
  expect_lte(max(tt$rank), 3)
})

test_that("plot.themescope builds a ggplot map (id and term labels)", {
  toks <- make_two_topic_tokens()
  res <- themescope(toks, concreteness_lexicon = NULL,
                    vocab_size = 16, threshold_percentile = 0.3,
                    min_community_size = 3, seed = 1, verbose = FALSE)
  expect_s3_class(plot(res, type = "map"), "ggplot")
  expect_s3_class(plot(res, type = "map", label = "terms", n_label_terms = 2), "ggplot")
  expect_s3_class(
    plot(res, type = "map", label = "terms", label_by = "frequency", n_label_terms = 2),
    "ggplot"
  )
})

test_that("themescope vocab is a build_vocab data frame and stores params", {
  toks <- make_two_topic_tokens()
  res <- themescope(toks, concreteness_lexicon = NULL, unit = "lemma",
                    vocab_size = 16, threshold_percentile = 0.3,
                    min_community_size = 3, normalization = "association",
                    community_algorithm = "louvain", seed = 1, verbose = FALSE)
  expect_s3_class(res$vocab, "data.frame")
  expect_true(all(c("lemma", "freq", "upos") %in% names(res$vocab)))
  expect_equal(res$params$normalization, "association")
  expect_equal(res$params$unit, "lemma")
})

test_that("themescope errors on a raw collection without a model", {
  coll <- data.frame(doc_id = "d1", text = "some text here")
  expect_error(themescope(coll), "model")
})
