test_that("build_vocab returns a data frame (unit/freq/upos) ranked by freq", {
  toks  <- make_tokens()
  vocab <- build_vocab(toks, unit = "lemma")
  expect_s3_class(vocab, "data.frame")
  expect_equal(names(vocab), c("lemma", "freq", "upos"))
  expect_true(all(c("dog", "cat") %in% vocab$lemma[1:2]))
  expect_true(all(vocab$upos == "NOUN"))
})

test_that("build_vocab respects unit = 'token' and vocab_size", {
  toks <- data.frame(doc_id = 1, sentence_id = 1,
                     token = c("dogs", "dogs", "cat"),
                     lemma = c("dog", "dog", "cat"), upos = "NOUN")
  v_tok <- build_vocab(toks, unit = "token")
  expect_true("dogs" %in% v_tok$token)
  expect_equal(nrow(build_vocab(toks, unit = "lemma", vocab_size = 1)), 1)
})

test_that("build_vocab filters by POS", {
  toks <- rbind(make_tokens(),
                data.frame(doc_id = 1, sentence_id = 1, lemma = "quickly", upos = "ADV"))
  expect_false("quickly" %in% build_vocab(toks, pos_filter = "NOUN")$lemma)
})

test_that("co-occurrence (frequency) is symmetric with known counts; presence counts sentences", {
  res <- build_cooccurrence_matrix(make_tokens(), unit = "lemma")  # normalization NULL -> frequency
  m <- res$cooc_matrix
  expect_equal(Matrix::nnzero(m - Matrix::t(m)), 0L)
  expect_true(all(Matrix::diag(m) == 0))
  expect_equal(as.numeric(m["dog", "cat"]), 3)        # 3 shared sentences
  expect_equal(unname(res$presence["dog"]), 4)        # dog in 4 sentences
  expect_s3_class(res$vocab, "data.frame")
})

test_that("document window counts co-occurrence at the document level", {
  # x and y are in the SAME document but DIFFERENT sentences.
  df <- data.frame(doc_id = c("d1", "d1"), sentence_id = c(1, 2),
                   lemma = c("x", "y"), upos = "NOUN", stringsAsFactors = FALSE)
  res_s <- build_cooccurrence_matrix(df, unit = "lemma", window = "sentence")
  res_d <- build_cooccurrence_matrix(df, unit = "lemma", window = "document")
  expect_equal(as.numeric(res_s$cooc_matrix["x", "y"]), 0)  # never share a sentence
  expect_equal(as.numeric(res_d$cooc_matrix["x", "y"]), 1)  # share the document
})

test_that("normalize_cooccurrence implements the expected measures", {
  res <- build_cooccurrence_matrix(make_tokens(), unit = "lemma")
  cnt <- res$cooc_matrix; pr <- res$presence  # dog-cat: c=3, a=4,4
  expect_equal(as.numeric(normalize_cooccurrence(cnt, pr, "association")["dog","cat"]), 3/16, tolerance = 1e-12)
  expect_equal(as.numeric(normalize_cooccurrence(cnt, pr, "jaccard")["dog","cat"]),    3/5,  tolerance = 1e-12)
  expect_equal(as.numeric(normalize_cooccurrence(cnt, pr, "salton")["dog","cat"]),     0.75, tolerance = 1e-12)
  expect_equal(as.numeric(normalize_cooccurrence(cnt, pr, "inclusion")["dog","cat"]),  0.75, tolerance = 1e-12)
})

test_that("compute_association_strength == normalize_cooccurrence('association')", {
  res <- build_cooccurrence_matrix(make_tokens(), unit = "lemma")
  a <- compute_association_strength(res$cooc_matrix, res$presence)
  b <- normalize_cooccurrence(res$cooc_matrix, res$presence, "association")
  expect_equal(a["dog", "cat"], b["dog", "cat"])
})

test_that("build_cooccurrence_network returns a weighted igraph", {
  res <- build_cooccurrence_matrix(make_two_topic_tokens(), unit = "lemma",
                                   normalization = "association")
  net <- build_cooccurrence_network(res$cooc_matrix, threshold_percentile = 0.3, verbose = FALSE)
  expect_true(igraph::is_igraph(net))
  expect_true(!is.null(igraph::E(net)$weight))
  expect_gt(igraph::ecount(net), 0)
})
