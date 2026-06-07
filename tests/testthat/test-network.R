test_that("build_vocab returns most frequent terms first and respects POS", {
  toks <- make_tokens()
  vocab <- build_vocab(toks, vocab_size = 10)
  expect_type(vocab, "character")
  expect_true(all(c("dog", "cat") %in% vocab[1:2]))

  toks2 <- rbind(toks, data.frame(doc_id = 1, sentence_id = 1,
                                  lemma = "quickly", upos = "ADV"))
  expect_false("quickly" %in% build_vocab(toks2, pos_filter = "NOUN"))
})

test_that("co-occurrence matrix is symmetric with zero diagonal and known counts", {
  res <- build_cooccurrence_matrix(make_tokens(), vocab_size = 10)
  m <- res$cooc_matrix
  expect_equal(nrow(m), ncol(m))
  expect_equal(Matrix::nnzero(m - Matrix::t(m)), 0L)
  expect_true(all(Matrix::diag(m) == 0))
  # dog & cat co-occur in d1s1, d1s2, d2s1 -> 3 sentences
  expect_equal(as.numeric(m["dog", "cat"]), 3)
})

test_that("presence counts sentences, not tokens", {
  res <- build_cooccurrence_matrix(make_tokens(), vocab_size = 10)
  # dog appears in d1s1, d1s2, d1s3, d2s1 -> 4 sentences
  expect_equal(unname(res$presence["dog"]), 4)
  # cat appears in d1s1, d1s2, d2s1, d2s2 -> 4 sentences
  expect_equal(unname(res$presence["cat"]), 4)
  expect_setequal(names(res$presence), res$vocab)
})

test_that("association strength is linear: AS(i,j) = c(i,j) / (a_i * a_j)", {
  res    <- build_cooccurrence_matrix(make_tokens(), vocab_size = 10)
  as_mat <- compute_association_strength(res$cooc_matrix, res$presence)
  c_ij <- as.numeric(res$cooc_matrix["dog", "cat"])
  expected <- c_ij / (res$presence["dog"] * res$presence["cat"])
  expect_equal(as.numeric(as_mat["dog", "cat"]), unname(expected), tolerance = 1e-12)
  expect_true(all(as_mat@x >= 0 & as_mat@x <= 1 + 1e-9))
})

test_that("build_cooccurrence_network returns a weighted igraph", {
  res    <- build_cooccurrence_matrix(make_two_topic_tokens(), vocab_size = 16)
  as_mat <- compute_association_strength(res$cooc_matrix, res$presence)
  net    <- build_cooccurrence_network(as_mat, threshold_percentile = 0.3, verbose = FALSE)
  expect_true(igraph::is_igraph(net))
  expect_true(!is.null(igraph::E(net)$weight))
  expect_gt(igraph::ecount(net), 0)
})
