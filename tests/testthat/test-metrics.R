test_that("compute_psi returns a named numeric vector per community", {
  obj <- make_test_network()
  psi <- compute_psi(obj$graph, obj$communities, obj$presence)
  expect_type(psi, "double")
  expect_named(psi)
  expect_equal(sort(names(psi)), sort(names(obj$communities)))
  expect_true(all(is.finite(psi)))
})

test_that("compute_psi errors on a non-igraph graph", {
  obj <- make_test_network()
  expect_error(compute_psi("nope", obj$communities, obj$presence))
})

test_that("compute_cs matches the closed-form on a trivial single edge", {
  g <- igraph::make_graph(c("a", "b"), directed = FALSE)
  igraph::E(g)$weight <- 1.0
  lex <- data.frame(word = c("a", "b"), conc.m = c(2.0, 4.0))
  cs <- compute_cs(g, list(C1 = c("a", "b")), lex)
  expect_equal(as.numeric(cs["C1"]), 3.0, tolerance = 1e-10)  # (2+4)/2
})

test_that("compute_cs is NA when no edge endpoints are in the lexicon", {
  obj <- make_test_network()
  lex <- data.frame(word = "notaword", conc.m = 3.0)
  expect_true(all(is.na(compute_cs(obj$graph, obj$communities, lex))))
})

test_that("term_relevance returns one row per assigned vertex with relevance", {
  obj <- make_test_network()
  rel <- term_relevance(obj$graph, obj$membership, obj$presence)
  expect_s3_class(rel, "data.frame")
  expect_true(all(c("term", "community", "relevance", "degree", "presence") %in% names(rel)))
  expect_true(all(rel$relevance >= 0))
  expect_setequal(unique(rel$community), c("C1", "C2"))
})

test_that("term_relevance matches the closed form on a 3-node graph", {
  g <- igraph::make_graph(c("a", "b", "a", "c"), directed = FALSE)
  igraph::E(g)$weight <- c(2, 1)
  memb     <- c(a = 1L, b = 1L, c = 2L)
  presence <- c(a = 3, b = 1, c = 1)
  rel <- term_relevance(g, memb, presence)
  # a: s_in = 2 (a-b, same community), s_out = 1 (a-c) -> log1p(3) * 2/3
  expect_equal(rel$relevance[rel$term == "a"], log1p(3) * 2 / 3, tolerance = 1e-12)
  # b: s_in = 2, s_out = 0 -> log1p(1) * 1
  expect_equal(rel$relevance[rel$term == "b"], log1p(1), tolerance = 1e-12)
  # c: s_in = 0, s_out = 1 -> 0
  expect_equal(rel$relevance[rel$term == "c"], 0)
})

test_that("top_terms ranks by relevance (default), frequency and degree", {
  obj  <- make_test_network()
  fake <- structure(
    list(graph = obj$graph, communities = obj$communities,
         membership = obj$membership, presence = obj$presence),
    class = "themescope"
  )

  # Default is relevance R_t; output carries relevance, frequency and degree.
  tt <- top_terms(fake, n = 5)
  expect_true(all(c("community", "rank", "term", "relevance", "frequency",
                    "degree") %in% names(tt)))
  expect_lte(max(tt$rank), 5)

  # frequency = term presence a_t, and rows are ordered by it within community.
  ttf <- top_terms(fake, n = 5, by = "frequency")
  expect_equal(ttf$frequency, unname(obj$presence[ttf$term]))
  for (cid in unique(ttf$community)) {
    v <- ttf$frequency[ttf$community == cid]
    expect_false(is.unsorted(rev(v)))  # non-increasing within the community
  }

  # relevance ranking is non-increasing within each community
  ttr <- top_terms(fake, n = 5, by = "relevance")
  for (cid in unique(ttr$community)) {
    v <- ttr$relevance[ttr$community == cid]
    expect_false(is.unsorted(rev(v)))
  }

  # invalid criterion is rejected
  expect_error(top_terms(fake, by = "nonsense"))
})

test_that("lexicon_coverage reports per-community and overall coverage", {
  obj <- make_test_network()
  fake <- structure(list(graph = obj$graph, communities = obj$communities),
                    class = "themescope")
  lex <- data.frame(word = paste0("term_", 1:5), conc.m = 3.0)
  cov <- lexicon_coverage(fake, lex)
  expect_s3_class(cov, "data.frame")
  expect_equal(cov$community, c("C1", "C2", "(all)"))
  expect_equal(cov$n_matched, c(5L, 0L, 5L))   # term_1..term_5 are all in C1
  expect_equal(cov$coverage, c(0.5, 0, 0.25))

  # Character-vector input
  cov2 <- lexicon_coverage(c("term_1", "term_99"), lex)
  expect_equal(cov2$coverage, 0.5)
  expect_error(lexicon_coverage(42, lex))
})

test_that("compute_network_stats reports the expected structure", {
  obj <- make_test_network()
  st  <- compute_network_stats(obj$graph, obj$communities)
  expect_named(st, c("community_stats", "global_stats"))
  expect_equal(nrow(st$community_stats), length(obj$communities))
  expect_equal(st$global_stats$n_nodes, igraph::vcount(obj$graph))
})

test_that("compute_coherence of a clique is 1", {
  g <- igraph::make_full_graph(4)
  igraph::V(g)$name <- letters[1:4]
  coh <- compute_coherence(g, list(C1 = letters[1:4]))
  expect_equal(as.numeric(coh["C1"]), 1.0, tolerance = 1e-10)
})
