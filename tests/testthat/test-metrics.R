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
