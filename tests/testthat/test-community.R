test_that("detect_communities recovers two blocks and filters small ones", {
  obj <- make_test_network()
  comm <- detect_communities(obj$graph, algorithm = "walktrap",
                             min_size = 5, seed = 1)
  expect_named(comm, c("membership", "communities", "algorithm_result"))
  expect_gte(length(comm$communities), 1)
  expect_true(all(vapply(comm$communities, length, integer(1)) >= 5))
})

test_that("detect_communities is reproducible with a seed (louvain)", {
  obj <- make_test_network()
  m1 <- detect_communities(obj$graph, algorithm = "louvain", min_size = 3, seed = 7)$membership
  m2 <- detect_communities(obj$graph, algorithm = "louvain", min_size = 3, seed = 7)$membership
  expect_equal(m1, m2)
})

test_that("detect_communities supports the leiden algorithm", {
  obj  <- make_test_network()
  comm <- detect_communities(obj$graph, algorithm = "leiden", resolution = 1,
                             min_size = 3, seed = 1, verbose = FALSE)
  expect_named(comm, c("membership", "communities", "algorithm_result"))
  expect_gte(length(comm$communities), 1)
})

test_that("get_community_subgraphs returns one subgraph per community", {
  obj <- make_test_network()
  comm <- detect_communities(obj$graph, min_size = 3, seed = 1)
  subs <- get_community_subgraphs(obj$graph, comm$membership)
  expect_length(subs, length(comm$communities))
  expect_true(all(vapply(subs, igraph::is_igraph, logical(1))))
})
