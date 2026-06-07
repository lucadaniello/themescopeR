# Shared fixtures for tests (no network / udpipe required).

# A tiny, reproducible tokens data frame (4 nouns across 5 sentences).
make_tokens <- function() {
  data.frame(
    doc_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    sentence_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 2, 2),
    lemma       = c("dog", "cat", "bird",
                    "dog", "fish", "cat",
                    "bird", "dog", "fish",
                    "cat", "dog", "bird",
                    "fish", "cat"),
    upos        = rep("NOUN", 14),
    stringsAsFactors = FALSE
  )
}

# A larger, clearly two-block tokens fixture for end-to-end pipeline tests.
make_two_topic_tokens <- function(seed = 42, n_sent = 40, per_sent = 5) {
  set.seed(seed)
  topic_a <- paste0("a", 1:8)
  topic_b <- paste0("b", 1:8)
  rows <- list()
  idx <- 1L
  for (block in list(list(t = topic_a, tag = "A"), list(t = topic_b, tag = "B"))) {
    for (s in seq_len(n_sent)) {
      did <- paste0(block$tag, "_d", s)
      terms <- sample(block$t, per_sent, replace = TRUE)
      rows[[idx]] <- data.frame(
        doc_id      = did,
        sentence_id = paste0(did, "_s1"),
        lemma       = terms,
        upos        = "NOUN",
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, rows)
}

# A small weighted igraph with two communities, for metric tests.
make_test_network <- function(seed = 42) {
  set.seed(seed)
  comm1 <- paste0("term_", 1:10)
  comm2 <- paste0("term_", 11:20)
  edges <- c(); weights <- c()
  for (cm in list(comm1, comm2)) {
    for (i in 1:(length(cm) - 1)) for (j in (i + 1):length(cm)) {
      edges <- c(edges, cm[i], cm[j]); weights <- c(weights, stats::runif(1, 0.1, 0.9))
    }
  }
  edges <- c(edges, "term_1", "term_11"); weights <- c(weights, 0.01)
  g <- igraph::make_graph(edges, directed = FALSE)
  igraph::E(g)$weight <- weights
  communities <- list(C1 = comm1, C2 = comm2)
  membership <- c(stats::setNames(rep(1L, 10), comm1), stats::setNames(rep(2L, 10), comm2))
  presence <- stats::setNames(as.numeric(sample(5:50, 20, replace = TRUE)),
                              c(comm1, comm2))
  list(graph = g, communities = communities, membership = membership, presence = presence)
}
