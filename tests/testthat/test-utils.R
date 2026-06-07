test_that("zscore standardises and preserves NA", {
  z <- zscore(c(1, 2, 3, 4, 5))
  expect_equal(mean(z), 0, tolerance = 1e-12)
  expect_equal(stats::sd(z), 1, tolerance = 1e-12)
  expect_true(is.na(zscore(c(1, NA, 3))[2]))
})

test_that("assign_quadrant maps the four corners correctly", {
  q <- assign_quadrant(c(1, 1, -1, -1), c(1, -1, 1, -1))
  expect_equal(as.character(q),
               c("Stable Core", "Ideological Core",
                 "Emerging Practices", "Latent Representations"))
})

test_that("match_concreteness is case-insensitive and NA for misses", {
  lex <- data.frame(word = c("dog", "freedom"), conc.m = c(4.8, 1.5))
  v <- match_concreteness(c("DOG", "freedom", "unknown"), lex)
  expect_equal(unname(v[1]), 4.8)
  expect_true(is.na(v[3]))
})

test_that("validate_tokens_df accepts valid and rejects invalid input", {
  expect_true(validate_tokens_df(make_tokens()))
  expect_error(validate_tokens_df(data.frame(x = 1)))
  expect_error(validate_tokens_df(data.frame(doc_id = 1, sentence_id = 1, upos = "NOUN")))
})
