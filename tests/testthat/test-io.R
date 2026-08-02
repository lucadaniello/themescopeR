test_that("read_collection reads a CSV and tidies columns", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(doc_id = c("d1", "d2"), text = c("hello world", "foo bar"),
               extra = c(1, 2)),
    tmp, row.names = FALSE
  )
  out <- read_collection(tmp)
  expect_s3_class(out, "data.frame")
  expect_identical(names(out)[1:2], c("doc_id", "text"))
  expect_equal(nrow(out), 2)
  expect_type(out$doc_id, "character")
  expect_true("extra" %in% names(out))
})

test_that("read_collection generates doc_id when absent and detects text", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(body = c("a b", "c d")), tmp, row.names = FALSE)
  out <- read_collection(tmp, text_col = "body")
  expect_equal(out$doc_id, c("doc_1", "doc_2"))
  expect_equal(out$text, c("a b", "c d"))
})

test_that("sequential_ids renumbers documents and keeps the source id", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(doc_id = c("t3_abc", "t3_def"), text = c("a b", "c d")),
    tmp, row.names = FALSE
  )
  out <- read_collection(tmp, sequential_ids = TRUE)
  expect_equal(out$doc_id, c("doc_1", "doc_2"))
  expect_equal(out$source_id, c("t3_abc", "t3_def"))
  expect_identical(names(out)[1:2], c("doc_id", "text"))
})

test_that("sequential_ids numbers a zip archive continuously", {
  d <- tempfile(); dir.create(d)
  writeLines("the cat sat", file.path(d, "doc1.txt"))
  writeLines("the dog ran", file.path(d, "doc2.txt"))
  zip_path <- tempfile(fileext = ".zip")
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  utils::zip(zip_path, files = c("doc1.txt", "doc2.txt"), flags = "-q")
  setwd(old)
  out <- read_collection(zip_path, sequential_ids = TRUE)
  expect_equal(out$doc_id, c("doc_1", "doc_2"))
  expect_setequal(out$source_id, c("doc1", "doc2"))
})

test_that("read_collection handles a zip of many txt files (one doc each)", {
  d <- tempfile(); dir.create(d)
  writeLines("the cat sat", file.path(d, "doc1.txt"))
  writeLines("the dog ran", file.path(d, "doc2.txt"))
  zip_path <- tempfile(fileext = ".zip")
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  utils::zip(zip_path, files = c("doc1.txt", "doc2.txt"), flags = "-q")
  setwd(old)
  out <- read_collection(zip_path)
  expect_equal(nrow(out), 2)
  expect_setequal(out$doc_id, c("doc1", "doc2"))
  expect_true(all(c("the cat sat", "the dog ran") %in% out$text))
})

test_that("read_collection errors on a missing file", {
  expect_error(read_collection(tempfile(fileext = ".csv")), "not found")
})

test_that("read_collection errors when text column is undetectable", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:2, b = 3:4), tmp, row.names = FALSE)
  expect_error(read_collection(tmp), "text column")
})
