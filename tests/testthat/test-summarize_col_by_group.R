library(testthat)

test_data_no_na <- data.frame(
  group = c("a", "b", "b"),
  summary = c(1, 2, 3)
)

test_that("function works on vector with no NA's", {
  result <- summarize_col_by_group(test_data_no_na, "group", "summary")
  expect_equal(nrow(result), 2)

  result_a = result[result$group == "a",]
  expect_equal(result_a$average, 1)
  expect_equal(result_a$max_value, 1)
  expect_equal(result_a$min_value, 1)

  result_b = result[result$group == "b",]
  expect_equal(result_b$average, 2.5)
  expect_equal(result_b$max_value, 3)
  expect_equal(result_b$min_value, 2)
})

test_that("function works on vector has NA's", {
  test_data_with_na <- test_data_no_na
  test_data_with_na$summary[c(1, 2)] <- NA

  result <- summarize_col_by_group(test_data_with_na, "group", "summary")
  expect_equal(nrow(result), 1)

  result_b = result[result$group == "b",]
  expect_equal(result_b$average, 3)
  expect_equal(result_b$max_value, 3)
  expect_equal(result_b$min_value, 3)
})

test_that("function works on vector of a different type", {
  test_data_diff <- data.frame(
    group = c("a", "b", "b"),
    summary = c("a", "b", "b")
  )

  expect_error(summarize_col_by_group(test_data_diff, "group", "summary"))
})

rm(test_data_no_na)
