context("test-make_counter.R")

test_counter <- make_counter()
skip_count_by_2 <- make_counter(count_by = 2)

test_that("default count increments by 1", {
  expect_equal(replicate(10, test_counter()), 1:10)
})

test_that("custom starting value works", {
  expect_equal(make_counter(init = 4)(), 5)
  expect_equal(make_counter(init = -1)(), 0)
})

test_that("make_counter skip counts correctly", {
  expect_equal(replicate(5, skip_count_by_2()), seq(2, 10, by = 2))
})

test_that("make_counter throws error with NA arguments", {
  expect_error(make_counter(init = NA)())
  expect_error(make_counter(count_by = NA)())
})

test_that("make_counter throws error with non-numeric count_by argument", {
  expect_error(make_counter(count_by = "a")())
  expect_error(make_counter(count_by = NA)())
})

test_that("make_counter throws error with count_by argument having length > 1", {
  expect_error(make_counter(count_by = 1:2)())
})
