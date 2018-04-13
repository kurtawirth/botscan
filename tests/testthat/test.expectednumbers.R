context("Expected Numbers")
library(botscan)

example <- botscan("#rstats")

test_that("result is less than one", {
  expect_that(example, is_less_than(1))
})
