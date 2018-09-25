context("Expected Numbers")
library(botscan)

example <- botscan("#rstats")

test_that("result is less than or equal to one and greater than or equal to 
          zero", {
            expect_that(example, is_less_than(1))
})
