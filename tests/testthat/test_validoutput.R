context("ValidProportions")
library(botscan)

load("test_object.RData")

test_that("proportions calculated are between 0 and 1, inclusive", 
          {
            expect_lte(test_object$prop_user_level_bots, 1)
            expect_gte(test_object$prop_user_level_bots, 0)
            expect_lte(test_object$prop_convo_level_bots, 1)
            expect_gte(test_object$prop_convo_level_bots, 0)
            }
          )
