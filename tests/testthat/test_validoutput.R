context("ValidProportions")
library(botscan)

example <- botscan("#rstats")

test_that("proportions calculated are between 0 and 1, inclusive", 
          {
            expect_lte(example$prop_user_level_bots, 1)
            expect_gte(example$prop_user_level_bots, 0)
            expect_lte(example$prop_convo_level_bots, 1)
            expect_gte(example$prop_convo_level_bots, 0)
            }
          )
