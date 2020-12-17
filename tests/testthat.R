# setting R_TESTS to empty string because of
# https://github.com/hadley/testthat/issues/144
# revert this when that issue in R is fixed.
Sys.setenv("R_TESTS" = "")

library(testthat)
library(farsfunction)
library(dplyr)
library(graphics)
library(maps)
library(readr)
library(tidyr)

# make_filename test

test_that("Testing make_filename", {
  expect_equal(farsfunction::make_filename(2019), "accident_2019.csv.bz2")})
