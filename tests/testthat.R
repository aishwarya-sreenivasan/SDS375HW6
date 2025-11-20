# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(SDS375HW6)

## Validity tests

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

## Coercion tests

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check coercion returns numeric", {
  x <- as(new("sparse_numeric",
              value = c(1, 2, 3, 1),
              pos = c(1L, 2L, 3L, 5L),
              length = 5L), "numeric")
  expect_type(x, "double")
  expect_true(is.numeric(x))
})

## Method existence checks

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

## Generic existence

test_that("sparse add generic", {
  expect_true(isGeneric("sparse_add"))
})

test_that("sparse mult generic", {
  expect_true(isGeneric("sparse_mult"))
})

test_that("sparse sub generic", {
  expect_true(isGeneric("sparse_sub"))
})

test_that("sparse crossprod generic", {
  expect_true(isGeneric("sparse_crossprod"))
})

## Formals

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

## Returned class

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

## Functional tests - sparse_add

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse_add_symbol", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    x + y
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

## Functional tests - sparse_mult

test_that("sparse_mult", {
  result <- as(c(0, 0, 0, 0, 8), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_mult(x, y)
  }, result)
})

test_that("sparse_mult_symbol", {
  result <- as(c(0, 0, 0, 0, 8), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    x * y
  }, result)
})

test_that("sparse_mult length mismatch MUST error", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(sparse_mult(x, y), "same length")
})

test_that("sparse_mult no overlap returns empty", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 0), "sparse_numeric")
  result <- x * y
  expect_equal(length(result@value), 0)
  expect_equal(length(result@pos), 0)
})

test_that("sparse_mult completely separate positions", {
  x <- new("sparse_numeric", value = 5, pos = 1L, length = 10L)
  y <- new("sparse_numeric", value = 3, pos = 10L, length = 10L)
  result <- sparse_mult(x, y)
  expect_equal(length(result@value), 0)
})

## Functional tests - sparse_sub

test_that("sparse_sub", {
  result <- as(c(-1, -1, 0, 1, -2), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_sub(x, y)
  }, result)
})

test_that("sparse_sub_symbol", {
  result <- as(c(-1, -1, 0, 1, -2), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    x - y
  }, result)
})

test_that("sparse_sub_zero", {
  result <- as(c(0, 0, 0, 1, -2), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 1, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_sub(x, y)
  }, result)
})

test_that("sparse_sub length mismatch MUST error", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(sparse_sub(x, y), "same length")
})

## Functional tests - sparse_crossprod

test_that("sparse_crossprod", {
  result <- 8
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_crossprod(x, y)
  }, result)
})

test_that("sparse_crossprod length mismatch MUST error", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(sparse_crossprod(x, y), "same length")
})

test_that("sparse_crossprod no overlap returns 0", {
  x <- new("sparse_numeric", value = 5, pos = 1L, length = 10L)
  y <- new("sparse_numeric", value = 3, pos = 10L, length = 10L)
  result <- sparse_crossprod(x, y)
  expect_equal(result, 0)
})

## Functional tests - sort

test_that("sort ascending", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(2L, 1L, 7L, 3L),
           length = 10L)
  result <- sort(x)

  expect_equal(result@value, c(1, 1, 2, 3))
  expect_equal(result@pos, c(2L, 3L, 1L, 7L))
  expect_equal(result@length, 10L)
})

test_that("sort descending", {
  x <- new("sparse_numeric",
           value = c(1, 2, 3, 1),
           pos = c(2L, 1L, 7L, 3L),
           length = 10L)
  result <- sort(x, decreasing = TRUE)

  expect_equal(result@value, c(3, 2, 1, 1))
  expect_equal(result@pos, c(7L, 1L, 2L, 3L))
})

test_that("sort empty returns empty", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  result <- sort(x)
  expect_equal(length(result@value), 0)
  expect_equal(result@length, 5L)
})

## Functional tests - mean

test_that("mean", {
  result <- 2
  expect_equal({
    x <- as(c(2, 2, 2, 2, 2), "sparse_numeric")
    mean(x)
  }, result)
})

test_that("mean sparse", {
  result <- 1.8
  expect_equal({
    x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
    mean(x)
  }, result)
})

test_that("mean empty vector returns 0", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  result <- mean(x)
  expect_equal(result, 0)
})

## Functional tests - norm

test_that("norm", {
  result <- sqrt(20)
  expect_equal({
    x <- as(c(2, 2, 2, 2, 2), "sparse_numeric")
    norm(x)
  }, result)
})

test_that("norm sparse", {
  result <- 5
  expect_equal({
    x <- as(c(3, 4, 0, 0), "sparse_numeric")
    norm(x)
  }, result)
})

test_that("norm empty vector returns 0", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  result <- norm(x)
  expect_equal(result, 0)
})

## Functional tests - standardize

test_that("standardize", {
  result <- 0
  expect_equal({
    x <- as(c(1, 2, 3, 0, 1), "sparse_numeric")
    mean(standardize(x))
  }, result)
})

test_that("standardize sparse", {
  x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
  result <- standardize(x)
  result_dense <- as(result, "numeric")

  expect_equal(mean(result_dense), 0)
  expect_equal(sd(result_dense), 1)
})

## Show method output test

test_that("show", {
  x <- as(c(1, 0, 3), "sparse_numeric")
  output <- capture.output(show(x))

  expect_true(any(grepl("Sparse numeric vector", output)))
  expect_true(any(grepl("Non-zero elements", output)))
})

test_that("show prints all zeros message", {
  x <- new("sparse_numeric", value = numeric(0), pos = integer(0), length = 5L)
  output <- capture.output(show(x))
  expect_match(paste(output, collapse = " "), "All elements are zero")
})

## Plot method tests

test_that("plot overlap case executes", {
  x <- new("sparse_numeric", value = c(1, 2), pos = c(1L, 3L), length = 5L)
  y <- new("sparse_numeric", value = c(3, 4), pos = c(1L, 3L), length = 5L)
  expect_no_error({
    pdf(NULL)
    plot(x, y)
    dev.off()
  })
})

test_that("plot no overlap case executes", {
  x <- new("sparse_numeric", value = 1, pos = 1L, length = 5L)
  y <- new("sparse_numeric", value = 2, pos = 5L, length = 5L)
  expect_no_error({
    pdf(NULL)
    plot(x, y)
    dev.off()
  })
})
