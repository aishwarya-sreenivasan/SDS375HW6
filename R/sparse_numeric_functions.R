## Define the sparse_numeric class

#' Sparse Numeric Vector Class
#'
#' @slot value Numeric vector of non-zero values
#' @slot pos Integer vector of positions of non-zero values
#' @slot length Integer specifying the total length of the vector
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

## Validity method for sparse_numeric

setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos)) {
    return("'value' and 'pos' must have the same length")
  }
  if (length(object@pos) > 0) {
    if (any(object@pos < 1) || any(object@pos > object@length)) {
      return("'pos' values must be between 1 and length")
    }
  }
  if (length(object@pos) != length(unique(object@pos))) {
    return("'pos' values must be unique")
  }
  if (length(object@length) != 1 || object@length < 0) {
    return("'length' must be a single non-negative integer")
  }
  TRUE
})

## Coerce from numeric to sparse_numeric

setAs("numeric", "sparse_numeric", function(from) {
  non_zero_idx <- which(from != 0)
  new("sparse_numeric",
      value = from[non_zero_idx],
      pos = as.integer(non_zero_idx),
      length = as.integer(length(from)))
})

## Coerce from sparse_numeric to numeric

setAs("sparse_numeric", "numeric", function(from) {
  result <- numeric(from@length)
  if (length(from@pos) > 0) {
    result[from@pos] <- from@value
  }
  result
})

## Generic function for adding two vectors

#' Add two sparse numeric vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments
#' @return A sparse_numeric object containing the sum
#' @examples
#' x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
#' y <- as(c(0, 2, 0, 4, 0), "sparse_numeric")
#' result <- sparse_add(x, y)
#' @export
setGeneric("sparse_add",
           function(x, y, ...)
           {standardGeneric("sparse_add")
           })

## Adding two sparse numeric vectors

#' @describeIn sparse_add Method for sparse_numeric vectors
#' @export
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) {
              stop("Vectors must have the same length")
            }
            all_pos <- sort(unique(c(x@pos, y@pos)))
            result_values <- numeric(length(all_pos))
            for (i in seq_along(all_pos)) {
              pos <- all_pos[i]
              x_val <- if (pos %in% x@pos) x@value[x@pos == pos] else 0
              y_val <- if (pos %in% y@pos) y@value[y@pos == pos] else 0
              result_values[i] <- x_val + y_val
            }
            non_zero <- result_values != 0
            new("sparse_numeric",
                value = result_values[non_zero],
                pos = all_pos[non_zero],
                length = x@length)
          })

## Alternative method for adding two vectors

#' Addition operator for sparse_numeric vectors
#'
#' @param e1 A sparse_numeric object
#' @param e2 A sparse_numeric object
#' @return A sparse_numeric object
#' @export
setMethod("+",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_add(e1, e2)
          })

## Generic function for multiplying two vectors

#' Multiply two sparse numeric vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments (not used)
#' @return A sparse_numeric object containing the product
#' @examples
#' x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
#' y <- as(c(2, 2, 2, 2, 2), "sparse_numeric")
#' result <- sparse_mult(x, y)
#' @export
setGeneric("sparse_mult",
           function(x, y, ...)
           {standardGeneric("sparse_mult")})

## Multiplying two sparse_numeric vectors

#' @describeIn sparse_mult Method for sparse_numeric vectors
#' @export
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) {
              stop("Vectors must have the same length")
            }
            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }
            result_values <- numeric(length(common_pos))
            for (i in seq_along(common_pos)) {
              pos <- common_pos[i]
              x_val <- x@value[x@pos == pos]
              y_val <- y@value[y@pos == pos]
              result_values[i] <- x_val * y_val
            }
            new("sparse_numeric",
                value = result_values,
                pos = common_pos,
                length = x@length)
          })

## Alternative method for multiplying two vectors

#' Multiplication operator for sparse_numeric vectors
#'
#' @param e1 A sparse_numeric object
#' @param e2 A sparse_numeric object
#' @return A sparse_numeric object
#' @export
setMethod("*",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_mult(e1, e2)
          })

## Generic method for subtracting two vectors

#' Subtract two sparse numeric vectors
#'
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments (not used)
#' @return A sparse_numeric object containing the difference
#' @examples
#' x <- as(c(5, 0, 3, 0, 1), "sparse_numeric")
#' y <- as(c(1, 0, 2, 0, 1), "sparse_numeric")
#' result <- sparse_sub(x, y)
#' @export
setGeneric("sparse_sub",
           function(x, y, ...)
           {standardGeneric("sparse_sub")})

## Subtracting two sparse_numeric vectors

#' @describeIn sparse_sub Method for sparse_numeric vectors
#' @export
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) {
              stop("Vectors must have the same length")
            }
            all_pos <- sort(unique(c(x@pos, y@pos)))
            result_values <- numeric(length(all_pos))
            for (i in seq_along(all_pos)) {
              pos <- all_pos[i]
              x_val <- if (pos %in% x@pos) x@value[x@pos == pos] else 0
              y_val <- if (pos %in% y@pos) y@value[y@pos == pos] else 0
              result_values[i] <- x_val - y_val
            }
            non_zero <- result_values != 0
            new("sparse_numeric",
                value = result_values[non_zero],
                pos = all_pos[non_zero],
                length = x@length)
          })

## Alternative method for subtracting two vectors

#' Subtraction operator for sparse_numeric vectors
#'
#' @param e1 A sparse_numeric object
#' @param e2 A sparse_numeric object
#' @return A sparse_numeric object
#' @export
setMethod("-",
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_sub(e1, e2)
          })

## Generic method for cross-multiplying two vectors

#' Cross product of two sparse numeric vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional arguments
#' @return A numeric scalar representing the cross product
#' @examples
#' x <- as(c(1, 0, 3, 0, 5), "sparse_numeric")
#' y <- as(c(2, 0, 4, 0, 6), "sparse_numeric")
#' result <- sparse_crossprod(x, y)
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

## Cross-multiplying two sparse_numeric vectors

#' @describeIn sparse_crossprod Method for sparse_numeric vectors
#' @export
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) {
              stop("Vectors must have the same length")
            }
            common_pos <- intersect(x@pos, y@pos)

            if (length(common_pos) == 0) {
              return(0)
            }
            result <- 0
            for (pos in common_pos) {
              x_val <- x@value[x@pos == pos]
              y_val <- y@value[y@pos == pos]
              result <- result + x_val * y_val
            }
            result
          })

## Generic function for calculating mean

#' Calculate mean of sparse numeric vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments
#' @return Numeric value representing the mean
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  if (length(x@value) == 0) {
    return(0)
  }
  sum(x@value) / x@length
})

## Generic function for calculating norm

#' Calculate norm of a vector
#'
#' @param x An object for which the norm should be calculated
#' @param ... Additional arguments passed to methods
#' @return Numeric value representing the norm
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

## Calculate squared norm of sparse_numeric vector

#' Calculate squared norm of sparse numeric vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments
#' @return Numeric value representing the L2 norm
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  if (length(x@value) == 0) {
    return(0)
  }
  sqrt(sum(x@value^2))
})

## Generic function for standardizing vectors

#' Standardize a vector
#'
#' @param x An object to be standardized
#' @param ... Additional arguments passed to methods
#' @return A standardized object of the same class
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

## Standardize sparse_numeric vector

#' Standardize sparse numeric vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments (not used)
#' @return A standardized sparse_numeric object
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  vec_mean <- mean(x)
  vec_sd <- sd(as(x, "numeric"))

  if (vec_sd == 0) {
    warning("Standard deviation is zero, returning original vector")
    return(x)
  }

  # Convert to dense, standardize, then convert back to sparse
  dense <- as(x, "numeric")
  standardized_dense <- (dense - vec_mean) / vec_sd
  as(standardized_dense, "sparse_numeric")
})

## Show method for printing vectors

#' Show method for sparse_numeric objects
#'
#' @param object A sparse_numeric object
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero elements:", length(object@value), "\n")
  if (length(object@value) > 0) {
    cat("Values:\n")
    for (i in seq_along(object@value)) {
      cat(sprintf("  [%d] = %g\n", object@pos[i], object@value[i]))
    }
  } else {
    cat("All elements are zero\n")
  }
})

## Plotting two vectors

#' Plot two sparse_numeric vectors
#'
#' @param x A sparse_numeric object
#' @param y A sparse_numeric object
#' @param ... Additional plotting parameters
#' @export
setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0) {
              plot.new()
              text(0.5, 0.5, "No overlapping non-zero elements", cex = 1.2)
              return(invisible(NULL))
            }
            x_vals <- x@value[match(common_pos, x@pos)]
            y_vals <- y@value[match(common_pos, y@pos)]

            plot(x_vals, y_vals,
                 xlab = "x values", ylab = "y values",
                 main = "Comparison of Overlapping Non-Zero Elements",
                 pch = 16, col = "blue", ...)
            abline(0, 1, col = "red", lty = 2)
          }
)

## Custom method: sorts the sparse_numeric vector by its values in increasing order

#' Sort sparse_numeric vector
#'
#' @param x A sparse_numeric object
#' @param decreasing values decrease
#' @param ... Additional arguments
#' @return A sparse_numeric object with sorted values
#' @export
setMethod("sort", "sparse_numeric", function(x, decreasing = FALSE, ...) {
  if (length(x@value) == 0) {
    return(x)
  }
  order_idx <- order(x@value, decreasing = decreasing)
  sorted_values <- x@value[order_idx]
  sorted_pos <- x@pos[order_idx]
  new("sparse_numeric",
      value = sorted_values,
      pos = sorted_pos,
      length = x@length)
})
