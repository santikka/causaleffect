#' Powerset
#'
#' Generates the power set of a given set. The power set is the set of all possible subsets of the original set, including the empty set and the set itself.
#'
#' @param set A vector representing the original set for which the power set will be generated. The set can contain any type of elements (e.g., numeric, character, or logical).
#'
#' @details The function computes all possible combinations of the elements of the input set. This includes the empty subset, individual elements, and all larger subsets up to and including the full set. The number of subsets in the power set of a set of size \code{n} is \code{2^n}.
#'
#' @return A list of vectors, where each vector is a subset of the original input set. The list contains \code{2^n} subsets, where \code{n} is the length of the input set. If the input set is empty, the function returns a list containing only the empty set.
#'
#' @examples
#'
#'
#' @seealso \code{\link{join}} for using powerset with conditional independence in probabilistic graphical models.
#'
#' @keywords set theory combinatorics
#' @concept power set
#' @concept subsets


powerset <- function(set) {
  n <- length(set)
# If the input set n is empty, return a list containing only the empty set
  if (n == 0) return(list(c()))
# Generate a representation of all possible combinations of elements being
# included or excluded from the subsets: all binary numbers from 0 to 2^n - 1.
# Then, convert them to logical vectors of length n. Each logical vector
# indicates which elements of the input set are included in a particular subset.
  indices <- sapply(0:(2^n-1), function(p) as.logical(intToBits(p)[1:n]), simplify = FALSE)
# Use these logical vectors to create subsets of the input set. Elements that
# correspond to TRUE values in the vector are extracted. Each logical
# vector corresponds to one possible subset.
  subsets <- lapply(indices, function(i) set[i])
  return(subsets)
}
