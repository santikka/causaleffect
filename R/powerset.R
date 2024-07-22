# Powerset
#
# Generates the power set of a given set. The power set is the set of all
# possible subsets of the original set, including the empty set and the set itself.
#
# set: vector representing original set for which the power set will be generated
#
# Returns: a list containing all subsets of the original input set


powerset <- function(set) {
  n <- length(set)
# If the input set n is empty, return a list containing only the empty set
  if (n == 0) return(list(c()))
# Generate a representatioin of all possible combinations of elements being
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
