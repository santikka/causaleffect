"%ts%" <- function(nodes, topological.order) {
  nodes[order(match(nodes, topological.order))]
}