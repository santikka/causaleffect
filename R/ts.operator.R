"%ts%" <- function(nodes, topo.order) {
  nodes[order(match(nodes, topo.order))]
}