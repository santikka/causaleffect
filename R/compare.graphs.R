compare.graphs <- function(G1, G2) {
  e1 <- as.data.frame(igraph::ends(G1, igraph::E(G1)))
  e1[ ,3] <- igraph::edge_attr(G1)
  e2 <- as.data.frame(igraph::ends(G2, igraph::E(G2)))
  e2[ ,3] <- igraph::edge_attr(G2)
  n1 <- nrow(e1)
  n2 <- nrow(e2)
  if (n1 != n2) return(FALSE)
  if (ncol(e1) == 2) e1$description <- "O"
  if (ncol(e2) == 2) e2$description <- "O"
  e1[which(is.na(e1[,3])), 3] <- "O"
  e2[which(is.na(e2[,3])), 3] <- "O"
  if (all(duplicated(rbind(e1, e2))[(n1+1):(2*n1)])) return(TRUE)
  return(FALSE)
}