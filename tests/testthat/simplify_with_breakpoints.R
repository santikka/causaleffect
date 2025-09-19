# This file was used to create `join` and `insert` unit tests for Vignettes #1, #2, and #3.
  # for example, I ran this code, then ran: `simplify(P_3_s1 (or s2), topo_3, G_3.unobs, G_3, G_3.obs)` with break points
  # (the browser() function). I added print statements after step #5 in simplify()
  # Example output:
    # Step 6 - Inside nested while loop before join operation
    # P$children[[k]]$var: y (this represents vari in simplify())
    # P$children[[k]]$cond: w x (this represents cond in simplify())
    # P$sumset[j]: w (this reprensents S in simplify())

simplify <- function(P, topo, G.unobs, G, G.obs) {
  step <- 0  # Initialize a step counter

  step <- step + 1
  cat("Step", step, "- Initialize j\n")
  j <- 0
  browser()  # Breakpoint 1: At the start of the function

  while (j < length(P$sumset)) {
    step <- step + 1
    cat("Step", step, "- Start of while loop\n")
    browser()  # Breakpoint 2: At the start of the while loop

    P.orig <- P
    irl.len <- 0
    irrel <- NULL
    terms <- list()
    vars <- sapply(P$children, "[[", "var")

    j <- j + 1
    i <- which(vars == P$sumset[j])
    k <- 1
    R.var <- character()
    R.cond <- list()
    J <- character()
    D <- character()

    step <- step + 1
    cat("Step", step, "- After initialization of variables\n")
    browser()  # Breakpoint 3: After initialization of variables

    if (i > 1) {
      irrel <- irrelevant(P$children[1:(i-1)], P$sumset[j], P$sumset, G.unobs)
      irl.len <- length(irrel)
      if (irl.len > 0) {
        i <- i - irl.len
        terms <- P$children[irrel]
        P$children[irrel] <- NULL
        vars <- vars[-irrel]
      }
    }

    step <- step + 1
    cat("Step", step, "- After removing irrelevant terms\n")
    browser()  # Breakpoint 4: After removing irrelevant terms

    M <- topo[!(topo %in% vars)]
    O <- topo[(topo %in% vars)]

    step <- step + 1
    cat("Step", step, "- After topological sorting\n")
    cat("M:", M, "\n")
    cat("O:", O, "\n")
    browser()  # Breakpoint 5: After topological sorting

    while (k <= i) {
      step <- step + 1
      cat("Step", step, "- Inside nested while loop before join operation\n")
      cat("P$children[[k]]$var:", P$children[[k]]$var, "\n")
      cat("P$children[[k]]$cond:", P$children[[k]]$cond, "\n")
      cat("P$sumset[j]:", P$sumset[j], "\n")
      browser()  # Breakpoint 6: Before join operation

      joint <- join(J, D, P$children[[k]]$var, P$children[[k]]$cond, P$sumset[j], M, O, G.unobs, G, G.obs, topo)

      cat("Step", step, "- Inside nested while loop after join operation\n")
      browser()  # Breakpoint 7: Inside the nested while loop after join operation

      if (length(joint[[1]]) <= length(J)) {
        J <- character()
        D <- character()
        k <- 1
        break
      } else {
        J <- joint[[1]]
        D <- joint[[2]]
        if (length(joint) > 2) {
          R.var <- union(R.var, joint[[3]])
          R.cond <- c(R.cond, list(joint[[4]]))
          M <- setdiff(M, R.var)
        } else {
          k <- k + 1
        }
      }
      step <- step + 1
      cat("Step", step, "- End of nested while loop iteration\n")
      browser()  # Breakpoint 8: End of nested while loop iteration
    }

    if (k == i + 1) {
      P <- factorize(J, D, P, topo, i)
      S <- P$sumset[j]
      P$sumset <- P$sumset[-j]
      if (length(R.var) > 0) {
        P.cancel <- cancel(P, R.var, R.cond, S)
        if (identical(P.cancel, P)) P <- P.orig
        else {
          P <- P.cancel
          j <- 0
        }
      } else j <- 0
      if (irl.len > 0) P$children <- c(terms, P$children)
    } else P <- P.orig

    step <- step + 1
    cat("Step", step, "- After potential simplification\n")
    browser()  # Breakpoint 9: After potential simplification
  }

  step <- step + 1
  cat("Step", step, "- Return statement\n")
  return(P)
}
