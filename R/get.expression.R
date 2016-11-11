get.expression <-
function(x) {
  P <- ""
  s.print <- length(x$sumset) > 0
  sum.string <- ""
  cond.string <- ""
  if (s.print) {
    sum.string <- paste(x$sumset, sep = "", collapse = ",")
    P <- paste(P, "\\left(\\sum_{", sum.string, "}", sep = "", collapse = "")
  }
  if (x$fraction) { 
    P <- paste(P, "\\frac{", get.expression(x$num), "}{", get.expression(x$den), "}", sep = "", collapse = "")
  }
  if (x$sum) {
    P <- paste(P, "\\left(", sep = "", collapse = "")
    add.strings <- c()
    for (i in 1:length(x$children)) {
      add.strings[i] <- paste(c("w_{", i, "}^{(", x$weight, ")}", get.expression(x$children[[i]])), sep = "", collapse = "")
    }
    add.strings <- paste(add.strings, sep = "", collapse = " + ") 
    P <- paste(P, add.strings, "\\right)", sep = "", collapse = "")
  }
  if (x$product) {
    for (i in 1:length(x$children)) P <- paste(P, get.expression(x$children[[i]]), sep = "", collapse = "")
  } 
  if (!(x$sum || x$product || x$fraction)) {
    P <- paste(P, "P", sep = "", collapse = "")
    if (length(x$do) > 0) {
      do.string <- paste(x$do, sep = "", collapse = ",")
      P <- paste(P, "_{", do.string, "}" , sep = "", collapse = "")
    }
    var.string <- paste(x$var, sep = "", collapse = ",")
    if (x$domain > 0) {
      if (x$domain == 1) P <- paste(P, "^*(", var.string, sep = "", collapse = "")
      else P <- paste(P, "^{(", letters[x$domain - 1], ")}(", var.string, sep = "", collapse = "")
    } else {
      P <- paste(P, "(", var.string, sep = "", collapse = "")   
    }
    if (length(x$cond) > 0) {
      cond.string <- paste(x$cond, sep = "", collapse = ",")   
      cond.string <- paste("\u007C", cond.string, ")", sep = "", collapse = "") 
    }
    else cond.string <- ")"
    P <- paste(P, cond.string, sep = "")
  }
  if (s.print) P <- paste(P, "\\right)", sep = "", collapse = ",")
  return (P)
}
