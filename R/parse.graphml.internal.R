parse.graphml.internal <- function(file, nodes, use.names) {
  doc <- XML::xmlParse(file, useInternalNodes = TRUE)
  top <- XML::xmlRoot(doc)
  graph <- top[["graph"]]
  if (!use.names) {
    if (XML::xmlSize(which(XML::xmlSApply(graph, XML::xmlName) == "node")) != length(nodes)) stop("Incorrect number of node names")
  }
  ns <- c(ns = "http://graphml.graphdrawing.org/xmlns")
  if (use.names) {
    node.data <- XML::getNodeSet(doc, "//ns:data[contains(@key,'d6')]/*", ns)
    for (i in 1:length(node.data)) nodes[i] <- XML::xmlValue(node.data[[i]]["NodeLabel"]$NodeLabel[1]$text)
  }
  all <- XML::getNodeSet(doc, "//*[position() > 1]", ns)
  keep <- XML::getNodeSet(doc, "//ns:edge | //ns:node | //ns:graph | //ns:key[@attr.name = 'description'] | //ns:data[contains(@key,'d9')]", ns)
  remove <- XML::getNodeSet(doc, "//ns:key[@for='port'] | //ns:key[@for='graphml'] | //ns:data[@key='d4'] | //ns:data[@key='d6'] 
 | //ns:data[@key='d7'] | //ns:data[@key='d8'] | //ns:data[@key='d10']" , ns)
  XML::removeNodes(union(setdiff(all, keep), remove))
  temp <- tempfile(fileext = ".graphml")
  temp.xml <- XML::saveXML(doc, file = temp)
  XML::free(doc)
  igrph <- igraph::read.graph(temp.xml, format = "graphml")
  igrph <- igraph::set.vertex.attribute(igrph, "name", value = nodes)
  return(igrph)
}
