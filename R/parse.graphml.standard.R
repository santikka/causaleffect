parse.graphml.standard <- function(file, nodes, use.names) {
  doc <- XML::xmlParse(file, useInternalNodes = TRUE)
  top <- XML::xmlRoot(doc)
  graph <- top[["graph"]]
  if (!use.names) {
    if (XML::xmlSize(which(XML::xmlSApply(graph, XML::xmlName) == "node")) != length(nodes)) stop("Incorrect number of node names")
  }
  ns <- c(ns = "http://graphml.graphdrawing.org/xmlns")
  edges <- which(XML::xmlSApply(graph, XML::xmlName) == "edge")
  remove.id <- 0
  removals <- list()
  for (i in 1:length(edges)) {
    current.edge <- graph[[edges[i]]]
    edge.attr <- XML::xmlAttrs(current.edge)
    datas <- which(XML::xmlSApply(current.edge, XML::xmlName) == "data")
    for (j in 1:length(datas)) {
      current.data <- current.edge[[datas[j]]]
      if (XML::xmlAttrs(current.data) == "d10") {
        arc <- current.data[[1]]
        arw.attr <- XML::xmlAttrs(arc[["Arrows"]])
        src <- edge.attr[["source"]]
        trgt <- edge.attr[["target"]]
        two.arrows <- !identical(arw.attr[["source"]], "none") & !identical(arw.attr[["target"]], "none")
        no.arrows <- identical(arw.attr[["source"]], "none") & identical(arw.attr[["target"]], "none")
        if (two.arrows | no.arrows) {
          remove.id <- remove.id + 1
          removals[[remove.id]] <- current.edge
          e1 <- XML::newXMLNode("edge", parent = doc, attrs = c(id = "e", source = src, target = trgt),
            XML::newXMLNode("data", attrs = c(key = "d9"), cdata = TRUE, "U" ))
          e2 <- XML::newXMLNode("edge", parent = doc, attrs = c(id = "e", source = trgt, target = src),
            XML::newXMLNode("data", attrs = c(key = "d9"), cdata = TRUE, "U" ))
          XML::addChildren(graph, kids = list(e1, e2))
        }
      }
    }
  }
  XML::removeNodes(removals)
  if (use.names) {
    node.data <- XML::getNodeSet(doc, "//ns:data[contains(@key,'d6')]/*", ns)
    for (i in 1:length(node.data)) nodes[i] <- XML::xmlValue(node.data[[i]]["NodeLabel"]$NodeLabel[1]$text)
  }
  all <- XML::getNodeSet(doc, "//*[position() > 1]", ns)
  keep <- XML::getNodeSet(doc, "//ns:edge | //ns:node | //ns:graph | //ns:key[@attr.name = 'description'] | //ns:data[contains(@key,'d9')]", ns)
  remove <- XML::getNodeSet(doc, "//ns:key[@for='port'] | //ns:key[@for='graphml'] | //ns:data[@key='d4'] | //ns:data[@key='d6'] | //ns:data[@key='d7'] | //ns:data[@key='d8'] | //ns:data[@key='d10']" , ns)
  XML::removeNodes(union(setdiff(all, keep), remove))
  temp <- tempfile(fileext = ".graphml")
  temp.xml <- XML::saveXML(doc, file = temp)
  XML::free(doc)
  igrph <- igraph::read_graph(temp.xml, format = "graphml")
  igrph <- igraph::set_vertex_attr(igrph, "name", value = nodes)
  return(igrph)
}
