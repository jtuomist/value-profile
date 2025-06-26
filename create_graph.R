# This code is forked from Op_fi5925/koronakide on page [[Koronavirus]]

library(tidyverse)
library(RCy3)

df <- read_csv("data/arguments.csv")

df <- infer_tree(df)

contexts <- c("climate")  # , "covid", "god")

for(context in contexts) {
  df2 = df[df$context==context,]
  nodes <- df2[c("context", "thesis", "level", "id", "text", "relevance", "truth", "class",
                "alias", "reference", "thesis_id" )]
  edges <- df2[df2$level>0 , c("id", "interaction", "target", "relevance", "class", "alias")]
  colnames(edges)[colnames(edges)=="id"] <- "source"
  
  createNetworkFromDataFrames(
    nodes = nodes,
    edges = edges,
    title = context,
    collection = "Contexts"
  )
}

# Formatting: http://cytoscape.org/RCy3/reference/mapVisualProperty.html

createVisualStyle(
  style.name = "argument",
  defaults = list(
    NODE_BORDER_PAINT="#C80000",
    NODE_FILL_COLOR="#C80000",
    NODE_LABEL_COLOR="#000000",
    NODE_LABEL_FONT_SIZE=3
  ),
  list(
    mapVisualProperty("node size", "truth", "c", c(0, 1), c(3, 100)),
    mapVisualProperty("node shape", "class", "d", c("fact", "value", "truth", "relevance"),
                      c("rectangle", "triangle", "ellipse", "diamond")),
    mapVisualProperty("node label", "text", "p"),
    mapVisualProperty("node tooltip", "text", "p"),
    mapVisualProperty("edge target arrow shape", "interaction", "d", c("Pro", "Con"),
                      c("delta", "T")),
    mapVisualProperty("edge target arrow unselected paint", "interaction", "d", c("Pro", "Con"),
                      c("green", "red")),
    mapVisualProperty("edge stroke unselected paint", "interaction", "d", c("Pro", "Con"),
                      c("green", "red")),
    mapVisualProperty("edge width", "relevance", "c", c(-1, 0, 1), c(8, 1, 8))
  )
)
setVisualStyle("argument")
