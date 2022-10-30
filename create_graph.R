# This code is forked from Op_fi5925/koronakide on page [[Koronavirus]]

library(tidyverse)
library(RCy3)

df <- read_csv("data/arguments.csv")

contexts <- c("climate", "covid", "god")

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
