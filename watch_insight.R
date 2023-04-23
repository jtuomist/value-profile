library(jsonlite)
library(ghql)
library(tidyr)
library(dplyr)
library(tidyjson)
library(tidyverse)
library(RCy3)
library(xml2)

# https://splashback.io/2021/08/using-graphql-with-r/

# Would be nice to get ancestor actions but it seems not work
# https://app.asana.com/0/1202880927552397/1204452759348401/f
query <- '
query @locale(lang:"fi") {
	plan(id:"greentransition") {
    name
    indicatorLevels {
      level
      indicator {
        name
        description
        relatedCauses {
          causalIndicator {name}
          effectType
          confidenceLevel
        }
        relatedEffects {
          effectIndicator {name}
          effectType
          confidenceLevel
        }
        relatedActions {
          action {
            name
            description
          }
          effectType
        }
      }
    }
  }
}'

con <- GraphqlClient$new(
  url = "https://api.watch.kausal.tech/v1/graphql/"
)
query1 <- Query$new()
query1$query('actionlist', query)
result <- con$exec(query1$queries$actionlist)
result1 <- result %>% as.tbl_json
result2 <- fromJSON(result, flatten = TRUE)[["data"]][["plan"]][["indicatorLevels"]]

# Remove the one indicator we don't want now

#remove <- c(
#  "Valtion on annettava suunniteltua enemmän rahaa sosiaali- ja terveydenhuoltoon, vaikka se tarkoittaisi säästöjä muualta, veronkorotuksia tai lisää velkaa.",
#  "P Vihreät Savo: lisätäänkö sote-rahoitusta?",
#  "Helsingissä ja muissa suurissa kaupungeissa on otettava käyttöön ruuhkamaksu",
#  "Turkistarhaus on kiellettävä Suomessa"
#)
result2 <- result2[result2$indicator.name != "P Vihreät Savo: lisätäänkö sote-rahoitusta?" , ]

nodes <- data.frame()
for (i in 1:nrow(result2)) {
  actions <- result2[i, 6][[1]]
  if(nrow(actions) > 0) {
    df <- data.frame(
      level = "Action",
      actions[2:3]
    )
    nodes <- rbind(nodes, df)
  }
}
colnames(nodes)[2:3] <- c("id", "description")

indicators <- result2[1:3]
colnames(indicators) <- c("level", "id", "description")

nodes <- rbind(nodes, indicators)
colnames(nodes)[colnames(nodes)=="level"] <- "class"
nodes <- nodes[!duplicated(nodes$id) , ]

edges <- data.frame()
for (i in 1:nrow(result2)) {
  parents <- result2[i, 4][[1]]
  if(nrow(parents) > 0) {
    df <- data.frame(
      source = parents[[3]],
      target = result2[i, 2],
      parents[1:2]
    )
    edges <- rbind(edges, df)
  }
}
for (i in 1:nrow(result2)) {
  children <- result2[i, 5][[1]]
  if(nrow(children) > 0) {
    df <- data.frame(
      source = result2[i, 2],
      target = children[[3]],
      children[1:2]
    )
    edges <- rbind(edges, df)
  }
}
for (i in 1:nrow(result2)) {
  actions <- result2[i, 6][[1]]
  if(nrow(actions) > 0) {
    df <- data.frame(
      source = actions[[2]],
      target = result2[i, 2],
      effectType = actions[[1]],
      confidenceLevel = "HIGH"
    )
    edges <- rbind(edges, df)
  }
}
colnames(edges)[colnames(edges)=="effectType"] <- "interaction"
edges <- edges[!duplicated(edges[c("source", "target")]), ]

# Count cumulative appearances

parties <- list(
  Vihr = c("Vihreät"),
  Kesk = c("Kepu"),
  Liik = c("Liike Nyt"),
  Sdp = c("Demarit", "SDP"),
  Kok = c("Kokoomus"),
  Ps = c("Perussuomalaiset"),
  Krist = c("Kristilliset"),
  Rkp = c("Ruotsalaiset"),
  Vas = c("Vasemmistoliitto")
)

actions <- list(
  HS.ymp.1 = "HS.ymp.1",
  HS.ymp.2 = "HS.ymp.2",
  HS.sote.1 = "sote-rahoitusta"
)

sites <- list(
  Uusimaa = "Uusimaa",
  Savo = "Savo",
  Helsinki = "Helsinki"
)

s <- s1 <- s2 <- ifelse(grepl("^P ", nodes$id), nodes$id, "")
for(i in 1:length(parties)) {
  for(name in parties[[i]]) {
    s = ifelse(grepl(name, s), names(parties)[i], s)
  }
}
nodes$party <- s

for(i in 1:length(actions)) {
  for(name in actions[[i]]) {
    s1 = ifelse(grepl(name, s1), names(actions)[i], s1)
  }
}
nodes$action <- s1

for(i in 1:length(sites)) {
  for(name in sites[[i]]) {
    s2 = ifelse(grepl(name, s2), names(sites)[i], s2)
  }
}
nodes$site <- s2

### Add contributions
# Add contributions to priorities
df <- nodes
df$contribution <- paste(df$action, df$party, df$site)

# Add contributions to actions and value profiles
get_tree_leaves <- function(tree, edges, df, direction = "down") {
  if(direction == "down") cols <- c("source", "target") else cols <- c("target", "source")
  for(i in 1:20) { # More than enough rounds; could develop more elegant approach.
    tree <- unique(c(tree, edges[edges[[cols[1]]] %in% tree , cols[2]]))
  }
  print(tree)
  out <- (df[df$id %in% tree[-1] , "contribution"])
  return(out[out != "  "])  # Remove non-priorities
}
actions <- nodes$id[nodes$class %in% c("Action", "TACTICAL")]
for(action in actions) {
  contribution <- get_tree_leaves(action, edges, df)
  print(action)
  print(nodes[nodes$id == action,])
  print(contribution)
  df <- rbind(df,
              data.frame(
                nodes[nodes$id == action, ],
                contribution = contribution,
                row.names = NULL
              )
            )
}

# Add contributions to other nodes
for(j in (1:nrow(nodes))[nodes$class == "OPERATIONAL"]) {
  if(nodes$description[j] == "") {
    print(paste("Solmussa", nodes$id[j], "täytyisi olla kuvaus."))
  } else {
    x <- read_html(nodes$description[j])
    print(x)
#    test <- xml_text(xml_find_all(x, "//h2")) == "Viitteet"
#    print(test)
#    xml_siblings(xml_find_all(x, "//h2"))[2]
    
    x2 <- xml_children(xml_find_first(x, "//body"))
    hypo <- 0
    viit <- 0
    for(i in 1:length(x2)) {
      if(xml_text(x2[i]) == "Hypoteesit") hypo <- i + 1
      if(xml_text(x2[i]) == "Viitteet") viit <- i + 1
    }
    if(viit > 0) {
      s <- xml_text(xml_children(x2[viit]))
      s <- gsub("\\([0-9]\\) ", "", s)
      s <- gsub("Perus", "Ps", s)
    } else {
      s = ""
    }
    df <- rbind(df,
                data.frame(
                  nodes[j , ],
                  contribution = s,
                  row.names = NULL
                )
    )
  }
}

df <- df[df$contribution != "" & df$contribution != "  " , ]
df <- df[order(df$contribution),]
df$contributor <- gsub("HS\\.ymp\\.[0-9] ", "", df$contribution)
# Make all columns have an empty cell to calculate duplicates correctly
df <- rbind(
  data.frame(class="", id="", description="", party="", action="", site="", contribution="", contributor=""),
  df
)

ggplot(df, aes(
  x=cumsum(!duplicated(contribution))-1,
  y=cumsum(!duplicated(ifelse(df$class=="OPERATIONAL",df$id,"")))-1,
  color="Nodes"))+geom_line(size = 1.0)+
  geom_line(size = 1.0, y=cumsum(!duplicated(df$contributor))-1, aes(color="Contributors"))+
  geom_line(size = 1.0, y=cumsum(!duplicated(ifelse(df$class=="Action",df$id,"")))-1, aes(color="Actions"))+
  geom_line(size = 1.0, y=cumsum(!duplicated(ifelse(df$class=="TACTICAL",df$id,"")))-1, aes(color="Value profiles"))+
  geom_line(size = 1.0, y=cumsum(!duplicated(ifelse(df$class=="STRATEGIC",df$id,"")))-1, aes(color="Priorities"))+
  geom_line(size = 1.0, y = cumsum(!duplicated(df$party))-1, aes(color="Parties"))+
  coord_cartesian(ylim = c(0,40))+
  labs(
    title = "Number of objects in the insight network",
    x = "Number of contributions",
    y = "Cumulative number"
  )

createNetworkFromDataFrames(
  nodes = nodes,
  edges = edges,
  title = "Green transition",
  collection = "Contexts"
)

# Formatting: http://cytoscape.org/RCy3/reference/mapVisualProperty.html

createVisualStyle(
  style.name = "insight",
  defaults = list(
    NODE_BORDER_PAINT="#C80000",
    NODE_FILL_COLOR="#C80000",
    NODE_LABEL_COLOR="#000000",
    NODE_LABEL_FONT_SIZE=3
  ),
  list(
    mapVisualProperty("node shape", "class", "d", c("Action", "OPERATIONAL", "TACTICAL", "STRATEGIC"),
                      c("rectangle", "triangle", "ellipse", "diamond")),
    mapVisualProperty("node label", "id", "p"),
    mapVisualProperty("node tooltip", "description", "p"),
    mapVisualProperty("edge target arrow shape", "interaction", "d", c("INCREASES", "DECREASES"),
                      c("delta", "T")),
    mapVisualProperty("edge target arrow unselected paint", "interaction", "d", c("INCREASES", "DEACREASES"),
                      c("green", "red")),
    mapVisualProperty("edge stroke unselected paint", "interaction", "d", c("INCREASES", "DECREASES"),
                      c("green", "red")),
    mapVisualProperty("edge width", "confidenceLevel", "d", c("HIGH", "MEDIUM", "LOW"), c(8, 4, 1))
  )
)
setVisualStyle("insight")
