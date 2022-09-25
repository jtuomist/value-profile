### UPDATING ARGUMENT TRUTHS AND RELEVANCES

# objects.latest("Op_en2382", code_name="update") # [[Discussion]] update_truth, update_relevance, infer_tree

infer_tree <- function (df, verbose = FALSE) {
  args_by_level <- df$id[order(-df$level)]
  cols <- c("text", "truth")  # Columns inherited from alias
  for (arg in args_by_level) {
    if (length(df$id[df$id == arg]) != 1) 
      warning("Argument ", arg, " is not unique.")
    parents_truth <- df$id[df$object == arg & df$class == "truth"]
    parents_relevance <- df$id[df$object == arg & df$class == "relevance"]
    alias <- df$alias[df$id == arg]
    if (!is.na(alias)) {if(alias != "") {
      df[df$id == arg, cols] <- df[df$id == alias, cols]
    }}
    for (parent in parents_truth) {
      pa = df$truth[df$id == arg]
      pb = df$truth[df$id == parent]
      relb = df$relevance[df$id == parent]
      out <- update_truth(pa, pb, relb)
      if (verbose) 
        print(c("truth", arg, parent, signif(c(pa, pb, relb, out), 3)))
      df$truth[df$id == arg] <- out
    }
    if (length(parents_relevance) > 0) {
      relb = df$relevance[df$id == arg]
      pci = df$truth[df$id %in% parents_relevance]
      relci = df$relevance[df$id %in% parents_relevance]
      out <- update_relevance(relb, pci, relci)
      if (verbose) 
        print(c("relevance", arg, parent, signif(c(relb, pci, relci, out), 3)))
      df$relevance[df$id == arg] <- out
    }
  }
  return(df)
}

update_truth <- function (pa, pb, relb) 
{
  if (any(pa >= 1) | any(pa <= 0)) 
    stop("probability P(A) must be between ]0,1[, not ", 
         pa)
  if (any(pb >= 1) | any(pb <= 0)) 
    stop("probability P(B) must be between ]0,1[, not", 
         pb)
  if (any(relb <= -1) | any(relb >= 1)) 
    stop("relb must be between ]-1,1[, not", relb)
  if (relb >= 0) {
    se <- pb + relb * (pmin(1, pb/pa) - pb)
  }
  else {
    se <- pb + relb * pb
  }
  pab <- pa * se/pb
  return(pab)
}

update_relevance <- function (relb, pci, relci) 
{
  if (any(relb <= -1) | any(relb >= 1)) 
    stop("relb must be between ]-1,1[, not", relb)
  if (any(pci >= 1) | any(pci <= 0)) 
    stop("probability P(C) must be between ]0,1[, not", 
         pci)
  if (any(relci <= -1) | any(relci >= 1)) 
    stop("rel(C_i) must be between ]-1,1[, not", relci)
  relb_sign <- sign(relb)
  out <- abs(relb)
  relci_weighted <- relci * pci
  for (weight in rev(sort(relci_weighted))) {
    if (weight >= 0) {
      out <- out + weight * (1 - out)
    }
    else {
      out <- out + weight * out
    }
  }
  out <- out * relb_sign
  return(out)
}

####### PREPARE GRAPHS

prepare_graph <- function(
    df,
    drop_gray = TRUE, # Drop gray branches?
    drop_higher_levels = 0, # Drop higher levels (0: drop nothing)?
    RELEVANCE_LIMIT = 0.2, # value below which argument is considered irrelevant and dropped from graph
    TRUTH_LIMIT = 0.05, # value below which argument is considered untrue and dropped from graph
    verbose=FALSE
) {
  thesis <- df$colour=="Thesis"
  df$edge.penwidth <- abs(df$relevance*15)
  df$node.width <- ifelse(thesis,1,df$truth)
  df$node.fontsize <- ifelse(thesis,15,df$truth*20)
  df$node.color <- ifelse(df$class=="truth","orange","blue")
  df$label <- substr(df$text, 1,30)
  df$label <- paste(df$label, ifelse(thesis, signif(df$truth,2),""))
  df$rel <- ifelse(toupper(df$colour)=="PRO","relevant defense","relevant attack")
  df$rel <- paste0(ifelse(abs(df$relevance) < RELEVANCE_LIMIT, "ir", ""), df$rel)
  df$type <- "argument"
  df$type <- ifelse(df$class %in% c("value","fact"), paste(df$class,"opening statement"), df$type)
  df$description <- df$text
  drop <- df$id[(df$truth<TRUTH_LIMIT | abs(df$relevance)<RELEVANCE_LIMIT) & df$level>0]
  if(verbose) print(drop)
  out <- character()
  tmp <- drop
  for(i in sort(na.omit(drop))) {
    branch <- tmp[grep(paste0("^",i), tmp)]
    if(length(branch)>0) {
      if(verbose) print(branch)
      out <- c(out, branch[1])
      tmp <- tmp[!tmp %in% branch[-1]]
    }
  }
  drop <- out
  if(verbose) print(drop)
  df$node.fillcolor <- "white"
  for(i in drop) {
    df$node.fillcolor = ifelse(grepl(paste0("^",i), df$id), "gray", df$node.fillcolor)
  }
  if(drop_gray) df <- df[df$node.fillcolor!="gray" | df$id %in% drop ,]
  if(drop_higher_levels>0) df <- df[df$level<=drop_higher_levels | df$level>10,]
  return(df)
}


# This is code makegraph3.R on github.com/jtuomist/corona/.
# It is a fork from Op_en3861/makeGraph2 on page [[Insight network]]
# It is an update to work with cytoscape rather than with DiagrammeR.

library(OpasnetUtils)

#' Making insight network graph object
#' 
#' makeGraph is a function for taking an insight ovariable and making a graph object.
#'
#' @param a is data.frame or ovariable defining nodes and edges with at least columns: Oldid, type, item, label, Relation, object, description. Other columns for nodes such as URL are allowed.
#' @return two data.frames: nodes_df and edges_df that are directly given as parameters for DiagrammeR::create_graph.

makeGraph <- function(ova, formatting=data.frame(), ...) {
  require(OpasnetUtils)
  require(RCy3)
  
  if(FALSE) { # Maybe not needed  
    if(!exists("formatted")) formatted <- data.frame()
    if(nrow(formatted)==0) {
      objects.latest("Op_en3861", code_name="formatted") # [[Insight network]] formatted
    }
    if(!exists("chooseGr")) {
      objects.latest("Op_en3861", code_name="chooseGr") # [[Insight network]] chooseGr
    }
  }
  
  if("ovariable" %in% class(ova)) {
    a <- ova@output
    meta <- ova@meta$insightnetwork
  } else {
    a <- ova
    meta <- NULL
  }
  a$truth <- signif(a$truth,2)
  a$relevance <- signif(a$relevance,2)
  for(i in 1:ncol(a)) {
    a[[i]] <- gsub("[\"']", " ", a[[i]])
  }
  
  # Fill in missing labels, items, and object nodes
  
  a$label <- ifelse(is.na(a$label),substr(a$item,1,30), a$label)
  a$item  <- ifelse(is.na(a$item),a$label, a$item)
  
  # Find nrow that matches the object based on id.
  tst <- rep(1:nrow(a),2)[match(a$object, c(a$id, a$label))]
  
  # Use item as object identifier when possible
  hasobj <- !(is.na(a$object) | a$object=="") # Rows of data.frame a that have object
  a$object[hasobj] <- a$item[tst][hasobj]
  
  # Find objects that have not been defined
  newbies  <- ifelse(is.na(tst), a$object,NA)
  newbies <- newbies[!is.na(newbies)]
  
  if(length(newbies)>0) {
    a <- orbind(
      a,
      data.frame(
        item=newbies,
        label=substr(newbies,1,30),
        stringsAsFactors = FALSE
      )
    )
  }
  
  nodes <- a[!(duplicated(a$item) | is.na(a$item) | a$item==""),]
  #  nodes$tooltip <- paste0(
  #    nodes$label, ". ",
  #    ifelse(nodes$label == nodes$item, "", paste0(nodes$item, ". ")), 
  #    ifelse(is.na(nodes$description), "", paste0("\n", nodes$description)),
  #    " (", nodes$Context, "/", nodes$id,")", 
  #  )
  nodes$tooltip <- paste0(
    nodes$item, ". ", nodes$description, "/ truth: ", nodes$truth, " relevance: ", nodes$relevance)
  #nodes <- merge(nodes, formatted[setdiff(colnames(formatted),colnames(nodes))],
  #               by.x="type", by.y="Resource")
  #colnames(nodes) <- gsub("node.","",colnames(nodes))
  nodes <- nodes[!grepl("edge.", colnames(nodes))]
  nodes$id <- 1:nrow(nodes)
  
  # Create edges
  edges <- a[!(is.na(a$object) | a$object=="") , ]
  
  # Flip unpreferred relations to their inverse relations
  
  if(FALSE) {  # Disable flipping for now
    inver <- opbase.data("Op_en7783", subset="Relation types")
    for(i in colnames(inver)) inver[[i]] <- as.character(inver[[i]])
    inve <- data.frame(
      rel = c(inver$`English name`,inver$`Finnish name`),
      inve = c(inver$`English inverse`,inver$`Finnish inverse`),
      stringsAsFactors = FALSE
    )
    
    #  flip <- edges$rel %in% inve$inve
    #  tmp <- edges$item
    #  edges$item[flip] <- edges$object[flip]
    #  edges$object[flip] <- tmp[flip]
    #  edges$rel[flip] <- inve$rel[match(edges$rel, inve$inve)][flip]
  }
  #  edges$from <- match(edges$item, nodes$item)
  #  edges$to <- match(edges$object, nodes$item)
  edges$label <- edges$rel
  #  edges <- merge(edges, formatted[setdiff(colnames(formatted),colnames(edges))],
  #                 by.x="rel", by.y="Resource", all.x = TRUE)
  # colnames(edges) <- gsub("edge.","",colnames(edges))
  edges <- edges[!grepl("node.", colnames(edges))]
  edges$id <- 1:nrow(edges)
  edges$labeltooltip <- paste0(edges$label, " (",edges$context, "/",edges$id, ")")
  
  #  colnames(nodes)[colnames(nodes)=="item"] <- "id"
  colnames(nodes)[colnames(nodes)=="type"] <- "group"
  colnames(nodes)[colnames(nodes)=="truth"] <- "score"
  colnames(edges)[colnames(edges)=="id"] <- "source"
  colnames(edges)[colnames(edges)=="object"] <- "target"
  colnames(edges)[colnames(edges)=="rel"] <- "interaction"
  colnames(edges)[colnames(edges)=="label"] <- "name"
  colnames(edges)[colnames(edges)=="relevance"] <- "weight"
  
  #  nodes <- nodes[c("id","group","score")]
  #  edges <- edges[c("source","target","interaction","weight")]
  
  gr <- list(
    nodes_df=nodes,
    edges_df=edges
  )
  
  return(gr) 
}

