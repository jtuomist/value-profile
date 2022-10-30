# This is code is forked from Op_en2382/update on page [[Discussion]]

update_truth <- function(
    pa, # P(A). This is updated to P(A|B).
    pb, # P(B)
    relb # parameter rel(B) for producing se = P(B|A)
) {
  if(any(pa>=1) | any(pa<=0)) stop("probability P(A) must be between ]0,1[, not ",pa)
  if(any(pb>=1) | any(pb<=0)) stop("probability P(B) must be between ]0,1[, not", pb)
  if(any(relb <= -1) | any(relb >=1)) stop("relb must be between ]-1,1[, not", relb)
  if(relb>=0) {
    se <- pb + relb * (pmin(1,pb/pa) - pb)
  } else {
    se <- pb + relb * pb
  }
  pab <- pa * se / pb
  return(pab)#list(pab=pab,se=se))
}

update_relevance <- function(
    relb, # relevance parameter prior for argument B: rel(B), ]-1,1[. This is updated.
    pci, # vector of probabilities P(C_i)
    relci # vector of relevance parameters for parent arguments C_i: rel(C_i), ]-1,1[
) {
  if(any(relb <= -1) | any(relb >=1)) stop("relb must be between ]-1,1[, not", relb)
  if(any(pci>=1) | any(pci<=0)) stop("probability P(C) must be between ]0,1[, not", pci)
  if(any(relci <= -1) | any(relci >=1)) stop("rel(C_i) must be between ]-1,1[, not", relci)
  relb_sign <- sign(relb)
  out <- abs(relb)
  relci_weighted <- relci * pci
  for(weight in rev(sort(relci_weighted))) {
    if(weight>=0) {
      out <- out + weight*(1-out)
    } else {
      out <- out + weight*out
    }
  }
  out <- out * relb_sign
  return(out)
}

infer_tree <- function(df, verbose=FALSE) {
  args_by_level <- df$Item[order(-df$level)]
  columns_inherited_from_alias <- c("text","truth")
  for (arg in args_by_level) {
    if(length(df$Item[df$Item==arg])!=1) warning("Argument ",arg," is not unique.")
    parents_truth <- df$Item[df$Object == arg & df$class=="truth"]
    parents_relevance <- df$Item[df$Object == arg & df$class=="relevance"]
    alias <- df$alias[df$Item==arg]
    if(alias!="") {
      df[df$Item==arg,columns_inherited_from_alias] <- df[df$Item==alias,columns_inherited_from_alias] 
    }
    for(parent in parents_truth) {
      pa = df$truth[df$Item == arg]
      pb = df$truth[df$Item == parent]
      relb = df$relevance[df$Item == parent]
      out <- update_truth(pa, pb, relb)
      if(verbose) print(c("truth",arg, parent, signif(c(pa,pb,relb, out),3)))
      df$truth[df$Item == arg] <- out
    }
    if(length(parents_relevance)>0) {
      relb = df$relevance[df$Item == arg]
      pci = df$truth[df$Item %in% parents_relevance]
      relci = df$relevance[df$Item %in% parents_relevance]
      out <- update_relevance(relb, pci, relci)
      if(verbose) print(c("relevance",arg, parent, signif(c(relb, pci, relci, out),3)))
      df$relevance[df$Item == arg] <- out
    }
  }
  return(df)
}
