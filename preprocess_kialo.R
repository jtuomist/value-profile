# This code is forked from Op_fi5925/koronakide on page [[Koronavirus]]

library(OpasnetUtils)
library(tidyverse)

# First version of data https://docs.google.com/spreadsheets/d/1Wzj_VqubkV6uomQS-St5UxzS5k25dDBs15DQFEsroOg/edit#gid=0

preprocess_arguments <- function(
    file, # Path and name of Kialo file
    relevants = NA, # vector of items for relevance arguments
    truth_prior = 0.3, # prior truth value without other information
    truth_prior_with_reference = 0.7, # prior truth value with credible reference
    sensitivity_prime = 0.3, # sensitivity defend value without other information
    sensitivity_prime_attack = -0.5 # sensitivity attack value without other information
) {
  print(file)
  dfl <- read_lines(file)
#  dfl <- strsplit(dfl, "\n")[[1]]  # FIXME Why is this?
  df_title <- gsub("Discussion Name: ", "", dfl[1])
  dfl <- dfl[-(1:2)]
  if(any(!grepl("^[0-9]", dfl))) {
    print(paste("Problematic page breaks near text ", dfl[!grepl("^[0-9]", dfl)]))
  }
  df <- data.frame(level = regexpr("\\. ",dfl))
  df$id = substr(dfl,1,df$level-1)
  df$interaction <- substr(dfl,df$level+2, df$level+4)  # FIXME What would be a better name than interaction?
  df$interaction <- ifelse(df$interaction %in% c("Pro","Con"), df$interaction, "Thesis")
  df$text = substr(dfl, ifelse(df$interaction=="Thesis",regexpr("\\. ",dfl), regexpr(":",dfl))+2,999)
  df$level <- nchar(gsub("[0-9]","", df$id))
#  FIXME This is needed for added arguments only. Not clear about the idea. Rethink.
#  added_argument <- unlist(lapply(strsplit(df$id,split="\\."), FUN=function(x) max(as.numeric(x))))
#  df$level <- df$level * ifelse(added_argument < 99, 1, ifelse(added_argument>999, 10, 0.1))
  df$target <- gsub("\\.[0-9]*$","", df$id)
  df$relevance <- ifelse(df$interaction=="Pro",sensitivity_prime, sensitivity_prime_attack)
  df$truth <- ifelse(grepl("http", df$text), truth_prior_with_reference, truth_prior)
  df$class <- "truth"  # Assume truth if not known that relevance
  df$class <- ifelse(df$level==0, "fact", df$class)  # Assume factual discussion
  if(!any(is.na(relevants))) df$class[df$id %in% relevants] <- "relevance"
  df$alias <- ifelse(grepl("^-> See", df$text), gsub("\\.$", "", substr(df$text,8,999)), "")
  df$alias[grepl("discussion",df$alias)] <- ""
  df$reference <- ""  # FIXME develop a good regex code to identify references from text.
  
  return(list(df_title, df))
}

out <- data.frame()
contexts <- c("covid", "climate")  # , "god")

for(context in contexts) {
  path <- paste0("data/", context, "/")
  files <- list.files(path)
  for(file in files) {
    l <- preprocess_arguments(
      file = paste0(path, file),
    #  relevants = relevants,
      truth_prior = 0.3,
      truth_prior_with_reference = 0.7,
      sensitivity_prime = 0.3,
      sensitivity_prime_attack = -0.5
    )
    out <- rbind(out,
                 data.frame(
                   context = context,
                   thesis = l[[1]],
                   l[[2]]
                 ))
  }
}

out$thesis_id <- as.numeric(as.factor(out$thesis))
out$id <- paste(out$thesis_id, out$id, sep=":")
out$target <- paste(out$thesis_id, out$target, sep=":")
out$alias <- ifelse(out$alias=="", "", paste(out$thesis_id, out$alias, sep=":"))

write_csv(out, "data/arguments.csv")
