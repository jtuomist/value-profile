preprocess_arguments <- function(
    file_path, # Name of zip file at Opasnet
    wiki = "opasnet_fi", # Wiki identifier
    file_name, # Name of file in the zip file
    relevants = NA, # vector of Items for relevance arguments
    addition = NA, # vector of arguments to be added to original list.
    truth_prior = 0.3, # prior truth value without other information
    truth_prior_with_reference = 0.7, # prior truth value with credible reference
    sensitivity_prime = 0.3, # sensitivity defend value without other information
    sensitivity_prime_attack = -0.5 # sensitivity attack value without other information
) {
  dfl <- opasnet.data(file_path, wiki, unzip=file_name)
  #  dfl <- read_lines("~/discussion/climate/what-is-the-best-measure-to-decrease-climate-change-31294.txt")
  dfl <- strsplit(dfl, "\n")[[1]]
  df_title <- gsub("Discussion Title: ", "", dfl[1])
  dfl <- dfl[-(1:2)]
  if(!any(is.na(addition))) dfl <- c(dfl, addition)
  df <- data.frame(level = regexpr("\\. ",dfl))
  df$Item = substr(dfl,1,df$level-1)
  df$class = substr(dfl,df$level+6,regexpr(":", dfl)-1)
  df$colour <- substr(dfl,df$level+2, df$level+4)
  df$colour <- ifelse(df$colour %in% c("Pro","Con"), df$colour, "Thesis")
  df$text = substr(dfl, ifelse(df$colour=="Thesis",regexpr("\\. ",dfl), regexpr(":",dfl))+2,999)
  df$level <- nchar(gsub("[0-9]","", df$Item))
  added_argument <- unlist(lapply(strsplit(df$Item,split="\\."), FUN=function(x) max(as.numeric(x))))
  df$level <- df$level * ifelse(added_argument < 99, 1, ifelse(added_argument>999, 10, 0.1))
  df$Object <- gsub("\\.[0-9]*$","", df$Item)
  df$relevance <- ifelse(df$colour=="Pro",sensitivity_prime, sensitivity_prime_attack)
  df$truth <- ifelse(grepl("http", df$text), truth_prior_with_reference, truth_prior)
  df$class <- ifelse(df$class=="", "truth", df$class)
  df$class <- ifelse(df$colour=="Thesis", "fact", df$class)
  if(!any(is.na(relevants))) df$class[df$Item %in% relevants] <- "relevance"
  df$alias <- ifelse(grepl("^-> See", df$text), gsub("\\.$", "", substr(df$text,8,999)), "")
  df$alias[grepl("discussion",df$alias)] <- ""
  
  return(list(df_title, df))
}

