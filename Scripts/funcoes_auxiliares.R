# Match names
match_names <- function(nome1, nome2) {
  nome1_split <- strsplit(nome1, " ")
  nome2_split <- strsplit(nome2, " ")
  n <- length(nome1_split)
  matches <- match(nome1, nome2)
  
  for (i in 1:n) {
    if (is.na(matches[i])) {
      prev <- which(sapply(lapply(nome2_split, "%in%", nome1_split[[i]]), 
                   function(x){x[1] & (sum(x) > 1)}))
      matches[i] <- ifelse(length(prev) == 0, NA, prev)
    }
  }
  matches
}

