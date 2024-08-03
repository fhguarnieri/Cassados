
# Função para 'arrumar' nome dos municípios

remove_acento <- function(vec, Toupper=F) {
  require("stringr")
  vec <- tolower(vec)
  vec <- gsub('á', 'a', vec) 
  vec <- gsub('à', 'a', vec)
  vec <- gsub('â', 'a', vec)
  vec <- gsub('ã', 'a', vec)
  vec <- gsub('é', 'e', vec) 
  vec <- gsub('ê', 'e', vec)
  vec <- gsub('í', 'i', vec)
  vec <- gsub('ó', 'o', vec) 
  vec <- gsub('ô', 'o', vec)
  vec <- gsub('õ', 'o', vec)
  vec <- gsub('ő', 'o', vec)
  vec <- gsub('ú', 'u', vec)
  vec <- gsub('ç', 'c', vec)
  vec <- gsub('ă', 'a', vec)
  vec <- gsub('ę', 'e', vec)
  vec <- gsub("http\\w+", " ", vec)
  vec = gsub("[ \t]{2,}", " ", vec)
  vec = gsub("^\\s+|\\s+$", " ", vec)
  #vec = str_replace_all(vec, "[[:punct:]]", "")
  #vec = str_replace_all(vec, "[^[:alnum:]]", " ")
  vec = str_trim(vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}

# função para agregar municípios

mudaMun <- function(x){
  novMun <- rep(NA,nrow(x))
  for(i in 1:nrow(x)) novMun[which(x$Mun == om[i,1])] <- om[i,2]
  novMun <- ifelse(is.na(novMun),x$Mun,novMun)
  x$novMun <- novMun
  return(x)
}

# Função para calcular QL

criaQL <- function(x){
  vm <- tapply(x$NumVotos, x$novMun, sum)
  vd <- tapply(x$NumVotos, x$Candidato, sum)
  x$vd <- vd[x$Candidato]
  x$vm <- vm[x$novMun]
  x$vt <- sum(vm)
  x$QL <- (x$NumVotos/x$vd)/(x$vm/x$vt)
  return(x)
}

# Função para preparar tabelsa de transição

fazfs <- function(x){
  require(tidyverse)
  novPart <- x$Partido
  novPart <- ifelse(x$Cassado == 1, "CAS",novPart)
  x$novPart <- novPart
  f <- x %>% group_by(novPart,novMun) %>% summarise(vt = sum(NumVotos))
  f <- f %>% pivot_wider(names_from = novPart, values_from = vt, values_fill = 0)
  f <- f[,-1]
  f <- f %>%
    rowwise() %>%
    mutate(row_sum = sum(c_across(everything()))) %>%
    mutate(across(everything(), ~ . / row_sum)) %>%
    select(-row_sum)
}

