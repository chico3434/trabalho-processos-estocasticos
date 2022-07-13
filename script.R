library(readxl)

dados <- read_excel("INPC_IPCA.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "text", "text"))

colnames(dados) <-  c("data", "inpc", "ipca_alimentacao", "ipca_habitacao", "ipca_saude")

intervalos.inpc <- cut(dados$inpc, breaks=c(quantile(dados$inpc, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))

intervalos.inpc[is.na(intervalos.inpc)] <- 1

freq <- function(dados) {
  frequencia <- matrix(rep(0,4*4), nrow = 4, ncol = 4)
  for(index in 2:length(dados)) {
    frequencia[dados[index-1],dados[index]] = frequencia[dados[index-1],dados[index]] + 1 
  }
  return(frequencia)
}

matriz.transicao <- function(dados) {
  frequencia <- freq(intervalos.inpc)
  for(estado in as.numeric(attributes(dados)$levels)) {
    frequencia[,estado] <- frequencia[,estado]/sum(frequencia[,estado])  
  }
  return(frequencia)
}

m <- matriz.transicao(intervalos.inpc)

