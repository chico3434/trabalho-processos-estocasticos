library(readxl)
dados <- read_excel("INPC_IPCA.xlsx", 
                    col_types = c("date", "numeric", "numeric", "text", "text"))

colnames(dados) <-  c("data", "inpc", "ipca_alimentacao", "ipca_habitacao", "ipca_saude")

dados1979_2019 <- dados[dados$data < "2019-04-01",]

dados1992_2021 <- dados[(dados$data <= "2021-01-01") & (dados$data >= "1992-01-01"),] 

freq <- function(dados) {
  frequencia <- matrix(rep(0,4*4), nrow = 4, ncol = 4)
  for(index in 2:length(dados)) {
    frequencia[dados[index-1],dados[index]] = frequencia[dados[index-1],dados[index]] + 1 
  }
  return(frequencia)
}

matriz.transicao <- function(dados) {
  frequencia <- freq(dados)
  for(estado in as.numeric(attributes(dados)$levels)) {
    frequencia[estado,] <- frequencia[estado,]/sum(frequencia[estado,])  
  }
  return(frequencia)
}

intervalos.inpc <- cut(dados1979_2019$inpc, breaks=c(quantile(dados1979_2019$inpc, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))

intervalos.inpc[is.na(intervalos.inpc)] <- 1

matriz.transicao.inpc <- matriz.transicao(intervalos.inpc)

intervalos.ipca <- cut(dados1992_2021$ipca_alimentacao, breaks=c(quantile(dados1992_2021$ipca_alimentacao, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))

matriz.transicao.ipca <- matriz.transicao(intervalos.ipca)

matriz.transicao.inpc

matriz.transicao.ipca

