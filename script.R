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

## verificar modelo

prever <- function(matriz, x0) {
  return(which.max(matriz[x0,]))
}

dados2019_atual <- dados[dados$data >= "2019-04-01",]

intervalos.inpc.teste <- cut(dados2019_atual$inpc, breaks=c(quantile(dados1979_2019$inpc, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))

dados2021_atual <- dados[dados$data > "2021-01-01",] 

intervalos.ipca.teste <- cut(dados2021_atual$ipca_alimentacao, breaks=c(quantile(dados1992_2021$ipca_alimentacao, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))

prever.todos <- function(matriz, dados, x0) {
  previsao <- c()
  for(index in 1:length(dados)) {
    previsao[index] <- prever(matriz, x0)
    x0 <- dados[index]
  }
  return(previsao)
}

acuracia <- function(matriz, dados, x0) {
  previsao <- prever.todos(matriz, dados, x0)
  acertos <- previsao == dados
  return(sum(acertos)/length(previsao))
}

x0 <- intervalos.inpc[length(intervalos.inpc)]

previsao <- prever.todos(matriz.transicao.inpc, intervalos.inpc.teste, x0)
intervalos.inpc.teste
previsao == intervalos.inpc.teste
acertos <- previsao == intervalos.inpc.teste
acertos
sum(acertos)
sum(acertos)/length(previsao)

acuracia(matriz.transicao.inpc, intervalos.inpc.teste, x0)
x0 <- intervalos.ipca[length(intervalos.ipca)]
acuracia(matriz.transicao.ipca, intervalos.ipca.teste, x0)
