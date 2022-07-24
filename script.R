library(readxl)
dados <- read_excel("INPC_IPCA.xlsx", 
                    col_types = c("date", "numeric", "numeric", "text", "text"))

colnames(dados) <-  c("data", "inpc", "ipca_alimentacao", "ipca_habitacao", "ipca_saude")

matriz.freq <- function(dados) {
  numero.estados <- length(attributes(dados)$levels)
  frequencia <- matrix(rep(0,numero.estados*numero.estados), nrow = numero.estados, ncol = numero.estados)
  for(index in 2:length(dados)) {
    frequencia[dados[index-1],dados[index]] = frequencia[dados[index-1],dados[index]] + 1 
  }
  return(frequencia)
}

matriz.transicao <- function(dados) {
  frequencia <- matriz.freq(dados)
  for(estado in as.numeric(attributes(dados)$levels)) {
    frequencia[estado,] <- frequencia[estado,]/sum(frequencia[estado,])  
  }
  return(frequencia)
}

prever <- function(matriz, x0) {
  return(which.max(matriz[x0,]))
}

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

inpc.quartis.analise <- function(dados) {
  dados1979_2019 <- dados[dados$data < "2019-04-01",]
  
  intervalos.inpc <- cut(dados1979_2019$inpc, breaks=c(quantile(dados1979_2019$inpc, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))
  intervalos.inpc[is.na(intervalos.inpc)] <- 1
  
  matriz.transicao.inpc <- matriz.transicao(intervalos.inpc)
  
  print('Matriz de transição')
  print(matriz.transicao.inpc)
  
  dados2019_atual <- dados[dados$data >= "2019-04-01",]
  
  intervalos.inpc.teste <- cut(dados2019_atual$inpc, breaks=c(quantile(dados1979_2019$inpc, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))
  
  x0 <- intervalos.inpc[length(intervalos.inpc)]
  
  previsao <- prever.todos(matriz.transicao.inpc, intervalos.inpc.teste, x0)
  
  acuracia(matriz.transicao.inpc, intervalos.inpc.teste, x0)
}

ipca.quartis.analise <- function(dados) {
  dados1992_2021 <- dados[(dados$data <= "2021-01-01") & (dados$data >= "1992-01-01"),] 
  dados2021_atual <- dados[dados$data > "2021-01-01",] 
  
  intervalos.ipca <- cut(dados1992_2021$ipca_alimentacao, breaks=c(quantile(dados1992_2021$ipca_alimentacao, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))
  
  matriz.transicao.ipca <- matriz.transicao(intervalos.ipca)
  
  print('Matriz de transição')
  print(matriz.transicao.ipca)
  
  intervalos.ipca.teste <- cut(dados2021_atual$ipca_alimentacao, breaks=c(quantile(dados1992_2021$ipca_alimentacao, probs=seq(0,1, by=0.25))), labels=c(1,2,3,4))
  
  x0 <- intervalos.ipca[length(intervalos.ipca)]
  
  acuracia(matriz.transicao.ipca, intervalos.ipca.teste, x0)
}