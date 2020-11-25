library(quantmod)

importa_retorno <- function(nome){
  # COletanto os Bitcoins
  preco <- getSymbols(nome, auto.assign = F,)
  
  # Removendo NAs
  preco <- preco %>% na.omit()
  
  # Coletando somente o pre?o adjustado
  preco_adj <- preco[,ncol(preco)]
  # preco_adj %>% plot()
  
  # Calculando o log do retorno
  ret <- diff(log(preco_adj), lag = 1)
  ret <- ret %>% na.omit()
  
  
  return(list(preco_adj,ret))
}



"%ni%" <- Negate("%in%")


datas_comuns <- function(serie1, serie2){
  
  # buscando datas duas séries
  serie1_idx <- serie1 %>% index() 
  serie2_idx <- serie2 %>% index()
  # buscando datas que não existem na outra série
  # ibov_idx[ibov_idx %ni% petr_idx]
  # petr_idx[petr_idx %ni% ibov_idx]
  
  # Filtrando somente pelas datas que existem nas duas séries
  serie1_filtered <- serie1[serie1_idx %in% serie2_idx]
  # serie2_filtered <- serie2[serie2_idx %in% serie1_idx]
  
  return(serie1_filtered)
  # return(list(serie1_filtered,serie2_filtered))
}
