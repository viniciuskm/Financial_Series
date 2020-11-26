library(quantmod)

importa_retorno <- function(nome){
  # COletanto os Bitcoins
  preco <- getSymbols(nome, auto.assign = F)
  
  # Coletando somente o pre?o adjustado
  preco_adj <- preco[,ncol(preco)]
  # preco_adj %>% plot()
  
  # Removendo NAs
  preco_adj <- preco_adj %>% na.locf()
  
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

datas_comuns_5 <- function(serie1,serie2,serie3,serie4,serie5){
  
  # buscando datas duas séries
  serie1_idx <- serie1 %>% index() 
  serie2_idx <- serie2 %>% index()
  serie3_idx <- serie3 %>% index() 
  serie4_idx <- serie4 %>% index()
  serie5_idx <- serie5 %>% index() 
  # buscando datas que não existem na outra série
  # ibov_idx[ibov_idx %ni% petr_idx]
  # petr_idx[petr_idx %ni% ibov_idx]
  
  # Filtrando somente pelas datas que existem nas duas séries
  serie1_filtered <- serie1[serie1_idx %in% serie2_idx]
  serie1_idx <- serie1_filtered %>% index() 
  serie1_filtered <- serie1_filtered[serie1_idx %in% serie3_idx]
  serie1_idx <- serie1_filtered %>% index() 
  serie1_filtered <- serie1_filtered[serie1_idx %in% serie4_idx]
  serie1_idx <- serie1_filtered %>% index() 
  serie1_filtered <- serie1_filtered[serie1_idx %in% serie5_idx]
  # serie2_filtered <- serie2[serie2_idx %in% serie1_idx]
  
  return(serie1_filtered)
  # return(list(serie1_filtered,serie2_filtered))
}

datas_N_comuns_5 <- function(serie1,serie2,serie3,serie4,serie5){
  
  # buscando datas duas séries
  serie1_idx <- serie1 %>% index() 
  serie2_idx <- serie2 %>% index()
  serie3_idx <- serie3 %>% index() 
  serie4_idx <- serie4 %>% index()
  serie5_idx <- serie5 %>% index() 
  # buscando datas que não existem na outra série
  # ibov_idx[ibov_idx %ni% petr_idx]
  # petr_idx[petr_idx %ni% ibov_idx]
  
  # Filtrando somente pelas datas que existem nas duas séries
  serie1_2_idx <- serie1_idx %ni% serie2_idx
  serie1_3_idx <- serie1_idx %ni% serie3_idx
  serie1_4_idx <- serie1_idx %ni% serie4_idx
  serie1_5_idx <- serie1_idx %ni% serie5_idx
  
  serie1_2_3 <- union(serie1_2_idx,serie1_3_idx)
  serie1_2_3_4 <- union(serie1_2_3,serie1_4_idx)
  serie1_2_3_4_5 <- union(serie1_2_3_4,serie1_5_idx)
  serie1_2_3_4_5 <- serie1_2_3_4_5 %>% unique()
  
  return(serie1_2_3_4_5)
  # return(list(serie1_filtered,serie2_filtered))
}
