rm(list = ls())

## Librerie ####
library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(doSNOW)
library(purrr)
library(pso) # per inventario
library(vroom)


## Dati e funzioni ####
source('/home/ciandri/codice/funzioni_tesi.R')

folder = '/home/ciandri/risultati_df/df_lumpy4'
csv = list.files(folder, full.names = T)
df_prev = map_df(csv, vroom)
df_prev = df_prev[,-c(1:which(colnames(df_prev) == 'Serie')-1)]

df_prev = as.data.frame(df_prev)

df_prev = df_prev %>% mutate(across(5:ncol(df_prev), round))
test = df_prev[df_prev$h > 28,]
val = df_prev[df_prev$h<29,]
metodi = unique(df_prev$Metodo)
h = unique(val$h) # 28 gg del validation set
N = length(metodi)

length(unique(df_prev$Serie))
table(test$Metodo)
df_prev1 = df_prev
str(df_prev)


## pesi inventario ####
cl <- parallel::makeCluster(4, outfile = ' ') # Usa tutti i core meno uno
registerDoSNOW(cl)

clusterEvalQ(cl, {
  library(dplyr)
  library(purrr)
  library(pso)
})
sss = Sys.time()

pesi_iniziali <- rep(1 /N+1e-16,N) # +1e-16 serve perché sennò l'approssimazione numerica di R fà sì che non si rispetti la condizione ui %*% theta - ci >= 0

df_comb_brier1 = vroom('/home/ciandri/dati/comb_brier_lumpy158.csv')
df_comb_brier1 = as.data.frame(df_comb_brier1)
df_comb_brier1 = df_comb_brier1[, -c(1:(which(colnames(df_prev) == 'Serie')-1))]
series_to_process <- unique(val$Serie)

df_comb_inv_opt <- foreach(serie = series_to_process, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% {

  print(serie)
  dd_test = test[test[,'Serie'] == serie ,] # voglio minimizzare la somma del punteggio di Brier
  dd_tmp = val[val[,'Serie'] == serie,] # validation per quella serie
  
  df_comb_inv = df_comb_inv2 =df_comb_inv3 =  df_comb_brier1
  df_comb_inv[,5:ncol(df_comb_inv)] = df_comb_inv3[,5:ncol(df_comb_inv)] = df_comb_inv2[,5:ncol(df_comb_inv2)] = 0 # metto tutto a 0 così posso vedere se c'è qualcosa che non va
  df_comb_inv3$Serie =df_comb_inv$Serie =df_comb_inv2$Serie = serie
  df_comb_inv3$Combinazione = 'cost4-opt'  
  df_comb_inv$Combinazione = 'cost9-opt'
  df_comb_inv2$Combinazione = 'cost19-opt'
  df_comb_inv3$vendite =df_comb_inv$vendite = df_comb_inv2$vendite = dd_test$vendite[1:28]
  print(paste('check0:', serie))
  
  ## Calcolo F_hat da usare nella funzione da minimizzare
  F_hat = list()
  horiz = 1:28 # validation set
  if(max(1000, max(dd_tmp[,ncol(dd_tmp)])) == 1000){  
    ysum = 0:1000
  }else{
    ysum = (max(dd_tmp[,ncol(dd_tmp)])-2000):max(dd_tmp[,ncol(dd_tmp)])
  }
  for(orizzonte in horiz){ # per ogni orizzonte temporale
    F_met = list()
    
    ## Calcolo F_comb
    for(met in metodi){
      ripartizione = ecdf(t(dd_tmp)[5:ncol(dd_tmp), (dd_tmp$h == orizzonte & dd_tmp$Metodo==met)]) # funzione di ripartizione
      F_met[[met]] = ripartizione
    }
    F_hat[[orizzonte]] = sapply(F_met, function(x) x(ysum))
    rownames(F_hat[[orizzonte]]) = ysum
  }  
  
  print(paste('check1:', serie))
  # minimizzo la funzione
  pesi_ott1 = psoptim(pesi_iniziali,
                     f = function(pesi) w_inventario2(serie = serie, horiz = 1:28, distrib = F_hat, pesi = pesi, costo = c(1,9)),
                     lower = 1e-16,
                     upper = 1, control = list(maxit = 100, reltol = .01))
  
  pesi_ott = pesi_ott1$par / sum(pesi_ott1$par)
  pesi_ott9 = cbind(metodi, pesi_ott)
  
  print(paste('check2:', serie))

  print(paste('check3:', serie))

  pesi_ott2 = psoptim(pesi_iniziali,
                      f = function(pesi) w_inventario2(serie = serie, horiz = 1:28, distrib = F_hat, pesi = pesi, costo = c(1,19)),
                      lower = 1e-16,
                      upper = 1, control = list(maxit = 100,  reltol = .01))
  
  pesi_ott19 = pesi_ott2$par / sum(pesi_ott2$par)
  pesi_ott19 = cbind(metodi, pesi_ott19)

print(paste('check3:', serie))
  

  pesi_ott3 = psoptim(pesi_iniziali,
                      f = function(pesi) w_inventario2(serie = serie, horiz = 1:28, distrib = F_hat, pesi = pesi, costo = c(1,4)),
                      lower = 1e-16,
                      upper = 1, control = list(maxit = 100, reltol = .01))

  pesi_ott4 = pesi_ott3$par / sum(pesi_ott3$par)
  pesi_ott4 = cbind(metodi, pesi_ott4)

  quantili2 = (sapply(metodi, function(x) test[test$Metodo == x & test$Serie == serie, 5:ncol(test)] * as.numeric(pesi_ott19[pesi_ott19[,1] == x,2])))
  quantili1 = (sapply(metodi, function(x) test[test$Metodo == x & test$Serie == serie, 5:ncol(test)] * as.numeric(pesi_ott9[pesi_ott9[,1] == x,2])))

  quantili = (sapply(metodi, function(x) test[test$Metodo == x & test$Serie == serie, 5:ncol(test)] * as.numeric(pesi_ott4[pesi_ott4[,1] == x,2])))
  for(righe in rownames(quantili)){
    pp1 = rep(0,28)
    pp2 = rep(0,28)
    pp = rep(0, 28)
    for(met in metodi){
      pp =  pp+ quantili[righe, met][[1]]
      pp1 = pp1+ quantili1[righe, met][[1]]
      pp2 = pp2+ quantili2[righe, met][[1]]    
    }
    df_comb_inv [,righe] = round(pp1)
    df_comb_inv2[,righe] = round(pp2)
    df_comb_inv3[, righe] = round(pp)
  }
  # salvo il file
  file_path = paste0('/home/ciandri/risultati_df/df_inv_lumpy4/comb_inv_', serie, '.csv')
  output = rbind(df_comb_inv, df_comb_inv2,df_comb_inv3)
  write.csv(output, file_path) 
 return(output)
}
end = Sys.time()
stopCluster(cl)
end-sss
write.csv(df_comb_inv_opt, '/home/ciandri/risultati_df/comb_inv_lumpy41.csv')





