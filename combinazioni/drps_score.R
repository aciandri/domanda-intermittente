rm(list = ls())

## Librerie ####
library(dplyr)
library(foreach)
library(doParallel)
library(doSNOW)
library(purrr)
library(vroom)
library(DescTools)

## Dati e funzioni ####
source('funzioni_tesi.R')
df_prev_int = vroom('previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo6.csv')
df_prev_int = df_prev_int[,-1]
df_prev_err = vroom('previsioni_df/df_prev_OK/df_prev_erratic_completo.csv')
df_prev_err = df_prev_err[,-1]
df_prev_lumpy = vroom('previsioni_df/df_prev_OK/df_prev_lumpy_completo.csv')
df_prev_smooth = vroom('previsioni_df/df_prev_OK/df_prev_smooth_completo.csv')
df_prev_smooth = df_prev_smooth[,-1]

df_prev_int = as.data.frame(df_prev_int)
df_prev_err = as.data.frame(df_prev_err)
df_prev_lumpy = as.data.frame(df_prev_lumpy)
df_prev_smooth = as.data.frame(df_prev_smooth)

df_prev = rbind(df_prev_int, df_prev_lumpy, df_prev_smooth, df_prev_err) # senza erratiche che le ho già fatte per ora
df_prev = df_prev %>% mutate(across(5:ncol(df_prev), round))
test = df_prev[df_prev$h > 28,]
val = df_prev[df_prev$h<29,]
metodi = unique(df_prev$Metodo)
h = unique(val$h) # 28 gg del validation set
N = length(metodi)

length(unique(df_prev$Serie))
table(table(val$Serie))
df_prev1 = df_prev
str(df_prev)


## drps ####
df_comb1 = vroom('previsioni_df/df_prev_OK/df_prev_comb/prev_combinazioni_bozza.csv')
df_comb1 = as.data.frame(df_comb1)
unique(val$Serie)[grep('lumpy', unique(val$Serie))]
folder_path <- 'previsioni_df/df_prev_OK/df_prev_comb/drps_score'
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella

cl <- makeCluster(detectCores() - 3, outfile = ' ') # Usa tutti i core meno uno
registerDoSNOW(cl)


clusterEvalQ(cl, {
  library(dplyr)
  library(purrr)
  library(DescTools)
})
sss = Sys.time()


# Inizializza i pesi
initial_pesi <- rep(1 /N+1e-16,N) # +1e-16 serve perché sennò l'approssimazione numerica di R fà sì che non si rispetti la condizione ui %*% theta - ci >= 0

# Imposta i vincoli
# A matrix for the equality constraint Ax = b
A_eq <- matrix(1, nrow = 1, ncol =N)
b_eq <- 1

# Boundary conditions
ui <- rbind(diag(1,N), A_eq) # vincoli inferiori
ci <- c(rep(0,N), b_eq)

df_comb_brier1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_score/comb_brier_lumpy158.csv')
df_comb_brier1 = as.data.frame(df_comb_brier1)
df_comb_brier1 = df_comb_brier1[, -1]
series_to_process <- unique(val$Serie)

## operazione iterativa per il calcolo dei pesi
df_comb_drps_opt <- foreach(serie = series_to_process, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% { #1:length(unique(df_prev$Serie))
  print(serie)
  print(length(series_to_process)-which(series_to_process == serie))
  dd_test = test[test[,'Serie'] == serie ,] # test per quella serie
  dd_tmp = val[val[,'Serie'] == serie,] # validation per quella serie
  
  df_comb_drps = df_comb_brier1
  df_comb_drps$Combinazione = 'drps-opt'
  df_comb_drps$Serie = serie
  df_comb_drps$vendite = dd_test$vendite[1:28]
  df_comb_drps[,5:ncol(df_comb_drps)] = 0 # metto tutto a 0 così posso vedere se c'è qualcosa che non va
  print(paste('check0:', serie))
  
  ## Creo F_comb
  F_hat = list() # contiene le funzioni ECDF per ogni orizzonte per ogni metodo
  unici = unique(unlist(dd_tmp[5:ncol(dd_tmp)])) # calcolo i valori unici e ci faccio una matrice
  ysum <- 0:max(100, max(dd_tmp$vendite)) # somma infinita
  indicatrice = list() # lista dei risultati della funzione indicatrice
  
  for(orizzonte in 1:28){ # per ogni orizzonte temporale
    dd = dd_tmp[dd_tmp[,'h'] == orizzonte,]
    F_met = list() # contiene le funzioni ECDF per ogni metodo per un orizzonte temporale
    
    for(met in metodi){
      F_met[[met]] = ecdf(t(dd_tmp)[5:ncol(dd_tmp), (dd_tmp$h == orizzonte & dd_tmp$Metodo==met)])
    }
    F_hat[[orizzonte]] = sapply(F_met, function(x) x(ysum)) # prendo i valori (che si usano) di F_hat per ogni metodo
    indicatrice[[orizzonte]] <- ifelse(ysum >= dd_tmp$vendite[orizzonte], 1, 0) 
  }
  
  
  
  result <- constrOptim(
    theta = initial_pesi,
    f = function(pesi) drps2(pesi = pesi, serie = serie, distribuzione = F_hat, indicatrice),
    grad = NULL,  
    ui = ui,
    ci = ci
  )
  ottimizzati =cbind(serie, metodi,result$par/sum(result$par))
  print(paste('check1:', serie))
  
  # previsioni x pesi
  quantili = (sapply(metodi, function(x) dd_test[dd_test$Metodo == x, 5:ncol(dd_test)] * as.numeric(ottimizzati[ottimizzati[,2] == x,3]))) 
  
  #
  for(righe in rownames(quantili)){
    pp = rep(0, 28)
    for(met in metodi){
      pp =  pp + quantili[righe, met][[1]]
    }
    df_comb_drps[, righe] = round(pp)
  }

  print(paste('check2:', serie))
  return(df_comb_drps)
}
end = Sys.time()
stopCluster(cl)
end-sss

write.csv(df_comb_drps_opt, 'previsioni_df/df_prev_OK/df_prev_comb/brier_drps/comb_drps_int6.csv')

