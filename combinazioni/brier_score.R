rm(list = ls())

## Librerie ####
library(dplyr)
library(foreach)
library(doParallel)
library(doSNOW)
library(purrr)
library(pso) # per inventario
library(vroom)
library(DescTools)

## Dati e funzioni ####
source('funzioni_tesi_pulite.R')
df_prev = vroom('previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo6.csv')
df_prev = vroom('previsioni_df/df_prev_OK/df_fare_brier_smooth.csv')
df_prev = df_prev[,-c(1:which(colnames(df_prev) == 'Serie')-1)]
df_prev_err = vroom('previsioni_df/df_prev_OK/df_prev_erratic_completo.csv')
df_prev_err = df_prev_err[,-1]
df_prev_lumpy = vroom('previsioni_df/df_prev_OK/df_prev_lumpy_completo.csv')
df_prev_smooth = vroom('previsioni_df/df_prev_OK/df_prev_smooth_completo.csv')
df_prev_smooth = df_prev_smooth[,-1]

df_prev= as.data.frame(df_prev)
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
table(table(test$Serie))
df_prev1 = df_prev
#df_prev = df_prev[which(!df_prev$Metodo %in% c('ARIMA', 'ETS')),]
str(df_prev)


## BRIER ####
df_comb1 = vroom('previsioni_df/df_prev_OK/df_prev_comb/prev_combinazioni_bozza.csv')
df_comb1 = as.data.frame(df_comb1)
unique(val$Serie)[grep('lumpy', unique(val$Serie))]
folder_path <- 'previsioni_df/df_prev_OK/df_prev_comb/brier_score'
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella

cl <- makeCluster(detectCores() -4, outfile = ' ') # Usa tutti i core meno uno
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

df_comb_brier1 = vroom('previsioni_df/df_prev_OK/df_prev_comb/brier_score/comb_brier_lumpy158.csv')
df_comb_brier1 = as.data.frame(df_comb_brier1)
df_comb_brier1 = df_comb_brier1[, -1]
series_to_process <- unique(val$Serie)[grep('lumpy', unique(val$Serie))]

## operazione iterativa per il calcolo dei pesi
df_comb_brier_opt <- foreach(serie = series_to_process, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% { #1:length(unique(df_prev$Serie))
  print(serie)
  dd_test = test[test[,'Serie'] == serie ,] # voglio minimizzare la somma del punteggio di Brier
  dd_tmp = val[val[,'Serie'] == serie,] # validation per quella serie
  
  df_comb_brier = df_comb_brier1
  df_comb_brier[,5:ncol(df_comb_brier)] = 0 # metto tutto a 0 così posso vedere se c'è qualcosa che non va
  df_comb_brier$Serie = serie
  df_comb_brier$vendite = dd_test$vendite[1:28]
  print(paste('check0:', serie))
  
  distribuzione = list() # lista di distribuzione per ogni orizzonte temporale
  unici = unique(unlist(dd_tmp[5:ncol(dd_tmp)])) # calcolo i valori unici e ci faccio una matrice
  for(orizzonte in 1:28){ # per ogni orizzonte temporale
    dd = dd_tmp[dd_tmp[,'h'] == orizzonte,]
    
    distrib = matrix(0, nrow = length(metodi), ncol = length(unici), dimnames = list(metodi, unici)) # metodi x valori unici dei quantili
    for(met in metodi){ # per ogni metodo
      tab = prop.table(table(t(dd[dd$Metodo == met, 5:ncol(dd)])))  # creo la distribuzione a partire dai quantili
      distrib[met, names(tab)] = tab
      #print(tab)
    }
    
    distribuzione[[orizzonte]] = distrib
  }
  
  
  result <- constrOptim(
    theta = initial_pesi,
    f = function(pesi) brier4(pesi, serie = serie, distribution = distribuzione),
    grad = NULL,  
    ui = ui,
    ci = ci
  )
  ottimizzati =cbind(serie, metodi,result$par/sum(result$par))
  print(paste('check1:', serie))
  
  #ottimizzati = rbind(ottimizzati, ottimizzati2)
  
  quantili = (sapply(metodi, function(x) dd_test[dd_test$Metodo == x, 5:ncol(dd_test)] * as.numeric(ottimizzati[ottimizzati[,2] == x,3])))
  
  for(righe in rownames(quantili)){ # righe = quantile
    pp = rep(0, 28)
    for(met in metodi){
      pp = pp + quantili[righe, met][[1]] # guardo un quantile per un metodo e lo sommo agli altri
    }
    df_comb_brier[, righe] = round(pp) # inserisco i risultati nel df
  }

  print(paste('check2:', serie))
  return(df_comb_brier)
}
end = Sys.time()
stopCluster(cl)
end-sss
write.csv(df_comb_brier_opt, 'previsioni_df/df_prev_OK/df_prev_comb/brier_drps/comb_brier_lumpy5.csv')

