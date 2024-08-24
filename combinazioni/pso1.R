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
library(DescTools)

## Dati e funzioni ####
source('~/Desktop/tesi_magistrale/pratica-tesi_mag/codiceR/funzioni_tesi.R')
df_prev_int = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo3.csv')
df_prev_int = df_prev_int[,-1]
df_prev_err = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_erratic_completo.csv')
df_prev_err = df_prev_err[,-1]
df_prev_lumpy = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_lumpy_completo.csv')
df_prev_smooth = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_smooth_completo.csv')
df_prev_smooth = df_prev_smooth[,-1]

df_prev_int = as.data.frame(df_prev_int)
df_prev_err = as.data.frame(df_prev_err)
df_prev_lumpy = as.data.frame(df_prev_lumpy)
df_prev_smooth = as.data.frame(df_prev_smooth)



#prendo un sample delle serie per mantenere la proporzione corretta
length(unique(df_prev_err$Serie)) # 15
length(unique(df_prev_lumpy$Serie)) # 326
length(unique(df_prev_smooth$Serie)) # 50
length(unique(df_prev_int$Serie)) # 1219

set.seed(1)
camp = sample(unique(df_prev_lumpy$Serie), 318)
df_prev_lumpy = df_prev_lumpy[df_prev_lumpy$Serie %in% camp,]

set.seed(1)
camp = sample(unique(df_prev_smooth$Serie), 26)
df_prev_smooth = df_prev_smooth[df_prev_smooth$Serie %in% camp,]

df_prev_int = df_prev_int[-which(df_prev_int$Serie == 'intermittent3494'),]
df_prev = rbind(df_prev_int, df_prev_lumpy, df_prev_smooth, df_prev_err)
#df_prev = df_prev[-which(df_prev$Metodo %in% c('ARIMA', 'ETS'))]
df_prev = df_prev %>% mutate(across(5:ncol(df_prev), round))
test = df_prev[df_prev$h > 28,]
val = df_prev[df_prev$h<29,]
metodi = unique(df_prev$Metodo)
h = unique(val$h) # 28 gg del validation set
N = length(metodi)

length(unique(df_prev$Serie))
table(test$Metodo)
df_prev1 = df_prev
#df_prev = df_prev[which(!df_prev$Metodo %in% c('ARIMA', 'ETS')),]
str(df_prev)


## pesi inventario ####
cl <- makeCluster(detectCores() - 5, outfile = ' ') # Usa tutti i core meno uno
registerDoSNOW(cl)

clusterEvalQ(cl, {
  library(dplyr)
  library(purrr)
  library(pso)
})
sss = Sys.time()

pesi_iniziali <- rep(1 /N+1e-16,N) # +1e-16 serve perché sennò l'approssimazione numerica di R fà sì che non si rispetti la condizione ui %*% theta - ci >= 0

df_comb_brier1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_score/comb_brier_lumpy158.csv')
df_comb_brier1 = as.data.frame(df_comb_brier1)
df_comb_brier1 = df_comb_brier1[, -1]
series_to_process <- unique(val$Serie)#[grep('lumpy',unique(val$Serie))]#[1203:length(unique(val$Serie)[grep('intermittent',unique(val$Serie))])]#[132: 191]

## PER DEFINIRE QUALE COMBINAZIONE MANDARE
combinazione = c(1,4)
costi = paste0('cost', combinazione[2] ,'-opt')
livello = combinazione[2]/sum(combinazione)

df_comb_inv_opt <- foreach(serie = series_to_process, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% {

  print(serie)
  dd_test = test[test[,'Serie'] == serie ,] # voglio minimizzare la somma del punteggio di Brier
  dd_tmp = val[val[,'Serie'] == serie,] # validation per quella serie
  
  df_comb_inv = df_comb_brier1
  df_comb_inv[,5:ncol(df_comb_inv)] = 0 # metto tutto a 0 così posso vedere se c'è qualcosa che non va
  df_comb_inv$Serie = serie
  df_comb_inv$Combinazione = costi
  df_comb_inv$vendite = dd_test$vendite[1:28]
  print(paste('check0:', serie))
  
  ## Calcolo F_hat da usare nella funzione da minimizzare
  F_hat = list()
  #unici = unique(unlist(dd_tmp[5:ncol(dd_tmp)])) # calcolo i valori unici e ci faccio una matrice
  horiz = 1:28 # validation set
  ysum = 0:1000
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
  pesi_ott = psoptim(pesi_iniziali,
                     f = function(pesi) w_inventario2(serie = serie, horiz = 1:28, distrib = F_hat, pesi = pesi, costo = combinazione),
                     lower = 1e-16,
                     upper = 1)
  
  pesi_ott = pesi_ott$par / sum(pesi_ott$par)
  pesi_ott = cbind(metodi, pesi_ott)
  
  print(paste('check2:', serie))
  # pesi * quantili
  quantili = (sapply(metodi, function(x) test[test$Metodo == x & test$Serie == serie, 5:ncol(test)] * as.numeric(pesi_ott[pesi_ott[,1] == x,2])))
  for(righe in rownames(quantili)){
    pp = rep(0, 28)
    for(met in metodi){
      pp =  pp+ quantili[righe, met][[1]]
    }
    df_comb_inv[, righe] = round(pp)
  }  
  print(paste('check3:', serie))
  
  
  # salvo il file
  file_path = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/',costi,'/comb_inv4_', serie, '.csv')
  write.csv(df_comb_inv, file_path)
  return(df_comb_inv)
}
end = Sys.time()
stopCluster(cl)
end-sss


## Valutazione ####
## Leggo i file
folder_path_pso <- '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/cost19-opt'
csv_files_pso <- list.files(folder_path_pso, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
#csv_files_drps = csv_files_drps[-c(grep('intermittent', csv_files_drps), grep('lumpy', csv_files_drps))]
combined_df_pso <- map_df(csv_files_pso, read.csv)
combined_df_pso = combined_df_pso[,-1]
df_prev = combined_df_pso

## Creo i pt
met = 'cost19-opt'
i = 1
n = 1:length(unique(df_prev$Serie))
orizzonte = sort(rep(unique(h), 10)) # 28 x 10 estrazioni dalla distribuzione
jit = 1/nrow(df_prev)
semi = cbind(metodi, 1:length(metodi)) # per sto cazzo di seme sennò ties

for(serie in unique(df_prev$Serie)){
  print(serie)
  p = data.frame(h = orizzonte, pt = rPIT(df_prev[df_prev$Serie == serie, 5:(ncol(df_prev)-1)], 
                                          df_prev$vendite[df_prev$Serie == serie], metodo = met, n = 10, jitter = jit, seed =n[i]))
  file_path = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb/rpit-', met, '/', serie, '_', met, '.csv')
  write.csv(p, file_path)
}

## Leggo i pt
safe_read_csv <- possibly(read.csv, otherwise = NULL)
#folder_path <- paste0('/rpit-', met) # individuali
folder_path <- paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb/rpit-', met) # comb

csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
combined_df_purrr <- map_df(csv_files, function(file) {
  message("Reading file: ", file)
  safe_read_csv(file)
})

D = ks.test(combined_df_purrr$pt, 'punif')#, exact = T)

ggplot(combined_df_purrr, aes(x=pt, y = after_stat(density))) + geom_histogram(bins = 20, color = 'black', fill = NA) +
  geom_hline(yintercept = 1, color = 'blue', linetype = 'dashed', size = .6) +
  xlab('rPIT') + ylab('Densità')+
  labs(title=paste0(met, ': D = ', round(D$statistic, 4))) + 
  coord_cartesian(ylim = c(0, 2.6)) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = rel(0.8)),
        panel.background = element_rect(fill = "#EDEAEA", color = NA),  # Sfondo rosato
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        axis.text = element_text(color = "black", size = rel(0.6)),
        axis.title = element_text(color = "black", size = rel(0.7)),
  )


## Sharpness
sharpn =sharp(met, df_prev[, 5:(ncol(df_prev)-1)], df_prev$vendite)

