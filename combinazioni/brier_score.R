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
source('~/Desktop/tesi_magistrale/pratica-tesi_mag/codiceR/funzioni_tesi_pulite.R')
df_prev = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo6.csv')
df_prev = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_fare_brier_smooth.csv')
df_prev = df_prev[,-c(1:which(colnames(df_prev) == 'Serie')-1)]
df_prev_err = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_erratic_completo.csv')
df_prev_err = df_prev_err[,-1]
df_prev_lumpy = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_lumpy_completo.csv')
df_prev_smooth = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_smooth_completo.csv')
df_prev_smooth = df_prev_smooth[,-1]

df_prev= as.data.frame(df_prev)
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

df_prev_int = df_prev_int[-which(df_prev_int$Serie == 'intermittent3494'),]# & df_prev_int$Metodo == 'GAM-QR(noco)'),]
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
df_comb1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/prev_combinazioni_bozza.csv')
df_comb1 = as.data.frame(df_comb1)
#da_fare = c("lumpy2513", "lumpy781" , "lumpy1485", "lumpy2515", "lumpy3145", "lumpy3724", "lumpy4233", "lumpy5286", "lumpy160",  "lumpy1486", "lumpy3725", "lumpy4234")
# fino a 22
unique(val$Serie)[grep('lumpy', unique(val$Serie))]
folder_path <- '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_score'
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
## guardo quali serie ho già fatto delle lumpy
#pp = sapply(sapply(strsplit(csv_files[grep('lumpy', csv_files)], '\\/'), function(x) x[11]), function(y) strsplit(y, split = '\\_'))
#which(unique(val$Serie)[grep('lumpy', unique(val$Serie))] %in% sapply(sapply(sapply(pp, function(x) x[3]), function(y) strsplit(y, split = '\\.')), function(z) z[1])  )

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

df_comb_brier1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_score/comb_brier_lumpy158.csv')
df_comb_brier1 = as.data.frame(df_comb_brier1)
df_comb_brier1 = df_comb_brier1[, -1]
series_to_process <- unique(val$Serie)#[grep('intermittent', unique(val$Serie))]#[132: 191]

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
  #file_path = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier/comb_brier_', serie, '.csv')
  #write.csv(df_comb_brier, file_path)
  return(df_comb_brier)
  # return(df_comb_brier)
}
end = Sys.time()
stopCluster(cl)
end-sss
write.csv(df_comb_brier_opt, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_drps/comb_brier_lumpy5.csv')
df_comb_brier_opt = vroom( '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_drps/comb_brier_int8.csv')

## Valutazione ####
## Leggo i file
folder_path_brier <- '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_score'
csv_files_brier <- list.files(folder_path_brier, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
#csv_files_drps = csv_files_drps[-c(grep('intermittent', csv_files_drps), grep('lumpy', csv_files_drps))]
combined_df_brier <- map_df(csv_files_brier, read.csv)
combined_df_brier = combined_df_brier[,-1]
df_prev = combined_df_brier

## Creo i pt

met = 'brier-opt'
i = 3
n = 1:length(unique(df_comb_brier_opt$Serie))
orizzonte = sort(rep(unique(h), 10)) # 28 x 10 estrazioni dalla distribuzione
jit = 1/nrow(df_comb_brier_opt)
semi = cbind(metodi, 1:length(metodi)) # per sto cazzo di seme sennò ties

pt = data.frame(Metodo = c(0), Serie = c(0), h = c(0), pt = c(0))
for(serie in unique(df_comb_brier_opt$Serie)){
  print(serie)
  p = data.frame(Metodo = met, Serie = serie, h = orizzonte, pt = rPIT(df_comb_brier_opt[df_comb_brier_opt$Serie == serie, 5:(ncol(df_comb_brier_opt)-1)], 
                                                                       df_comb_brier_opt$vendite[df_comb_brier_opt$Serie == serie], metodo = met, n = 10, jitter = jit, seed =n[i]))
  pt = rbind(pt, p)
  #file_path = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb/rpit-', met, '1/', serie, '_', met, '.csv')
  #write.csv(p, file_path)
}
pt = pt[-1,]
write.csv(pt, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit-int6/rpitcomb_lumpy3-brier.csv')

## Leggo i pt
safe_read_csv <- possibly(read.csv, otherwise = NULL)
#folder_path <- paste0('/rpit-', met) # individuali
folder_path <- paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit-int11/rpit-', met, '1') # comb

csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
da_fare = c()
for(serie in unique(val$Serie)){
 da_fare = c(da_fare, grep(paste0(serie,'_'),csv_files))
  }
combined_df_purrr <- map_df(csv_files[da_fare], function(file) {
  message("Reading file: ", file)
  safe_read_csv(file)
})

D = ks.test(pt$pt, 'punif')#, exact = T)

ggplot(pt, aes(x=pt, y = after_stat(density))) + geom_histogram(bins = 20, color = 'black', fill = NA) +
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
sharpn =sharp(met, df_comb_brier_opt[, 5:(ncol(df_comb_brier_opt)-1)], df_comb_brier_opt$vendite)

