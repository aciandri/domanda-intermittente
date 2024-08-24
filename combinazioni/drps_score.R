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
df_prev_int = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo6.csv')
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

df_prev_int = df_prev_int[-which(df_prev_int$Serie == 'intermittent3494'),]# & df_prev_int$Metodo == 'GAM-QR(noco)'),]
df_prev = rbind(df_prev_int, df_prev_lumpy, df_prev_smooth, df_prev_err) # senza erratiche che le ho già fatte per ora
#df_prev = df_prev[-which(df_prev$Metodo %in% c('ARIMA', 'ETS'))]
df_prev = df_prev %>% mutate(across(5:ncol(df_prev), round))
test = df_prev[df_prev$h > 28,]
val = df_prev[df_prev$h<29,]
metodi = unique(df_prev$Metodo)
h = unique(val$h) # 28 gg del validation set
N = length(metodi)

length(unique(df_prev$Serie))
table(table(val$Serie))
df_prev1 = df_prev
#df_prev = df_prev[which(!df_prev$Metodo %in% c('ARIMA', 'ETS')),]
str(df_prev)


## drps ####
df_comb1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/prev_combinazioni_bozza.csv')
df_comb1 = as.data.frame(df_comb1)
#da_fare = c("lumpy2513", "lumpy781" , "lumpy1485", "lumpy2515", "lumpy3145", "lumpy3724", "lumpy4233", "lumpy5286", "lumpy160",  "lumpy1486", "lumpy3725", "lumpy4234")
# fino a 22
unique(val$Serie)[grep('lumpy', unique(val$Serie))]
folder_path <- '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/drps_score'
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
## guardo quali serie ho già fatto delle lumpy
#pp = sapply(sapply(strsplit(csv_files[grep('lumpy', csv_files)], '\\/'), function(x) x[11]), function(y) strsplit(y, split = '\\_'))
#which(unique(val$Serie)[grep('lumpy', unique(val$Serie))] %in% sapply(sapply(sapply(pp, function(x) x[3]), function(y) strsplit(y, split = '\\.')), function(z) z[1])  )

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
series_to_process <- unique(val$Serie)#[1001:length(unique(val$Serie))]#[132: 191]

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
  #file_path = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/drps_score/comb_drps_', serie, '.csv')
  #write.csv(df_comb_drps, file_path)
  return(df_comb_drps)
}
end = Sys.time()
stopCluster(cl)
end-sss

df1 = df_comb_drps_opt[[1]]

for(el in 2:length(df_comb_drps_opt)){
  df1 = rbind(df1, df_comb_drps_opt[[el]])
}
write.csv(df_comb_drps_opt, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_drps/comb_drps_int6.csv')


## Valutazione ####
## Leggo i file
folder_path_drps <- '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/drps_score'
csv_files_drps <- list.files(folder_path_drps, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
#csv_files_drps = csv_files_drps[-grep('intermittent', csv_files_drps)]
combined_df_drps <- map_df(csv_files_drps, read.csv)
combined_df_drps = combined_df_drps[,-1]
df_prev = combined_df_drps

## Creo i pt
met = 'drps-opt'
i = 2
n = 1:length(unique(df_prev$Serie))
orizzonte = sort(rep(unique(h), 10)) # 28 x 10 estrazioni dalla distribuzione
jit = 1/nrow(df_prev)
semi = cbind(metodi, 1:length(metodi)) # per sto cazzo di seme sennò ties
pt = data.frame(Metodo = c(0), Serie = c(0), h = c(0), pt = c(0))
for(serie in unique(df_prev$Serie)){
  print(serie)
  p = data.frame(Metodo = met, Serie = serie, h = orizzonte, pt = rPIT(df_comb_drps_opt[df_comb_drps_opt$Serie == serie, 5:(ncol(df_comb_drps_opt)-1)], 
                                                         df_comb_drps_opt$vendite[df_comb_drps_opt$Serie == serie], metodo = met, n = 10, jitter = jit, seed =n[i]))
  #file_path = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb/rpit-', met, '1/', serie, '_', met, '.csv')
  #write.csv(p, file_path)
  pt = rbind(pt, p)
}
write.csv(pt, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit-int6/rpit_int6_drps.csv')
## Leggo i pt
safe_read_csv <- possibly(read.csv, otherwise = NULL)
#folder_path <- paste0('/rpit-', met) # individuali
folder_path <- paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit-int45/rpit-', met, '1') # comb

csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) # legge tutti i file della cartella
da_fare = c()
for(serie in unique(val$Serie)){
  da_fare = c(da_fare, grep(paste0(serie,'_'),csv_files))
}
combined_df_purrr <- map_df(csv_files, function(file) {
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
sharpn =sharp(met, df_comb_drps_opt[, 5:(ncol(df_comb_drps_opt)-1)], df_comb_drps_opt$vendite)

## costi
  costi =  fun.costi2(met, df_comb_drps_opt[, 5:(ncol(df_comb_drps_opt)-1)], df_comb_drps_opt$vendite)

  costi = as.data.frame(costi)
  colnames(costi) = c('Metodo', 'Cost(1,4)', 'Cost(1,9)', 'Cost(1,19)')
  costi <- costi %>%
    mutate(across(c(`Cost(1,4)`, `Cost(1,9)`, `Cost(1,19)`), as.numeric))
  print(xtable(costi), include.rownames = F)
  
  
  ## Trade off curves ####
  #curve = data.frame( Metodo = c(0), Livello = c(0), Investment= c(0), Lostsales_totali = c(0), CSL_deviation = c(0), Costo = c(0) )
  curve = rep(NA, 5)

  curve =  to_curves2(met, df_comb_drps_opt[ , 5:(ncol(df_comb_drps_opt)-1)], 
                      df_comb_drps_opt$vendite)

  curve
  #curve = curve[-1,]
  curve = as.data.frame(curve)
  colnames(curve) = c('Metodo', 'Livello', 'Investment', 'Lostsales', 'CSLdeviation')
  curve <- curve %>%
    mutate(across(colnames(curve)[3:ncol(curve)], as.numeric))
  curve$CSLdeviation = abs(curve$CSLdeviation)
  #curve = curve[c(7:19),]
  
  graf1 = ggplot(curve) + 
    geom_line(aes(x = Investment, y = CSLdeviation, group = Metodo, color = Metodo)) + 
    geom_point(aes(x = Investment, y = CSLdeviation, group = Metodo, color = Metodo)) +
    #  coord_cartesian(ylim = c(0, .13), xlim = c(0, 12))+
    ylab('CSL deviation')
  graf3 = ggplot(curve) + 
    geom_line(aes(x = log(Investment), y = log(Lostsales), group = Metodo, color = Metodo)) + 
    geom_point(aes(x = log(Investment), y = log(Lostsales), group = Metodo, color = Metodo)) 
  graf2 = ggplot(curve) + 
    geom_line(aes(x = Lostsales, y = CSLdeviation, group = Metodo, color = Metodo)) +
    geom_point(aes(x = Lostsales, y = CSLdeviation, group = Metodo, color = Metodo)) + 
    scale_x_reverse() +  
    ylab('CSL deviation')
  #gridExtra::grid.arrange(graf1,graf2, graf3, nrow = 1,common.legend = TRUE, legend="bottom")
  
  
  tikz("/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/risultati-bozza/trade_off3.tex", width = 6, height = 3)
  combined_plot <- (graf1 | graf2 | graf3) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
  print(combined_plot)
  dev.off()
  
  curve$Livello = as.numeric(curve$Livello)
  curve$CSL_ach = curve$CSLdeviation + curve$Livello
  graf4 = ggplot(curve) + 
    geom_line(aes(x = Livello, y = CSL_ach, group = Metodo, color = Metodo)) +
    geom_point(aes(x = Livello, y = CSL_ach, group = Metodo, color = Metodo)) +#+  scale_x_reverse()
    geom_abline(intercept = 0, slope = 1, col = 'grey', linetype = 'dashed') +
    coord_cartesian(ylim = c(0.6, 1), xlim = c(0.6,1))+
    xlab('Target CSL') + ylab ('Achieved CSL')+
  
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(0.8)),
          panel.background = element_rect(fill = "#EDEAEA", color = NA),  # Sfondo rosato
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          axis.text = element_text(color = "black", size = rel(0.6)),
          axis.title = element_text(color = "black", size = rel(0.7)),
    )
  










