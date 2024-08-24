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
library(tikzDevice)

## Dati e funzioni ####
source('~/Desktop/tesi_magistrale/pratica-tesi_mag/codiceR/funzioni_tesi_pulite.R')
df_prev = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int13.csv')
df5 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/altri_finali/df_prev_lumpy5.csv')
df_prev= df_prev[,-c(1)]
#write.csv(df_prev, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo11.csv')

#df_prev = df_prev[df_prev$Serie %in% setdiff(unique(df_prev$Serie), unique(df_prev1$Serie)),]
df_prev = as.data.frame(df_prev)

length(unique(df_prev$Serie)) 

df_prev = df_prev %>% mutate(across(5:ncol(df_prev), round))
test = df_prev[df_prev$h > 28,]
val = df_prev[df_prev$h<29,]
metodi = unique(df_prev$Metodo)
h = unique(val$h) # 28 gg del validation set
N = length(metodi)

table(df_prev$Serie)[(table(df_prev$Serie) > 784)]
table(table(test$Serie))

nomi = names(table(df_prev$Serie)[(table(df_prev$Serie) > 784)])
df_prev1 = df_prev[!df_prev$Serie %in% nomi,]
for(el in nomi){
  df_prev1 = rbind(df_prev1, df_prev[df_prev$Serie== el,][1:784,])
}
write.csv(df_prev1, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/altri_finali/df_prev_lumpy2.csv')
## Leggo quelle già fatte
#df_comb = read.csv( '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/semplici/df_comb_semplici_int2.csv')
str(df_comb)
table(df_comb$Combinazione)

## SEMPLICI ####
### SA ####

## Media
Combinazione = 'round_SA'
df_comb_round = cbind(Combinazione, as.data.frame(test %>%
                                                    group_by(Serie, h) %>%
                                                    summarise(across(colnames(test)[4:ncol(test)], ~ round(mean(.)), .names = "{col}"))))

str(df_comb_round)
table(table(df_comb_round$h, df_comb_round$Serie) == 1)

## Mediana
Combinazione = 'Mediana'
df_comb_median = cbind(Combinazione, as.data.frame(test %>%
                                                     group_by(Serie, h) %>%
                                                     summarise(across(colnames(test)[4:ncol(test)], median))))
df_comb1 = rbind(df_comb_round, df_comb_median)
table(table(df_comb1$Combinazione))

write.csv( df_comb1,'/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/semplici/df_comb_semplici_int13.csv') ## fatto!

df_comb1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/semplici/df_comb_semplici_int12.csv')
### LOG ####

## creo df con i punteggi log
df_comb_brier1 = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/brier_score/comb_brier_lumpy158.csv')
df_comb_brier1 = as.data.frame(df_comb_brier1)
df_comb_brier1 = df_comb_brier1[, -c(1:(which(colnames(df_comb_brier1) == 'Combinazione')-1))]
series_to_process <- unique(val$Serie)
df1 = df_comb_brier1[1,]

cl <- makeCluster(detectCores() - 4, outfile = ' ') # Usa tutti i core meno uno
registerDoSNOW(cl)

sss = Sys.time()
df_comb_log <- foreach(serie = series_to_process, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% { #1:length(unique(df_prev$Serie))
#for(serie in unique(val$Serie)){  
print(length(series_to_process)- which(series_to_process == serie))
print(serie)
  dd_test = test[test[,'Serie'] == serie ,] # voglio minimizzare la somma del punteggio di Brier
  dd_tmp = val[val[,'Serie'] == serie,] # validation per quella serie
  
  df_comb_brier = df_comb_brier1
  df_comb_brier[,5:ncol(df_comb_brier)] = 0 # metto tutto a 0 così posso vedere se c'è qualcosa che non va
  df_comb_brier$Serie = serie
  df_comb_brier$vendite = dd_test$vendite[1:28]
  df_comb_brier$Combinazione = 'log'
  print(paste('check0:', serie))

  pesi = log_score_comb2 (serie = serie,
                          df_previsto = dd_tmp, h =28,
                          #distribuzione = distribuzione,
                          metodi = unique(val$Metodo))
  quantili = (sapply(unique(val$Metodo), function(x) dd_test[dd_test$Metodo == x, 5:ncol(dd_test)] * as.numeric(pesi[x])))
  
  for(righe in rownames(quantili)){ # righe = quantile
    pp = rep(0, 28)
    for(met in unique(val$Metodo)){
      pp = pp + quantili[righe, met][[1]] # guardo un quantile per un metodo e lo sommo agli altri
    }
    df_comb_brier[, righe] = round(pp) # inserisco i risultati nel df
  }
  #df1 = rbind(df1, df_comb_brier)
  return (df_comb_brier)
}
end = Sys.time()
stopCluster(cl)
end -sss
table(table(df_comb1$Combinazione))
df1 = df1[-1,]

df_comb1 = rbind(df_comb1, df_comb_log)
write.csv( df_comb1,'/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/semplici/df_comb_semplici_int13.csv') ## fatto!


pt_log = rPIT(df1[ , 5:(ncol(df1)-1)], y = df1$vendite,metodo = 'log')
D_log = ks.test(pt_log, 'punif')#, exact = T)
print(D_log$statistic) #0.04739824

grafico1 = ggplot(as.data.frame(pt_log), aes(x=pt_log, y = after_stat(density))) + geom_histogram(bins = 20, color = 'black') +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 1, color = 'navyblue', linetype = 'dashed', linewidth = .6) +
  
  xlab('rPIT') + ylab('Densità')+
  labs(title=paste0( 'Log su 15 erratiche: D = ', round(D_log$statistic, 4))) + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/risultati-bozza/15erratiche/rPIT-log_semplice.png", plot = grafico1, width = 12, height = 8)


sharpn = sharp('Log', df1[, 5:(ncol(df1)-1)], df1$vendite, sharpn)

df_comb = rbind(df_comb_round, df_comb_median, df1)
str(df_comb)
table(df_comb$Combinazione)
table(df_comb$Serie)


#write.csv(df_comb, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/previsioni_comb_erratiche.csv', row.names = FALSE)


# LOG & CL OTTIM ####

cl <- makeCluster(detectCores() - 4, outfile = ' ') # Usa tutti i core meno uno
registerDoSNOW(cl)

sss = Sys.time()
threshold = .001
metodi = unique(val$Metodo)
h = unique(val$h)
pesi_init =  rep(1/length(metodi), length(metodi)) 


df_comb_log_cl_opt <- foreach(serie = unique(df_prev$Serie), .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% {
  #for(serie in unique(df_prev$Serie)[151:length(unique(df_prev$Serie))]){
  print(length(unique(df_prev$Serie))-which(unique(df_prev$Serie) == serie))
  print(serie)
  dd = val[val$Serie == serie,]
  dd_test = test[test$Serie == serie,]
  
  P_matrix_log = matrix(NA, nrow = length(h), ncol = length(metodi), dimnames = list(h, metodi)) # P_{h x N}
  P_matrix_cl = matrix(NA, nrow = length(h), ncol = length(metodi), dimnames = list(h, metodi)) # contiene gli elementi per la formula (f_i)
  t_df = t(dd[,  5:ncol(df_prev)])
  area_totale = 0:100
  
  
  
  for(i in h){
    for(met in metodi){
      print(met)
      riga = which(dd$Metodo == met & dd$h == i) # quale riga del df stiamo guardando
      distrib = prop.table(table(t_df[,riga])) # distribuzione di probabilità in base ai quantili
      
      P_matrix_log [i, met] = ifelse( dd$vendite[i] %in% names(distrib), distrib[names(distrib) == dd$vendite[i]], 0)  # f(y) oppure 0 se y non è in distrib
      
      ## Per CL
      distrib_cum = ecdf(t_df[,riga])# definisco l'area di interesse A_t come y_t > 90% quantile
      area_totale =  as.numeric(unique(names(distrib)))#seq(0:max((as.numeric(names(distrib)))))
      A_t = area_totale[(which(sapply(area_totale, function(x)distrib_cum(x)) > .9))]# definisco l'area di interesse A_t come y_t > 90% quantile
      y_t = dd$vendite[i] 
      
      P_matrix_cl [i, met] =  ifelse( y_t %in% A_t, ifelse(y_t %in% names(distrib), distrib[names(distrib) == y_t], 0), 
                                      ifelse(y_t %in% names(distrib), distrib_cum(A_t[1]-1) , (1e-16)))
      print(P_matrix_cl [i, met])
      #print(ifelse( y_t %in% A_t, distrib[names(distrib) == y_t], 
                  #  ifelse(y_t %in% names(distrib), distrib_cum(min(A_t)-1) , (1e-16))))
      # se y_t nell'area di interesse => f_i ; altrimenti se non è nella distribuzione => 0 , sennò prende l'area sotto la curva nell'area non di interesse (integrale nel discreto = somma)
    }
    
  }
  
  ## Calcolo i pesi LOG
  w_k = pesi_init # inizializzo i pesi come SA
  iterazione = 1
  flag = F
  while (!flag ){
    print(paste('iter log:', iterazione))
    w_1 = w_k
    pt1_cost = w_1 / length(h) 
    denominatori_log = sapply(h, function(x) ifelse(sum(P_matrix_log[x,]) == 0, 1e-16, sum(P_matrix_log[x,]*w_1) )) # denominatore della funzione di aggiornamento (uno per ogni h)
    
    w_k = sapply(1:length(metodi), function(x) 
      pt1_cost[x] *sum(sapply(h, function(y)  P_matrix_log[y, x] /denominatori_log[y])))
    
    flag = all(w_k-w_1 <= threshold)
    iterazione = iterazione +1
    
  } 
  w_k_log = w_k
  names(w_k_log) = metodi
  prev_pesate_log = data.frame(t(sapply(metodi, function(x) w_k_log[x] * dd_test[dd_test$Metodo == x, 5:ncol(dd)]))) 
  # dataframe dove ogni colonna è un quantile e ogni riga contiene una lista delle previsioni di un metodo
  
  combinate_log = (data.frame(sapply(colnames(prev_pesate_log), function(x) round(rowSums(data.frame(prev_pesate_log[,x]))))) )
  # per ogni quantile, faccio la somma delle previsioni pesate per ogni punto nel tempo
  
  ## Calcolo i pesi CL
  w_k =pesi_init # inizializzo i pesi come SA
  iterazione = 1
  flag = F
  while (!flag ){
    print(paste('iter cl:', iterazione))
    w_1 = w_k
    pt1_cost = w_1 / length(h) 
    denominatori_cl = sapply(h, function(x) ifelse(sum(P_matrix_cl[x,]) == 0, .001, sum(P_matrix_cl[x,]*w_1) )) # denominatore della funzione di aggiornamento (uno per ogni h)
    
    w_k = sapply(1:length(metodi), function(x) 
      pt1_cost[x] *sum(sapply(h, function(y)  P_matrix_cl[y, x] /denominatori_cl[y])))
    
    flag = all(w_k-w_1 <= threshold)
    iterazione = iterazione +1
    
  } 
  w_k_cl = w_k
  names(w_k_cl) = metodi
  
  prev_pesate_cl = data.frame(t(sapply(metodi, function(x) w_k_cl[x] * dd_test[dd_test$Metodo == x, 5:ncol(dd)]))) 
  # dataframe dove ogni colonna è un quantile e ogni riga contiene una lista delle previsioni di un metodo
  
  combinate_cl = (data.frame(sapply(colnames(prev_pesate_cl), function(x) round(rowSums(data.frame(prev_pesate_cl[,x]))))) )
  
  ## output
  output_log = cbind(data.frame(Combinazione = 'log-opt', Serie = serie, h = 1:28, vendite = dd_test$vendite[1:28]), combinate_log)
  output_cl = cbind(data.frame(Combinazione = 'cl-opt', Serie = serie, h = 1:28, vendite = dd_test$vendite[1:28]), combinate_cl)
  output = rbind(output_log, output_cl)
  #df_comb_log_cl_opt = rbind(df_comb_log_cl_opt, output)
  return(output)
}
end = Sys.time()
stopCluster(cl)
end-sss


length(unique(df_comb_log_cl_opt$Serie))
length(unique(df_comb_log_cl_opt$Combinazione))
table(table(df_comb_log_cl_opt$h))

#df_comb = df_comb[-which(df_comb$Combinazione == 'log-opt'),]
df_comb1 = rbind(df_comb1, df_comb_log_cl_opt)
length(unique(df_comb$Serie))
length(unique(df_comb$Combinazione))
write.csv(df_comb_log_cl_opt, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_comb/semplici/df_comb_log_cl_int13.csv', row.names = FALSE)

### Valutazione 

#####
pt_log_opt = rPIT(df_comb_log_cl_opt [ df_comb_log_cl_opt$Combinazione == 'cl-opt', 5:(ncol(df_comb_log_cl_opt)-1)], y = df_comb_log_cl_opt$vendite,metodo ='cl-opt')
D_log = ks.test(pt_log_opt, 'punif')#, exact = T)
write.csv(pt_log_opt, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit-int45/rpit_cl-opt_int45.csv', row.names = FALSE)



grafico1 = ggplot(as.data.frame(pt_log_opt), aes(x=pt_log_opt, y = after_stat(density))) + geom_histogram(bins = 20, color = 'black') +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 1, color = 'navyblue', linetype = 'dashed', linewidth = .6) +
  
  xlab('rPIT') + ylab('Densità')+
  labs(title=paste0( 'Log_opt : D = ', round(D_log$statistic, 4))) + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/risultati-bozza/15erratiche/rPIT-log_opt.png", plot = grafico1, width = 12, height = 8)

## CL
pt_cl_opt = rPIT(df_comb [ df_comb$Combinazione == 'cl-opt', 4:(ncol(df_comb_log1)-1)], y = 'vendite',metodo ='cl-opt')
D_log = ks.test(pt_cl_opt, 'punif')#, exact = T)

grafico1 = ggplot(as.data.frame(pt_cl_opt), aes(x=pt_cl_opt, y = after_stat(density))) + geom_histogram(bins = 20, color = 'black') +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 1, color = 'navyblue', linetype = 'dashed', linewidth = .6) +
  
  xlab('rPIT') + ylab('Densità')+
  labs(title=paste0( 'cl-opt su 15 erratiche: D = ', round(D_log$statistic, 4))) + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/risultati-bozza/15erratiche/rPIT-cl_opt.png", plot = grafico1, width = 12, height = 8)


sharpn = sharp('log-opt',df_comb_log_cl_opt [ df_comb_log_cl_opt$Combinazione == 'log-opt', 5:(ncol(df_comb_log_cl_opt)-1)], df_comb_log_cl_opt$vendite[ df_comb_log_cl_opt$Combinazione == 'log-opt'], sharpn)




