rm(list = ls())

library(vroom)

source('~/Desktop/tesi_magistrale/pratica-tesi_mag/codiceR/funzioni_tesi_pulite.R')
#dati = read.csv('~/Desktop/tesi_magistrale/pratica-tesi_mag/codiceR/codice-tesi_magistrale/mini_ds/mini1_completo.csv', sep = ';', dec = ',')
da_eliminare = read.table('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/da_eliminare.txt')
sku = read.csv('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/generali/sku_new2.csv') # non ha quelle da levare
#da_analizzare = which(!sku$SBC[sku$cat_SBC == 'intermittent'] %in% da_eliminare$x) # prendo solo i file non da eliminare

### levo quelle che ho già fatto ####
df_prev_int = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_intermittenti_completi/df_prev_int_completo1.csv')
df_prev_int = df_prev_int[,-1]
df_prev_err = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_erratic_completo.csv')
df_prev_err = df_prev_err[,-1]
df_prev_lumpy = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_lumpy_completo.csv')
df_prev_smooth = vroom('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_smooth_completo.csv')
df_prev_smooth = df_prev_smooth[,-1]
#sku$fatto = ifelse(sku$SBC %in% unique(c(df_prev_err$Serie, df_prev_int$Serie, df_prev_lumpy$Serie, df_prev_smooth$Serie)), 1, 0)
table(sku$fatto)
#sku$fatto[sku$SBC %in% da_eliminare$x] = 2

df_prev_int = as.data.frame(df_prev_int)
df_prev_err = as.data.frame(df_prev_err)
df_prev_lumpy = as.data.frame(df_prev_lumpy)
df_prev_smooth = as.data.frame(df_prev_smooth)

da_fare = unique(sku$SBC[which((!sku$SBC %in% unique(df_prev_lumpy$Serie) & 
                                  sku$fatto == 0 &
                                  sku$cat_SBC == 'lumpy'))])#[660:1500]
da_fare = da_fare[6000:length(da_fare)]
#serie_fatte = c(df_prev_err$Serie, df_prev_int$Serie, df_prev_lumpy$Serie, df_prev_smooth$Serie)
folder_path_df <- '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_OK/df_prev_intermittenti'
csv_files_df <- list.files(folder_path_df, pattern = "\\.csv$", full.names = TRUE)[1:39] # dal 40 in poi sono quelli completi
serie_fatte2 = sapply(sapply(sapply(csv_files_df, function(x) strsplit(x, split = '\\/')), 
              function(y) strsplit(y[10], '\\.')[[1]][1]), 
       function(z) strsplit(z, split = '\\_')[[1]][2])
da_analizzare1 = which(!sku$SBC %in% c(da_eliminare$x, serie_fatte))#, serie_fatte2) )# prendo solo i file non da eliminare
#write.csv(sku, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/generali/sku_new2.csv')

set.seed(1)
prova6 = sample(da_fare[!da_fare %in% c(prova, prova2, prova3, prova4, prova5)], 1500)
write.table(prova6, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/da_fare6.txt')



prova = sku$SBC[da_analizzare1]


previsioni = read.csv('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_erratic_completo.csv')

table(previsioni$Serie)
length(unique(previsioni$Serie))
table(previsioni$Metodo)
da_fare = sku$SBC[sku$cat_SBC == 'erratic'][da_analizzare][which(!sku$SBC[sku$cat_SBC == 'erratic'][da_analizzare] %in% previsioni$Serie)][1:10]
da_fare = sku$SBC[sku$cat_SBC == 'erratic'][da_analizzare][1:10]

table(da_fare %in% unique(previsioni$Serie))
#perc_0 = c() # percentuale di 0 nelle serie

setwd('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/')
da_fare = unique(sku$SBC[which((!sku$SBC %in% unique(df_prev$Serie) & 
                                  sku$fatto == 0 &
                                  sku$cat_SBC == 'erratic'))])[1:5]

### Librerie ####

library(foreach)
library(doParallel)
library(doSNOW)


cl <- makeCluster(detectCores() - 1, outfile = ' ') # Usa tutti i core meno uno
#registerDoParallel(cl)
registerDoSNOW(cl)

clusterEvalQ(cl, {
  library(vroom)
  library(dplyr)
  library(lubridate)
  
  #library(scoring) # per regole di scoring
  #library(ggplot2) # per grafico PIT
  #library(dgof) # per test KS
  library(mgcv) # per GAM 
  library(quantreg) # per QR
  library(forecast) # per Croston e ARIMA etc
  library(tsintermittent) # per SBA
  library(truncnorm) # per ARIMA
  library(maxLik) # damped mean
  library(smooth) # per iETS
})
sss = Sys.time()
tau = seq(.01, .99, by = .01)
flaggati = c() # contiene le serie BN che vengono fatte con Poisson (caso limite)
setwd('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/')

#intermittent3494

prev_tmp <- foreach(nome_serie = da_fare, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% {
  
  print(nome_serie)
  file_path = paste0('lumpy/', 'lumpy4823' ,'.csv')
 # if(!file.exists(file_path)) next
  dati = vroom(file_path)
  dati = as.data.frame(dati)
  dati = dati %>% select(-c(state_id, cat_id, SBC, dist_first_sale, dist_last_sale, last_sales_bet))
#  perc_0 = c(perc_0 ,prop.table(table(dati$vendite == 0))[2]) # per calcolare la % di 0
  df_prev= data.frame(Serie = nome_serie, h = seq(1:56)) # per gamqr
  
  #####
  ### Operazioni preliminari ####
  #####

  ####
  ## Divido i dati ####
  ###
  for(miss in NA_fun(dati)$variabile){
    dati[is.na(dati[,miss]), miss] = ' '
  }
  
  train = dati[dati$periodo == 1,]
  test = dati[dati$periodo != 1,]
  
  train_ts = ts(train$vendite, start = c(2012, 75), frequency = 365.25)
  test_ts = ts(test$vendite, start = c(2016, 86), frequency = 365.25) # "2016-03-27" (115)
  
  train$date = test$date = train$periodo = test$periodo =NULL # elimino la data, mi serviva solo per dividere i dati
  test$event_name_1[which(!(test$event_name_1) %in% (train$event_name_1))] = ' ' # per evitare problemi in fase di previsione
  
  ####
  ### GAM QR ####
  ####
  
  # quant qual
  train[,costanti(train)] = NULL
  
  # controllo eventuali variabili problematiche (prezzo): potrebbe essere costante (da levare) o con pochi valori (da trasformare in fattore)
  info_prezzo = c('prezzo', 'relative_price_cat', 'relative_price_dep')
  
  formula_co = 'vendite ~  event_type_1 + event_name_1+ s(prop_week, k = length(unique(train$prop_week))) + s(media_28,  k =  length(unique(train$media_28))) + s(media_7,  k =  length(unique(train$media_7)))' 
  
  for(price in info_prezzo){
    if( price %in% colnames(train) ) { # se non è costante
      
      if(length(unique(train[,price]))<= 3) { # se price ha meno di 3 valori unici (quindi da mettere come fattore nel gamqr)
        if('FALSE' %in% names(table(unique(test[,price]) %in% unique(train[,price])))) { # se ci sono prezzi nel test che non sono nel train, eliminare la variabile e si passa al ciclo successivo
          train[,price] = test[,price] = NULL
          next
        }
        
        train[,price] = factor(train[,price]) 
        test[,price] = factor(test[,price]) 
      }
      
      formula_co = paste0(formula_co, '+', ifelse(is.factor(train[,price]), price , paste0('s(',price,', k = length(unique(train$',price, ')))'))) # se ha meno di 3 valori unici come fattore, sennò normale
      #formula_co = paste0(formula_co, '+', aggiunta)
      }
    }
    
  formula_co = as.formula(formula_co)
  
  # normalizzo i dati in base al train
  nomi_x = colnames(train %>% select(-vendite))
  quant_gamqr = nomi_x[which(lapply(nomi_x, function(x) is.numeric(train[,x])) == T)]
  qual_gamqr = setdiff(nomi_x, quant_gamqr)
  for(var in qual_gamqr){
    train[,var] = factor(train[,var])
    test[,var]= factor(test[,var])
  }
  
  medie = colMeans(train[,quant_gamqr])
  std = apply(train[, quant_gamqr], 2, sd)
  train[,quant_gamqr] = scale(train[,quant_gamqr])
  test[,quant_gamqr] = scale(test[,quant_gamqr], center = medie, scale = std )
  
  ##### GAM ####
  mod_gam_co = gam(formula_co, family = nb ( link = 'log'), data = train, method = 'REML', optimizer = c('outer','bfgs'), control = gam.control( irls.reg = .5))
  
  mod_gam_noco = gam(vendite ~ s(media_28,  k = length(unique(train$media_28))) + s(media_7,  k =  length(unique(train$media_7))) , family = nb ( link = 'log'), data = train)
  #summary(mod_gam_noco)
  
  effetti_noco = predict(mod_gam_noco, type = "terms")
  effetti_co = predict(mod_gam_co, type = 'terms')
  if(costanti(effetti_co, unici = 2,numero = T) != 0){ # se hanno meno di 2 valopri unici => problema di non singolarità della matrice
    effetti_co = effetti_co [,-which(colnames(effetti_co) %in% costanti(effetti_co, unici = 2))]
  }
  
  ##### Regressione quantile per dati di conteggio ####
  
  ## No covariate
  gamqr_noco = count_quantile_avg_jittering(train, y = 'vendite', x = effetti_noco, test = test, mod_gam = mod_gam_noco, m = 50, flag = T)

  prev_gamqr_noco = data.frame(Metodo = 'GAM-QR(noco)', vendite = test$vendite)
  prev_gamqr_noco = cbind(prev_gamqr_noco,
                          sapply(names(gamqr_noco), function(quantile) ceiling(as.numeric(quantile) + exp(gamqr_noco[[as.character(quantile)]])-1)))
  colnames(prev_gamqr_noco)[3:ncol(prev_gamqr_noco)] = sapply(tau, function(x) paste0('quant_', x))
  
  ## Con covariate
  gamqr_co = count_quantile_avg_jittering(train, y = 'vendite', x = effetti_co, test = test, flag = T, mod_gam = mod_gam_co, m = 50)
  
  prev_gamqr_co = data.frame(Metodo = 'GAM-QR(co)', vendite = test$vendite)
  prev_gamqr_co = cbind(prev_gamqr_co,
                          sapply(names(gamqr_co), function(quantile) ceiling(as.numeric(quantile) + exp(gamqr_co[[as.character(quantile)]])-1)))
  colnames(prev_gamqr_co)[3:ncol(prev_gamqr_co)] = sapply(tau, function(x) paste0('quant_', x))
  
  # salvo i risultati ####
  df_prev = cbind(df_prev, rbind(prev_gamqr_co, prev_gamqr_noco))
  df_prev$quant_1 = df_prev$quant_0.99
  
  ## check
  #print('OK GAM QR')
  
  ####
  ### Croston & SBA #####
  ####
  
  mod_crost = crost(train_ts, h = 56, type = 'croston')
  mod_sba = crost(train_ts, 56, type = 'sba')
  
  ## Estraggo i quantili
  prev_crost = df_prev[1:56,]
  prev_crost$Metodo = 'Croston'
  
  prev_crost [,5:103]= t(sapply(mod_crost$frc.out, function(x) qnbinom(tau, size = 10*x, mu = x)))
  prev_crost[,104] = prev_crost[,103]
  
  ## Salvo le previsioni ####
#  head(prev_crost)
  df_prev = rbind(df_prev, prev_crost)
  
  #### SBA ####
  prev_sba = df_prev[1:56,]
  prev_sba$Metodo = 'SBA'
  prev_sba [,5:103]= t(sapply(mod_sba$frc.out, function(x) qnbinom(tau, size = 10*x, mu = x)))
  prev_sba[,104 ] = prev_sba[,103]
  
  ## Salvo le previsioni ####
  #head(prev_sba)
  df_prev = rbind(df_prev, prev_sba)
  
  ####tsb ####
  mod_tsb = tsb(train_ts, 56)

  prev_tsb = df_prev[1:56,]
  prev_tsb$Metodo = 'TSB'
  prev_tsb [,5:103]= t(sapply(mod_tsb$frc.out, function(x) qnbinom(tau, size = 10*x, mu = x)))
  prev_tsb[,104] = prev_tsb[,103]
  
  ## Salvo le previsioni ####
  df_prev = rbind(df_prev, prev_tsb)
  
  ## check
  #print('OK SBA e Croston')
  
  ####
  ### ARIMA ####
  ####
  
  mod_arima = auto.arima(train_ts) #ARIMA(1,1,2)
  fore_arima = forecast(mod_arima, h = 56)
  
  std_devs <- (fore_arima$upper[,2] - fore_arima$lower[,2]) / (2 * 1.96)
  prev_arima = df_prev[1:56,]
  prev_arima$Metodo = 'ARIMA'
  prev_arima[, 5:103] = round(t(sapply(1:56, function(i) qtruncnorm(tau, a = 0, mean = fore_arima$mean[i], sd = std_devs[i]))))
  prev_arima[,104] = prev_arima[,103]
  
  ## Salvo le previsioni ####
  df_prev = rbind(df_prev, prev_arima)
  #table(df_prev$Metodo)
  
  ## check
  #print('OK ARIMA')
  
  ####
  ### ETS ####
  ####
  
  mod_ets = ets(train_ts, model = 'AZN')
  fore_ets = forecast(mod_ets, h = 56)
  
  std_devs <- (fore_ets$upper[,2] - fore_ets$lower[,2]) / (2 * 1.96)
  prev_ets = df_prev[1:56,]
  prev_ets$Metodo = 'ETS'
  prev_ets[, 5:103] = round(t(sapply(1:56, function(i) qtruncnorm(tau, a = 0, mean = fore_ets$mean[i], sd = std_devs[i]))))
  prev_ets[,104] = prev_ets[,103]
  
  ## Salvo le previsioni ####
  df_prev = rbind(df_prev, prev_ets)
  #table(df_prev$Metodo)
  
  ## iETS(o) ####
  mod_iets = adam(train_ts, "MNN", occurrence="o", oesmodel="ZZN", h=56, holdout=TRUE, silent=FALSE,
                  distribution = "dgamma") # dava MMN e MMN. Il paper diceva MNN
  fore_iets = forecast(mod_iets, h = 56, interval = 'simulated', level = c(0,seq(.02, .98, by = .02)))#seq(.01,.99, by = .01))
  #str(fore_iets)
  
  fore_iets_df = data.frame(fore_iets$lower)
  fore_iets_df = fore_iets_df[, ncol(fore_iets_df):1]
  fore_iets_df =cbind(fore_iets_df, data.frame(fore_iets$upper[,2:ncol(fore_iets_df)]))# levo .5 da fore_iets_df$upper (altrimenti ce ne sono due)
  
  prev_iets = prev_arima
  prev_iets$Metodo = 'iETS(MNN)[o]'
  prev_iets[,5:103] = sapply(1:ncol(fore_iets_df), function(quantile) ceiling(fore_iets_df[,quantile]))
  prev_iets$quant_1 = prev_iets$quant_0.99
  
  ## iETS altri ####
  prev_iets_i <- iETS_quant_new(train = train_ts, tipo = 'inverse-odds-ratio',  prev_arima = prev_arima)
  prev_iets_d <- iETS_quant_new(train = train_ts, tipo = 'direct', prev_arima = prev_arima)
  prev_iets_g <- iETS_quant_new(train = train_ts, tipo = 'general', prev_arima = prev_arima)
  
  df_prev = rbind(df_prev, prev_iets_i, prev_iets_g, prev_iets_d)
  
  ## check
  #print('OK ETS e iETS')

  ## bootstrap WSS ####
  wss = boot_wss()
  #str(wss)
  
  prev_wss = prev_arima
  prev_wss$Metodo = 'Bootstrap WSS'
  prev_wss[,5:103] = wss[,2:ncol(wss)] # la prima colonna contiene le vendite
  prev_wss$quant_1 = prev_wss$quant_0.99
  
  ## Salvo #####
  df_prev = rbind(df_prev, prev_wss)
  table(df_prev$Metodo)

  ## check
  #print('OK WSS')
  
  ## Poisson ####
  mu = mean(train$vendite)
  alpha = .1
  phi = .1
  #tau = seq(.01, .99, by = .01)
  
  prva = maxLik(pois_fun_iniz, start =  c(.1, .1, mean(train_ts)), method = 'BFGS')
  tryCatch({
    prva <- maxLik(pois_fun_iniz, start =  c(.1, .1, mean(train_ts)), method = 'BFGS')
  }, error = function(e) {
    print("Error in Poisson model optimization:")
    print(e)
  })
  
  alpha = prva$estimate[1]
  phi = prva$estimate[2]
  mu = prva$estimate[3]
  mu1 = yt1 = mu_est = train_ts[1]
  pt1_formula = (1-alpha-phi)*mu 

  for(i in 3:nrow(train)){ #]) # i primi due li ho già stimati prima come valori iniziali e maxLik
    mu_est = c(mu_est, pt1_formula+ phi*mu_est[length(mu_est)] + alpha*train_ts[i-1])
  }
  
  ## Simulazioni
  # voglio 1000 simulazioni per ogni orizzonte temporale? Quindi 56x1000
  simulazioni = data.frame(h = seq(1:56), matrix(NA, 56, 1000))
  colnames(simulazioni)[2:ncol(simulazioni)] = sapply(1:1000, function(x) paste0('sim_', x)) 
  mu_t1 = mu_est[length(mu_est)]
  yt1 = train$vendite[nrow(train)]

  
  for(oss in 1:56){
    mu_t = pt1_formula + phi*mu_t1 + alpha*yt1 # utilizzo la formula per la poisson con media damped
    
    # aggiorno le componenti che variano nel tempo
    mu_t1 = mu_t
    set.seed(1)
    yt1 = rpois(1000, mu_t) # per avere y_{t-1} pesco una osservazione casuale dalla poisson corrente
    #print(table(check.integer(yt1)))
    simulazioni[oss,2:ncol(simulazioni)] = yt1
  }
  
  prev_pois = prev_arima[1:56,]
  prev_pois$Metodo = 'Poisson'
  prev_pois[,5:103] =  round(t(sapply(1:56, function(x) quantile(t(simulazioni)[2:ncol(simulazioni),x], tau))))

  prev_pois$quant_1 = prev_pois$quant_0.99
  
  df_prev = rbind(df_prev, prev_pois)
  
  ## check
  #print('OK pois')
  
  ## Binomiale negativa ####
  mu = mean(train$vendite)
  alpha = .1
  phi = .1
  b = mu/(var(train$vendite) - mu)# mu/k in base alla distribuzione
  tau = seq(.01, .99, by = .01)
  flag = T # diventa F se b > 99 => in base a Snyder andiamo a prendere la Poisson come caso limite
  if(b <= 0 | b > 99){ # se b <0 indica sottodispersione (come quando b > 99) => si torna al caso limite poisson
    flaggati = c(flaggati, nome_serie)
    
    prev_nbinom = df_prev[df_prev$Metodo == 'Poisson' & df_prev$Serie == nome_serie,]
    prev_nbinom$Metodo = 'NegBinom'
    flag = F
  }
  
  if(flag){
    prva = maxLik(bn_fun_iniz, start =  c(.1, .1, b, mean(train_ts)), method = 'BFGS')
    tryCatch({
      prva <- maxLik(bn_fun_iniz, start =  c(.1, .1, b, mean(train_ts)), method = 'BFGS')
    }, error = function(e) {
      print("Error in Negative Binomial model optimization:")
      print(e)
    })
    
    alpha = prva$estimate[1]
    phi = prva$estimate[2]
    b = prva$estimate[3]
    mu = prva$estimate[4]
    mu1 = yt1 = mu_est = train_ts[1]
    if(b > 99) {
      flaggati = c(flaggati, nome_serie)
      
      prev_nbinom = df_prev[df_prev$Metodo == 'Poisson' & df_prev$Serie == nome_serie,]
      prev_nbinom$Metodo = 'NegBinom'
      flag = F
    }}
  
  if(flag){
    pt1_formula = (1-alpha-phi)*mu # parte tempo indipendente della formula di snydere
    
    for(i in 3:nrow(train)){
      mu_est = c(mu_est,pt1_formula + phi*mu_est[length(mu_est)] + alpha*train_ts[i-1])
    }
    
    ## Simulazioni
    # voglio 1000 simulazioni per ogni orizzonte temporale? Quindi 56x1000
    simulazioni = data.frame(h = seq(1:56), matrix(NA, 56, 1000))
    colnames(simulazioni)[2:ncol(simulazioni)] = sapply(1:1000, function(x) paste0('sim_', x))
    mu_t1 = mu_est[length(mu_est)]
    yt1 = train$vendite[nrow(train)]
    
    for(oss in 1:nrow(test)){
      mu_t = pt1_formula + phi*mu_t1 + alpha*yt1 # utilizzo la formula per la poisson con media damped
      
      # aggiorno le componenti che variano nel tempo
      mu_t1 = mu_t
      set.seed(1)
      yt1 = rnbinom(1000, mu_t*b, b/(1+b)) # per avere y_{t-1} pesco una osservazione casuale dalla poisson corrente
      simulazioni[oss,2:ncol(simulazioni)] = yt1
    }
    
    prev_nbinom = prev_arima[1:56,]
    prev_nbinom$Metodo = 'NegBinom'
    prev_nbinom[,5:103] =  round(t(sapply(1:56, function(x) quantile(t(simulazioni)[2:ncol(simulazioni),x], tau))))
    
    prev_nbinom$quant_1 = prev_nbinom$quant_0.99
    
  }
  
  df_prev = rbind( df_prev, prev_nbinom)
  #print('OK BN')
  return(df_prev)
  #write.csv(previsioni, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_int_completo.csv', row.names = FALSE)#, append = T)
  
}
end = Sys.time()
stopCluster(cl)
end-sss

table(prev_tmp$Metodo)
length(unique(prev_tmp$Serie))
unique(prev_tmp$Serie) == da_fare
unique(prev_tmp$Serie) %in% unique(previsioni$Serie)
which(prev_tmp[prev_tmp$Serie == unique(prev_tmp$Serie)[1],100] != previsioni [previsioni$Serie == unique(prev_tmp$Serie)[1],100])
previsioni = rbind(previsioni, previs[which(previs$Serie %in% unique(previs$Serie) [which(!unique(previs$Serie) %in% unique(previsioni$Serie))]),])

previsioni = rbind(previsioni, prev_tmp)
table(previsioni$Metodo)
length(unique(previsioni$Serie))
#previsioni = previsioni %>% mutate(across(5:ncol(previsioni), round, 3))
write.csv(previsioni, '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/df_prev_erratic_completo.csv', row.names = FALSE)#, append = T)

