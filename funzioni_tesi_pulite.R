#######################
#### FUNZIONI TESI ####
#######################

## gestione valori mancanti nei prezzi
NA_fun = function(data){
  n_NA = sapply(data, function(col) sum(is.na(col)))
  n_NA = sort(n_NA[n_NA > 0])
  n_NA = data.frame(
    variabile = names(n_NA),
    freq_assoluta = as.numeric(n_NA),
    freq_relativa = round(as.numeric(n_NA)/nrow(data), 4)
  )
  n_NA }

## controllo che le date siano ordinate
ordinati_x_data <- function(DF, date_column) {
  if (!(date_column %in% colnames(DF))) {
    stop("La colonna della data specificata non è presente nel dataset.")
  }
  dati_ordinati <- DF[order(DF[[date_column]]), ]
  date_ordinate <- na.omit(dati_ordinati[[date_column]])
  date_DS <- na.omit(DF[[date_column]])
  if (all(date_ordinate==date_DS)) {
    cat("Le osservazioni sono ordinate per data (a meno degli NA).\n")
  } else {
    cat("Le osservazioni NON sono ordinate per data (a meno degli NA).\n")
  }
}



miss_date = function(df = dati, date = 'date', variabile = 'date'){
  i = 1 # indicatore di riga
  ind1 = c() # contiene l'ultima osservazione prima di NA quando dati problematici
  ind2 = c() # contiene la prima osservazione prima di NA quando dati problematici
  diff = c() # contiene le differenze tra le due pointer per dati problematici
  n_na = c() # contiene il numero di missing tra le 2 pointer
  var = c() # contiene i valori della variabile
  risolto = c() # contiene T/F per verificare se il problema è stato risolto in automatico
  
  n2 = 0 # numero na risolti
  id_data = which(colnames(df) == date)
  id_var = which(colnames(df) == variabile)
  
  while (i <= nrow(df)){
    # print(i)
    pointer2 = i
    # print(is.na(df[i,id_var]))
    if(i == 1) pointer1 = i # prima riga pointer1 = 1
    if( i != 1){
      if(pointer1 == 1 & pointer2 == 2 & is.na(df[pointer1, id_var]) & !is.na(df[pointer2, id_var])){
        ind1 = c(ind1, pointer1)
        ind2 = c(ind2, pointer2)
        #print(cbind(pointer1, dati$data[pointer1]))
        #print(cbind(pointer2, dati$data[pointer2]))
        diff = c(diff,df[pointer2, id_data]- df[pointer1, id_data])
        n_na = c(n_na, pointer2-pointer1)
        var = c(var, paste0(df[pointer1, id_var],'-', df[pointer2, id_var]))
        risolto = c(risolto, T)
      }
      if(!is.na(df[i, id_var])) { pointer1 = i }# primo record diverso da NA => pointer1 = i
      else {
        j = 0 # numero di record dopo i che sono NA
        while(is.na(df[i+j, id_var]) ) {
          j = j+1 # quanti record ci sono dopo il primo NA
          #print(j)
        }
        pointer2 = i+j # primo record diverso da NA dopo una serie di NA
        if(is.na(df[pointer1, id_var])) {
          #print(paste('pointer1:', pointer1))
          #print(paste('pointer2:', pointer2))
          #df[pointer1:pointer2, id_var] = df[pointer2, id_var] 
          
          ind1 = c(ind1, pointer1)
          ind2 = c(ind2, pointer2)
          #print(cbind(pointer1, dati$data[pointer1]))
          #print(cbind(pointer2, dati$data[pointer2]))
          diff = c(diff,df[pointer2, id_data]- df[pointer1, id_data])
          n_na = c(n_na, pointer2-pointer1)
          var = c(var, paste0(df[pointer1, id_var],'-', df[pointer2, id_var]))
          risolto = c(risolto, T)
        }
        else{
          if(df[pointer1, id_var] == df[pointer2, id_var] ) {
            df[pointer1:pointer2, id_var] = df[pointer1, id_var] 
            n2 = n2+1
            #print(paste('risolto', pointer1, pointer2))
            
            
          }
          # se le due pointer (ultima data non NA prima degli NA e prima data non NA dopo la serie di NA) 
          # sono uguali => le date mancanti nel mezzo saranno pari alle due pointer
          else {
            ind1 = c(ind1, pointer1)
            ind2 = c(ind2, pointer2)
            #print(cbind(pointer1, dati$data[pointer1]))
            #print(cbind(pointer2, dati$data[pointer2]))
            diff = c(diff,df[pointer2, id_data]- df[pointer1, id_data])
            n_na = c(n_na, pointer2-pointer1)
            var = c(var, paste0(df[pointer1, id_var],'-', df[pointer2, id_var]))
            risolto = c(risolto, F)
            #print(diff)
          }
        }
      }
    }
    i = pointer2+1
  }
  return(data.frame(pointer1 = df[ind1,id_data ], pointer2 = df[ind2, id_data], num_NA = n_na, giorni = diff, Variabile = var, Risolto = risolto))
  
  
}

## Matrice diagonale ####
is_diagonal <- function(matrice) {
  diagonale_principale <- diag(matrice)  # Ottieni la diagonale principale
  zeros_out_diagonal <- sum(matrice - diag(diagonale_principale)) == 0  # Controlla se gli elementi al di fuori della diagonale principale sono tutti uguali a zero
  return(zeros_out_diagonal)
}

## Costanti ####
costanti = function(data = dati, unici = 1, numero = F, cost = F){
  n = 0
  costant = c()
  for (el in colnames(data)){
    if (length(unique(data[,el])) <= unici){ 
      if(cost == T)  data = data[,-which(colnames(data) == el)]
      costant = c(costant, el)
      n = n+1
    } }
  #print(n)
  if(numero) return(n)
  if(cost) return(data)
  return(costant)
}

## SBC ####
SBC_fun = function(dati, serie, grafico = F, unico = T, 
                   categorie = data.frame(Serie = c(0), Categoria = c(0), ADI = c(0), CV2 = c(0))){
  # dati deve avere in colonna una serie
  
  library(tsintermittent)
  
  p = idclass(dati, type = 'SBC')#, outplot = 'detail')
  # valutazione SBC solo sul train o su tutte? Per essere corretti in previsione sarebbe
  # meglio solo train e comunque non si perdono tante osservazioni
  cv2 = p$cv2
  adi = p$p
  
  if(grafico){
    plot(log(cv2), log(adi), ylab ='ln(CV2)', xlab = 'ln(ADI)')
    abline(h = log(1.32), col = 2)
    abline(v = log(.49), col = 2)
  }
  
  if(!unico){
    cat = rep (NA, (ncol(dati)))
    
    cat[which(cv2 >= .49 & adi < 1.32)] = 'erratic' # 495
    cat[which(cv2 >= .49  & adi  >= 1.32)] = 'lumpy' # 5682
    cat[which(cv2 < .49  & adi< 1.32)] = 'smooth' # 975
    cat[which(cv2 < .49  & adi >= 1.32)] = 'intermittent' # 21750
    
  }
  if(unico) {
    cat = ifelse((cv2 < .49  & adi >= 1.32), 'intermittent',
                 ifelse((cv2 >= .49  & adi  >= 1.32), 'lumpy',
                        ifelse((cv2 < .49  & adi< 1.32), 'smooth', 'erratic')))
  }
  
  
  categorie = rbind(categorie, c(serie, cat, round(adi, 2), round(cv2, 2)))
  return(categorie)
}


## Creo mini datasets ####
creo_mini = function (dati = dati, prezzi = prezzi1, sku = sku, identificativo, cartella = 'erratic'){
  # guardo 1 sola serie
  dati1 = dati[, c(1:13, identificativo)] 
  #dati1$date = as.Date(dati1$date) # sennò è chr
  sku1 = sku[which(sku$id == colnames(dati)[identificativo]),] 
  prezzi2 = prezzi[ prezzi$store_id == sku1$store_id & prezzi$item_id == sku1$item_id,]
  
  dati1 = dati1[order(dati1$date),] # ordino i dati per data
  nome_item = colnames(dati1)[identificativo]
  dati1 = rename(dati1, vendite = colnames(dati1)[ncol(dati1)])
  
  ## Elimino gli 0 iniziali
  inizio = min(dati1$date[which(dati1$vendite!= 0 )]) 
  if(inizio != min(dati1$date)) dati1 = dati1[which(dati1$date == inizio):nrow(dati1),]
  ts.plot(ts(dati1$vendite))
  
  ## Aggiungo le info sul prezzo
  #ordinati_x_data(prezzi1, 'wm_yr_wk')
  dati1 = left_join(dati1, prezzi2[,3:ncol(prezzi2)], by = 'wm_yr_wk')
  
  ## controllo
  if(dim(NA_fun(dati1))[1] > 0 | min(dati1$relative_price_cat) < 0 |min(dati1$relative_price_dep)<0){
    #print(identificativo)
    break
  }
  
  mini = cbind(dati1, sku1[,c('cat_id', 'state_id', 'SBC')])
  
  ### Creo variabili ####
  
  #### proportion of the day in a year ####
  # ordinati_x_data(mini, 'date') # non sono ordinati per data
  mini$prop_year = -1
  for(el in unique(mini$year)){
    
    start = as.Date(paste0(el, '-01-01'))
    durata = as.numeric(difftime(as.Date(paste0(el, '-12-31')) , start, units = 'days')) # n. gg in un anno
    tmp = which(mini$year == el)
    
    mini$prop_year[tmp] = ifelse(mini$date[tmp] == start, 0,  
                                 as.numeric(difftime(mini$date[tmp], start, units = 'days')) / durata)
    # calcolo prop_days come la differenza tra la data della vendita e il primo giorno dell'anno fratto la durata dell'anno
  }
  
  #### the proportion of the day in a week ####
  proporzioni_giorni <- c(1/7, 2/7, 3/7, 4/7, 5/7, 6/7, 1)
  indici_giorni <- match(mini$weekday, c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
  settimana <- proporzioni_giorni[indici_giorni]
  mini$prop_week = settimana
  
  ######
  #### Vendite medie ####
  mini$media_28 = 0
  #ordinati_x_data(mini, 'date')
  
  for(i in 2:nrow(mini)){
    # se tra l'osservazione i e l'inizio non intercorrono almeno 28 giorni, faccio la media di tutto.
    if((difftime(mini$date[i], inizio, units = 'days')) < 29) { mini$media_28[i] = mean(mini$vendite[1:(i-1)]) }
    
    # altrimenti, faccio la media solo degli ultimi 28 giorni
    else{ mini$media_28[i]= mean(mini$vendite[(i-28):(i-1)])  }
  }
  
  mini$media_7 = 0
  
  for(i in 2:nrow(mini)){
    # se tra l'osservazione i e l'inizio non intercorrono almeno 28 giorni, faccio la media di tutto.
    if((difftime(mini$date[i], inizio, units = 'days')) < 8) { mini$media_7[i] = mean(mini$vendite[1:(i-1)]) }
    
    # altrimenti, faccio la media solo degli ultimi 28 giorni
    else{ mini$media_7[i]= mean(mini$vendite[(i-7):(i-1)])  }
  }

  ## ALTRE OPERAZIONI #####
  ## Elimino le variabili che non mi servono
  mini$wm_yr_wk = mini$month = mini$year = NULL
  
  #print(summary(mini))
  #print(identificativo)
  if(dim(NA_fun(mini)) [1]> 0) {
    #print('Problema!')
    break
  }
  if(min(mini$dist_last_sale) < 0 |
     min(mini$last_sales_bet) < 0 |
     min(mini$media_7) < 0 |
     min(mini$media_28) <0 ){
    print('Problema!')
    break
  }
  
  nome_file = paste0('/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/datasets/', cartella, '/', mini$SBC[1],'.csv')
  write.csv(mini, nome_file , row.names = FALSE)
}

## Regression quantile jittered: T(Z,tau) ####
T_ztau = function(Z = Z, tau = tau){
  if(length(tau) == 1) return( ifelse ( Z > tau, log(Z - tau), log(10^(-5))))
  
  t = data.frame(z = Z) # data frame dove ogni colonna è la trasformazione di Z per ogni quantile in esame
  #print(is.data.frame(t))
  
  for(el in tau){
    #print(el)
    col = paste0('quant_', el, collapse = NULL)
    # se la funzione produce NA (ossia u = 0 e Y = 0), allora si pone a 0, altrimenti si segue la formula per la trasformazione di Z
    t[,col] = ifelse ( Z == el , 0, ifelse(Z > el, log(Z - el), log(10^(-5))))
    #print(str(t))
  }
  
  return(t)
}

## Funzione per regressione quantile per dati di conteggio (Machado & Silva) con funzioni GAM come kernel integrato ####
creo_z = function(df = dati, y = 'y', seme = 1){
  set.seed(seme)
  u = runif(dim(df)[1], 0, .9999) # jittering
  Z = df[,y] + u # variabile continua da quella di conteggio
}

count_quantile_avg_jittering = function( df = dati, y = 'y', x = colnames(dati)[-idY], quantili = tau, m = 1,
                                         seme = seq(1, m), test = NULL, flag = F, mod_gam = mod_gam){
  # idY = which(colnames(dati) == 'y')
  # x = covariate da usare nel modello
  # quantili = quali quantili vogliamo guardare
  # kernel = funzioni del kernel integrato
  # flag = T -> dò il test e faccio previsione
  # m > 1 -> average jittering (minimo 100)
  
  ## Inizializzo le liste in cui andranno i modelli di regressione quantile (forse inutile) e le previsioni per ogni quantile
  quantile_fits <- list() # contiene tutti i modelli
  predictions = list() # contiene le previsioni
  #modello = list() # contiene i coefficienti del modello
  if(flag)   Z_test =creo_z(df = test, y = y, seme = seme[1]) 
  
  
  ## Cambio i nomi delle colonne delle covariate (essendo prese dagli effetti del GAM non vanno bene per la formula del QR)
  for(col in colnames(x)){
    #print(el)
    #print(strsplit(col,'\\(')[[1]][2])
    if(!is.na(strsplit(col,'\\(')[[1]][2])) colnames(x)[which(colnames(x) == col)] =  strsplit(strsplit(col,'\\(')[[1]][2], '\\)')
  }
  #print(colnames(x))
  formula = as.formula(paste0('new ~', paste0(colnames(x), collapse = '+'))) # formula per la regressione quantile
  
  for(i in 1:m){
    # Creo Z = Y + U
    ## sapply fino a quantile_fits (!flag)
    Z =  creo_z(df = df, y = y, seme = seme[i])
    #print(paste0('m: ', i))
    
    # per ogni quantile, calcolo la trasformazione T, la regressione quantile e le previsioni
    for(el in tau){ 
      
      ## T(Z, tau)
      trasformazione = ifelse( Z > el, log(Z-el), log(10^(-5)))
      
      #trasformazione[Z > el] = log( Z[Z > el] - el )
      
      ## Nuovo dataset da usare nella regressione quantile (t(z,tau) e le funzioni GAM)
      new = data.frame(new = trasformazione)
      new = cbind(new, x)
      
      ## Regressione quantile
      
      Q_Tztau = rq(formula,tau = el, data = new) # regressione quantile su trasformazione
      
      if(as.character(el) %in% names(quantile_fits)){ 
        for(j in 1:length(quantile_fits[[as.character(el)]])){
          #print(j)
          
          quantile_fits[[as.character(el)]][j] = quantile_fits[[as.character(el)]][j]+ coef(Q_Tztau)[j]
        }
        
      }
      else{ 
        quantile_fits[[as.character(el)]] <- coef(Q_Tztau) 
      }
    }
  }
  #str(Q_Tztau)
  
  
  #print(paste0('quantile fit iniziali: ',quantile_fits))
  quantile_fits = as.data.frame(sapply(quantile_fits , function(x) x/m))
  #print(paste0('coefficienti medi: ',str(quantile_fits)))
  
  if(!flag) return(quantile_fits)
  # previsione ####
  for(el in tau){
    print(paste0('tau: ', el))
    # print(predict(mod_gam, newdata = test, type = "terms"))
    gam_effects_test <- data.frame(predict(mod_gam, newdata = test, type = "terms")) # li usa come variabili esplicative
    #print('ok')
    gam_effects_test$new = Z_test # per forma, in predict non lo guarda
    
    
    for(nome in colnames(gam_effects_test)){
      if(!is.na(strsplit(nome,'\\.')[[1]][2])) colnames(gam_effects_test)[which(colnames(gam_effects_test) == nome)] =  strsplit(nome,'\\.')[[1]][2]
    }
    
    
    Q_Tztau$coefficients = quantile_fits[,as.character(el)]
    #print(coef(Q_Tztau))
    pred = predict(Q_Tztau, gam_effects_test) # previsione per il quantile el per y
    
    predictions[[as.character(el)]] = pred
  }
  
  return(predictions)
}

## iETS ####
iETS_quant_new = function(train = train_ts, tipo = c("o", "inverse-odds-ratio", 'direct', 'general'), 
                          prev_arima = prev_arima){
  mod_iets = adam(train, "MNN", occurrence= tipo, oesmodel="ZZN", h=56, holdout=TRUE, silent=FALSE,
                  distribution = "dgamma") # dava MMN e MMN. Il paper diceva MNN
  fore_iets = forecast(mod_iets, h = 56, interval = 'simulated', level = c(0,seq(.02, .98, by = .02)))#seq(.01,.99, by = .01))
  #str(fore_iets)
  
  fore_iets_df = data.frame(fore_iets$lower)
  fore_iets_df = fore_iets_df[, ncol(fore_iets_df):1]
  fore_iets_df =cbind(fore_iets_df, data.frame(fore_iets$upper[,2:ncol(fore_iets_df)]))# levo .5 da fore_iets_df$upper (altrimenti ce ne sono due)
  
  prev_iets = prev_arima
  prev_iets$Metodo = paste0('iETS(MNN)[', strsplit(tipo, split = "(?=.)", perl = T)[[1]][1], ']')
  prev_iets[,5:103] = sapply(1:ncol(fore_iets_df), function(quantile) ceiling(fore_iets_df[,quantile]))
  prev_iets$quant_1 = prev_iets$quant_0.99
  
  return(prev_iets)
}


## WSS ####

boot_wss = function(train_y = train$vendite, test_y = test$vendite, jitter = T,
                    seme = 1, tau = tau, rep = 1000){
  
  vendite01_train = ifelse(train_y ==0 , 0 , 1)
  
  n00 = n10 = n01 = n11 = n0 = n1 = 0
  for(i in 1:(length(vendite01_train)-1)){
    
    if(vendite01_train[i] == 0){ # ieri = 0
      n0 = n0+1
      
      if(vendite01_train[i+1] == 0) { n00 = n00 +1 }
      else{ n01 = n01 +1 }
    }
    
    else{ # ieri = 1
      n1 = n1+1
      
      if(vendite01_train[i+1] == 0) { n10 = n10 +1 }
      else{ n11 = n11 +1 }
    }
  }
  
  transizione = data.frame(Ieri = c(0,0,1,1), Oggi = c(0,1,0,1), Prob = c(n00/n0, n01/n0, n10/n1, n11/ n1))
  
  # LTD = c()
  previsioni = data.frame(vendite = test_y)
  for( s in 1:rep){
    #print(s)
    set.seed(s)
    seme = sample(1:10000, 1)
    
    ## Step2: generare una sequenza di valori 0 e non 0 durante l'orizzonte temporale di previsione, 
    # condizionatamente all'ultima domanda osservata
    occurrence = c()
    last = vendite01_train[length(vendite01_train)] # se è la prima iterazione guardo l'ultima oss
    
    for ( i in 1:nrow(test)){ # per h
      if(i > 1) last = occurrence[length(occurrence)] # guardo la simulazione precedente
      
      sim_test_wss = transizione$Prob[transizione$Ieri == last & transizione$Oggi == 1] # prob domanda dato ieri
      
      set.seed(seme)
      rand = runif(1,0,1) # numero casuale U[0,1]
      seme = seme + 1
      
      if(rand < sim_test_wss) { occurrence = c(occurrence, 1) }
      else{  occurrence = c(occurrence, 0) }
      
    }
    
    # print(occurrence)
    ## Step3: Sostituzione dei marker di stato non 0 con un valore numerico campionato 
    # casualmente con reinserimento dall'insieme di domande non 0 osservate.
    set.seed(s*10)
    seme = sample(1:10000, 1)
    
    for(i in which(occurrence == 1)){
      set.seed(seme)
      occurrence[i] = sample(train_y[train_y != 0],1) # campiono un valore casuale da y
      seme = seme +1
      
      # Step4: jittering
      if(jitter){
        
        set.seed(seme)
        Z = rnorm(1)
        
        J = 1 + floor(sqrt(occurrence[i]) * Z) # jittered
        
        if(J >= 0) occurrence[i ] = J
      }
    }
    #print(paste('jittered' , occurrence))
    
    # LTD = c(LTD, sum(occurrence))
    previsioni[, paste0('camp_', s)] = occurrence
    
  }
  
  ## Ricavo i quantili per confronto con altri metodi
  prev_wss = data.frame(vendite = test_y[1])
  F_hat = ecdf(t(previsioni)[2:ncol(previsioni), 1])
  #print('ok1')
  q_tau = round(quantile(F_hat, probs = seq(.01, .99, .01)))
  #print('ok')
  prev_wss = cbind(prev_wss, t(q_tau))
  
  for(ossne in 2:length(test_y)){
    F_hat = ecdf(t(previsioni)[2:ncol(previsioni), ossne])
    #plot(F_hat, main = ossne)
    
    q_tau = round(quantile(F_hat, probs = seq(.01, .99, .01)))
    #print(q_tau)
    prev_wss[ossne, ] = c(test_y[ossne], t(q_tau))
  }
  return(prev_wss)
}

## Poisson e BN ####
pois_fun_iniz = function(parametri){
  alpha = parametri[1]
  phi = parametri[2]
  mu = parametri[3]
  mu1 = train_ts[1]
  
  if(alpha + phi > 1 || alpha <= 0 || phi <= 0 || mu <= 0) return (NA)
  sum(dpois(train_ts[2:length(train_ts)], (1-alpha-phi)*mu +phi * mu1 + alpha*lag(train$vendite)[2:length(train_ts)]
            , log = T))
}

bn_fun_iniz = function(parametri){
  alpha = parametri[1]
  phi = parametri[2]
  b = parametri[3]
  mu = parametri[4]
  mu1 = train_ts[1]
  at = ((1-alpha-phi)*mu +phi * mu1 + alpha*(train$vendite)[1])*b
  
  if(alpha + phi >= 1 || alpha <= 0 || phi <= 0 || mu <= 0 || b <= 0 || at <= 0) return (NA)
  sum(dnbinom(train_ts[2:length(train_ts)], at, b/(1+b), log = T))
}


## rPIT ####
rPIT = function (quantili = prev_gamqr_co[, 3:(ncol(prev_gamqr_co))], y =  val$vendite, 
                 metodo = 'GAM-QR', n = 1, plotta = F, jitter = 1/nrow(quantili), seed = 1){
  # quantili = df con colnames=c(quantili)
  # y = variabile con y
  # n = numero di osservazioni da campionare per ottenere la rPIT per ogni t
  
  #library(dplyr)
  Niter = nrow(quantili)
  pt = rep(NA, Niter * n) # contiene pt campionati da U[F(y-1), F(y)]
  #  jitter = 1/Niter#.25
  
  
  for(i in 1:Niter){ # per ogni punto nel tempo futuro di interesse
    seme = seed * i*nrow(quantili)
    F_hat = ecdf(t(quantili)[, i])
    if(plotta) plot(F_hat)
    
    y_t = y[i]
    F1 = F_hat(y_t-1)
    F2 =  F_hat(y_t)
    
    set.seed(seme)
    if(F1 != F2){
      posizione = (i-1)*n
      pt [(posizione+ 1): (posizione+ n) ] = runif(n, F1, F2) # rPIT
    } else {
      camp = runif(2, 0, jitter)
      F_y1 = F1 + min(camp)
      F_y2 = F2 + max(camp)
      
      posizione = (i-1)*n
      pt [(posizione+ 1): (posizione+ n) ] <-  runif(n, F_y1, F_y2)
    }
  }
  we <- pt >= 1 # Potremmo aver ottenuto valori >= 1.
  if (any(we)) {
    pt[we] <- 1 - runif(sum(we),0,jitter)
  }
  return(pt)
  
}

## log score ####
log_score1 = function(df_previsto, y, h =28){
  # df_previsto = dataset dove ogni riga è un'osservazione e ogni variabile è un quantile (vendite, quantili)
  # y è un vettore con i valori osservati
  punteggio = c()
  n = 1
  
  for(i in 1:length(y)){
    #print(i)
    if(n == 1) score = 0
    
    distrib = prop.table(table(t(df_previsto)[,i])) # probabilità dei valori
    prev = ifelse( y[i] %in% names(distrib), distrib[names(distrib) == y[i]], 0) # probabilità di prevedere il vero valore
    #print(ifelse( prev > 0, log(prev), 1e-5))
    score = score + ifelse( prev > 0, log(prev), log(1e-5)) #/ nrow(test) # log score
    if(n == 28) {
      punteggio = c(punteggio, score/28)
      n = 0
    }
    n = n+1
  }
  return(mean(punteggio))
}

log_score_comb = function(df_previsto, y, h =28){
  # df_previsto = dataset dove ogni riga è un'osservazione e ogni variabile è un quantile (vendite, quantili)
  # y è un vettore con i valori osservati
  punteggio = c()
  score =0 
  #prva = c()
  
  for(i in 1:length(y)){
    #print(i)
    
    distrib = prop.table(table(t(df_previsto)[,i])) # probabilità dei valori
    prev = ifelse( y[i] %in% names(distrib), distrib[names(distrib) == y[i]], 0) # probabilità di prevedere il vero valore
    #print(ifelse( prev > 0, log(prev), 1e-5))
    score = score + ifelse( prev > 0, log(prev), log(1e-5)) #/ nrow(test) # log score
    #prva = c(prva, nomi[i])
    if(i %% h ==0 ) {
      #print(table(prva))
      punteggio = c(punteggio, score/h)
      score =0 
      #prva = c()
    }
  }
  return(punteggio)
}

## brier score ####
brier_score = function(df_previsto, y, h = 28){
  brier = c()
  score =0 
  
  for(i in 1:length(y)){
    
    distrib = prop.table(table(t(df_previsto)[,i])) # probabilità dei valori
    #print(distrib)
    prev = ifelse( y[i] %in% names(distrib), distrib[names(distrib) == y[i]], 0) # probabilità di prevedere il vero valore
    
    score = score -2*prev + sum(distrib^2) # somma dei punteggi di brier
    #print(brier)
    
    if(i/h == 1){
      brier = c(brier, score / h)
      score = 0
    }
  }
  return(brier)
  return(mean(brier))
}

## Punteggio di Brier per una serie per calcolo pesi per tutti metodi per una serie
brier4 = function(serie = unique(val$Serie)[1], pesi = rep(1/length(metodi), length(metodi)), distribution = distribuzione, dd = dd_test){
  # serie = nome serie
  # pesi = c(pesi)
  # distribution = lista dove ogni elemento è una matrice della distribuzione in un certo orizzonte temporale
  # dd = test set di quella serie
  brier = 0
  pesi = pesi / sum(pesi) # mi assicuro che i pesi legati ai risultati sommino ad 1. alla fine della procedura fare result$par/sum(result$par)
  
  for(i in 1:length(distribuzione)){
    f_comb = sweep(distribuzione[[i]], 1, pesi, `*`)
    brier = brier + BrierScore(resp = dd$vendite[i], pred = f_comb) #da guardare su internet (quaderno)
  }
  
  return(brier/length(h)) # magari problemi valori troppo bassi
}

## DRPS score ####
drps_score = function(df_previsto, y){
  drps = 0
  massimo = max(y)
  
  for(i in 1:length(y)){ # per ogni punto nel tempo futuro di interesse
    F_hat = ecdf(t(df_previsto)[, i])
    #print(i)
    
    drps = drps + sum(sapply(0:massimo, function(k) (F_hat(k) - ifelse(y[i] <= k, 1, 0))^2))
  }
  return(drps/length(y))
}

## Punteggio di DRPS per una serie per calcolo pesi
drps2 = function(serie = unique(df_prev$Serie)[1], horiz = 1:28, pesi = rep(1/length(metodi), length(metodi)), 
                 distribuzione= F_hat, ysum = 100){
  # serie = nome serie
  # horiz = orizzonte in analisi
  # pesi = c(pesi)
  # ysum = valori da guardare
  # distribuzione = lista di orizzonti di lista di metodi di ecdf
  # indicatrice = risultati della funzione indicatrice 
  score = 0
  pesi = pesi / sum(pesi) # mi assicuro che i pesi legati ai risultati sommino ad 1. alla fine della procedura fare result$par/sum(result$par)
  
  for(i in 1:length(distribuzione)){
    F_comb = rowSums(sweep(distribuzione[[i]], 2, pesi, `*`) )# trovo F_comb come la somma di tutti gli F pesati
    indicatrice1 = indicatrice[[i]]
    score = score + sum((F_comb - indicatrice1)^2)
  }
  return(score/length(horiz))
}

## Pesi inventario ####

w_inventario1 = function(serie = unique(df_prev$Serie)[1], horiz = 1:28, validation_set = val, pesi = rep(1/N, N), costo =  rbind(c(1,4), c(1,9), c(1,19))){
  # costo è una delle coppie di valori scritti
  print(pesi)
  
  
  stock = lostsales = 0
  lev = costo[2] / sum(costo)
  
  for(orizzonte in horiz){ # per ogni orizzonte temporale
    dd = validation_set[validation_set[,'Serie'] == serie & validation_set[,'h'] == orizzonte,]
    y_t = dd$vendite
    
    ## Calcolo F_comb
    unici <- dd %>%  select(5:ncol(dd)) %>%       # Seleziona le colonne dalla quinta all'ultima
      map(unique) %>%              # Applica unique a ogni colonna
      unlist() %>%                 # Combina tutti i risultati in un singolo vettore
      unique()                     # Rimuove i duplicati dal vettore risultante
    
    distrib = matrix(0, nrow = length(metodi), ncol = length(unici), dimnames = list(metodi, unici)) # metodi x valori unici dei quantili
    for(met in metodi){
      tab = cumsum(prop.table(table(t(dd[dd$Metodo == met, 5:ncol(dd)]))) )
      valore_1 = as.numeric(names(tab)[1])
      for(valore in names(tab)){
        if(which(colnames(distrib) == valore)-which(colnames(distrib) == valore_1) > 1){ 
          distrib[met, which(colnames(distrib) == valore_1):which(colnames(distrib) == valore)] = tab[as.character(valore_1)] # Così non è decrescente
        }
        distrib[met, valore] = tab[valore]
        valore_1 = as.numeric(valore)
      }
      if(valore_1 < max(unici)) distrib[met, which(colnames(distrib) == valore_1):ncol(distrib)] = 1 # I valori più grandi del 100% percentile devono essere pari ad 1
    }
    
    pesi = pesi / sum(pesi)
    F_comb = colSums(pesi * distrib) # 1 previsione (distribuzione) per ogni punto nel tempo
    q_tau = as.numeric(names(F_comb)[min(which(F_comb > lev))])
    
    stock = stock + max(0, q_tau-y_t)
    lostsales = lostsales + max(0, y_t-q_tau)
  }  
  # Penalità per la somma dei pesi diversa da 1
  #penalty <- 10000 * abs(sum(pesi) - 1)
  
  return(costo[1]*stock/max(horiz) + costo[2]* lostsales /max(horiz) )
  
}


## valutazione sharpness ####
sharp = function(modello, df_previsto, y,  h = 28){
  # df_previsto = contiene solo le previsioni
  # y = test$y
  
  ## segno la riga da aggiungere alla matrice
  #riga = 1
  #if (matr.sharp[1,1] != 0){ riga = dim(matr.sharp)[1]+1  }
  
  logaritmico = brier = drps = 0
  massimo = max(y)
  df_previsto_t <- t(df_previsto)
  distribuzioni <- apply(df_previsto_t, 2, function(col) prop.table(table(col)))
  Niter = length(y)
  
  ## per drps
  #massimo = max(y)
  
  ## calcolo lo score per ogni h = 1,..., 28 e faccio la media dentro i c()
  for(i in 1:Niter){
    distrib = distribuzioni[[i]] # probabilità dei valori
    prev <- ifelse(y[i] %in% names(distrib), distrib[names(distrib) == y[i]], 0)
    
    #print(ifelse( prev > 0, log(prev), log(1e-5)))
    
    logaritmico = logaritmico + ifelse( prev > 0, log(prev), log(1e-5) )#/ nrow(test) # log score
    brier = brier -2*prev + sum(distrib^2) 
    
    F_hat = ecdf(df_previsto_t[, i])
    drps = drps + sum(sapply(0:massimo, function(k) (F_hat(k) - ifelse(y[i] <= k, 1, 0))^2))
    
    ## se son passati 28 periodi, faccio la media di ogni punteggio
    
  }
  
  return(c(modello, 
           round(-(logaritmico/Niter),4),
           round((drps/Niter), 4), 
           round((brier/Niter),4)))
}

## valutazione costo inventario ####
fun.costi2 = function( metodo, df_previsto, y,
                       matr.inv = data.frame(Metodo = c(0), 
                                             `Cost(1,4)` = c(0), 
                                             `Cost(1,9)`=c(0),
                                             `Cost(1,19)` = c(0)),
                       costo =  rbind(c(1,4), c(1,9), c(1,19))){
  # costo = rbind(c(1,4), c(1,9), c(1,19))
  
  livello = costo[,2] / rowSums(costo)
  
  df_previsto_t = t(df_previsto)
  Niter = length(y)
  # n = 1
  costo_tot = c()
  
  
  for(l in 1:nrow(costo)){
    lev = livello[l]
    stock = 0
    lostsales = 0
    
    
    for(i in 1:Niter){
      F_hat = cumsum(prop.table(table(df_previsto_t[,i])))
      print(F_hat)
      q_tau = min(as.numeric(names(which(F_hat >= lev))))
      if(max(0, q_tau-y[i]) > 100) break
      stock = stock + max(0, q_tau-y[i])
      lostsales = lostsales + max(0, y[i]-q_tau)
      print(lostsales)
      #print(max(0, y[i]-q_tau))
    }
    stock = stock / Niter
    lostsales = lostsales/Niter
    costo_tot = c(costo_tot, round( stock + costo[l,2]* lostsales , 4))
  }
  
  return (c(met, costo_tot))
}

## Trade off curves ####
to_curves2 = function(metodo,df_previsto, y,  plot = F, Metodo, # metodo o combinazioni
                      # matr.cost = data.frame(
                      #  Metodo = c(0), Livello = c(0), Investment= c(0), Lostsales = c(0), CSL_deviation = c(0)#, Costo = c(0)
                      #),
                      costo =  rbind(c(1,4), c(1,9), c(1,19))){
  print(metodo)
  colnames(df_previsto[,Metodo]) = 'Metodo'
  
  livello = costo[,2] / rowSums(costo)
  matr.cost = matrix(NA, 3, 5)
  
  df_previsto_t = t(df_previsto)
  Niter = length(y)
  
  for(l in 1:nrow(costo)){
    lev = livello[l]
    print(lev)
    stock = 0
    lostsales = 0
    CSL_dev = 0
    
    
    # Funzione per calcolare cumsum di prop.table di una colonna e qtau per tutti
    cumsum_prop_table <- function(column) {
      cumsum(prop.table(table(column)))
    }
    qtau_fun = function(Fhat){
      min(as.numeric(names(which(Fhat >= lev))))
    }
    
    # F-hat e q_tau per ogni orizzonte temporale e ogni serie
    result_apply <- apply(df_previsto_t, 2, cumsum_prop_table)
    qtau_tutte = lapply(result_apply, qtau_fun)
    
    for(i in 1:Niter){
      F_hat = result_apply[[i]]
      q_tau = qtau_tutte[[i]]
      
      stock = stock + max(0, q_tau-y[i])
      lostsales = lostsales + max(0, y[i]-q_tau) #Somma di vendite perse in tutti gli orizzonti. Media da aggiungere tra SKU
      
      CSL_dev = CSL_dev + (F_hat[which(names(F_hat) == q_tau)]-lev)
    }
    
    stock = stock / Niter # investimento
    CSL_dev = CSL_dev/Niter
    lostsales = lostsales/(Niter / 28) # voglio lostsales avg by sku per tutti gli orizzonti
    
    matr.cost[l,] = c(metodo, lev, 
                      round(stock, 4),
                      round(lostsales, 4),
                      round( (CSL_dev), 4))
  }
  return(matr.cost)
}
