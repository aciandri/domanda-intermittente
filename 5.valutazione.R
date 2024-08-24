
## librerie ####
library(vroom)
library(purrr)
library(doParallel)
library(foreach)
library(doSNOW)
library(xtable)
library(dplyr)
library(ggplot2)
library(patchwork)



folder = 'previsioni_df/df_prev_OK/df_prev_comb'
csv = list.files(folder, full.names = T)[c(1,4,11)]
dd = list.files(csv[2], full.names = T)
dd1 = dd[grepl('int', dd)]
erratiche1 = map_df(dd1, vroom)
erratiche1 = erratiche1[,-c(1:(which(colnames(erratiche1) == 'Combinazione')-1))]
erratiche1 = erratiche1
table(table(erratiche1$Combinazione, erratiche1$h))
table(erratiche1$h)
erratiche1 = as.data.frame(erratiche1)
df_prev1 = rbind(df_prev1, df_comb_brier)
erratiche1 = rbind(erratiche1, df_comb_brier)

folder1 = 'previsioni_df/df_prev_OK/altri_finali'

csv1 = list.files(folder1, full.names = T)
df = map_df(csv1[6], vroom)
length(unique(df$Serie))

df_prev = prova[grep('erratic', prova$Serie),]
df_prev = erratiche1
df_prev = rbind(df_prev, erratiche1)
colnames(df_prev)[1] = 'Metodo'
table(df_prev$Metodo)

df_prev$Metodo[df_prev$Metodo == 'round_SA'] = 'SA'
df_prev$Metodo[df_prev$Metodo == 'Mediana'] = 'mediana'

metodi = unique(df_prev$Metodo)
h = unique(df_prev1$h) # 28 gg del validation set
N = length(metodi)

## rPIT ####

## Creo i file rpit: ognuno contiene una serie per metodo e sono suddivise in cartelle in base al metodo
cl <- makeCluster(4, outfile = ' ')
#registerDoParallel(cl)
registerDoSNOW(cl)

sss = Sys.time()
orizzonte = sort(rep(29:56, 10)) # 28 x 10 estrazioni dalla distribuzione
jit = 1/28
semi = cbind(metodi, (1:length(metodi))) # per sto cazzo di seme sennò ties
rpit <- foreach(met = metodi, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% {
  #for(met in metodi){
  print(met)
  pt = data.frame(Metodo = c(0), Serie = c(0), h = c(0), pt = c(0))
  for(serie in unique(df_prev$Serie)){
    print(serie)
    p = data.frame(Metodo = met, Serie = serie, h = orizzonte, pt = rPIT(df_prev[df_prev$Serie == serie & df_prev$Metodo == met, 5:(ncol(df_prev)-1)], 
                                                                         df_prev$vendite[ df_prev$Serie == serie & df_prev$Metodo == met], metodo = met, n = 10, jitter = jit, seed = as.numeric(semi[semi[,1] == met,2])))#, seed = as.numeric(semi[semi[,1] == met,2])))
    pt = rbind(pt, p)
    #write.csv(p, file_path)
  }
  pt = pt[-1,]
  write.csv(pt, paste0('previsioni_df/risultati_vari/rpit_comb/rpit-lumpy/rpit_lumpy-', met, '.csv'))
  
  return(pt)
}
end = Sys.time()
stopCluster(cl)
end-sss



## Sharpness ####
cl <- makeCluster(detectCores() - 4, outfile = ' ')
#registerDoParallel(cl)
registerDoSNOW(cl)

clusterEvalQ(cl, {
  library(DescTools)
})

sss = Sys.time()
sharpness1 <- foreach(met = metodi, .combine = rbind,  .packages = c('foreach', 'doParallel')) %dopar% {
  print(met)
  
  sharpn =sharp(met, df_prev[df_prev$Metodo == met, 5:(ncol(df_prev)-1)], df_prev$vendite[df_prev$Metodo == met])
  return(sharpn)
}
stopCluster(cl)

sharpness1# log basso e DRPS basso
sharpness1 = as.data.frame(sharpness1)
colnames(sharpness1) = c('Metodo', 'Logaritmico', 'DRPS', 'Brier')
str(sharpness1)
sharpness1 <- sharpness1 %>%
  mutate(across(c(Logaritmico, Brier, DRPS), as.numeric))
sharpness1$peso = length(unique(df_prev$Serie))

write.csv(sharpness1, 'previsioni_df/risultati_vari/rpit_comb/rpit-smooth/sharpness_err_new.csv')
print(xtable(sharpness1, digits = 3), include.rownames=FALSE)


## Costi #####
cl <- makeCluster(4)#detectCores() - 4, outfile = ' ')
#registerDoParallel(cl)
registerDoSNOW(cl)

sss = Sys.time()
costi3 <- foreach(met = metodi, .combine = rbind,  .packages = c('foreach', 'doParallel')) %dopar% {
  costi_tmp =  fun.costi2(metodo = met, df_prev[df_prev$Metodo == met, 5:(ncol(df_prev)-1)], df_prev$vendite[df_prev$Metodo == met])
  return(costi_tmp)
}
stopCluster(cl)
costi4 = as.data.frame(costi3)
colnames(costi4) = c('Metodo', 'Cost(1,4)', 'Cost(1,9)', 'Cost(1,19)')
costi1 <- costi4 %>%
  mutate(across(c(`Cost(1,4)`, `Cost(1,9)`, `Cost(1,19)`), as.numeric))
costi1$peso = length(unique(df_prev$Serie))
write.csv(costi1, 'previsioni_df/risultati_vari/rpit_comb/rpit-err/costi_tab-err.csv')
print(xtable(costi1, digits = 3), include.rownames=FALSE)


## Trade off curves ####
#curve = data.frame( Metodo = c(0), Livello = c(0), Investment= c(0), Lostsales_totali = c(0), CSL_deviation = c(0), Costo = c(0) )
#curve = rep(NA, 5)

cl <- makeCluster(4, outfile = ' ')
#registerDoParallel(cl)
registerDoSNOW(cl)

sss = Sys.time()
curve <- foreach(met = metodi, .combine = rbind, .packages = c('foreach', 'doParallel')) %dopar% {
  curve_tmp =  to_curves2(met, df_prev[df_prev$Metodo == met  , 5:(ncol(df_prev)-1)], 
                          df_prev$vendite[df_prev$Metodo == met])
  return(curve_tmp)
}
stopCluster(cl)
curve
#curve = curve[-1,]
curve = as.data.frame(curve)
colnames(curve) = c('Metodo', 'Livello', 'Investment', 'Lostsales', 'CSLdeviation')
curve <- curve %>%
  mutate(across(colnames(curve)[3:ncol(curve)], as.numeric))
curve$peso = length(unique(df_prev$Serie))
write.csv(curve, 'previsioni_df/risultati_vari/curve_smooth.csv')

#### METTO TUTTO INSIEME #####

folder_pit = '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb'
csv_pit = list.files(folder_pit)#, full.names = T) 
file_pit = csv_pit[c(13,14,17,20)]
cartella = list()
for(el in file_pit){
  cartella[[el]] = list.files(paste0(folder_pit, '/', el))#, full.names = T)
}
metodi = c('SA', 'median','cost19-opt', 'cost9-opt', 'cost4-opt', 'log', 'cl-opt', 'log-opt', 'brier-opt', 'drps-opt')
setwd(folder_pit)
folder = 'previsioni_df/df_prev_OK/altri_finali'
csv = list.files(folder, full.names = T)
df_tmp = map_df(csv, vroom)
interesse = unique(df_tmp$Serie)
grafici = list()
for(met in metodi){
  print(met)
  
  ## per scrivere bene i nomi
  if(strsplit(met, split = '\\[')[[1]][1] == 'iETS(MNN)'){
    titolo = paste0('iETS\\\ textsubscript{', 
                    toupper(strsplit(strsplit(met, split = c('\\[' ))[[1]][2], split = '\\]')), '}')
  } else{ titolo = met}
  
  if(strsplit(met, split = '\\_')[[1]][1] == 'round') titolo = 'SA'
  
  print('graf')
  
  ## leggo file PIT
  
  rpit = vroom(paste0(names(cartella)[1], '/', cartella[[1]][grep(paste0( met), cartella[[1]])]))
  for(i in 2:length(cartella)){
    print(length(cartella)-i)
    ff = paste0(names(cartella)[i], '/', cartella[[i]][grep(paste0( met), cartella[[i]])])
    print('read')
    pt = vroom(ff)
    print('read ok')
    rpit = rbind(rpit, pt)
    print('ok')
  }
  rpit = rpit[-which(rpit$Metodo == '0'),]
  #rpit = rpit[c(which(rpit$Serie %in% interesse), ok),]
  
  print(paste('letto:', met))
  ## Creo grafico
  D = ks.test(rpit$pt, 'punif')#, exact = T)
  
  grafici[[met]] =  ggplot(rpit, aes(x=pt, y = after_stat(density))) + geom_histogram(bins = 15, color = 'black', fill = NA) +
    geom_hline(yintercept = 1, color = 'blue', linetype = 'dashed', size = .6) +
    xlab('rPIT') + ylab('Densità')+
    labs(title=paste0(titolo, ': D = ', round(D$statistic, 4))) + 
    coord_cartesian(ylim = c(0, 2.6)) +
    
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(0.8)),
          panel.background = element_rect(fill = "#EDEAEA", color = NA),  # Sfondo rosato
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          axis.text = element_text(color = "black", size = rel(0.6)),
          axis.title = element_text(color = "black", size = rel(0.7)),
    )
  print('ggplot')
}
do.call(gridExtra::grid.arrange, c(grafici, ncol = 2))

tikz("/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb/rpit_combinazioni.tex", width = 2, height = 5)
combined_grafici = do.call(gridExtra::grid.arrange, c(grafici, ncol = 2))
dev.off()


## sharpness

folder_pit = '/Users/aurora/Desktop/tesi_magistrale/pratica-tesi_mag/previsioni_df/risultati_vari/rpit_comb'
csv_pit = list.files(folder_pit)#, full.names = T) 
file_pit = csv_pit[c(16, 17,20,23)]
cartella = list()
for(el in file_pit){
  cartella[[el]] = list.files(paste0(folder_pit, '/', el))#, full.names = T)
}

setwd(folder_pit)
sharpn = cbind(cartella = names(cartella)[1], vroom(paste0(names(cartella)[1], '/', cartella[[1]][grep('sharpness', cartella[[1]], fixed = T)]))[,-1])
for(i in 2:length(cartella)){
  print(length(cartella)-i)
  ff = paste0(names(cartella)[i], '/', cartella[[i]][grepl( 'sharpness', cartella[[i]])])
  print('read')
  pt = vroom(ff)
  print('read ok')
  sharpn = rbind(sharpn,cbind(cartella = names(cartella)[i], pt[,-1]))
  print('ok')
}


# Unisci il dataframe pesi_df con sharpn in base alla colonna cartella
sharpn_pesato = sharpn$peso*sharpn[,3:5]
sharpn_pesato1 = cbind(Metodo = sharpn[,2], sharpn_pesato)
sharpn_pesato1$Metodo[sharpn_pesato1$Metodo == 'Mediana'] = 'mediana'
sharpn_pesato1$Metodo[sharpn_pesato1$Metodo == 'round_SA'] = 'SA'
pesi = sharpn[sharpn$Metodo %in% c('SA', 'round_SA'),c(1,6)]

prova = as.data.frame(sharpn_pesato1 %>%
                        group_by(Metodo) %>%
                        summarise(across(colnames(sharpn_pesato1)[2:ncol(sharpn_pesato1)], ~ (sum(.)), .names = "{col}")))
#prova = prova[,c(1,6:8)]
colnames(prova) = c('Metodo', 'Logaritmico', 'DRPS', 'Brier')
#sharpn1 = rbind(prova, sharpness, sharpness1)
sharpn1 = as.data.frame(sharpn1 %>%
                          group_by(Metodo) %>%
                          summarise(across(colnames(sharpn1)[2:ncol(sharpn1)], ~ (sum(.)), .names = "{col}")))

prova[,2:4] = prova[,2:4] / (sum(unique(sharpn$peso)))
prova[,2:4] = prova[,2:4] / 12902#sum(pesi$peso)
prova[,2:4] = round(prova[,2:4], 3)
write.csv(prova, 'previsioni_df/risultati_vari/rpit_comb/sharpness_comb.csv')

print(xtable(prova), include.rownames = F)


### COSTI ####

setwd(folder_pit)
costi = cbind(cartella = names(cartella)[1], vroom(paste0(names(cartella)[1], '/', cartella[[1]][grepl('costi_tab', cartella[[1]])]))[,-1])
for(i in 2:length(cartella)){
  print(length(cartella)-i)
  ff = paste0(names(cartella)[i], '/', cartella[[i]][grepl( 'costi_tab', cartella[[i]])])
  print('read')
  pt = vroom(ff)
  print('read ok')
  costi = rbind(costi,cbind(cartella = names(cartella)[i], pt[,-1]))
  print('ok')
}

costi$Metodo[costi$Metodo == 'round_SA'] = 'SA'
costi$Metodo[costi$Metodo == 'Mediana'] = 'mediana'
costi[,3:5] = costi[,3:5] * costi$peso
prova = as.data.frame(costi %>%
                        group_by(Metodo) %>%
                        summarise(across(colnames(costi)[3:ncol(costi)], ~ (sum(.)), .names = "{col}")))
prova$peso = NULL

prova[,2:4] = prova[,2:4] / (sum(unique(costi$peso)))

prova[,2:4] = prova[,2:4] / sum(pesi$peso)
prova[,2:4] = round(prova[,2:4], 3)
write.csv(prova, 'previsioni_df/risultati_vari/rpit_comb/costi_comb.csv')

print(xtable(prova), include.rownames = F)


## CURVE ####
setwd(folder_pit)
costi = cbind(cartella = names(cartella)[1], vroom(paste0(names(cartella)[1], '/', cartella[[1]][grep('curve', cartella[[1]], fixed = T)]))[,-1])
for(i in 2:length(cartella)){
  print(length(cartella)-i)
  ff = paste0(names(cartella)[i], '/', cartella[[i]][grep( 'curve', cartella[[i]], fixed = T)])
  print('read')
  pt = vroom(ff)
  print('read ok')
  costi = rbind(costi,cbind(cartella = names(cartella)[i], pt[,-1]))
  print('ok')
}

# Unisci il dataframe pesi_df con sharpn in base alla colonna cartella
costi1 = costi
costi1 [,4:6] = costi1[,4:6] * costi1$peso
costi1$Metodo[costi1$Metodo == 'round_SA'] = 'SA'
costi1$Metodo[costi1$Metodo == 'Mediana'] = 'mediana'
prova = as.data.frame(costi1 %>%
                        group_by(Metodo, Livello) %>%
                        summarise(across(colnames(costi1)[4:6], ~ (sum(.)), .names = "{col}")))
#prova = prova[,c(1,6:8)]

prova[,3:5] = prova[,3:5] / sum(unique(costi$peso))
#colnames(prova)[3:5] = c('Investment', 'Lostsales', 'CSLdeviation')
write.csv(prova, 'previsioni_df/risultati_vari/rpit_comb/curve_comb.csv')
####

graf1 = ggplot(curve) + 
  geom_line(aes(x = Investment, y = CSLdeviation, group = Metodo, color = Metodo), linetype = "longdash") + 
  geom_point(aes(x = Investment, y = CSLdeviation, group = Metodo, color = Metodo)) +
  ylab('CSL deviation')
graf3 = ggplot(curve) + 
  geom_line(aes(x = log(Investment), y = log(Lostsales), group = Metodo, color = Metodo), linetype = "longdash") + 
  geom_point(aes(x = log(Investment), y = log(Lostsales), group = Metodo, color = Metodo)) 
graf2 = ggplot(curve) + 
  geom_line(aes(x = Lostsales, y = CSLdeviation, group = Metodo, color = Metodo), linetype = "longdash") +
  geom_point(aes(x = Lostsales, y = CSLdeviation, group = Metodo, color = Metodo)) + 
  scale_x_reverse() +  
  ylab('CSL deviation')

curve_comb = (graf1 | graf2 | graf3) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')





