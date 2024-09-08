### Creazione df_storico ####
dati = read.csv('m5_data/original_m5/sales_train_evaluation.csv')
str(dati) # 30490 1947 (1941 dates)
length(unique(dati$item_id)) # 3049
length(unique(dati$cat_id)) # 3
tail(colnames(dati)) # d_1941

### CALENDARIO
calendario = read.csv('m5_data/original_m5/calendar.csv')
str(calendario)
length(unique(calendario$d))
head(calendario) #2011-01-29 
calendario[1941,] # 2016-05-22 

## new ####
df_storico = calendario[1:(which(calendario$d == (colnames(dati))[dim(dati)[2]])),] # inserisco info calendario
t_df = data.frame(t(dati))
prova = t_df[c(1, 7:dim(t_df)[1]),]
colnames(prova) = prova['id',]
str(prova)
View(prova)

prova = prova[2:dim(prova)[1],]
pp = prova

for(i in 1:ncol(pp)){
  pp[,i] = as.numeric(pp[,i])
  print(i)
}

str(pp)
pp [,'d'] = rownames(pp)

df_storico1 = merge(df_storico, pp, by = 'd')
str(df_storico1)

write.csv2(df_storico1, file = "df_storico1.csv", row.names = FALSE) # 1 osservazione = num.vendite di tutti i prodotti in un certo giorno.

      
#### Creo df
library(readxl)
una_serie = read_excel("nomi_df.xlsx")
str(una_serie)
una_serie = as.data.frame(una_serie)
intermi = which(sapply(
  sapply(una_serie[,2],function(x) strsplit(x, split = '1|2|3|4|5|6|7|8|9')), # divido la stringa quando incontro un numero
  function(x) 'intermittent' %in% x)) # quali serie sono intermittenti

# sku$id[which(sku$SBC %in% attr(intermi, 'names'))] # id delle serie intermittenti
da_elaborare = which(colnames(dati) %in% sku$id[which(sku$SBC %in% attr(intermi, 'names'))])
sss = Sys.time()
for (serie in da_elaborare){
  creo_mini(dati = dati, prezzi = prezzi1, sku = sku, identificativo = serie, cartella = 'intermittenti')
}
eee = Sys.time()
eee-sss

### SBC
eliminati = c()
categ = data.frame(Serie = c(0), Categoria = c(0), ADI = c(0), CV2 = c(0))

for(i in 1:length(intermi)){
  print(length(intermi)-i)
  nome = attr(intermi, 'names')[i]
  file_path  = paste0( 'datasets/intermittenti/', nome, '.csv')
  inter1 = read.csv(file_path)

  # elimino le serie con meno del 20% di 0 dopo la prima osservazione diversa da 0 (voglio solo quelle intermittenti)
  if(prop.table(table(inter1$vendite == 0))[2] < .2){
    eliminati = c(eliminati, nome)
    next
  }
  categ = SBC_fun(dati = inter1$vendite, serie = nome, categorie =  categ)
}
cv2 = categ$cv2
adi = categ$p

plot(log(cv2), log(adi), ylab ='ln(CV2)', xlab = 'ln(ADI)')
abline(h = log(1.32), col = 2)
abline(v = log(.49), col = 2)

                  
