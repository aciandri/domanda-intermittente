# Combinazioni di previsioni probabilistiche per la domanda intermittente
Questo archivio contiene i codici utilizzati nella tesi "Combinazioni di previsioni probabilistiche per la domanda intermittente" per l'analisi di un campione casuale di 12902 serie estratto dai dati forniti da Kaggle per la "M5 Competition" (https://www.kaggle.com/c/m5-forecasting-accuracy/data). Contatto: auroraciandri@gmail.com.

Il file **funzioni.R** contiene tutte le funzioni (scritte da me) utilizzate nell'analisi e viene richiamata in ognuno dei file usati per svolgere l'analisi, mentre il file **grafici_illustrativi.R** contiene un codice semplice per la produzione di alcuni dei grafici dei capitoli 1 e 4.

### Descrizione dei dati
I dati utilizzati nell'analisi, sono stati forniti da Kaggle per la "M5 Competition", il cui scopo era la previsione di 42840 serie storiche relative alle vendite unitarie di prodotti commercializzati da Walmart tra il 29/01/2011 e il 23/05/2016 (1941 giorni). I dati presentano una struttura gerarchica complessa, di cui si considera solo il livello più basso (30490 serie storiche). 

Oltre allo storico delle vendite unitarie per prodotto, i *dataset* forniscono anche alcune informazioni esogene riguardo al bene stesso e agli eventi di calendario che potrebbero influenzare l'andamento delle vendite, come ad esempio l'attività a scopo promozionale SNAP (*Supplemental Nutrition Assistance Program*), e alcune festività.

Nello specifico, i dati contengono 3049 prodotti venduti in 10 negozi (locati in California, Texas e Wisconsin) e classificati in 7 dipartimenti, a loro volta disaggragati in 3 categorie (*Hobbies, Foods, Household*).

I dati originali forniti da Kaggle sono suddivisi in 3 *datasets*, relativi rispettivamente allo storico delle vendite del prodotto (ogni riga corrisponde allo storico delle vendite di una SKU), alle informazioni di calendario e ai prezzi di vendita e alle caratteristiche della SKU specifica (e.g., dipartimento, categoria, ...).

I dati disponibili (1969 giorni, ossia circa 5.4 anni) sono stati suddivisi in tre parti, in base alla divisione adottata durante la competizione:
- Il periodo 29/01/2011 - 27/03/2016 (circa 1885 osservazioni) è stato utilizzato come \textit{training set} dei metodi individuali (introdotti nel capitolo 2 della tesi). Per ogni serie, si è selezionato solo il periodo a partire dalla prima vendita positiva del prodotto.
- Il periodo 28/03/2016 - 24/04/2016 (28 osservazioni) è stato usato come *validation set* per il calcolo dei pesi per la combinazione dei metodi individuali (introdotti nel capitolo 3 della tesi), e per la valutazione delle previsioni dei metodi individuali.
- Infine, il periodo 25/04/2016 - 22/05/2016 (28 osservazioni) è stato usato come *test set* per la valutazione delle previsioni delle combinazioni dei metodi di previsione probabilistica.

### Analisi dei dati
Per l'analisi dei dati sopra descritti, si sono eseguiti 4 passaggi riportati nei file e cartelle presenti nella *repository*. Si è iniziato con un'elaborazione preliminare dei dati (**preliminari.R**), in cui:
1. Si è ricavato un dataset per ognuna delle serie analizzate in cui ogni riga rappresenta una vendita e si sono poi trasformate alcune delle covariate presenti nei dati.
2. Si è effettuata una scrematura dei dati in base ad una soglia arbitraria, scartando le serie che contengono meno del 20% di osservazioni pari a zero, in quanto non intermittenti. Si sono inoltre eliminate le serie caratterizzate da vendite costanti nulle nel *validation set*.
3. Si è applicata la classificazione SBC.

Si sono poi applicati iterativamente i quattordici metodi individuali descritti nel capitolo 2 (**individuali.R**), i cui risultati sono stati usati per il calcolo delle combinazioni di previsioni descritte nel capitolo 3. Il codice usato per quest'ultima operazione è contenuto nella cartella "**combinazioni**" ed è diviso in 4 file:

- **combinazioni_semplici_logopt.R**: contiene il calcolo dei pesi semplici e di quelli ottimali per i punteggi logaritmico e CL (*Censored Likelihood*).
- **brier_score.R**: contiene il calcolo dei pesi ottimali in base al punteggio di Brier.
- **drps_score.R**: contiene il calcolo dei pesi ottimali in base al punteggio DRPS.
- **pso2.R**: contiene il calcolo dei pesi ottimali basati sui costi di inventario simulati ipotizzando un classico *newsvendor problem*.

Infine, si è svolta una valutazione dei risultati (**valutazione.R**) in base alla calibrazione, *sharpness* e *performance* di inventario.

In alcuni file (ad esempio, valutazione.R) si mostra il codice per un caso specifico (ad esempio, per la valutazione delle combinazioni per serie strettamente intermittenti). 


