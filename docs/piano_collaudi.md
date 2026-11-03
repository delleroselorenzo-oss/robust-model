# Piano dei collaudi della skill modello-robusto

Obiettivo: almeno 2 dataset per ogni situazione difficile. Ogni dataset passa per il flow completo (profilo → checkpoint A con l'utente → lineare → checkpoint B → logistica solo se serve → riepilogo + lezioni).
Per ognuno si annota: gira senza errori? tempo; **quante PROPOSTE sono state cambiate** (qualità delle regole); MASE contro il naive e contro la sola x più forte; lezioni nuove.

## Già collaudati
| # | Dataset | y | Situazioni coperte | Esito |
|---|---|---|---|---|
| 0a | Hotel Booking (progetto) | adr | n grande, data/deriva, qualitative, NULL | ok, riproduce il progetto |
| 0b | Banca d'Italia (Formitalia02) | YL | leakage per componenti, collinearità a catena | ok dopo 2 correzioni |
| 0c | Ames Housing | SalePrice | NA = assenza, totali = parti, n piccolo vs parametri | ok dopo 5 correzioni |
| 0d | FIFA (Atleti_Performance) | Sprint speed | x gemella di y, solo numeriche | ok; aggiunto confronto con la sola x più forte |

## Da collaudare (fonti pubbliche raggiungibili: GitHub, TidyTuesday, mirror CRAN)
| # | Dataset | n | y proposta | Situazioni |
|---|---|---|---|---|
| 1 | Palmer penguins ✅ | 344 | body_mass_g | n piccolo, NA, qualitative — convalida della procedura; MASE 0,36; R² fuori 0,86; finale = iniziale con meno termini; 2 regole corrette (Hill con p-value, Box-Cox con IC che comprende 1); logistica saltata (bimodalità 0,53); proposte cambiate dall'utente: 0 |
| 2 | CEO salary (wooldridge ceosal2) ✅ | 177 | salary | n piccolo, y asimmetrica — riconosce le colonne-copia; ritrova log-log; R² 0,53 in-sample ma 0,12 fuori campione (MASE 0,83): stipendi poco prevedibili; logistica > 1 milione AUC 0,72; aggiunti OR per incrementi; proposte cambiate dall'utente: 0 |
| 3 | Student performance (UCI, math) ✅ | 395 | G3 (voto 0-20) | n piccolo, zeri, leakage (G1, G2 esclusi) — introdotto il modello a due parti; voto poco prevedibile (MASE 0,97); scoperto artefatto assenze = 0 → separazione: nuovo controllo per x numeriche + rimozione automatica termini esplosi; logistica onesta AUC 0,64; proposte cambiate dall'utente: 0 (ma una richiesta nuova: due parti) |
| 4 | Body fat (TH.data) ✅ | 71 | DEXfat | n piccolissimo, x gemelle — R² fuori 0,90, MASE 0,29; collinearità tra 4 indici gemelli risolta (stabile 80%); finale 5% peggiore dell'iniziale in previsione (n minuscolo); logistica obesità > 35% AUC 0,96 ma bootstrap tutto esploso → nuovo avviso; proposte cambiate dall'utente: 0 |
| 5 | Affitti San Francisco (TidyTuesday 2022-07-05) ✅ | 63k (2011-18 con superficie) | price | n enorme, data, alta cardinalità, NA strutturali — introdotto il trend (+12,3%/anno 2011-15; sul 2016-18 rallenta); collinearità tra fattori ora con AIC (tiene il quartiere, 80→21 gruppi); MASE 0,46 (stessa epoca), 0,56 (futuro); proposte cambiate dall'utente: 1 (tenere gli annunci anomali) + richiesta nuova: trend |
| 6 | Voli NYC 2013 (+ meteo orario unito) ✅ | 336.776 (costruzione 5.640, verifica 50.000) | arr_delay + 87 | n grande, y con negativi e coda lunga, segnale debole, MNAR (cancellati), unione tabelle — solo variabili pre-partenza: R2 fuori campione 0,07 -> 0,11 col meteo, MAE 25,1 min (MASE 0,98); logistica >15 min AUC 0,71; trovati 5 difetti (influenti unilaterali, y mancante, compagna VIF, esplosione logit, smearing negli IC) |
| 7 | Diamonds (ggplot2) ✅ | 53.940 (costruzione 2.000) | price | n grande, non linearità, collinearità estrema, errori di registrazione — sqrt(y), carat cubico + interazioni; R2 fuori campione 0,96, MAE 410 $ (MASE 0,15); trovati 5 difetti (profilo zeri/estremi, eccezione di Hill senza tetto, categoria cancellata dalla pulizia, estrapolazione dei polinomi, incremento degli effetti) |
| 8 | Spotify songs (TidyTuesday 2020-01-21) ✅ | 28.356 canzoni (32.833 righe, deduplicate) | track_popularity | unita' ripetute, testo libero, fattori annidati, y limitata con 9% di zeri (due parti) — lineare R2 fuori campione 0,12 (segnale debole, onesto); zeri: logistica AUC 0,76 dopo la forma dell'anno sul logit; trovati 6 difetti (unita' ripetute, testo libero, NA=assenza su 5 NA, annidamento, potenze non centrate, GAM logistico dopo la selezione) |
| 9 | Board games (TidyTuesday 2019-03-12) ✅ | 10.532 | average_rating | liste di etichette, pesi (n valutazioni), codici 0, testo libero — WLS con 1/n peggiorava (R2 0,35); FGLS = OLS (0,41); MAE 0,50 (MASE 0,74); nuove funzioni: etichette, pesi/FGLS |
| 10 | Film del Bechdel test (FiveThirtyEight) ✅ | 1.794 | intgross_2013 (scelta insieme all'utente) | domanda di ricerca da definire, versioni rivalutate, numeri come testo, liste di etichette, variabili post-uscita, NA a blocchi — log(y); R2 fuori campione 0,41, MAE 122 mln (MASE 0,79); Bechdel non significativo; primi report HTML 0/1/2 consegnati |
| 11 | Titanic ✅ | 891 | Survived (scelta: y binaria) | y binaria nativa (nuova modalita'), tariffa per biglietto (valore di gruppo), NA (eta' 19,9%), cabina esclusa — AUC 0,859 in convalida, 81% classificati; titolo, classe, eta', famiglia |
| 12 | Coffee ratings ✅ | 1.338 | total_cup_points | y = somma dei voti sensoriali (esclusi), errori (0 punti, altitudini 190 km), Robusta tenuti — verifica piccola -> convalida; percorso fuori campione; R2 ~0,2; conta l'ente che valuta; segnale nel proprietario e nella sessione d'assaggio |
| 13 | California housing ✅ | 20.640 | median_house_value | censura a 500.001 (due parti con tappo), totali -> rapporti, coordinate collineari (protette), zona a griglia — R2 fuori campione 0,70, MAE 36.400 $ (MASE 0,47); logistica sopra il tappo AUC 0,95 |
| 14 | Bike sharing (giornaliero) ✅ | 731 | registered | data/deriva 2011→2012, totale = casual + registered (leakage), colonna ritardata (cnt di due giorni prima) — verifica sul futuro (ago-dic 2012) + trend (+1.640 noleggi/anno); x protette season e temp: MAE futuro 704 -> 690, R2 fuori campione 0,81 -> 0,83 (senza protezione 723); senza trend MAE 1.825 |
| 15 | Medical insurance ✅ | 1.338 | charges | asimmetria, interazione fumo × BMI, y bimodale, una riga doppia — prima esecuzione: log(y) e R2 in convalida 0,53 (peggio dell'iniziale 0,75); trovati 4 difetti (scala di y scelta solo da Box-Cox, x trasformate escluse dallo screening delle interazioni, nessuna soglia, effetti medi che nascondono l'interazione); dopo: y non trasformata, fumo x BMI con soglia BMI 30, R2 fuori campione 0,87, MAE 2.360 $ (-44%); fumo +13.500 $ sotto BMI 30, +33.100 sopra; 4,7% di casi con spese molto sopra il previsto = variabile mancante |
| 16 | Concrete strength (UCI) ✅ | 1.030 | strength | non linearita' (eta'), interazioni, 25 righe doppie, stessa miscela provata a piu' eta' (427 miscele) — rapporti acqua/cemento e legante; legante = somma delle parti: default ora "quote" (R2 fuori campione 0,835 col solo totale, 0,854 con le quote), MAE 4,7 MPa (MASE 0,36); effetto dell'eta' come raddoppio (+5 MPa); convalida per miscela: stesso errore, ma SE +25% |

Con i 4 già fatti si arriva a 20; il primo giro si chiude a 30 con dataset scelti dall'utente (anche da Kaggle, scaricati in creare_modelli).


## Controllo delle regressioni
`scripts/rm_suite.R collaudi <uscita>`: 12 collaudi (01-10, hotel, FIFA) in modalita' veloce, ~9 minuti, confronto con `collaudi/riferimento.json` (aggiornato il 01/11/2026 dopo la revisione delle regole: nessun peggioramento).
