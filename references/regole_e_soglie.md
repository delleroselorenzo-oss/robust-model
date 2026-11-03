# Regole e soglie della routine

Ogni riga dice cosa fa la pipeline, con che soglia, da dove viene la soglia e se la decisione è AUTO (regola netta), PROPOSTA (serve giudizio) o UTENTE (scelta nel config).

**Fonte:**
- **[C]** procedura del corso (slide e dispense);
- **[F]** letteratura (Hill-Griffiths-Lim, Harrell, Winship & Radbill, Pargent, Kuhn & Johnson…);
- **[E]** euristica della routine, fissata in un collaudo: la lezione indicata tra parentesi dice dove. Le [E] sono le prime da rimettere in discussione se un dataset nuovo le smentisce.

## Preparazione e divisione

| Controllo | Regola / soglia | Fonte | Tipo |
|---|---|---|---|
| x con troppi NA | NA > 20% → elimino la x; sotto il 20% imputo (mice, y esclusa) | [C] | AUTO |
| y mancante | righe tolte; oltre l'1% si chiede il perché (MNAR?) | [E] (53) | PROPOSTA |
| NA = assenza | NA quando una colonna gemella vale 0, almeno 20 NA (o 0,5%) → livello "Nessuno" | [E] (16, 70) | PROPOSTA |
| codici numerici finti | 999…, -99999, -1 isolati all'estremo o ≥ 1% delle righe | [E] (18) | PROPOSTA |
| zeri impossibili | zeri < 1% con minimo positivo > 10% della mediana | [E] (62) | PROPOSTA |
| valori estremi isolati | oltre P99 + 3·(P99 − P1), meno dello 0,1% | [E] (62) | PROPOSTA |
| unità ripetute | colonna-codice con valori ripetuti → `deduplica` | [E] (69) | PROPOSTA |
| testo libero | qualitativa con distinti > 30% delle righe → esclusa | [E] (70) | PROPOSTA |
| liste di etichette | separatore in ≥ 30% delle righe, etichette ricorrenti → dummy delle 15 più frequenti | [E] (75) | UTENTE |
| numeri scritti come testo, versioni alternative (prefisso comune, \|cor\| > 0,85) | segnalazione | [E] (83) | PROPOSTA |
| leakage | y ricostruibile (R² > 0,99) da altre colonne; \|cor\| > 0,8 da controllare | [E] (4, 19) | PROPOSTA |
| divisione | costruzione = 40 oss. × parametro candidato (min 2000, max 10000), stratificata per y | [F] Harrell 15-20 oss./parametro; 40 [E] (25) | AUTO |
| copertura dei livelli | ogni livello con ≥ 30 casi nel dataset ne ha ≥ 30 (o la metà) in costruzione; selezione sulle x non distorce l'OLS | [F] Winship & Radbill 1994; 30 [E] (79) | AUTO |
| livelli rari | "Altro" se < 30 casi nel dataset intero o < 10 in costruzione | [F] decisione senza y su tutti i dati (Kuhn & Johnson); soglie [E] (79) | PROPOSTA |
| n piccolo | 80% < 15 oss./parametro → convalida incrociata della procedura (5 fold) | [F] Harrell | AUTO |
| pesi | FGLS: var = a + b/peso stimata dai residui OLS, peso = 1/var | [F] Hill 8.5 | UTENTE |

## Modello lineare (campione di costruzione)

| Passo | Regola / soglia | Fonte | Tipo |
|---|---|---|---|
| collinearità esatta | coefficiente NA → tolgo la x aliasata; fattori annidati → tolgo il grossolano; totale = somma delle parti → si chiede | [C]; annidati [E] (71); totali (19) | PROPOSTA |
| VIF | GVIF^(1/(2Df))² > 5; compagna cercata tra tutte le x; tolgo la meno utile (F parziale; AIC se c'è un fattore) | [C] VIF 5; compagna e AIC [E] (48, 57) | PROPOSTA |
| eccezione di Hill | tengo una x collineare se p del test F parziale < 0,01 e VIF ≤ 20 | [F] Hill 6.4; p e tetto [E] (36, 63) | PROPOSTA |
| qualitative | chi-quadro normalizzato > 0,8 → tolgo una (decide l'AIC) | [C] | AUTO |
| Box-Cox | λ arrotondato a −1, −0,5, 0, 0,5, 1 dentro l'IC al 95%; se l'IC comprende 1, nessuna trasformazione | [C]; "IC comprende 1" [E] | PROPOSTA |
| optimal grouping | fusione dei livelli, AIC minimo (marginale vs residui parziali) | [C] (factorMerger) | AUTO |
| forme delle x | GAM: edf > 1,5 → lin/poly2/poly3/log/sqrt/bin per AIC, la più semplice entro 2 punti; potenze centrate se \|media\| > 3 sd | [C]; centratura [E] (72) | AUTO |
| interazioni | a due vie tra le x più forti: ΔSBC < −10, nessun coefficiente NA | [E] | PROPOSTA |
| selezione | stepAIC (AIC, SBC); ibrido = AIC + elimino p > 0,05 con gerarchia | [C] | PROPOSTA |
| influenza | Cook > 4/(n−p−2) (interpretativo) o \|DFFITS\| > 2√(p/n) (previsivo) | [C] (Fox) | PROPOSTA |
| influenti come coda | non elimino se toglierli sposta la media di y di oltre 0,05 sd | [E] (52) | PROPOSTA |
| livello cancellato | non elimino gli influenti di un livello che scenderebbe sotto 10 casi | [E] (64) | AUTO |
| eteroschedasticità | Breusch-Pagan, White; SE HC3 | [C] | AUTO |
| bootstrap | per casi, R = 999-1999; ~200 bastano per gli SE, ~1000 per gli IC | [C]; [F] Efron-Tibshirani | AUTO |

## Verifica (dati mai visti)

| Cosa | Regola / soglia | Fonte | Tipo |
|---|---|---|---|
| x fuori campo | riportate al bordo dell'intervallo di costruzione (tranne il trend) | [E] (65) | AUTO |
| precisione | MAE, RMSE, MASE (< 1 = meglio della mediana), R² fuori campione, distorsione; metrica guida scelta al checkpoint A: `metrica = "tipico"` (MAE) o `"media"` (RMSE) | [E] (55, 85) | UTENTE |
| y non trasformata | se λ ≠ 1, la stessa formula con y originale viene confrontata sulla metrica guida; se vince oltre il 2% → proposta λ = 1 | [E] (89) | PROPOSTA |
| finale vs iniziale | finale peggiore dell'iniziale di oltre il 2% → segnalato | [E] | PROPOSTA |
| stabilità | formula congelata riestimata sulla verifica: ≥ 90% dei significativi con lo stesso segno | [E] | AUTO |
| livelli in "Altro" | test F a posteriori sui livelli con ≥ 30 casi; p < 0,01 → tenerli separati | [E] (80) | PROPOSTA |
| trend | proiettato, congelato o assente: vince il MAE sul futuro (congelato se migliore di oltre il 2%) | [E] (68) | PROPOSTA |
| coefficienti finali | formula scelta sul campione, stima e inferenza su tutti i dati; test H1/H2 informativo | [E] (81) | AUTO |
| forma o informazione | GAM flessibile sulle stesse x e righe: se non fa meglio di 0,02 di R², mancano informazioni | [E] (87) | AUTO |
| colonne escluse | media leave-one-out ristretta (shrinkage) dei residui nello stesso gruppo: R² > 0,02 = informazione utile | [F] Pargent 2022, Gelman-Hill; 0,02 [E] (87) | PROPOSTA |

## Logistica

| Controllo | Regola / soglia | Fonte | Tipo |
|---|---|---|---|
| serve? | soglia sostanziale o `evento`; bimodalità > 0,555 **sulla y trasformata**; 10-90% di zeri | [F] coefficiente di bimodalità; [E] (84) | AUTO / UTENTE |
| soglia | mediana se non c'è una soglia sostanziale | [C] | PROPOSTA |
| separazione | varianza quasi nulla (freqRatio > 19) o nessun evento fuori dalla moda → tolgo la x | [C] | AUTO |
| celle | grouping binomiale; celle con < 5 eventi fuse col livello più simile | [C] | AUTO |
| forma sul logit | GAM logistico su tutte le numeriche prima della selezione; log, log², poly2, poly3 se l'AIC scende di 10 | [E] (73, 84) | AUTO |
| coefficienti esplosi | SE > 5 → tolgo il termine; bootstrap: \|b·sd(x)\| > 10 in oltre il 10% delle repliche → OR non affidabili | [E] (58) | AUTO |
| selezione | step AIC + ibrido LRT p > 0,05 | [C] | PROPOSTA |
| adattamento | McFadden; Hosmer-Lemeshow solo con n < 5000 (con n grande rifiuta sempre: decide la calibrazione) | [C]; [E] (29) | descrittivo |
| verifica | AUC, Brier skill, log-loss, calibrazione, stabilità | [F] | AUTO |

## Conseguenze delle violazioni (per spiegare le scelte)

- **Collinearità**: l'OLS resta corretto ma le varianze esplodono (SE grandi, segni instabili). Con collinearità esatta il coefficiente non è identificato.
- **Forma funzionale sbagliata** (RESET): stimatori distorti e inconsistenti. È il problema più grave.
- **Non normalità**: con n grande l'inferenza resta valida asintoticamente. Residui asimmetrici segnalano una y da trasformare.
- **Outlier influenti**: possono spostare le stime. La regressione robusta (IRLS) li pesa meno invece di toglierli. Se sono la coda del fenomeno, toglierli distorce la media prevista.
- **Eteroschedasticità**: l'OLS resta corretto e consistente ma non efficiente, e gli SE OLS sono distorti → SE di White, oppure FGLS se la varianza segue una forma nota.
- **Trasformazione di y**: il modello insegue il valore tipico (mediana); per la media serve lo smearing, e con code pesanti la y non trasformata può prevedere meglio la media.
- **Logistica**: con separazione la MLE non esiste (coefficienti verso ±∞, SE enormi).
