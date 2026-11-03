---
name: modello-robusto
description: Costruisce in modo semi-automatico il miglior modello lineare robusto (y quantitativa) e, se serve, una logistica (y binaria) su un dataset qualsiasi, in R, con verifica su dati mai visti e report HTML per ogni decisione. Usare per /modello-robusto o per trovare il modello migliore/robusto.
---

# Modello robusto semi-automatico

L'ossatura è la procedura del corso di Data Mining (modello lineare robusto, poi logistica), estesa dove serve per ottenere il risultato migliore: l'obiettivo è il modello migliore, non la fedeltà al corso. È validata sul progetto Hotel Booking e collaudata su 16 dataset (pinguini, CEO, studenti, grasso corporeo, affitti, voli, diamanti, Spotify, giochi da tavolo, film, Titanic, caffè, California, bike sharing, assicurazioni, calcestruzzo). Ogni collaudo ha lasciato lezioni in `lezioni_apprese.md`; le soglie, con la loro fonte, sono in `regole_e_soglie.md`.

Il lavoro meccanico è automatico, le scelte di merito restano all'utente. **L'utente vede tre pagine HTML, una per momento decisionale, e le riceve sempre tutte e tre** (SendUserFile, display render):
1. **esplorazione**: serve a decidere insieme y e obiettivo;
2. **modello iniziale**: il termine di confronto, già misurato su dati mai visti;
3. **modello finale**: guadagni, parametri, robustezza, scelte da confermare.

I report 1 e 2 si aprono con un riquadro **"In breve"** in parole semplici: quanto sbaglia su casi mai visti (in unità di y; per una binaria "su 100 casi ne indovina…"), contro chi (nessun modello, modello iniziale), quanto spiega (giudizio a parole), **quale modello si consegna e perché**. Nel messaggio all'utente si riportano queste stesse frasi, non sigle (lezione 116).

**Si consegna il modello che prevede meglio sui dati mai visti**, non il finale per forza: entro il 2% vince il più semplice; se vince l'iniziale si consegna l'iniziale e lo si dice (lezione 117).

Il modello si **sceglie** su un campione di costruzione di dimensione adattiva e si **verifica** sui dati mai visti. Fissata la formula, i coefficienti e l'inferenza si stimano su tutti i dati.

Ogni decisione finisce nel registro (`registro.csv`) con uno di tre tipi:
- **AUTO**: regola netta;
- **PROPOSTA**: serve giudizio; la pipeline sceglie un default motivato, da confermare;
- **UTENTE**: scelta scritta nel config.

Non c'è fretta: la qualità viene prima dei tempi. Le correzioni alla libreria si fanno a blocchi e poi si rilanciano i collaudi già validati.

## 0. Recuperare gli script e preparare R

File: `scripts/` (`rm_lib.R`, `rm_run.R`, `rm_report.R`, `rm_suite.R`, `config_template.json`), `collaudi/` (config dei collaudi + `riferimento.json`), `data/` (dataset dei collaudi), `references/` (`regole_e_soglie.md`, `lezioni_apprese.md`). La fonte e' il repository https://github.com/delleroselorenzo-oss/robust-model: se la cartella della skill non li contiene, si clona il repository e si copia `scripts/` in `mr/`.

**Leggere sempre `lezioni_apprese.md` prima del checkpoint A.**

R nel container (circa 3 minuti, in background mentre si legge il dataset):
```bash
DEBIAN_FRONTEND=noninteractive apt-get install -y -qq r-base-core r-cran-car r-cran-mass r-cran-lmtest \
  r-cran-sandwich r-cran-mice r-cran-leaps r-cran-mgcv r-cran-robustbase r-cran-boot r-cran-knitr \
  r-cran-jsonlite r-cran-base64enc
git clone -q --depth 1 https://github.com/cran/gvlma.git /tmp/gvlma && R CMD INSTALL /tmp/gvlma   # facoltativo
```
`factorMerger` è fuori da CRAN dal 2022: l'optimal grouping è in `og_merge()` (AIC minimo). Il dataset si copia nel container: R gira solo lì.

## 1. Esplorazione (fase `profilo`) → report 0

```bash
cp mr/config_template.json config.json      # dati, output, y (o vuota) e consegna se c'è
Rscript mr/rm_run.R config.json profilo     # pochi secondi
```

Produce `output/report_0_esplorazione.html` (da mandare all'utente con SendUserFile, display render) e `riepilogo_0_profilo.md` (per Claude). La pagina contiene:
- numeri chiave e distribuzione della y;
- **R² univariato** di ogni x con y, con correlazione al quadrato o ANOVA: una sola scala per quantitative e qualitative;
- dispersioni e boxplot delle x più legate;
- distribuzioni di tutte le quantitative;
- dizionario con le descrittive;
- segnalazioni ("cose da decidere insieme").

**La y e l'obiettivo si decidono insieme all'utente, guardando il report 0.**
- Se l'utente indica la y, si usa quella e si segnalano solo i problemi oggettivi.
- Se non c'è una consegna (nessun committente), si propone una domanda di ricerca a partire dal report: 1-3 y candidate, ciascuna con ciò che misura, la distribuzione, il legame con le x e il senso anche come binaria. Poi si rilancia il profilo con la y scelta.

Indagare ogni segnalazione con poche righe di R:

| Segnalazione | Cosa fare | Lezioni |
|---|---|---|
| Valori sospetti, codici numerici isolati (999, -1), **zeri impossibili**, valori estremi isolati | → `codici_mancanti`, `filtro`, `codice_preparazione` | 1, 2, 18, 62 |
| NA = ASSENZA | → `na_come_livello` | 16, 70 |
| y mancante oltre l'1% | Chiedere perché manca (MNAR?) e dichiararlo | 53 |
| Totale = somma di parti | Da chiedere; default `quote` (conserva l'informazione) | 19, 112 |
| Colonne che sono trasformazioni di altre, versioni alternative (es. rivalutate) | Tenerne una | 83 |
| Numeri scritti come testo ("134 min") | Convertire in `codice_preparazione` | 83 |
| Correlazione con y oltre 0,8 | Controllare che non sia la y o una sua parte (leakage) | 4 |
| Identificativi, testo libero | → `escludi` | 70 |
| Unità ripetute (stesso codice su più righe) | → `deduplica` | 69 |
| MISURE RIPETUTE (stesse x, diversa "occasione": la stessa miscela a più età) | → `gruppo = "paste(...)"`: verifica, bootstrap e SE per unità | 113 |
| Righe doppie (identiche in tutto) | Tolte in automatico; `righe_doppie = "tieni"` per tenerle | 110 |
| RITARDO (colonna = y di qualche riga prima) | Escludere, salvo che il valore sia noto al momento della previsione | 102 |
| Liste di etichette ("Card Game,Fantasy") | → `etichette = {"col": 15}` | 75 |
| MOLTE CATEGORIE (regione, azienda, regista: 50+ valori) | Se è una causa plausibile → `effetti_gruppo = ["col"]` (media di gruppo ristretta, fuori fold); altrimenti `escludi` | 118 |
| Date | Estrarre anno/mese in `codice_preparazione`; versione ordinabile per `tempo` | |
| y con valori negativi | Box-Cox su y + c tramite `codice_preparazione` (la soglia della logistica va traslata anche lei) | 54 |

### Checkpoint A: popolazione, variabili, obiettivo

Prima si legge la documentazione del dataset, se esiste (lezioni 21-22). Con AskUserQuestion (al massimo 4 domande per blocco, la raccomandazione come prima opzione) si chiede:
1. popolazione (filtro) e unità di analisi;
2. x da escludere perché note solo **dopo** y o derivate da y;
3. trattamento dei valori sospetti, degli zeri impossibili e dei duplicati;
4. obiettivo **interpretativo** o **previsivo** (Cook o DFFITS al passo 4) e **metrica guida**: interessa il valore *tipico* (`metrica = "tipico"`, MAE) o la *media / il totale atteso* (`"media"`, RMSE: incassi, costi, volumi)? La metrica decide anche la scala di y: se Box-Cox propone una trasformazione, il passo 2 viene rifatto anche con y non trasformata e vince la convalida sulla scala di y (2.2b). Una scala voluta dall'utente va in `decisioni.lambda` (lezioni 85, 89, 106, 111);
5. totale, parti o quote (lezione 19);
6. se c'è una data: verifica temporale e taglio (lezione 26); se interessa la crescita nel tempo, il `trend` (lezione 49);
7. **esiste una tabella collegata con le cause?** (meteo, calendario, anagrafica: lezione 61);
8. pesi: se y è una media su n giudizi, `pesi` con `pesi_modo = "fgls"` (lezione 76);
9. zeri numerosi: modello a due parti (`due_parti`, lezioni 41-43).

Le risposte vanno nel config:
- **Preparazione:** `filtro`, `escludi`, `id`, `fattori`, `codice_preparazione`, `obiettivo`, `metrica`, `codici_mancanti`, `na_come_livello`, `imputa_per_gruppo`.
- **Struttura dei dati:** `deduplica`, `righe_doppie`, `gruppo`, `etichette`, `effetti_gruppo`, `pesi`/`pesi_modo`.
- **Tempo:** `tempo`, `taglio_tempo`, `trend`.
- **Modello:** `due_parti`, `decisioni.totali`.

Formati utili:
- `codice_preparazione`: codice R che modifica `d`, es. `d$agenzia <- as.integer(d$agent != 'NULL')`;
- `filtro`: espressione R, es. `year >= 2011 & !is.na(sqft)`;
- `na_come_livello`: x in cui NA significa "assenza": diventano livello "Nessuno", o dummy `_assente` + mediana se numeriche;
- `codici_mancanti`: lista globale `[-99999]` o per colonna `{"x": [999]}`;
- `imputa_per_gruppo`: es. `{"Lot.Frontage": "Neighborhood"}`;
- `decisioni.totali`: per ogni totale `"totale"`, `"parti"` o `"quote"` (default), es. `{"Gr.Liv.Area": "parti"}`;
- `gruppo`: espressione R che identifica l'unità, es. `"paste(cement, slag, water)"` o `"id_paziente"`;
- `decisioni.tieni`: x da non togliere per collinearità (la compagna resta anch'essa); proposta in automatico quando il passo 1 peggiora fuori campione (lezione 105).

Se l'utente non c'è, si adottano le scelte più prudenti e le si scrive all'inizio del lavoro.

## 2. Modello iniziale (fase `iniziale`) → report 1

```bash
Rscript mr/rm_run.R config.json iniziale    # pochi secondi
```
`output/report_1_iniziale.html` mostra il termine di confronto (tutte le x candidate, nessuna trasformazione, OLS sul campione di costruzione):
- R² e test F;
- peso dei termini (|t|);
- coefficienti con segno e significatività;
- RESET, Breusch-Pagan, VIF massimo;
- diagnostica e partial plot.

Lo si manda all'utente con due righe di lettura: quanto prevede già su casi mai visti (la frase del riquadro), cosa conta, quali problemi si vedono (forma, code, collinearità).

## 3. Modello lineare robusto (fase `lineare`) → report 2

```bash
Rscript mr/rm_run.R config.json lineare     # 0,3-5 minuti con divisione; di più con la convalida della procedura
```

**Divisione** (`dividi()`):
- la costruzione ha 40 osservazioni per parametro candidato, minimo 2000 e massimo 10000, ed è stratificata per y;
- **copertura dei livelli**: ogni livello con almeno 30 casi nel dataset intero ne ha almeno 30 (o la metà) in costruzione (lezione 79);
- la verifica è **temporale** se c'è `tempo` ("stessa epoca" + "futuro"), altrimenti casuale (al massimo 50000 righe); con `gruppo` si dividono unità intere;
- con **n piccolo** si fa la convalida incrociata della procedura (5 fold, ogni fold ripete tutte le scelte).

**Sul campione di costruzione** i passi 0-7 del corso:

| Passo | Cosa fa | Lezioni |
|---|---|---|
| Livelli rari | Decisi sulle frequenze del dataset intero | 79 |
| Imputazione | mice, y esclusa | |
| Pesi | Funzione di varianza FGLS, se richiesti | 76 |
| Collinearità | Esatta (fattori annidati: si toglie quello grossolano); chi-norm e VIF con compagna cercata tra tutte le x; tra fattori decide l'AIC; eccezione di Hill solo fino a VIF 20 | 57, 63, 71 |
| Box-Cox | Se l'IC comprende 1, nessuna trasformazione | |
| Scala di y | Box-Cox, poi confronto in convalida con y non trasformata rifacendo il passo 2 (2.2b) | 106, 111 |
| Forme delle x | Optimal grouping (devianza in forma chiusa: veloce anche con 140 livelli); GAM → forme parametriche (potenze centrate se x è lontana da zero); screening delle interazioni (ΔSBC < -10; importanza di una x = tutti i suoi termini); forma della x nell'interazione scelta in convalida sulla scala di y; soglie dentro le interazioni fattore × numerica (I(x >= c), ΔSBC < -10 - ln n) | 72, 107, 108, 115 |
| Selezione | Ibrida AIC + p-value con gerarchia | |
| Influenza | Cook/DFFITS; **non si eliminano** se sono una coda del fenomeno (media di y spostata di oltre 0,05 sd) o se cancellerebbero un livello | 52, 64 |
| Eteroschedasticità | BP, White, SE HC3 (cluster-robusti per unità con `gruppo`) | 113 |
| Bootstrap | car::Boot; per unità intere con `gruppo` | 113 |
| Confronto | Iniziale vs finale sulla scala originale (smearing) | |

**Verifica sui dati mai visti** (le regole imparate si applicano senza ristimarle; le x fuori campo vengono riportate al bordo, tranne il trend):
- precisione contro naive, contro la sola x più forte e contro il modello iniziale (MAE, RMSE, MASE, R² fuori campione, distorsione) e calibrazione; la stessa formula con y non trasformata viene confrontata sulla metrica guida;
- stabilità dei coefficienti sulla formula congelata;
- deriva e trend: proiettato, congelato o assente (lezione 68);
- livelli finiti in "Altro": test F a posteriori (lezione 80);
- **modello consegnato** (9.9, per la binaria L.11): iniziale o finale, con il motivo;
- **coefficienti finali stimati su tutti i dati** (`coef_finali = "tutti"`, default; lezione 81). Il test H1/H2 resta come informazione.
- **perché non prevede di più** (`diagnosi_tetto`, lezione 87): GAM flessibile sulle stesse x e interazioni (forma o informazione?), errore relativo tipico, **sottogruppo nascosto** nei residui (coda unilaterale oltre 3 sd), segnale nelle colonne escluse ad alta cardinalità. Claude aggiunge nel config `cosa_manca`: la sua lettura di quali dati servirebbero (es. sale d'apertura, marketing, franchise per gli incassi). Finisce nel report 2.

Produce `output/report_2_finale.html` e `riepilogo_lineare.md`. Il report contiene:
- guadagni (grafico e tabella);
- percorso passo per passo;
- formula, effetti in unità di y con IC (anche **condizionati** dentro i gruppi delle interazioni; per x entrate come log(x), il raddoppio), coefficienti HC3;
- previsti contro osservati e calibrazione;
- residui iniziale/finale;
- perché non prevede di più e cosa manca;
- robustezza;
- scelte da confermare e scelte già fatte.

Lo si **guarda** prima di mandarlo (screenshot con playwright), insieme a `2_gam_componenti.png` e `4_influence_plot.png`.

### Checkpoint B: le PROPOSTE

Si presentano solo le proposte che cambiano il modello, con raccomandazione e motivo:

| Proposta | Domanda | Dove si cambia |
|---|---|---|
| Collinearità | Interpretabilità della x tolta | `decisioni.togli_collinearita` + `escludi`, oppure `decisioni.tieni` |
| Totale vs parti | Quale tenere | `decisioni.totali` |
| x tenute per Hill 6.4 | Collinearità che conviene accettare? | `soglie.p_tieni`, `soglie.vif_max_tieni` |
| λ di Box-Cox | | `decisioni.lambda` |
| Chi-norm 0,5-0,8, VIF di singole dummy | Valutare nel merito (lezioni 3, 5, 6) | `escludi`, riferimento |
| Interazioni | Hanno senso sostanziale? | `decisioni.interazioni` (lista `["a:b"]`, nomi originali) |
| Influenti | Chi sono, oltre il 5%? | `decisioni.influenti`: `cook` / `dffits` / `nessuna` |
| Forma di una x | Il grafico GAM suggerisce altro? | `decisioni.trasf_x`, es. `{"x": "log"}` (lin, log, sqrt, poly2, poly3, bin) |

Dalla verifica:
- deriva o trend;
- coefficienti instabili;
- ottimismo oltre il 20%;
- livelli da separare da "Altro";
- modello consegnato = iniziale (i passi non hanno aiutato: dirlo e capire perché);
- un passo che peggiora fuori campione (proposta `decisioni.tieni` o `fino_a`);
- **segnale debole**: va detto apertamente (lezioni 55, 74, 87); si cerca se l'informazione mancante sta in una colonna esclusa (artista, editore) o in una tabella collegata.

Poi si rilancia `lineare`: le scelte diventano UTENTE. Di solito basta un giro, al massimo due.

## 4. Logistica (fase `logistica`): solo se serve

Con `tutto` e `logistica: "auto"` parte solo con:
- una **soglia sostanziale** (`decisioni.soglia_binaria`), oppure un **evento** su più colonne (`evento`, es. `"incasso > 2.5 * budget"`, lezione 88);
- una **y bimodale** (coefficiente di bimodalità > 0,555, misurato sulla y trasformata: lezione 84);
- il **10-90% di zeri**;
- il modello a due parti.

`"si"` la forza, `"no"` la esclude. Le fasce ordinate (y con 10 valori o meno) richiederebbero la logistica ordinale: per ora viene solo segnalata.

### Checkpoint C: la soglia

Si mostrano mediana, Q3 e P90 e si chiede se esiste una soglia sostanziale. Senza risposta si usa la mediana, dichiarandolo.

```bash
Rscript mr/rm_run.R config.json logistica
```
- **Prima della stima:** separazione e varianza quasi nulla; grouping binomiale; **GAM logistico su tutte le numeriche prima della selezione**, con log, log² o polinomio centrato se l'AIC scende di 10 (lezioni 73, 84).
- **Stima e lettura:** step AIC + LRT; OR con IC; effetti marginali; McFadden e HL; DIFCHISQ.
- **Robustezza:** bootstrap, con le "esplosioni" misurate su scala standardizzata (lezione 58).
- **Verifica:** AUC, Brier skill, calibrazione, stabilità.

Il report 2 viene rigenerato con la sezione logistica.

## 5. Consegna

I tre report HTML sono già la consegna di lavoro. Per un committente si prepara un report di sintesi (tipo Docs se disponibile, altrimenti artifact) con:
1. **in una pagina**: cosa spiega y, 5-8 effetti in unità originali, quanto sbaglia su dati nuovi, cosa NON dice (associazioni, popolazione, deriva, limiti);
2. il percorso e le decisioni con il motivo;
3. la verifica;
4. le alternative scartate;
5. la logistica;
6. i limiti dichiarati.

Se il computer dell'utente è raggiungibile, si copiano `output/` e `config.json` nella sua cartella: con questi due file l'analisi si riproduce identica.

## Dopo ogni uso: accumulare esperienza

Ogni trappola nuova diventa una lezione numerata in `lezioni_apprese.md`, con una pull request al repository. Si distingue un **bug** (si corregge sempre) da una **regola nuova** (si aggiunge solo se serve anche su altri dataset e la si annota in `regole_e_soglie.md` con la sua fonte; troppe soglie cucite sui collaudi sono un rischio).

Dopo ogni blocco di correzioni a `rm_lib.R` si lancia il **controllo delle regressioni** (mai modificare gli script mentre un'esecuzione è in corso: lezioni 97, 119):
```bash
Rscript mr/rm_suite.R mr/collaudi /tmp/suite            # ~17 minuti, 2 processi, modalita' veloce
Rscript mr/rm_suite.R mr/collaudi /tmp/suite --aggiorna # quando le differenze sono accettate
```
Confronta termini del modello, λ, MAE, R² e AUC di ogni collaudo con `riferimento.json` e segnala PEGGIORA / migliora / cambia forma. Un nuovo collaudo validato si aggiunge copiando il suo config in `collaudi/`. I config puntano a `data/` (percorsi relativi alla radice del repository); la suite gira anche su GitHub Actions a ogni modifica.

## Regole di comportamento

- L'obiettivo è il modello migliore. Le estensioni rispetto al corso (divisione e verifica, MASE, copertura dei livelli, FGLS, GAM di confronto, stima finale su tutti i dati) si dichiarano nel report, ma non si rinuncia a una tecnica migliore per fedeltà alle slide. Tecniche di machine learning (foreste, boosting, reti) restano fuori da questa skill: arriveranno con la skill della fase ML.
- Mai presentare come causali gli effetti stimati.
- Mai togliere osservazioni o variabili senza una riga nel registro.
- Se la pipeline si ferma, si corregge il config. Se l'errore è nella libreria, la si corregge e si dice all'utente cosa è cambiato.
- Se l'utente vuole capire la teoria: per ogni passo, teoria in breve e lettura degli output (`regole_e_soglie.md`, sezione conseguenze delle violazioni).
