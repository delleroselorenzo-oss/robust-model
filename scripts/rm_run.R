######################################################################
# rm_run.R - esegue la procedura del modello robusto
#   Rscript rm_run.R config.json profilo     # solo profilo del dataset (fase 0a)
#   Rscript rm_run.R config.json iniziale    # solo il modello iniziale (termine di confronto) + report_1
#   Rscript rm_run.R config.json lineare     # passi 0-7 (y quantitativa)
#   Rscript rm_run.R config.json logistica   # parte 1b (richiede 'lineare')
#   Rscript rm_run.R config.json tutto       # lineare + logistica
# Output in cfg$output: registro.csv, riepilogo_*.md, tabelle/, grafici/, stato_*.rds
######################################################################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("uso: Rscript rm_run.R config.json profilo|lineare|logistica|tutto")
qui <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(qui, "rm_lib.R")); source(file.path(qui, "rm_report.R"))
cfg <- jsonlite::fromJSON(args[1], simplifyVector = TRUE)
fase <- args[2]
# modello a due parti: true = zeri ("zero o positivo?"), {"tappo": v} = valori censurati ("sopra il tappo?")
dp_tappo <- if (is.list(cfg$due_parti) && !is.null(cfg$due_parti$tappo)) as.numeric(cfg$due_parti$tappo) else NULL
dp_attivo <- isTRUE(cfg$due_parti) || !is.null(dp_tappo)
dp_parte <- function(d, y) if (!is.null(dp_tappo)) d[d[[y]] < dp_tappo, ] else d[d[[y]] > 0, ]
if (is.null(cfg$output)) cfg$output <- file.path(dirname(normalizePath(args[1])), "output_modello")
rm_init(cfg$output); .RM$veloce <- isTRUE(cfg$veloce)
t0 <- Sys.time()
log_t <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

tab_reg <- function(fasi) { r <- .RM$reg[.RM$reg$fase %in% fasi, c("passo", "oggetto", "valore", "azione", "tipo")]
  r$valore <- substr(r$valore, 1, 120); rownames(r) <- NULL; r }

# ===================================================================
if (fase == "profilo") {
  d <- leggi_dati(cfg)
  ord <- !is.null(cfg$tempo) && nzchar(cfg$tempo)
  if (ord) { tt <- tryCatch(with(d, eval(parse(text = cfg$tempo))), error = function(e) NULL); if (!is.null(tt)) d <- d[order(tt), ] else ord <- FALSE }
  pr <- profilo(d, cfg$y, ordinato = ord); salva_tab(pr, "00_profilo")
  md_add("# Profilo del dataset: ", basename(cfg$dati), "\n")
  md_add(sprintf("%d righe, %d colonne. y = `%s`.\n", nrow(d), ncol(d), if (is.null(cfg$y)) "(da scegliere)" else cfg$y))
  if ((nd <- righe_doppie(d)) > 0) md_add(sprintf("**Righe doppie:** %d righe identiche in tutte le colonne: verranno tolte (config righe_doppie = \"tieni\" per tenerle).\n", nd))
  if (!is.null(cfg$y) && cfg$y %in% names(d) && is.numeric(d[[cfg$y]])) {
    yy <- d[[cfg$y]]
    md_add(sprintf("y: media %.3g, mediana %.3g, sd %.3g, min %.3g, max %.3g, asimmetria %.2f, valori <= 0: %d, NA: %d\n",
                   mean(yy, na.rm = TRUE), median(yy, na.rm = TRUE), sd(yy, na.rm = TRUE), min(yy, na.rm = TRUE),
                   max(yy, na.rm = TRUE), skew(yy), sum(yy <= 0, na.rm = TRUE), sum(is.na(yy))))
  }
  if (!is.null(cfg$consegna) && nzchar(cfg$consegna)) md_add("\n**Consegna:** ", cfg$consegna, "\n")
  if (is.null(cfg$y) || !nzchar(cfg$y)) {
    # y non indicata: candidate = quantitative continue, non identificativi, pochi NA.
    # La scelta resta all'utente, guidata dalla consegna (la pipeline non decide la y).
    num <- names(d)[sapply(d, is.numeric)]
    cand <- do.call(rbind, lapply(num, function(v) { x <- d[[v]]; nd <- length(unique(x[!is.na(x)]))
      data.frame(variabile = v, distinti = nd, pct_NA = round(100 * mean(is.na(x)), 1),
                 pct_zeri = round(100 * mean(x == 0, na.rm = TRUE), 1), asimmetria = round(skew(x), 2),
                 mediana = signif(median(x, na.rm = TRUE), 4)) }))
    if (!is.null(cand)) {
      cand <- cand[cand$distinti > 20 & cand$distinti < nrow(d) * 0.98 & cand$pct_NA < 20, ]
      cand$nota <- ifelse(cand$pct_zeri > 30, "molti zeri: filtrare y > 0 o modellare prima zero/positivo", "")
      rownames(cand) <- NULL
      md_add("\n## y candidate (da confrontare con la consegna)\n")
      md_add("Quantitative continue, non identificativi, NA < 20%. Una buona y risponde alla consegna, ",
             "non e' una somma o una trasformazione di altre colonne e ha senso anche come binaria.\n")
      md_tab(cand)
      salva_tab(cand, "00_y_candidate")
    }
  }
  md_add("\n## Variabili\n"); md_tab(pr[, setdiff(names(pr), "livello_top")])
  md_add("\n## Variabili da guardare (segnalazioni)\n")
  md_tab(pr[pr$segnalazione != "" | pr$valori_sospetti != "", c("variabile", "tipo", "distinti", "pct_NA",
                                                                    "valori_sospetti", "segnalazione")])
  tot <- pr[pr$totale_di != "", ]
  if (nrow(tot)) md_add("\n**Totali che sono somma di altre colonne** (domanda per il checkpoint A, `decisioni.totali`): ",
                        paste0(tot$variabile, " = ", tot$totale_di, collapse = "; "), "\n")
  trf <- pr[pr$trasformazione_di != "", ]
  if (nrow(trf)) md_add("\n**Colonne che sono trasformazioni di altre** (escludere la copia: la forma funzionale la sceglie la pipeline): ",
                        paste0(trf$variabile, " <- ", trf$trasformazione_di, collapse = "; "), "\n")
  na_ass <- pr$variabile[pr$na_significa != ""]
  if (length(na_ass)) md_add("\n**NA che sembrano 'assenza'** (candidati per `na_come_livello`): ", paste(na_ass, collapse = ", "), "\n")
  md_salva("riepilogo_0_profilo")
  if (!isTRUE(cfg$veloce)) tryCatch(report_esplorazione(d, pr, cfg), error = function(e) message("report esplorazione: ", conditionMessage(e)))
  log_t("profilo scritto in", cfg$output, "(report_0_esplorazione.html)"); quit(save = "no")
}

# ===================================================================
if (fase == "iniziale") {     # anteprima: il modello iniziale sul campione di costruzione, prima di tutto il resto
  y <- cfg$y
  if (is.null(y) || !nzchar(y)) stop("y non indicata")
  d_all <- prepara_base(leggi_dati(cfg), cfg)
  if (dp_attivo) d_all <- dp_parte(d_all, y)
  div <- if (isFALSE(cfg$verifica)) list(build = d_all[setdiff(names(d_all), ".tempo")], descr = "verifica disattivata") else dividi(d_all, cfg)
  d <- prepara_apprendi(div$build, cfg)
  fit0 <- modello_iniziale(d, y)
  fo0 <- tryCatch(fuori_iniziale(fit0, d, div, cfg, y_binaria(d[[y]], cfg)), error = function(e) { message("fuori campione iniziale: ", conditionMessage(e)); NULL })
  if (!isTRUE(cfg$veloce)) report_iniziale(fit0, d, cfg, div, fo0)
  log_t(div$descr); log_t("modello iniziale scritto (report_1_iniziale.html)"); quit(save = "no")
}

# ===================================================================
if (fase %in% c("lineare", "tutto")) {
  y <- cfg$y
  if (is.null(y) || !nzchar(y)) stop("y non indicata: sceglierla con l'utente (profilo + consegna) e scriverla nel config")
  .RM$reg <- .RM$reg[0, ]          # registro pulito: niente righe rimaste da esecuzioni precedenti
  # tabelle pulite: una tabella rimasta da un'esecuzione precedente (verifica 80/20 poi passata a convalida)
  # finiva nel report accanto ai numeri nuovi (caffe': 1,653 su 270 righe sotto un verdetto di 1,613)
  unlink(list.files(file.path(cfg$output, "tabelle"), pattern = "^[1-9]_.*\\.csv$", full.names = TRUE))
  log_t("passo 0: preparazione e divisione costruzione/verifica")
  d_all <- prepara_base(leggi_dati(cfg), cfg)
  if (!is.numeric(d_all[[y]])) stop("la y deve essere numerica")
  d_full <- NULL
  if (dp_attivo) {     # modello a due parti: lineare sulla parte "normale", logistica sull'altra (zeri o tappo) su tutti
    d_full <- d_all; d_all <- dp_parte(d_all, y)
    registra("0_preparazione", "0.1", "modello a due parti", if (is.null(dp_tappo))
             sprintf("lineare su %d righe con y > 0; logistica su tutte le %d (y = 0: %d)", nrow(d_all), nrow(d_full), sum(d_full[[y]] == 0)) else
             sprintf("lineare su %d righe con y < %s; logistica 'sopra il tappo' su tutte le %d (censurate: %d)", nrow(d_all), dp_tappo, nrow(d_full), sum(d_full[[y]] >= dp_tappo)),
             if (is.null(dp_tappo)) "gli zeri sono un fenomeno diverso dal livello di y" else "valori censurati: il vero valore e' oltre il tappo, non si inventa",
             "due modelli separati", "UTENTE")
  }
  div <- if (isFALSE(cfg$verifica)) list(build = d_all[setdiff(names(d_all), ".tempo")], val = list(), t_val = list(), modo = "nessuna", descr = "verifica disattivata") else dividi(d_all, cfg)
  log_t(div$descr)
  if (y_binaria(d_all[[y]], cfg)) {
    # ---- y binaria nativa: preparazione + collinearita', poi logistica e verifica
    log_t("y binaria: preparazione e collinearita', poi logistica")
    cfg$decisioni$soglia_binaria <- 0.5
    st <- costruisci_binaria(div$build, cfg, completo = TRUE)
    lo <- logistica(st$d1, y, st$x1, cfg, character(), cfg$R_boot_logit %||% 999)
    cvb <- if (div$modo == "cv") cv_binaria(div$build, cfg, cfg$K_cv %||% 5) else NULL   # dopo: logistica() azzera il registro della fase
    vl <- if (length(div$val)) verifica_logistica(st, lo, div) else NULL
    cfr <- if (length(div$val)) confronti_binaria(st, lo, div) else NULL
    if (!is.null(cvb)) {      # n piccolo: la precisione viene dalla convalida della procedura
      prev <- mean(lo$d$.yb)
      vl <- rbind(`costruzione (in-sample)` = metriche_logit(lo$d$.yb, fitted(lo$g), prev, prev), `convalida della procedura` = cvb$tab["finale", ])
      cfr <- list(convalida = cvb$tab)
    }
    # il modello iniziale del report 1 (tutte le x candidate), misurato con lo stesso metro: e' lui il termine di confronto
    fo0 <- tryCatch({ vq <- .RM$quiet; .RM$quiet <- TRUE; on.exit(.RM$quiet <- vq, add = TRUE)
      d0 <- prepara_apprendi(div$build, cfg); f0 <- lmx(f_da_termini(y, x_iniziali(d0, y)), data = d0)
      r <- fuori_iniziale(f0, d0, div, cfg, TRUE); .RM$quiet <- vq; r }, error = function(e) { message("iniziale fuori campione: ", conditionMessage(e)); NULL })
    if (length(cfr) && !is.null(fo0$met)) { t1 <- cfr[[1]]; m0 <- fo0$met[colnames(t1)]; names(m0) <- colnames(t1)
      rownames(t1) <- sub("^tutte le x \\(prima della selezione\\)$", "dopo la preparazione (prima della selezione)", rownames(t1))
      i_n <- grep("^naive", rownames(t1)); t1 <- rbind(t1[i_n, , drop = FALSE], `iniziale (tutte le x candidate, report 1)` = m0, t1[-i_n, , drop = FALSE]); cfr[[1]] <- t1 }
    if (length(cfr)) st$consegna <- tryCatch(scelta_consegna_bin(cfr[[1]], fo0$p %||% length(coef(lo$g_full)), length(coef(lo$g))), error = function(e) NULL)
    if (!is.null(st$consegna) && !is.null(fo0$modello)) st$consegna$modello0 <- fo0$modello
    saveRDS(list(st = st, div = div), file.path(cfg$output, "stato_lineare.rds"))
    saveRDS(list(lo = lo, ver = vl, confronti = cfr), file.path(cfg$output, "stato_logistica.rds"))
    if (!isTRUE(cfg$veloce)) tryCatch(report_binaria(st, lo, vl, cfr, cfg), error = function(e) message("report binaria: ", conditionMessage(e)))
    log_t("logistica completata (y binaria) in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min"); quit(save = "no")
  }
  log_t("passi 0-7 sul campione di costruzione")
  st <- costruisci_lineare(div$build, cfg, completo = TRUE)
  ver <- cvr <- NULL
  if (div$modo == "cv") {
    log_t("verifica: convalida incrociata della procedura")
    cvr <- cv_procedura(div$build, cfg, cfg$K_cv %||% 5)
  } else if (length(div$val)) {
    log_t("verifica sui dati mai visti")
    ver <- verifica_lineare(st, div, cfg)
  }
  st$consegna <- tryCatch(scelta_consegna(st, ver, cvr, cfg), error = function(e) { message("scelta consegna: ", conditionMessage(e)); NULL })
  log_t("diagnosi: forma o informazione?")
  tet <- tryCatch(diagnosi_tetto(st, div, cfg, cvr), error = function(e) { message("diagnosi tetto: ", conditionMessage(e)); NULL })
  saveRDS(list(st = st, div = div, ver = ver, cvr = cvr, d_full = d_full, tetto = tet), file.path(cfg$output, "stato_lineare.rds"))
  # ---- riepilogo
  md_add("# Modello lineare robusto - riepilogo automatico\n")
  md_add(sprintf("Dati: `%s` - y = `%s` - generato il %s in %.1f minuti.\n", basename(cfg$dati), y,
                 format(Sys.time(), "%d/%m/%Y %H:%M"), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  md_add("\n## Divisione dei dati\n", div$descr, "\n")
  pr <- .RM$reg[.RM$reg$tipo == "PROPOSTA" & .RM$reg$fase != "8_logistica", ]
  md_add("\n## Decisioni da confermare (PROPOSTA)\n")
  md_add("Sono le scelte in cui la regola del corso non basta: vanno confermate o cambiate nel config (`decisioni`).\n")
  pr <- pr[, c("fase", "passo", "oggetto", "valore", "azione")]; rownames(pr) <- NULL; md_tab(pr)
  md_add("\n## Il percorso (campione di costruzione)\n"); md_tab(st$perc)
  md_add("\n## Modello finale\n```\n", paste(deparse(formula(st$fit4), width.cutoff = 100), collapse = "\n"), "\n```\n")
  if (!is.null(ver)) {
    md_add("\n## Verifica sui dati mai visti\n")
    md_add("MASE = errore assoluto medio diviso quello del previsore 'naive' (la mediana di y): < 1 meglio di nessun modello. ",
           "R2 fuori campione = quota di varianza spiegata su dati nuovi.\n\n"); md_tab(ver$tab, 4)
    md_add("\n**Coefficienti finali:** ", if (ver$decisione == "tutti") paste("riestimati su tutti i dati (n =", ver$n_finale, "): formula scelta sul campione, inferenza su tutti i dati; IC da N(b, V_HC3)") else "quelli del campione di costruzione", "\n")
    if (!is.null(ver$pareto)) { md_add("\nTest H1/H2 (costruzione vs costruzione + H1, valutati su H2):\n\n"); md_tab(ver$pareto, 4) }
    if (!is.null(ver$effetti)) { md_add("\n## Effetti medi sulla scala originale (modello finale)\n"); md_tab(ver$effetti, 2) }
  }
  if (!is.null(cvr)) { md_add("\n## Verifica: convalida incrociata della procedura\n"); md_tab(cvr$tab, 4)
    md_add("\nQuota di fold in cui ogni variabile e' stata scelta:\n\n"); md_tab(data.frame(quota = round(as.numeric(cvr$freq), 2), row.names = names(cvr$freq))) }
  md_add("\n## Iniziale vs finale (campione di costruzione, scala originale di y)\n"); md_tab(st$fin$confronto)
  md_add("\n## Effetti medi sul campione di costruzione (IC bootstrap percentile)\n"); md_tab(st$fin$effetti, 2)
  for (f in c("0_preparazione", "0_divisione", "1_collinearita", "2_trasformazioni", "3_selezione", "4_influenza",
              "5_eteroschedasticita", "6_bootstrap", "7_finale", "9_verifica")) { md_add("\n## Registro: ", f, "\n"); md_tab(tab_reg(f)) }
  md_salva("riepilogo_lineare")
  if (!isTRUE(cfg$veloce)) tryCatch(report_finale(st, ver, cvr, cfg, tetto = tet), error = function(e) message("report finale: ", conditionMessage(e)))
  log_t("lineare completato in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min")
}

# ===================================================================
if (fase == "tutto") {     # in "tutto" la logistica parte solo se serve; "logistica" la forza
  sl0 <- readRDS(file.path(cfg$output, "stato_lineare.rds"))
  # bimodalita' misurata sulla y trasformata (Box-Cox): su una y molto asimmetrica il coefficiente di
  # bimodalita' supera 0,555 anche senza due gruppi (incassi dei film: 0,7 con distribuzione unimodale)
  sv <- serve_logistica(ytrasf(sl0$st$d[[cfg$y]], sl0$st$lambda), cfg)
  if (dp_attivo) { sv$esegui <- TRUE; sv$motivi <- c(if (is.null(dp_tappo)) "modello a due parti: zero o positivo" else "modello a due parti: sopra il tappo", sv$motivi); sv$scelta <- "due_parti" }
  registra("8_logistica", "L.0", "serve la parte binaria?", if (length(sv$motivi)) paste(sv$motivi, collapse = "; ") else
           sprintf("nessun segnale (bimodalita' %.2f, zeri %.0f%%, nessuna soglia sostanziale)", sv$bc, 100 * sv$zeri),
           "logistica solo con soglia sostanziale, y bimodale o massa di zeri (config: logistica = auto|si|no)",
           if (sv$esegui) "eseguita" else "saltata", if (identical(sv$scelta, "auto")) "PROPOSTA" else "UTENTE")
  if (!sv$esegui) { log_t("logistica saltata: nessun motivo per una y binaria (forzarla con logistica = 'si')"); fase <- "fine" }
  if (sv$esegui && is.null(cfg$decisioni$soglia_binaria) && sv$zeri >= 0.10 && sv$zeri <= 0.90) cfg$decisioni$soglia_binaria <- 0
}
if (fase %in% c("logistica", "tutto")) {
  sl <- readRDS(file.path(cfg$output, "stato_lineare.rds")); st <- sl$st
  log_t("parte 1b: logistica")
  if (dp_attivo && !is.null(sl$d_full)) {
    # la logistica lavora su TUTTI i dati (zeri o censurati compresi): preparazione, divisione e convalida rifatte su questi
    if (is.null(dp_tappo)) cfg$decisioni$soglia_binaria <- 0 else cfg$evento <- sprintf("%s >= %s", cfg$y, format(dp_tappo, scientific = FALSE))
    .RM$quiet <- TRUE
    sl$div <- if (isFALSE(cfg$verifica)) list(build = sl$d_full[setdiff(names(sl$d_full), ".tempo")], val = list(), t_val = list(), modo = "nessuna") else dividi(sl$d_full, cfg)
    st <- costruisci_lineare(sl$div$build, cfg, completo = FALSE)
    sl$cvr <- if (sl$div$modo == "cv") cv_procedura(sl$div$build, cfg, cfg$K_cv %||% 5) else NULL
    .RM$quiet <- FALSE
    log_t("due parti: logistica su", nrow(sl$d_full), "righe")
  }
  it_lin <- grep(":", termini(st$fit4), value = TRUE)
  it_lin <- unname(sapply(it_lin, function(i) paste(sub("_og$", "", strsplit(i, ":")[[1]]), collapse = ":")))
  RL <- cfg$R_boot_logit %||% 999
  lo <- logistica(st$d1, cfg$y, st$x1, cfg, it_lin, RL)
  vl <- NULL
  if (length(sl$div$val)) { log_t("verifica della logistica sui dati mai visti"); vl <- verifica_logistica(st, lo, sl$div) }
  if (!is.null(sl$cvr)) { log_t("verifica della logistica: convalida della procedura"); vl <- cv_logistica(sl$cvr, sl$div$build, cfg, it_lin) }
  saveRDS(list(lo = lo, ver = vl), file.path(cfg$output, "stato_logistica.rds"))
  md_add("# Regressione logistica - riepilogo automatico\n")
  md_add("\n## Decisioni da confermare (PROPOSTA)\n")
  pr <- .RM$reg[.RM$reg$tipo == "PROPOSTA" & .RM$reg$fase == "8_logistica", c("passo", "oggetto", "valore", "azione")]; rownames(pr) <- NULL; md_tab(pr)
  md_add("\n## Modello finale\n```\n", paste(deparse(formula(lo$g), width.cutoff = 100), collapse = "\n"), "\n```\n")
  md_add("\n## Adattamento (costruzione)\n"); md_tab(t(t(round(lo$fit, 4))))
  if (!is.null(vl)) { md_add("\n## Verifica sui dati mai visti\n"); md_add("AUC: 0.5 = a caso, 1 = perfetto. Brier skill: miglioramento rispetto a prevedere sempre la prevalenza.\n\n"); md_tab(vl, 3) }
  md_add("\n## Odds ratio\n"); md_tab(lo$or, 3)
  md_add("\n## Effetti marginali medi (punti percentuali di probabilita')\n")
  md_tab(data.frame(AME_pp = round(100 * unlist(lo$ame), 2)))
  md_add("\n## Classificazione (solo cenno: tema del modulo ML)\n"); md_tab(lo$class)
  md_add("\n## Registro\n"); md_tab(tab_reg("8_logistica"))
  md_salva("riepilogo_logistica")
  sl1 <- readRDS(file.path(cfg$output, "stato_lineare.rds")); .RM$reg <- read.csv(file.path(cfg$output, "registro.csv"), stringsAsFactors = FALSE)
  if (!isTRUE(cfg$veloce)) tryCatch(report_finale(sl1$st, sl1$ver, sl1$cvr, cfg, logit = lo, tetto = sl1$tetto), error = function(e) message("report finale: ", conditionMessage(e)))
  log_t("logistica completata")
}
