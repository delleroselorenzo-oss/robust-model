######################################################################
# rm_lib.R - libreria per la costruzione semi-automatica di un modello
# lineare robusto (y quantitativa) e di un modello logistico (y binaria)
#
# Procedura e soglie: corso di Data Mining (Opzione 1). Ogni funzione
#   - applica la regola del corso quando la decisione e' meccanica (AUTO)
#   - propone una scelta motivata quando serve giudizio (PROPOSTA)
#   - usa la scelta dell'utente se e' scritta nel config (UTENTE)
# e annota tutto nel registro delle decisioni (registro.csv).
######################################################################

suppressPackageStartupMessages({
  library(MASS); library(car); library(lmtest); library(sandwich)
})
options(scipen = 999, warn = 1)
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))   # accenti corretti nei grafici

# ---------------------------------------------------------------------
# 0. UTILITA': registro, salvataggi, statistiche semplici
# ---------------------------------------------------------------------
.RM <- new.env()                      # stato globale: cartella output, registro

rm_init <- function(out_dir) {
  dir.create(file.path(out_dir, "grafici"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "tabelle"), recursive = TRUE, showWarnings = FALSE)
  .RM$out <- out_dir
  f <- file.path(out_dir, "registro.csv")
  .RM$reg <- if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE) else
    data.frame(fase = character(), passo = character(), oggetto = character(),
               valore = character(), regola = character(), azione = character(),
               tipo = character(), stringsAsFactors = FALSE)
}

# tipo: AUTO (regola netta), PROPOSTA (giudizio: da confermare), UTENTE (scelta nel config)
registra <- function(fase, passo, oggetto, valore, regola, azione, tipo = "AUTO") {
  if (isTRUE(.RM$quiet)) return(invisible(NULL))
  .RM$reg <- rbind(.RM$reg[!(.RM$reg$fase == fase & .RM$reg$passo == passo &
                               .RM$reg$oggetto == oggetto), ],
                   data.frame(fase = fase, passo = passo, oggetto = oggetto,
                              valore = as.character(valore), regola = regola,
                              azione = azione, tipo = tipo, stringsAsFactors = FALSE))
  write.csv(.RM$reg, file.path(.RM$out, "registro.csv"), row.names = FALSE)
  invisible(NULL)
}
# all'inizio di una fase cancella le righe vecchie della stessa fase (rilancio pulito)
reset_fase <- function(fase) {
  if (isTRUE(.RM$quiet)) return(invisible(NULL))
  .RM$reg <- .RM$reg[.RM$reg$fase != fase, ]
  write.csv(.RM$reg, file.path(.RM$out, "registro.csv"), row.names = FALSE)
}

salva_tab <- function(x, nome) {
  if (isTRUE(.RM$quiet)) return(invisible(x))
  write.csv(x, file.path(.RM$out, "tabelle", paste0(nome, ".csv")), row.names = TRUE)
  invisible(x)
}
png_apri <- function(nome, w = 9, h = 6) {
  if (isTRUE(.RM$quiet) || isTRUE(.RM$veloce)) return(pdf(NULL))      # modalita' veloce (collaudi): niente grafici
  png(file.path(.RM$out, "grafici", paste0(nome, ".png")), width = w, height = h,
      units = "in", res = 110)
}
md <- new.env(); md$righe <- character()
md_add <- function(...) md$righe <- c(md$righe, paste0(...))
md_tab <- function(x, digits = 3) {
  md_add(paste(knitr::kable(x, format = "pipe", digits = digits), collapse = "\n"), "\n")
}
md_salva <- function(nome) {
  writeLines(md$righe, file.path(.RM$out, paste0(nome, ".md"))); md$righe <- character()
}

skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }
kurt <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^4) / sd(x)^4 - 3 }
press <- function(m) sum((residuals(m) / (1 - hatvalues(m)))^2)
reset_stat <- function(m) tryCatch(unname(resettest(m, power = 2, type = "fitted")$statistic),
                                   error = function(e) NA)
shapiro_w <- function(r) { r <- r[!is.na(r)]; if (length(r) > 5000) { set.seed(1); r <- sample(r, 5000) }
  shapiro.test(r)$statistic }
termini <- function(m) attr(terms(m), "term.labels")
# modelli con formula e dati incorporati nella chiamata: update(), stepAIC(), boxcox() e Boot()
# funzionano da qualunque ambiente (altrimenti cercano oggetti che non vedono)
# minimi quadrati pesati (WLS): se i dati hanno la colonna .w (pesi, config "pesi") lm la usa come weights;
# resta nella chiamata come simbolo, quindi update(), Boot() e stepAIC() la ritrovano nei nuovi dati
lmx  <- function(form, data, ...) {
  a <- list(formula = as.formula(form), data = data, ...)
  if (".w" %in% names(data) && is.null(a$weights)) a$weights <- as.name(".w")
  do.call("lm", a) }
glmx <- function(form, data, ...) do.call("glm", list(formula = as.formula(form), data = data, family = binomial, ...))
f_da_termini <- function(y, t) as.formula(paste(y, "~", if (length(t)) paste(t, collapse = " + ") else "1"))
pval <- function(p) format.pval(p, digits = 2, eps = 1e-4)

# variabili "di base" coinvolte in un termine (lead_time per I(lead_time^2), a e b per a:b)
var_base <- function(term) all.vars(as.formula(paste("~", term)))
grado <- function(term) { k <- regmatches(term, regexpr("\\^[0-9]+", term)); if (length(k)) as.numeric(sub("\\^", "", k)) else 1 }

criteri_lm <- function(m, mse_full = NULL) {
  n <- nobs(m); p <- length(coef(m)); sse <- sum(residuals(m)^2)
  c(n = n, p = p, adjR2 = summary(m)$adj.r.squared, AIC = AIC(m), SBC = BIC(m),
    Cp = if (is.null(mse_full)) NA else sse / mse_full - n + 2 * p,
    PRESS = press(m), RESET = reset_stat(m))
}

# Duan smearing / correzione per riportare le previsioni sulla scala originale di y
# trasformazione: lambda = 1 nessuna, 0 log, altrimenti potenza y^lambda
ytrasf <- function(y, lambda) if (lambda == 0) log(y) else if (lambda == 1) y else y^lambda
yinv_naive <- function(eta, lambda) if (lambda == 0) exp(eta) else if (lambda == 1) eta else
  pmax(eta, 0)^(1 / lambda)
yinv <- function(eta, lambda, res) {           # previsione della MEDIA di y (non della mediana)
  if (lambda == 1) return(eta)
  # smearing robusto: un residuo enorme (punto influente non tolto) fa esplodere mean(exp(res)) (California,
  # passo 2: MAE 9e30). Si limitano i residui a mediana +- 6 MAD: con residui normali non cambia nulla
  res <- res[is.finite(res)]; m <- median(res); s <- mad(res); if (is.finite(s) && s > 0) res <- pmin(pmax(res, m - 6 * s), m + 6 * s)
  if (lambda == 0) return(exp(eta) * mean(exp(res)))
  if (lambda == 0.5) return(eta^2 + mean(res^2))
  set.seed(7); e <- if (length(res) > 200) sample(res, 200) else res   # smearing di Duan
  rowMeans(yinv_naive(outer(eta, e, "+"), lambda))
}
nome_trasf <- function(l) switch(as.character(l), "1" = "y (nessuna)", "0" = "log(y)",
  "0.5" = "sqrt(y)", "-1" = "1/y", "-0.5" = "1/sqrt(y)", "2" = "y^2", paste0("y^", l))

# ---------------------------------------------------------------------
# OPTIMAL GROUPING (sostituisce factorMerger, non piu' su CRAN)
# Livelli ordinati per media (o proporzione) di y; si fondono via via le coppie
# adiacenti piu' simili (minimo aumento di devianza); si sceglie il numero di
# gruppi con AIC minimo (GIC con penalita' 2, come factorMerger).
# ---------------------------------------------------------------------
og_merge <- function(resp, f, family = "gaussian", penalita = 2) {
  f <- droplevels(as.factor(f)); ok <- !is.na(resp) & !is.na(f)
  resp <- resp[ok]; f <- f[ok]
  gruppi <- as.list(levels(f)); names(gruppi) <- levels(f)
  fam <- if (family == "binomial") binomial() else gaussian()
  # devianza e AIC in forma chiusa dalle statistiche sufficienti per livello (identici a glm, senza stimare
  # nulla): con 140 quartieri l'unione per coppie richiedeva ~10.000 glm (affitti: 37 minuti)
  sl <- tapply(resp, f, sum); ql <- tapply(resp^2, f, sum); nl <- tapply(resp, f, length); N <- length(resp)
  dev_di <- function(g) {
    S <- vapply(g, function(lv) sum(sl[lv]), 0); Q <- vapply(g, function(lv) sum(ql[lv]), 0); n <- vapply(g, function(lv) sum(nl[lv]), 0)
    k <- length(g)
    if (family == "binomial") {
      p <- S / n; ll <- sum(ifelse(S > 0, S * log(p), 0) + ifelse(n - S > 0, (n - S) * log(1 - p), 0))
      dev <- -2 * ll; aic <- dev + 2 * k
    } else {
      dev <- sum(Q - S^2 / n); aic <- N * (log(2 * pi * dev / N) + 1) + 2 * (k + 1)
    }
    c(dev = dev, aic = aic - 2 * (k - 1) + penalita * (k - 1))
  }
  storia <- list(); storia[[1]] <- list(g = gruppi, aic = dev_di(gruppi)["aic"])
  while (length(gruppi) > 1) {
    medie <- sapply(gruppi, function(lv) mean(resp[f %in% lv]))
    o <- order(medie); gruppi <- gruppi[o]
    cand <- lapply(seq_len(length(gruppi) - 1), function(i) {
      g2 <- gruppi[-(i + 1)]; g2[[i]] <- c(gruppi[[i]], gruppi[[i + 1]]); g2 })
    dev <- sapply(cand, function(g) dev_di(g)["dev"])
    gruppi <- cand[[which.min(dev)]]
    storia[[length(storia) + 1]] <- list(g = gruppi, aic = dev_di(gruppi)["aic"])
  }
  aic <- sapply(storia, `[[`, "aic"); best <- storia[[which.min(aic)]]$g
  etich <- sapply(best, function(lv) {
    s <- paste(lv, collapse = "+"); if (nchar(s) > 40) paste0(substr(s, 1, 37), "...") else s })
  map <- rep(etich, lengths(best)); names(map) <- unlist(best)
  list(mappa = map, n_gruppi = length(best), aic_percorso = aic,
       nuovo = function(x) factor(unname(map[as.character(x)])))
}

relevel_max <- function(x) relevel(droplevels(x), ref = names(which.max(table(droplevels(x)))))

# ---------------------------------------------------------------------
# FASE 0 - PROFILO DEL DATASET (prima di qualunque decisione)
# ---------------------------------------------------------------------
leggi_dati <- function(cfg) {
  sep <- if (is.null(cfg$sep)) "," else cfg$sep
  na <- if (is.null(cfg$na_strings)) c("NA", "") else unlist(cfg$na_strings)
  d <- if (grepl("\\.rds$", cfg$dati)) readRDS(cfg$dati) else
    read.csv(cfg$dati, sep = sep, na.strings = na, stringsAsFactors = FALSE, check.names = TRUE)
  names(d) <- make.names(names(d), unique = TRUE)
  d
}

profilo <- function(d, y = NULL, ordinato = FALSE) {
  if (!is.null(y) && !nzchar(y)) y <- NULL         # y vuota = da scegliere (altrimenti "" combacia con ogni campo vuoto)
  n <- nrow(d)
  prof <- data.frame(
    variabile = names(d),
    tipo = sapply(d, function(x) class(x)[1]),
    distinti = sapply(d, function(x) length(unique(x[!is.na(x)]))),
    pct_NA = round(100 * sapply(d, function(x) mean(is.na(x))), 2),
    stringsAsFactors = FALSE)
  prof$livello_top <- sapply(d, function(x) { t <- sort(table(x), decreasing = TRUE)
    if (length(t)) paste0(names(t)[1], " (", round(100 * t[1] / n, 1), "%)") else "" })
  prof$valori_sospetti <- sapply(d, function(x) {
    if (is.numeric(x)) return("")          # i codici numerici (999, -1) li valuta il controllo (a), che guarda se sono isolati
    s <- intersect(c("NULL", "null", "Undefined", "undefined", "unknown", "Unknown", "?", "-",
                     "N/A", "n/a", "none", "None", "999", "9999", "-1"), unique(as.character(x)))
    paste(s, collapse = ",") })
  prof$segnalazione <- ""
  prof$segnalazione[prof$distinti <= 1] <- "costante -> escludere"
  prof$segnalazione[prof$distinti == n & prof$tipo %in% c("integer", "character")] <- "identificativo?"
  prof$segnalazione[prof$tipo == "character" & prof$distinti > 50 & prof$distinti < n] <-
    "MOLTE CATEGORIE: se e' una causa plausibile (regione, azienda, regista) -> effetti_gruppo = [\"col\"] (media di gruppo ristretta); come fattore resterebbero solo i livelli con 30+ casi"
  prof$segnalazione[prof$tipo %in% c("integer", "numeric") & prof$distinti <= 10 &
                      prof$distinti > 2] <- "numerica con pochi valori: codice di categoria o conteggio?"
  prof$segnalazione[prof$pct_NA > 20] <- "NA > 20%: escludere (regola del corso)"
  # testo libero o identificativo con ripetizioni (titoli, codici): non e' un predittore
  k <- prof$tipo == "character" & prof$distinti > 0.7 * n & prof$distinti < n     # quasi tutti diversi: titoli, codici
  prof$segnalazione[k] <- "identificativo o testo libero (non predittore) -> escludi"
  # numeri scritti come testo ("134 min", "143,446"): da convertire, non da raggruppare
  numtesto <- sapply(d, function(x) { if (!is.character(x)) return(FALSE); z <- x[!is.na(x) & nzchar(x)]
    length(z) >= 30 && mean(grepl("^[$€£]?\\s*-?[0-9][0-9.,]*(\\s+[A-Za-z]{1,5}|\\s*%)?$", head(z, 5000))) > 0.9 })
  prof$segnalazione[numtesto] <- "NUMERO SCRITTO COME TESTO (unita' o separatori): convertire in codice_preparazione, es. as.numeric(gsub('[^0-9.]', '', x))"
  # versioni alternative della stessa misura (budget e budget_2013 rivalutato): nomi con prefisso comune e |cor| > 0,95
  numv <- names(d)[sapply(d, is.numeric)]
  prof$versione_di <- ""
  for (a in numv) for (b in numv) if (a != b && startsWith(b, a) && nchar(b) > nchar(a)) {
    r <- suppressWarnings(cor(d[[a]], d[[b]], use = "pairwise.complete.obs"))
    if (isTRUE(abs(r) > 0.85)) prof$versione_di[prof$variabile == b] <- a }
  kv <- prof$versione_di != ""
  prof$segnalazione[kv] <- paste0("VERSIONE ALTERNATIVA di ", prof$versione_di[kv], " (es. rivalutata): tenerne una sola")
  # valori di gruppo ripetuti: una numerica costante dentro i gruppi di una colonna-codice ripetuta
  # (tariffa del Titanic = prezzo del biglietto, ripetuto per ogni passeggero dello stesso biglietto)
  prof$valore_di_gruppo <- ""
  cod <- names(d)[sapply(d, function(z) { if (!is.character(z)) return(FALSE); u <- length(unique(z[!is.na(z)]))
    u >= 10 && u < 0.95 * n && mean(duplicated(z[!is.na(z)]) | duplicated(z[!is.na(z)], fromLast = TRUE)) > 0.1 })]
  rip0 <- names(d)[sapply(names(d), function(v) { x <- d[[v]]; u <- length(unique(x[!is.na(x)]))
    grepl("(^|_)id$|_id_|^id_|codice|code", v, ignore.case = TRUE) && u > 0.5 * n && u < n })]   # unita' ripetute: non sono gruppi
  for (cc in setdiff(cod, rip0)) { g <- d[[cc]]; tg <- table(g); multi <- names(which(tg >= 2)); if (length(multi) < 10) next
    if (median(tg[multi]) > 10) next          # gruppi piccoli (biglietti, famiglie), non categorie ampie (anno + esito)
    for (v in names(d)[sapply(d, function(z) is.numeric(z) && length(unique(z[!is.na(z)])) > 10)]) {
      k <- g %in% multi & !is.na(d[[v]]); if (sum(k) < 20) next
      costante <- tapply(d[[v]][k], g[k], function(z) length(unique(z)) == 1)
      # un totale di gruppo cresce con la dimensione del gruppo (tariffa e passeggeri sul biglietto: rho 0,35);
      # una colonna che e' solo la versione numerica del codice (altitudine "1200 m" -> 1200) no
      dim_g <- as.numeric(tg[as.character(g)]); rho <- suppressWarnings(cor(d[[v]], dim_g, method = "spearman", use = "complete.obs"))
      if (mean(costante) > 0.95 && isTRUE(rho > 0.2) && prof$valore_di_gruppo[prof$variabile == v] == "") prof$valore_di_gruppo[prof$variabile == v] <- cc } }
  kg <- prof$valore_di_gruppo != ""
  prof$segnalazione[kg] <- paste0("VALORE DI GRUPPO ripetuto su piu' righe (uguale per ogni ", prof$valore_di_gruppo[kg],
    "): se e' un totale del gruppo, dividerlo per la dimensione del gruppo", ifelse(prof$segnalazione[kg] == "", "", paste0("; ", prof$segnalazione[kg])))
  # liste di etichette ("Card Game,Fantasy,Dice"): non testo libero ma piu' qualitative binarie insieme
  prof$etichette <- sapply(d, function(x) {
    if (!is.character(x)) return("")
    z <- x[!is.na(x) & nzchar(x)]; if (length(z) < 30) return("")
    zz <- head(z, 5000); sep <- c(",", ";", "|")[which.max(sapply(c(",", ";", "|"), function(s) mean(grepl(s, zz, fixed = TRUE))))]
    if (mean(grepl(sep, zz, fixed = TRUE)) < 0.3) return("")
    tok <- trimws(unlist(strsplit(zz, sep, fixed = TRUE))); tt <- table(tok)
    # etichette ricorrenti: poche etichette diverse rispetto ai valori, e le piu' frequenti coprono molte righe
    if (length(tt) > 0.2 * length(zz) || max(tt) < 0.05 * length(zz)) return("")
    sprintf("%d etichette (sep '%s'), %.1f per riga; prime: %s", length(tt), sep, length(tok) / length(zz),
            paste(names(sort(tt, decreasing = TRUE))[1:min(4, length(tt))], collapse = ", ")) })
  k <- prof$etichette != ""
  prof$segnalazione[k] <- paste0("LISTA DI ETICHETTE: ", prof$etichette[k], " -> etichette = {\"", prof$variabile[k], "\": 15} (una dummy per le piu' frequenti)")
  # unita' ripetute: una colonna-codice con valori ripetuti (track_id) -> deduplicare
  rip <- names(d)[sapply(names(d), function(v) { x <- d[[v]]; u <- length(unique(x[!is.na(x)]))
    grepl("(^|_)id$|_id_|^id_|codice|code", v, ignore.case = TRUE) && u > 0.5 * n && u < n })]
  for (v in rip) prof$segnalazione[prof$variabile == v] <- sprintf("UNITA' RIPETUTE: %d righe in piu' rispetto alle %d unita' -> deduplica = \"%s\" (o divisione per gruppo)",
    n - length(unique(d[[v]])), length(unique(d[[v]])), v)
  # misure ripetute senza codice: righe identiche in tutte le x tranne una numerica con pochi valori
  # (la stessa miscela provata a diverse eta') = la stessa unita' misurata piu' volte
  {
    xs <- setdiff(names(d), c(y, ".tempo"))
    for (v in xs[sapply(d[xs], function(z) is.numeric(z) && length(unique(z)) <= 30)]) {
      altre <- setdiff(xs, v); if (length(altre) < 2) next
      k <- do.call(paste, c(d[altre], sep = "|")); nu <- length(unique(k))
      if (nu < 0.8 * n && nu > 0.05 * n && mean(ave(d[[v]], k, FUN = function(z) length(unique(z))) > 1) > 0.2) {
        prof$segnalazione[prof$variabile == v] <- sprintf("MISURE RIPETUTE: %d unita' (stesse altre x) misurate a diversi valori di %s (%.0f%% delle righe in unita' ripetute) -> gruppo = \"paste(%s)\": verifica e bootstrap per unita'",
          nu, v, 100 * mean(duplicated(k) | duplicated(k, fromLast = TRUE)), paste(altre, collapse = ", "))
        break }
    }
  }
  # quasi costante = quasi tutte le righe con lo stesso valore. Il solo rapporto primo/secondo valore segnalava
  # anche le quantitative con il 16-23% di zeri (speechiness, instrumentalness: lezione 124)
  nzv <- sapply(d, function(x) { t <- sort(table(x), decreasing = TRUE)
    length(t) > 1 && t[1] / t[2] > 19 && length(t) / n < 0.1 && t[1] / sum(t) >= 0.9 })
  prof$segnalazione[nzv & prof$segnalazione == ""] <- "varianza quasi nulla"
  # (a) codici numerici che stanno per "mancante": 999, -99999, 99999999... o -1 in una x altrimenti >= 0
  prof$codici_numerici <- sapply(d, function(x) {
    if (!is.numeric(x)) return("")
    u <- unique(x[!is.na(x)]); s <- u[grepl("^-?9{3,}(\\.0+)?$", format(u, scientific = FALSE, trim = TRUE))]
    if (sum(u == -1) && all(u[u != -1] >= 0)) s <- c(s, -1)
    # un vero codice e' isolato all'estremo (lontano dagli altri valori) oppure frequente (>= 1% delle righe)
    isolato <- function(v) { altri <- u[u != v]; if (!length(altri)) return(FALSE)
      md <- median(x, na.rm = TRUE); q <- if (v > md) max(altri) else min(altri)
      (v > md && v > max(altri) && (v - q) > 2 * abs(q - md)) || (v < md && v < min(altri) && (q - v) > 2 * abs(md - q)) ||
        (v == -1 && all(altri >= 0)) }
    s <- s[vapply(s, function(v) isolato(v) || mean(x == v, na.rm = TRUE) >= 0.01, logical(1))]
    if (!length(s)) return("")
    paste0(paste(s, collapse = ","), " (", sum(x %in% s), " righe)") })
  prof$segnalazione[prof$codici_numerici != ""] <- paste0("codici sospetti ", prof$codici_numerici[prof$codici_numerici != ""],
    ": mancanti mascherati? -> codici_mancanti")
  # (a2) zeri isolati in una misura positiva (dimensione 0 di un diamante) e valori estremi isolati
  #      (larghezza 58,9 mm quando il 99% sta sotto 9): errori di registrazione, non fenomeno
  prof$impossibili <- sapply(d, function(x) {
    if (!is.numeric(x)) return("")
    z <- x[!is.na(x)]; if (length(unique(z)) <= 10) return("")
    out <- character(); pos <- z[z > 0]
    if (any(z == 0) && mean(z == 0) < 0.01 && length(pos) && min(pos) > 0.1 * median(pos))
      out <- c(out, sprintf("%d zeri isolati (minimo positivo %s)", sum(z == 0), signif(min(pos), 3)))
    q <- quantile(z, c(.01, .5, .99)); ampio <- q[3] - q[1]
    if (ampio > 0) { alti <- z[z > q[3] + 3 * ampio]; bassi <- z[z < q[1] - 3 * ampio]
      if (length(alti) && length(alti) < 0.001 * length(z)) out <- c(out, sprintf("%d valori estremi isolati (max %s, P99 %s)", length(alti), signif(max(z), 3), signif(q[3], 3)))
      if (length(bassi) && length(bassi) < 0.001 * length(z)) out <- c(out, sprintf("%d valori estremi isolati in basso (min %s, P1 %s)", length(bassi), signif(min(z), 3), signif(q[1], 3))) }
    paste(out, collapse = "; ") })
  # censura (tappo): il massimo o il minimo ripetuto in molte righe, staccato dal resto della distribuzione
  # (valore delle case fermo a 500.001 per 992 quartieri: il vero valore e' "almeno 500.001")
  prof$censura <- sapply(d, function(x) {
    if (!is.numeric(x)) return(""); z <- x[!is.na(x)]; if (length(unique(z)) <= 20) return("")
    out <- character()
    for (lato in c("max", "min")) {
      v <- if (lato == "max") max(z) else min(z); nv <- sum(z == v)
      vic <- if (lato == "max") z[z < v & z >= quantile(z, .95)] else z[z > v & z <= quantile(z, .05)]
      if (lato == "min" && v == 0) next             # una massa di zeri e' un fenomeno (conteggi, due parti), non un tappo
      # 1 o 100 su una variabile >= 0: il bordo di un punteggio 0-1 o di una percentuale, oltre non si va
      # (danceability = 1, frequenza = 100%: lezione 124); un tappo vero e' un limite di registrazione (500.001 $)
      if (lato == "max" && v %in% c(1, 100) && min(z) >= 0) next
      if (length(unique(vic)) < 2) next              # coda senza valori vicini (anno di raccolta dei dati): non giudicabile
      tipico <- median(table(vic))
      if (nv >= 0.01 * length(z) && nv > 20 * tipico)
        out <- c(out, sprintf("%s = %s ripetuto in %d righe (%.1f%%)", if (lato == "max") "massimo" else "minimo", signif(v, 6), nv, 100 * nv / length(z)))
    }
    paste(out, collapse = "; ") })
  kc <- prof$censura != ""
  prof$segnalazione[kc] <- paste0("CENSURA? ", prof$censura[kc], ": valore tappo (il vero valore e' oltre); per la y: escludere, modellare a parte o dichiararlo",
    ifelse(prof$segnalazione[kc] == "", "", paste0("; ", prof$segnalazione[kc])))
  # coordinate geografiche: lat e long da sole sono una superficie, non due effetti lineari. Una zona a griglia
  # (fattore) + optimal grouping la cattura (California: R2 fuori campione 0,65 -> 0,70, MAE -10%)
  kgeo <- grepl("^(lat|latitude|latitudine|lon|lng|long|longitude|longitudine)$", tolower(prof$variabile)) & prof$tipo %in% c("numeric", "integer")
  if (sum(kgeo) >= 2) prof$segnalazione[kgeo] <- paste0("COORDINATE: proporre una zona a griglia in codice_preparazione, es. ",
    "d$zona <- paste(round(d$lat*2)/2, round(d$lon*2)/2) (mezzo grado), lasciando all'optimal grouping l'unione delle celle",
    ifelse(prof$segnalazione[kgeo] == "", "", paste0("; ", prof$segnalazione[kgeo])))
  # valori passati (ritardi): con le righe in ordine di tempo, una colonna uguale a un'altra di k righe prima
  # (cnt_2d_bfr = noleggi di 2 giorni prima): nota solo k periodi prima, quindi inutile per prevedere piu' lontano
  prof$ritardo_di <- ""
  if (ordinato) { numr <- names(d)[sapply(d, function(z) is.numeric(z) && length(unique(z)) > 20)]
    numr <- numr[!sapply(d[numr], function(z) { z <- z[!is.na(z)]; all(diff(z) > 0) })]   # contatori (instant, giorni): non sono ritardi
    for (a in numr) for (b in setdiff(numr, a)) for (k in 1:7) {
      xa <- d[[a]][(k + 1):n]; xb <- d[[b]][1:(n - k)]; ok <- !is.na(xa) & !is.na(xb)
      if (sum(ok) > 30 && mean(xa[ok] == xb[ok]) > 0.95) { prof$ritardo_di[prof$variabile == a] <- sprintf("%s di %d righe prima", b, k); break } } }
  kr <- prof$ritardo_di != ""
  prof$segnalazione[kr] <- paste0("RITARDO: ", prof$variabile[kr], " = ", prof$ritardo_di[kr], ": e' noto solo con quell'anticipo; escluderlo se si prevede piu' lontano",
    ifelse(prof$segnalazione[kr] == "", "", paste0("; ", prof$segnalazione[kr])))
  k <- prof$impossibili != ""
  prof$segnalazione[k] <- paste0(ifelse(prof$segnalazione[k] == "", "", paste0(prof$segnalazione[k], "; ")),
    prof$impossibili[k], ": errori di registrazione? -> codici_mancanti o filtro")
  # (b) NA che significano "assenza" (piscina che non c'e'): quando la x e' NA, un'altra
  #     colonna numerica vale 0 quasi sempre, e quasi mai 0 quando la x e' presente
  numc <- names(d)[sapply(d, is.numeric)]
  prof$na_significa <- sapply(names(d), function(v) {
    ind <- is.na(d[[v]]); if (sum(ind) < max(20, 0.005 * n) || sum(!ind) < 5) return("")   # 5 metadati mancanti non sono un'assenza strutturale
    for (w in setdiff(numc, v)) { z <- d[[w]]
      if (isTRUE(mean(z[ind] == 0, na.rm = TRUE) > 0.95) && isTRUE(mean(z[!ind] == 0, na.rm = TRUE) < 0.2)) return(paste0("NA quando ", w, " = 0")) }
    # qualitativa con molti NA e nessun livello "None": probabile assenza da verificare nella documentazione
    x <- d[[v]]; if (!is.numeric(x) && mean(ind) > 0.2 &&
      !any(tolower(unique(x[!ind])) %in% c("none", "no", "nessuno", "na", "absent")))
      return("nessuna colonna gemella: verificare nella documentazione")
    "" })
  # totali = somma esatta di altre colonne (da chiedere: totale, parti o totale + quote)
  prof$totale_di <- ""
  numt <- numc[sapply(d[numc], function(z) mean(is.na(z)) <= 0.2 && isTRUE(sd(z, na.rm = TRUE) > 0))]
  if (length(numt) >= 3 && length(numt) <= 150) for (v in numt) {
    altri <- setdiff(numt, v); cc <- complete.cases(d[c(v, altri)]); if (sum(cc) < 30) next
    m <- tryCatch(lm(d[[v]][cc] ~ as.matrix(d[cc, altri])), error = function(e) NULL)
    if (is.null(m) || summary(m)$r.squared < 0.99999) next
    b <- coef(m)[-1]; names(b) <- altri; parti <- names(b)[!is.na(b) & abs(b - 1) < 1e-4]
    if (length(parti) >= 2) {
      r2 <- summary(lm(d[[v]][cc] ~ rowSums(as.matrix(d[cc, parti]))))$r.squared
      if (r2 > 0.99999) prof$totale_di[prof$variabile == v] <- paste(parti, collapse = " + ")
    }
  }
  # trasformazioni gia' calcolate (log, quadrati, scale): correlazione di rango ~ 1 con un'altra colonna
  prof$trasformazione_di <- ""
  nt <- numc[sapply(d[numc], function(z) length(unique(z[!is.na(z)])) > 10)]
  if (length(nt) >= 2) {
    R <- suppressWarnings(cor(d[nt], method = "spearman", use = "pairwise.complete.obs"))
    for (i in seq_along(nt)) for (j in seq_along(nt)) if (i != j && isTRUE(abs(R[i, j]) > 0.999)) {
      a <- nt[i]; b <- nt[j]
      # quale e' la "derivata"? quella con il nome che contiene l'altro (lsales <- sales), altrimenti la seconda
      der <- if (grepl(b, a, fixed = TRUE) && nchar(a) > nchar(b)) a else if (grepl(a, b, fixed = TRUE) && nchar(b) > nchar(a)) b else max(a, b)
      ori <- setdiff(c(a, b), der)
      if (prof$trasformazione_di[prof$variabile == der] == "") prof$trasformazione_di[prof$variabile == der] <- ori
    }
  }
  ktr <- prof$trasformazione_di != ""
  prof$segnalazione[ktr] <- ifelse(!is.null(y) & prof$trasformazione_di[ktr] == y,
    paste0("LEAKAGE: trasformazione della y (", prof$trasformazione_di[ktr], ") -> escludere"),
    paste0("trasformazione di ", prof$trasformazione_di[ktr], " (log, quadrato...): tenerne una sola, la forma la sceglie la pipeline"))
  kt <- prof$totale_di != ""
  prof$segnalazione[kt] <- paste0("TOTALE = ", prof$totale_di[kt], ": chiedere se tenere totale, parti o totale + quote")
  k <- prof$na_significa != ""
  prof$segnalazione[k] <- paste0("NA = ASSENZA? (", prof$na_significa[k], "): livello 'Nessuno' -> na_come_livello, non eliminare",
    ifelse(prof$segnalazione[k] == "", "", paste0("; ", prof$segnalazione[k])))
  if (!is.null(y) && y %in% names(d) && is.numeric(d[[y]])) {
    yy <- d[[y]]
    prof$cor_y <- sapply(d, function(x) if (is.numeric(x)) round(suppressWarnings(
      cor(x, yy, use = "pairwise.complete.obs")), 3) else NA)
    k <- !is.na(prof$cor_y) & abs(prof$cor_y) > 0.8 & prof$variabile != y
    prof$segnalazione[k] <- paste0("|cor con y| > 0.8: molto legata a y; controllare che non sia la y stessa o una sua parte (leakage)",
      ifelse(prof$segnalazione[k] == "", "", paste0("; ", prof$segnalazione[k])))
    # y ricostruibile da una combinazione lineare delle altre numeriche (es. totale = somma di parti)
    num <- setdiff(names(d)[sapply(d, is.numeric)], c(y, ".w"))
    num <- num[sapply(d[num], function(x) mean(is.na(x)) <= 0.2 && isTRUE(sd(x, na.rm = TRUE) > 0))]
    if (length(num) >= 2 && sum(complete.cases(d[c(y, num)])) > length(num) + 10) {
      cc <- complete.cases(d[c(y, num)])
      m <- tryCatch(lm(d[[y]][cc] ~ as.matrix(d[cc, num])), error = function(e) NULL)
      if (!is.null(m) && summary(m)$r.squared > 0.99) {
        r2_senza <- sapply(num, function(v) summary(lm(d[[y]][cc] ~ as.matrix(d[cc, setdiff(num, v)])))$r.squared)
        resp <- names(r2_senza)[r2_senza < 0.99]
        prof$segnalazione[prof$variabile %in% resp] <- paste0("LEAKAGE: y si ricostruisce (R2 > 0.99) da ",
          paste(resp, collapse = " + "), " -> escludere")
      }
    }
  }
  if (!is.null(y)) { ky <- prof$trasformazione_di == y
    prof$segnalazione[ky] <- paste0("LEAKAGE: trasformazione della y (", y, ") -> escludere") }
  prof
}

# ---------------------------------------------------------------------
# FASE 0 - PREPARAZIONE, DESCRITTIVE, MODELLO INIZIALE, MANCANTI
# ---------------------------------------------------------------------
# parte deterministica: regole che non guardano i dati (si applica a tutto, prima della divisione)
prepara_base <- function(d, cfg) {
  .RM$ausiliarie <- setdiff(unlist(cfg$ausiliarie), cfg$y)
  y <- cfg$y; F <- "0_preparazione"; reset_fase(F)
  n_in <- nrow(d)
  if (!is.null(cfg$codice_preparazione) && nzchar(cfg$codice_preparazione)) {
    eval(parse(text = cfg$codice_preparazione))             # ricodifiche "di merito" scritte da Claude
    registra(F, "0.1", "ricodifiche", "codice_preparazione", "significato dei dati",
             "applicate le ricodifiche del config (vedi config.json)", "UTENTE")
  }
  if (!is.null(cfg$filtro) && nzchar(cfg$filtro)) {
    d <- d[which(with(d, eval(parse(text = cfg$filtro)))), ]
    registra(F, "0.1", "popolazione", paste(nrow(d), "righe su", n_in), "definizione della popolazione",
             paste("filtro:", cfg$filtro), "UTENTE")
  }
  # codici numerici che stanno per "mancante": lista globale o {colonna: [valori]}
  cm <- cfg$codici_mancanti
  if (!is.null(cm) && length(cm)) {
    if (is.list(cm) && !is.null(names(cm))) { for (v in intersect(names(cm), names(d))) {
        k <- d[[v]] %in% unlist(cm[[v]]); d[[v]][k] <- NA
        registra(F, "0.8", paste("codici mancanti", v), paste(sum(k), "valori:", paste(unlist(cm[[v]]), collapse = ",")),
                 "codice numerico che significa 'non rilevato'", "trasformati in NA", "UTENTE") }
    } else { cm <- unlist(cm); for (v in setdiff(names(d)[sapply(d, is.numeric)], c(y, ".w"))) {
        k <- d[[v]] %in% cm; if (any(k)) { d[[v]][k] <- NA
          registra(F, "0.8", paste("codici mancanti", v), paste(sum(k), "valori"), "codici globali", "trasformati in NA", "UTENTE") } } }
  }
  # NA che significano "assenza" -> livello "Nessuno" (non sono mancanti: niente imputazione ne' eliminazione)
  for (v in intersect(unlist(cfg$na_come_livello), names(d))) {
    k <- is.na(d[[v]]); if (!any(k)) next
    if (is.numeric(d[[v]])) {        # es. anno del garage quando il garage non c'e': 0 sarebbe assurdo
      d[[paste0(v, "_assente")]] <- as.integer(k); d[[v]][k] <- median(d[[v]], na.rm = TRUE)
      registra(F, "0.8", paste("NA = assenza", v), paste(sum(k), "NA"), "il NA indica assenza, non un dato perso",
               paste0("dummy ", v, "_assente + mediana (il coefficiente di ", v, " vale per chi ce l'ha)"), "UTENTE")
    } else { x <- as.character(d[[v]]); x[k] <- "Nessuno"; d[[v]] <- x
      registra(F, "0.8", paste("NA = assenza", v), paste(sum(k), "NA"), "il NA indica assenza, non un dato perso",
               "livello 'Nessuno'", "UTENTE") }
  }
  n_na_y <- sum(is.na(d[[y]]))
  if (n_na_y) registra(F, "0.1", "y mancante", sprintf("%d righe (%.1f%%) senza y, tolte", n_na_y, 100 * n_na_y / nrow(d)),
    "la y non si imputa; se manca per un motivo legato al suo valore (voli cancellati = ritardi estremi, MNAR) il modello vale solo per i casi osservati",
    if (n_na_y / nrow(d) > 0.01) "ATTENZIONE: chiedere all'utente perche' manca (MNAR?) e dichiararlo nel report" else "tolte", if (n_na_y / nrow(d) > 0.01) "PROPOSTA" else "AUTO")
  d <- d[!is.na(d[[y]]), ]
  # unita' (gruppo): righe della stessa unita' (la stessa miscela di calcestruzzo provata a 3, 28, 90 giorni)
  # non sono indipendenti. Verifica, convalida e bootstrap lavorano per unita', gli SE sono cluster-robusti.
  # Il gruppo non e' un predittore: vive in .RM$gruppi, indicizzato per nome di riga
  .RM$gruppi <- NULL
  if (!is.null(cfg$gruppo) && nzchar(cfg$gruppo)) {
    g <- as.character(eval(parse(text = cfg$gruppo), envir = d, enclos = environment()))
    .RM$gruppi <- setNames(g, rownames(d))
    registra(F, "0.1", "unita' ripetute (gruppo)", sprintf("%d righe, %d unita' (%s); unita' con piu' righe: %d", nrow(d), length(unique(g)), cfg$gruppo,
             sum(table(g) > 1)), "righe della stessa unita' sono correlate: l'informazione e' minore di quanto dica n",
             "verifica, convalida e bootstrap per unita'; SE cluster-robusti (vcovCL)", "UTENTE")
  }
  # ordine temporale (per la verifica sul futuro): calcolato prima delle esclusioni, non e' un predittore
  if (!is.null(cfg$tempo) && nzchar(cfg$tempo)) d$.tempo <- with(d, eval(parse(text = cfg$tempo)))
  # trend: tempo continuo (es. anni con decimali) come predittore, per stimare e proiettare la crescita di y
  if (!is.null(cfg$trend) && nzchar(cfg$trend)) {
    d$trend <- with(d, eval(parse(text = cfg$trend)))
    registra(F, "0.1", "trend", sprintf("trend = %s (da %.2f a %.2f)", cfg$trend, min(d$trend, na.rm = TRUE), max(d$trend, na.rm = TRUE)),
             "il tempo come predittore: stima la crescita di y a parita' delle altre x", "aggiunto, sempre lineare (proiettabile sul futuro)", "UTENTE")
  }
  # pesi WLS: il voto medio di un gioco valutato da 50 utenti e' piu' rumoroso di quello valutato da 60.000
  # (var = sigma^2 / n): ogni riga pesa in proporzione alla sua precisione. Normalizzati a media 1
  if (!is.null(cfg$pesi) && nzchar(cfg$pesi)) {
    w <- with(d, eval(parse(text = cfg$pesi)))
    if (any(!is.finite(w) | w <= 0, na.rm = TRUE)) stop("pesi non positivi o non finiti")
    d$.w <- w / mean(w, na.rm = TRUE)
    registra(F, "0.1", "pesi WLS", sprintf("pesi = %s (da %.3g a %.3g, rapporto max/min %.0f)", cfg$pesi, min(d$.w), max(d$.w), max(d$.w) / min(d$.w)),
             "varianza dell'errore proporzionale a 1/peso: WLS = OLS sui dati riscalati per la radice del peso (efficiente, SE corretti)",
             if (identical(cfg$pesi_modo %||% "fgls", "fgls")) "FGLS: in costruzione stimo var = a + b / peso dai residui OLS e peso con 1 / var; metriche di verifica NON pesate"
             else "pesi proporzionali in tutti i passi lineari; metriche di verifica NON pesate (ogni unita' conta uguale)", "UTENTE")
  }
  # righe identiche in tutte le colonne con almeno una colonna quasi continua (>= 50% di valori distinti):
  # quasi certamente un record doppio (due persone con la stessa spesa al centesimo sono improbabili);
  # una copia in costruzione e una in verifica rendono la verifica ottimista. Config righe_doppie = "tieni" per tenerle
  nd <- righe_doppie(d)
  if (nd > 0 && !identical(cfg$righe_doppie, "tieni")) {
    d <- d[!duplicated(d), ]
    registra(F, "0.1", "righe doppie", sprintf("%d righe identiche in tutte le colonne (con colonne quasi continue): tolte, restano %d", nd, nrow(d)),
             "record doppio: la stessa unita' due volte pesa doppio e puo' finire sia in costruzione sia in verifica",
             "tolte (config righe_doppie = \"tieni\" per tenerle)", "AUTO")
  }
  # unita' ripetute (la stessa canzone in piu' playlist): una riga per unita', altrimenti la stessa unita'
  # finisce sia in costruzione sia in verifica e la verifica e' ottimista (copie quasi identiche)
  if (!is.null(cfg$deduplica) && nzchar(cfg$deduplica) && cfg$deduplica %in% names(d)) {
    k <- duplicated(d[[cfg$deduplica]]); d <- d[!k, ]
    registra(F, "0.1", "deduplica", sprintf("%d righe ripetute tolte (%s): restano %d unita'", sum(k), cfg$deduplica, nrow(d)),
             "una riga per unita': copie della stessa unita' in costruzione e verifica gonfiano la verifica", "tenuta la prima riga di ogni unita'", "UTENTE")
  }
  # liste di etichette -> una dummy 0/1 per ciascuna delle k etichette piu' frequenti (il resto e' "altro")
  if (length(cfg$etichette)) for (v in intersect(names(cfg$etichette), names(d))) {
    k <- as.integer(cfg$etichette[[v]]); x <- d[[v]]; x[is.na(x)] <- ""
    sep <- c(",", ";", "|")[which.max(sapply(c(",", ";", "|"), function(s) mean(grepl(s, x, fixed = TRUE))))]
    tok <- lapply(strsplit(x, sep, fixed = TRUE), trimws); top <- names(sort(table(unlist(tok)), decreasing = TRUE))
    top <- head(setdiff(top, ""), k)
    for (e in top) d[[make.names(paste0(v, "_", e))]] <- as.integer(vapply(tok, function(t) e %in% t, logical(1)))
    d[[paste0(v, "_n")]] <- lengths(lapply(tok, function(t) t[nzchar(t)]))       # quante etichette ha la riga
    d[[v]] <- NULL
    registra(F, "0.1", paste("etichette", v), sprintf("%d dummy (%s) + %s_n", length(top), paste(head(top, 5), collapse = ", "), v),
             "lista di etichette: ogni etichetta e' una qualitativa si'/no", "create; la colonna originale e' tolta", "UTENTE")
  }
  esc <- unique(c(unlist(cfg$escludi), unlist(cfg$id)))
  esc <- intersect(esc, names(d))
  if (length(esc)) { d[esc] <- NULL
    registra(F, "0.1", "x escluse", paste(esc, collapse = ", "), "identificativi, leakage, info successive a y",
             "escluse", "UTENTE") }
  for (v in intersect(unlist(cfg$fattori), names(d))) d[[v]] <- as.character(d[[v]])
  # le righe vengono rinumerate: il gruppo va reindicizzato sui nuovi nomi, altrimenti con un filtro o con
  # y mancanti le unita' finiscono sulle righe sbagliate (energia: 221 "unita'" su 174 paesi)
  if (!is.null(.RM$gruppi)) .RM$gruppi <- setNames(unname(.RM$gruppi[rownames(d)]), seq_len(nrow(d)))
  rownames(d) <- NULL
  d
}

# parte "appresa": decisioni prese guardando i dati di costruzione, da applicare poi alla verifica
prepara_apprendi <- function(d, cfg) {
  y <- cfg$y; F <- "0_preparazione"; cpieni <- attr(d, "conteggi_pieni")
  rn0 <- rownames(d); rownames(d) <- NULL
  # categorie con centinaia di valori (regione, azienda, produttore): come fattore sopravvivono solo i pochi
  # livelli con 30+ casi, il resto finisce in "Altro". Si usa invece l'effetto di gruppo ristretto (empirical
  # Bayes, come nei modelli multilivello): media di y del gruppo tirata verso la media generale con peso
  # n/(n + k), k = varianza entro / varianza tra gruppi. In costruzione e' calcolato FUORI fold (5 fold: la
  # riga non vede la propria y), sui dati nuovi con tutta la costruzione; gruppi nuovi = 0
  eff_g <- list()
  for (v in intersect(unlist(cfg$effetti_gruppo), names(d))) {
    g <- as.character(d[[v]]); g[is.na(g) | !nzchar(trimws(g))] <- NA; yy <- d[[y]]
    stima <- function(i) { ok <- i & !is.na(g) & !is.na(yy); mu <- mean(yy[ok]); gm <- tapply(yy[ok], g[ok], mean); gn <- tapply(yy[ok], g[ok], length)
      vw <- mean(tapply(yy[ok], g[ok], var), na.rm = TRUE); vb <- max(var(gm) - vw / mean(gn), 1e-12 * var(yy[ok])); k <- vw / vb
      list(dev = (gm - mu) * gn / (gn + k), k = k, mu = mu, icc = vb / (vb + vw)) }
    set.seed(cfg$seed %||% 1234); fo <- sample(rep_len(1:5, nrow(d))); e <- rep(0, nrow(d))
    for (f in 1:5) { s_f <- stima(fo != f); i <- which(fo == f); m <- s_f$dev[g[i]]; m[is.na(m)] <- 0; e[i] <- m }
    tot <- stima(rep(TRUE, nrow(d))); nv <- paste0(v, "_eff"); d[[nv]] <- e; d[[v]] <- NULL
    eff_g[[v]] <- list(nome = nv, dev = tot$dev)
    registra(F, "0.3", paste("effetto di gruppo", v), sprintf("%d gruppi; quota di varianza tra gruppi (ICC) %.1f%%; k = %.1f (un gruppo con k casi conta per meta')",
             length(tot$dev), 100 * tot$icc, tot$k),
             "categoria ad alta cardinalita': media di gruppo ristretta (empirical Bayes), calcolata fuori fold in costruzione",
             paste("nuova x numerica", nv, "(scarto del gruppo dalla media; gruppi nuovi = 0)"), "UTENTE")
  }
  # x con > 20% di NA: eliminate (regola del corso); soglie.na_max la cambia (es. mancanti casuali da imputare)
  na_max <- cfg$soglie$na_max %||% 0.20
  pna <- sapply(d, function(x) mean(is.na(x))); via <- setdiff(names(pna)[pna > na_max], y)
  if (length(via)) { d[via] <- NULL
    registra(F, "0.8", sprintf("x con NA > %.0f%%", 100 * na_max), paste(via, collapse = ", "),
             sprintf("NA > %.0f%% -> elimino la x", 100 * na_max), "eliminate") }
  tenute_na <- setdiff(names(pna)[pna > 0.20 & pna <= na_max], y)
  if (length(tenute_na)) registra(F, "0.8", "x con NA > 20% tenute e imputate", paste(tenute_na, collapse = ", "),
             sprintf("soglia alzata a %.0f%% dall'utente (mancanti giudicati casuali)", 100 * na_max), "imputate", "UTENTE")
  # costanti
  cost <- setdiff(names(d)[sapply(d, function(x) length(unique(x[!is.na(x)])) <= 1)], y)
  if (length(cost)) { d[cost] <- NULL
    registra(F, "0.3", "x costanti", paste(cost, collapse = ", "), "varianza zero", "eliminate") }
  # caratteri -> fattori; livelli rari -> "Altro"
  min_liv <- if (is.null(cfg$livelli_rari_min)) 30 else cfg$livelli_rari_min
  for (v in setdiff(names(d), c(y, ".w"))) {
    if (is.character(d[[v]]) || is.logical(d[[v]])) d[[v]] <- factor(d[[v]])
    if (is.factor(d[[v]])) {
      tb <- table(d[[v]]); cp <- cpieni[[v]]
      # raro = raro nel dataset intero (decisione che non usa y: si puo' prendere su tutti i dati),
      # oppure con meno di 10 casi in costruzione (coefficiente non stimabile)
      rari <- if (!is.null(cp)) names(tb)[tb < 10 | cp[names(tb)] < min_liv] else names(tb)[tb < min_liv]
      if (length(rari) >= 2 && nlevels(d[[v]]) > 3) {
        x <- as.character(d[[v]]); x[x %in% rari] <- "Altro"; d[[v]] <- factor(x)
        registra(F, "0.3", paste("livelli rari", v), paste(length(rari), "livelli -> Altro"),
                 paste("livelli con <", min_liv, "casi nel dataset intero (o < 10 in costruzione): coefficiente non stimabile bene"), "uniti in 'Altro' (la verifica controlla se qualcuno andava tenuto)",
                 "PROPOSTA")
      }
      d[[v]] <- relevel_max(d[[v]])
    }
  }
  # fattori con un solo livello dopo le ricodifiche
  uno <- names(d)[sapply(d, function(x) is.factor(x) && nlevels(droplevels(x)) < 2)]
  if (length(uno)) d[uno] <- NULL
  attr(d, "stato_prep") <- list(colonne = names(d),
    livelli = lapply(d[sapply(d, is.factor)], levels), effetti_gruppo = eff_g)
  if (nrow(d) == length(rn0)) rownames(d) <- rn0   # i nomi di riga servono a ritrovare l'unita' (gruppo)
  d
}

descrittive <- function(d, y) {
  yy <- d[[y]]
  sy <- c(n = length(yy), media = mean(yy), mediana = median(yy), sd = sd(yy), min = min(yy),
          max = max(yy), asimmetria = skew(yy), curtosi = kurt(yy), quota_y_le_0 = mean(yy <= 0))
  salva_tab(t(t(round(sy, 3))), "0_descrittive_y")
  num <- setdiff(names(d)[sapply(d, is.numeric)], c(y, ".w")); fat <- names(d)[sapply(d, is.factor)]
  cy <- sort(sapply(num, function(v) cor(d[[v]], yy, use = "complete.obs")), decreasing = TRUE)
  salva_tab(data.frame(cor_con_y = round(cy, 3)), "0_correlazioni_y")
  an <- t(sapply(fat, function(v) { a <- anova(lm(yy ~ d[[v]]))
    c(livelli = nlevels(d[[v]]), F = a$`F value`[1], p = a$`Pr(>F)`[1], R2 = a$`Sum Sq`[1] / sum(a$`Sum Sq`)) }))
  if (length(fat)) salva_tab(round(an[order(-an[, "R2"]), , drop = FALSE], 4), "0_anova_una_via")
  png_apri("0_y_istogramma", 8, 4); par(mfrow = c(1, 2))
  hist(yy, breaks = 50, col = "#cfe0ef", main = paste("Distribuzione di", y), xlab = y)
  qqnorm(yy, pch = 16, cex = .4); qqline(yy, col = "#a23b3b"); dev.off()
  if (length(num) >= 2) {
    cm <- cor(d[c(y, num)], use = "pairwise.complete.obs")
    png_apri("0_correlazioni", 9, 8); heatmap(cm, symm = TRUE, margins = c(9, 9), main = ""); dev.off()
  }
  if (length(fat)) {
    top <- head(rownames(an)[order(-an[, "R2"])], 6)
    png_apri("0_boxplot_qualitative", 10, 6); par(mfrow = c(2, 3), mar = c(8, 4, 2, 1))
    for (v in top) boxplot(yy ~ d[[v]], main = v, xlab = "", ylab = y, las = 2, col = "#cfe0ef", outcex = .4)
    dev.off()
  }
  list(y = sy, cor = cy, anova = if (length(fat)) an else NULL)
}

imputa <- function(d, y, metodo = "mice", seed = 500, per_gruppo = NULL) {
  F <- "0_preparazione"
  # imputazione con conoscenza del dominio: mediana (o moda) della x dentro un gruppo affine
  # (es. facciata del lotto per quartiere), poi mice per il resto
  for (v in intersect(names(per_gruppo), names(d))) {
    g <- per_gruppo[[v]]; if (!g %in% names(d)) next
    k <- is.na(d[[v]]); if (!any(k)) next
    val <- if (is.numeric(d[[v]])) ave(d[[v]], d[[g]], FUN = function(z) median(z, na.rm = TRUE)) else
      ave(as.character(d[[v]]), d[[g]], FUN = function(z) { t <- table(z); if (length(t)) names(which.max(t)) else NA })
    d[[v]][k] <- if (is.factor(d[[v]])) val[k] else val[k]
    registra(F, "0.8", paste("imputazione per gruppo", v), paste(sum(k), "NA"), paste("valore tipico di", v, "dentro", g),
             "imputata (conoscenza del dominio)", "UTENTE")
  }
  nna <- sapply(d, function(x) sum(is.na(x))); con_na <- setdiff(names(nna)[nna > 0], y)
  if (!length(con_na)) { registra(F, "0.8", "mancanti", "nessun NA", "-", "nessuna imputazione"); return(d) }
  tab <- data.frame(n_NA = nna[con_na], pct = round(100 * nna[con_na] / nrow(d), 2))
  salva_tab(tab, "0_mancanti")
  # meccanismo: i NA dipendono dalle altre x osservate? (MCAR vs MAR; MNAR non verificabile)
  mecc <- sapply(con_na, function(v) {
    ind <- is.na(d[[v]]); altri <- setdiff(names(d), c(v, y, con_na))
    ps <- sapply(altri, function(a) tryCatch({
      if (is.factor(d[[a]])) suppressWarnings(chisq.test(table(d[[a]], ind))$p.value)
      else t.test(d[[a]] ~ ind)$p.value }, error = function(e) NA))
    if (any(ps < 0.01, na.rm = TRUE)) paste("MAR (dipende da", paste(head(names(sort(ps)), 2), collapse = ", "), ")")
    else "compatibile con MCAR" })
  X <- d[setdiff(names(d), c(y, ".w"))]                       # la y NON entra nell'imputazione
  if (metodo == "mice" && requireNamespace("mice", quietly = TRUE)) {
    imp <- tryCatch(mice::mice(X, m = 1, maxit = 10, seed = seed, printFlag = FALSE), error = function(e) NULL)
    if (is.null(imp))      # x collineari: predittori scelti da quickpred (correlazione minima 0.1)
      imp <- tryCatch(mice::mice(X, m = 1, maxit = 10, seed = seed, printFlag = FALSE,
                                 predictorMatrix = mice::quickpred(X, mincor = 0.1)), error = function(e) NULL)
    if (is.null(imp)) return(imputa(d, y, "naive", seed))
    X <- mice::complete(imp, 1); met <- paste("mice", paste(unique(imp$method[imp$method != ""]), collapse = "/"))
  } else {
    for (v in con_na) X[[v]][is.na(X[[v]])] <- if (is.numeric(X[[v]])) median(X[[v]], na.rm = TRUE) else
      names(which.max(table(X[[v]]))); met <- "naive (mediana/moda)"
  }
  for (v in con_na) registra(F, "0.8", paste("imputazione", v), paste0(tab[v, "pct"], "% NA; ", mecc[v]),
                             "NA < 20%: imputo le x, y esclusa", paste("imputata con", met))
  d[names(X)] <- X; d
}

# il modello iniziale usa solo le righe complete: un fattore puo' restarvi con un solo livello
x_iniziali <- function(d, y) {
  x <- setdiff(names(d), c(y, ".w", .RM$ausiliarie)); cc <- complete.cases(d[c(y, x)])   # le ausiliarie servono solo a mice
  x[!sapply(x, function(v) is.factor(d[[v]]) && nlevels(droplevels(d[[v]][cc])) < 2)]
}
modello_iniziale <- function(d, y) {
  x <- x_iniziali(d, y)
  fit0 <- lmx(f_da_termini(y, x), data = d)
  registra("0_preparazione", "0.7", "modello iniziale fit0",
           sprintf("n=%d p=%d R2=%.3f AIC=%.1f", nobs(fit0), length(coef(fit0)), summary(fit0)$r.squared, AIC(fit0)),
           "termine di confronto", "stimato su tutte le x")
  png_apri("0_diagnostica_fit0", 9, 7); par(mfrow = c(2, 2)); plot(fit0, cex = .4); dev.off()
  num <- x[sapply(d[x], is.numeric)]
  if (length(num)) { png_apri("0_avplots_fit0", 10, 7)
    try(avPlots(fit0, terms = as.formula(paste("~", paste(head(num, 6), collapse = "+"))),
                col = adjustcolor("black", .25), pch = 16, cex = .5), silent = TRUE); dev.off() }
  fit0
}

# ---------------------------------------------------------------------
# FASE 1 - COLLINEARITA'
# ---------------------------------------------------------------------
vif_tab <- function(m) {
  v <- tryCatch(vif(m), error = function(e) NULL)
  if (is.null(v)) return(NULL)
  if (is.null(dim(v))) v <- cbind(GVIF = v, Df = 1, `GVIF^(1/(2*Df))` = sqrt(v))
  data.frame(termine = rownames(v), GVIF = v[, 1], Df = v[, 2],
             VIF_equiv = v[, 3]^2, TOL_equiv = 1 / v[, 3]^2, stringsAsFactors = FALSE)
}

chi_norm_coppie <- function(d, fat) {
  if (length(fat) < 2) return(NULL)
  cb <- combn(fat, 2)
  out <- do.call(rbind, lapply(seq_len(ncol(cb)), function(j) {
    tb <- table(d[[cb[1, j]]], d[[cb[2, j]]]); s <- suppressWarnings(chisq.test(tb)$statistic)
    data.frame(x1 = cb[1, j], x2 = cb[2, j], chi_norm = unname(s / (sum(tb) * (min(dim(tb)) - 1)))) }))
  out[order(-out$chi_norm), ]
}

collinearita <- function(d, y, cfg) {
  F <- "1_collinearita"; reset_fase(F)
  soglia_vif <- if (is.null(cfg$soglie$vif)) 5 else cfg$soglie$vif
  soglia_chi <- if (is.null(cfg$soglie$chi_norm)) 0.8 else cfg$soglie$chi_norm
  forzate <- unlist(cfg$decisioni$togli_collinearita)          # scelte dell'utente
  x <- setdiff(names(d), c(y, forzate, ".w"))
  if (length(forzate)) registra(F, "1", "x tolte dall'utente", paste(forzate, collapse = ", "),
                                "scelta di merito", "eliminate", "UTENTE")
  quote_def <- list()
  # (a) collinearita' esatta: coefficienti NA
  repeat {
    m <- lmx(f_da_termini(y, x), data = d); al <- names(coef(m))[is.na(coef(m))]
    if (!length(al)) break
    mm_assign <- attr(model.matrix(m), "assign"); tl <- termini(m)
    v <- tl[unique(mm_assign[match(al, colnames(model.matrix(m)))])][1]
    am <- tryCatch(alias(m)$Complete, error = function(e) NULL)
    r <- if (!is.null(am)) am[1, ] else NULL
    soci <- if (!is.null(r)) names(r)[r != 0 & names(r) != "(Intercept)"] else character()
    # totale = somma delle parti (es. superficie totale = 1o piano + 2o piano): la scelta e' dell'utente
    #   "totale": tengo il totale, tolgo le parti                      -> effetto della grandezza complessiva
    #   "parti": tengo le parti, tolgo il totale                       -> effetto di ogni componente
    #   "quote" (default): totale + quota di ogni parte (tranne una)   -> grandezza e composizione separate
    if (length(soci) >= 2 && all(soci %in% x) && all(sapply(d[soci], is.numeric)) && is.numeric(d[[v]]) &&
        all(abs(r[soci] - 1) < 1e-6)) {
      scelta <- cfg$decisioni$totali[[v]]; tipo <- if (is.null(scelta)) "PROPOSTA" else "UTENTE"
      # default "quote": conserva tutta l'informazione (grandezza + composizione); "totale" butta la composizione
      # (calcestruzzo: legante = cemento + loppa + ceneri; col solo totale R2 0,835, con le quote 0,854)
      if (is.null(scelta)) scelta <- "quote"
      rel <- paste0(v, " = ", paste(soci, collapse = " + "))
      if (scelta == "parti") {
        x <- setdiff(x, v)
        azione <- paste("tengo le parti e tolgo il totale", v)
      } else if (scelta == "quote") {
        rif <- soci[which.max(colMeans(d[soci], na.rm = TRUE))]       # la parte piu' grande fa da riferimento
        quote <- character()
        for (p in setdiff(soci, rif)) { qn <- paste0("quota_", p)
          d[[qn]] <- ifelse(d[[v]] > 0, d[[p]] / d[[v]], 0); quote <- c(quote, qn)
          quote_def[[qn]] <- c(p, v) }
        x <- c(setdiff(x, soci), quote)
        azione <- paste0("tengo il totale ", v, " + le quote di ", paste(setdiff(soci, rif), collapse = ", "),
                         " (riferimento: ", rif, ")", if (tipo == "PROPOSTA") " - default che conserva l'informazione; alternative: decisioni.totali = totale o parti" else "")
      } else {
        x <- setdiff(x, soci)
        azione <- paste("tengo il totale", v, "e tolgo le parti - CHIEDERE: totale, parti o totale + quote?")
      }
      registra(F, "1.1", paste("totale e parti", v), rel, "collinearita' esatta: totale = somma delle parti",
               azione, tipo)
      next
    }
    # fattori annidati (sottogenere dentro genere): il fattore grossolano e' funzione di quello fine.
    # Tolgo il grossolano, non il fine: il fine contiene tutta la sua informazione e in piu' il dettaglio
    if (is.factor(d[[v]])) {
      grossi <- Filter(function(f) f != v && is.factor(d[[f]]) &&
        all(tapply(as.character(d[[f]]), d[[v]], function(z) length(unique(z))) <= 1, na.rm = TRUE), x)
      if (length(grossi)) { x <- setdiff(x, grossi)
        registra(F, "1.1", paste("fattori annidati", v), paste(grossi, collapse = ", "),
                 paste("ogni livello di", v, "sta in un solo livello di", paste(grossi, collapse = ", ")),
                 paste("tolgo il fattore grossolano", paste(grossi, collapse = ", "), "e tengo quello fine", v, "(per l'effetto del grossolano: escludi", v, ")"), "PROPOSTA")
        next }
    }
    x <- setdiff(x, v)
    registra(F, "1.1", paste("collinearita' esatta", v), paste0("coef NA: ", al[1], " = combinazione di ",
             paste(head(soci, 4), collapse = ", ")), "combinazione lineare esatta (alias)",
             paste("tolgo", v, "- se e' piu' interpretabile di quelle con cui e' legata, scambiare (escludi le altre)"), "PROPOSTA")
  }
  # (b) chi-quadro normalizzato tra coppie di qualitative
  fat <- x[sapply(d[x], is.factor)]
  chi <- chi_norm_coppie(d, fat)
  if (!is.null(chi)) salva_tab(head(chi, 15), "1_chi_quadro_normalizzato")
  F_di <- function(xx) { m <- lmx(f_da_termini(y, xx), data = d)
    a <- drop1(m, test = "F"); setNames(a$`F value`[-1], rownames(a)[-1]) }
  P_di <- function(xx) { m <- lmx(f_da_termini(y, xx), data = d)
    a <- drop1(m, test = "F"); setNames(a$`Pr(>F)`[-1], rownames(a)[-1]) }
  # tra due fattori associati decide l'AIC (contributo totale contro costo in parametri), non l'F parziale:
  # l'F divide per i gradi di liberta' e favorisce sempre il fattore con meno livelli (contea contro quartiere)
  aic_senza <- function(xx, v) AIC(lmx(f_da_termini(y, setdiff(xx, v)), data = d))
  # x protette dall'utente: decisioni.tieni e le componenti delle interazioni imposte (latitudine e longitudine
  # insieme definiscono il luogo: togliere una delle due perche' collineari distrugge l'informazione)
  protette <- unique(c(unlist(cfg$decisioni$tieni), unlist(lapply(unlist(cfg$decisioni$interazioni), function(i) strsplit(i, ":")[[1]]))))
  while (!is.null(chi) && nrow(chi) && chi$chi_norm[1] > soglia_chi) {
    cp <- c(chi$x1[1], chi$x2[1]); a_s <- sapply(cp, function(v) aic_senza(x, v))
    via <- cp[which.min(a_s)]; resta <- setdiff(cp, via)        # tolgo quello la cui assenza costa meno
    if (any(cp %in% protette)) { chi <- chi[-1, , drop = FALSE]; next }   # una x protetta resta e non fa uscire la compagna
    x <- setdiff(x, via)
    registra(F, "1.5", paste("chi-norm", cp[1], "-", cp[2]), round(chi$chi_norm[1], 3),
             paste("chi-norm >", soglia_chi), sprintf("tolgo %s, tengo %s (AIC senza %s = %.1f, senza %s = %.1f; i livelli si riducono poi con l'optimal grouping)",
             via, resta, via, a_s[via], resta, a_s[resta]), "PROPOSTA")
    chi <- chi_norm_coppie(d, x[sapply(d[x], is.factor)])
  }
  # (c) VIF / tolleranza (GVIF^(1/(2Df)) al quadrato, confrontabile con la soglia del VIF)
  # regola: prendo la x con VIF massimo, trovo la sua "compagna" (la x che, tolta, le abbassa
  # di piu' il VIF) e tra le due tolgo quella con F parziale minore (contribuisce meno a y)
  # eccezione (Hill-Griffiths-Lim 6.4): la collinearita' e' un problema quando rende le stime imprecise.
  # Se la x resta molto significativa (F parziale > F_tieni) e il suo VIF e' "diffuso" (nessuna singola
  # compagna lo abbassa di almeno il 30%), oppure se anche la compagna e' molto significativa, la tengo.
  # Per applicare la regola del corso alla lettera: soglie.F_tieni = 1e9.
  p_tieni <- cfg$soglie$p_tieni %||% 0.01      # tengo una x collineare se resta significativa (p del test F parziale)
  if (!is.null(cfg$soglie$F_tieni) && cfg$soglie$F_tieni >= 1e8) p_tieni <- 0   # regola del corso alla lettera
  storia <- NULL; tenute <- character()
  repeat {
    m <- lmx(f_da_termini(y, x), data = d); vt <- vif_tab(m)
    if (is.null(vt)) break
    vt_c <- vt[!vt$termine %in% tenute, ]
    if (!nrow(vt_c) || max(vt_c$VIF_equiv) <= soglia_vif) break
    vmax <- vt_c$termine[which.max(vt_c$VIF_equiv)]; vif_max <- max(vt_c$VIF_equiv)
    cand <- setdiff(vt$termine, vmax)   # tutte: il GVIF equivalente di un fattore con molti livelli e' piccolo anche quando e' la compagna (dest per distance)
    vif_senza <- sapply(cand, function(w) { v2 <- vif_tab(lmx(f_da_termini(y, setdiff(x, w)), data = d))
      if (is.null(v2)) Inf else v2$VIF_equiv[v2$termine == vmax] })
    partner <- names(which.min(vif_senza))
    fv <- F_di(x)[c(vmax, partner)]; pv <- P_di(x)[c(vmax, partner)]
    diffuso <- min(vif_senza) > 0.7 * vif_max
    # l'eccezione vale solo per collinearita' moderata: oltre vif_max_tieni (default 4 x soglia = 20) i coefficienti
    # non sono piu' leggibili anche se significativi (diamanti: dimensioni con VIF 1000 tenute perche' n = 2000)
    vif_cap <- cfg$soglie$vif_max_tieni %||% (4 * soglia_vif)
    if (any(c(vmax, partner) %in% protette)) {     # una x protetta resta e non fa uscire la compagna: si tengono entrambe
      tenute <- c(tenute, vmax)
      registra(F, "1.2", paste("VIF", vmax, "- protetta"), sprintf("VIF %.1f con %s", vif_max, partner), "x protette dall'utente (decisioni.tieni o interazione imposta)",
               "tenute entrambe: SE da leggere con cautela, l'effetto congiunto resta stimato bene", "UTENTE")
      next
    }
    if (vif_max <= vif_cap && pv[vmax] < p_tieni && (diffuso || pv[partner] < p_tieni)) {
      tenute <- c(tenute, vmax)
      registra(F, "1.2", paste("VIF", vmax, "- tenuta"), sprintf("VIF %.1f; F parziale %.1f; %s", vif_max, fv[vmax],
               if (diffuso) "collinearita' diffusa (nessuna compagna la riduce del 30%)" else
                 sprintf("anche la compagna %s e' molto significativa (F %.1f)", partner, fv[partner])),
               sprintf("VIF > %s ma stima precisa: p del test F parziale %s < %s (Hill et al. 6.4)", soglia_vif, pval(pv[vmax]), p_tieni),
               "tenuta: la collinearita' non rende la stima insignificante; SE da leggere con cautela", "PROPOSTA")
      next
    }
    via <- names(which.min(fv)); resta <- setdiff(c(vmax, partner), via)
    if (any(sapply(d[c(vmax, partner)], function(z) is.factor(z) && nlevels(z) > 2))) {   # con fattori decide l'AIC
      a_s <- sapply(c(vmax, partner), function(v) aic_senza(x, v)); via <- names(which.min(a_s)); resta <- setdiff(c(vmax, partner), via) }
    storia <- rbind(storia, data.frame(vif_max = vmax, VIF = round(vif_max, 2), compagna = partner,
      VIF_senza_compagna = round(min(vif_senza), 2), F_vmax = round(fv[vmax], 1), F_compagna = round(fv[partner], 1), tolta = via))
    x <- setdiff(x, via)
    registra(F, "1.2", paste("VIF", vmax, "-", partner), sprintf("VIF %s = %.1f (%.1f senza %s)", vmax, vif_max,
             min(vif_senza), partner), paste("VIF >", soglia_vif, "(tol <", round(1 / soglia_vif, 2), ")"),
             if (exists("a_s") && length(a_s) == 2 && all(c(vmax, partner) %in% names(a_s)))
               sprintf("tolgo %s (AIC senza = %.1f), tengo %s (AIC senza = %.1f): fattore coinvolto, decide l'AIC", via, a_s[via], resta, a_s[resta])
             else sprintf("tolgo %s (F parziale %.1f), tengo %s (F %.1f)", via, fv[via], resta, fv[resta]), "PROPOSTA")
    if (exists("a_s")) rm(a_s)
  }
  m <- lmx(f_da_termini(y, x), data = d); vt <- vif_tab(m)
  if (!is.null(vt)) salva_tab(vt[order(-vt$VIF_equiv), ], "1_vif_finale")
  # avvisi che la regola non impone ma che vanno guardati (scelte di merito)
  chi2 <- chi_norm_coppie(d, x[sapply(d[x], is.factor)])
  if (!is.null(chi2)) for (j in which(chi2$chi_norm > 0.5)) registra(F, "1.5", paste("chi-norm", chi2$x1[j], "-", chi2$x2[j]),
    round(chi2$chi_norm[j], 3), "tra 0.5 e 0.8: la regola non impone nulla", 
    "valutare nel merito (una delle due e' nota solo dopo y? misurano la stessa cosa?)", "PROPOSTA")
  X <- model.matrix(m)[, -1, drop = FALSE]; X <- X[, apply(X, 2, sd) > 0, drop = FALSE]
  vd <- tryCatch(diag(solve(cor(X))), error = function(e) NULL)
  if (!is.null(vd)) { vd <- sort(vd[vd > soglia_vif], decreasing = TRUE)
    salva_tab(data.frame(VIF_colonna = round(vd, 2)), "1_vif_singole_dummy")
    if (length(vd)) registra(F, "1.2", "VIF di singole dummy", paste(paste0(names(vd), "=", round(vd, 1))[1:min(4, length(vd))], collapse = "; "),
      paste("VIF > ", soglia_vif, " su singole colonne (spesso riferimento raro o due fattori che si sovrappongono)"),
      "valutare: cambiare riferimento, unire livelli o togliere uno dei due fattori", "PROPOSTA") }
  if (!is.null(storia)) salva_tab(storia, "1_vif_eliminazioni")
  registra(F, "1", "modello fit1", sprintf("x=%d p=%d adjR2=%.3f AIC=%.1f max VIF=%.2f", length(x),
           length(coef(m)), summary(m)$adj.r.squared, AIC(m), if (is.null(vt)) NA else max(vt$VIF_equiv)),
           "nessun VIF sopra soglia, nessun NA", "modello di lavoro")
  list(x = x, fit = m, d = d, quote_def = quote_def)
}

# ---------------------------------------------------------------------
# FASE 2 - LINEARITA' E TRASFORMAZIONI
# ---------------------------------------------------------------------
boxcox_scelta <- function(d, y, x, cfg) {
  F <- "2_trasformazioni"
  yy <- d[[y]]
  if (any(yy <= 0)) {
    registra(F, "2.2", "Box-Cox", paste(sum(yy <= 0), "valori <= 0"), "Box-Cox richiede y > 0",
             "nessuna trasformazione automatica: valutare filtro o y + c", "PROPOSTA")
    return(list(lambda = 1, tab = NULL))
  }
  m <- lmx(f_da_termini(y, x), data = d, y = TRUE, qr = TRUE)
  bc <- boxcox(m, lambda = seq(-2, 2, 0.01), plotit = FALSE)
  lopt <- bc$x[which.max(bc$y)]; ic <- range(bc$x[bc$y > max(bc$y) - qchisq(0.95, 1) / 2])
  cand <- c(-1, -0.5, 0, 0.5, 1)
  conf <- t(sapply(cand, function(l) {
    dd <- d; dd$.yt <- ytrasf(yy, l); f <- lmx(f_da_termini(".yt", x), data = dd)
    c(lambda = l, adjR2 = summary(f)$adj.r.squared, asimm_res = skew(residuals(f)),
      curt_res = kurt(residuals(f)), RESET = reset_stat(f), BP = unname(bptest(f)$statistic)) }))
  rownames(conf) <- sapply(cand, nome_trasf)
  salva_tab(round(conf, 3), "2_boxcox_confronto")
  png_apri("2_boxcox", 6, 4); boxcox(m, lambda = seq(max(-2, lopt - 1), min(2, lopt + 1), 0.01)); dev.off()
  dentro <- cand[cand >= ic[1] & cand <= ic[2]]
  lsc <- if (length(dentro)) dentro[which.min(abs(dentro - lopt))] else cand[which.min(abs(cand - lopt))]
  if (1 %in% dentro) lsc <- 1      # l'IC comprende lambda = 1: i dati non chiedono di trasformare y (parsimonia)
  tipo <- "PROPOSTA"
  if (!is.null(cfg$decisioni$lambda)) { lsc <- cfg$decisioni$lambda; tipo <- "UTENTE" }
  registra(F, "2.2", "Box-Cox lambda", sprintf("ottimo %.2f, IC95 [%.2f; %.2f]", lopt, ic[1], ic[2]),
           "lambda arrotondato a una trasformazione interpretabile dentro l'IC al 95%; se l'IC comprende 1, nessuna trasformazione",
           paste("y trasformata:", nome_trasf(lsc)), tipo)
  list(lambda = lsc, lopt = lopt, ic = ic, tab = conf)
}

grouping_fattori <- function(d, yv, x, cfg) {
  F <- "2_trasformazioni"
  fat <- x[sapply(d[x], function(z) is.factor(z) && nlevels(z) > 2)]
  if (!length(fat)) return(list(d = d, x = x, tab = NULL, mappe = list()))
  no_og <- unlist(cfg$decisioni$no_grouping)
  fat <- setdiff(fat, no_og)
  aic_orig <- AIC(lmx(f_da_termini(yv, x), data = d))
  tab <- NULL; mappe <- list()
  for (v in fat) {
    altri <- setdiff(x, v)
    og_m <- og_merge(d[[yv]], d[[v]])                                  # marginale
    res_p <- residuals(lmx(f_da_termini(yv, altri), data = d))
    og_p <- og_merge(res_p, d[[v]])                                    # su residui parziali
    aic_og <- function(og) if (og$n_gruppi < 2) AIC(lmx(f_da_termini(yv, altri), data = d)) else
      AIC(lmx(f_da_termini(yv, c(altri, ".og")), data = cbind(d, .og = og$nuovo(d[[v]]))))
    aic_m <- aic_og(og_m); aic_p <- aic_og(og_p)
    best <- which.min(c(aic_orig, aic_m, aic_p))
    tab <- rbind(tab, data.frame(fattore = v, livelli = nlevels(d[[v]]), marg_gruppi = og_m$n_gruppi,
      marg_dAIC = round(aic_m - aic_orig, 1), parz_gruppi = og_p$n_gruppi, parz_dAIC = round(aic_p - aic_orig, 1),
      scelta = c("originale", "marginale", "parziale")[best]))
    if (best > 1 && list(og_m, og_p)[[best - 1]]$n_gruppi < 2) {
      x <- setdiff(x, v)
      registra(F, "2.3", paste("optimal grouping", v), "1 gruppo (AIC minimo senza il fattore)",
               "AIC minimo", paste("tolgo", v, ": non distingue livelli di y a parita' delle altre x"), "PROPOSTA")
    } else if (best > 1) {
      og <- list(og_m, og_p)[[best - 1]]; nv <- paste0(v, "_og")
      d[[nv]] <- relevel_max(og$nuovo(d[[v]])); x[x == v] <- nv
      mappe[[nv]] <- list(var = v, mappa = og$mappa, livelli = levels(d[[nv]]))
      mapping <- data.frame(livello = names(og$mappa), gruppo = unname(og$mappa))
      salva_tab(mapping, paste0("2_grouping_", v))
      registra(F, "2.3", paste("optimal grouping", v), paste(nlevels(d[[v]]), "->", og$n_gruppi, "gruppi"),
               "AIC minimo (marginale vs residui parziali)", paste("uso", nv), "AUTO")
    }
  }
  salva_tab(tab, "2_grouping_riepilogo")
  list(d = d, x = x, tab = tab, mappe = mappe)
}

trasforma_x <- function(d, yv, x, cfg) {
  F <- "2_trasformazioni"
  num <- x[sapply(d[x], function(z) is.numeric(z) && length(unique(z)) >= 5)]
  termini_x <- setNames(as.list(x), x)                 # ogni x -> vettore di termini
  if (!length(num)) return(list(termini = termini_x, tab = NULL))
  asim <- function(z) { s0 <- sd(z); if (!isTRUE(s0 > 0)) 0 else mean((z - mean(z))^3) / s0^3 }
  skew <- num[sapply(num, function(v) min(d[[v]]) >= 0 && asim(d[[v]]) > 2)]   # x >= 0 con coda lunga (lezione 134)
  if (requireNamespace("mgcv", quietly = TRUE)) {
    altri <- setdiff(x, num)
    sm <- sapply(num, function(v) { k <- min(10, length(unique(d[[v]])) - 1); paste0("s(", v, ", k=", k, ")") })
    fg <- as.formula(paste(yv, "~", paste(c(altri, sm), collapse = " + ")))
    g <- tryCatch(mgcv::gam(fg, data = d, method = "REML", weights = if (".w" %in% names(d)) d$.w else NULL), error = function(e) NULL)
    if (!is.null(g)) {
      st <- summary(g)$s.table; edf <- setNames(st[, "edf"], num); pg <- setNames(st[, "p-value"], num)
      # x >= 0 molto asimmetriche (conteggi con coda lunga): sulla scala grezza il GAM schiaccia la coda e vede
      # una retta (assenze: edf 1,0, p 0,58). Rifaccio il GAM con s(log(x + 1)) e tengo il segnale piu' forte (lezione 134)
      if (length(skew)) {
        sm2 <- sapply(num, function(v) { k <- min(10, length(unique(d[[v]])) - 1)
          if (v %in% skew) paste0("s(log(", v, " + 1), k=", k, ")") else paste0("s(", v, ", k=", k, ")") })
        g2 <- tryCatch(mgcv::gam(as.formula(paste(yv, "~", paste(c(altri, sm2), collapse = " + "))), data = d, method = "REML",
                                 weights = if (".w" %in% names(d)) d$.w else NULL), error = function(e) NULL)
        if (!is.null(g2)) { st2 <- summary(g2)$s.table
          for (i in which(num %in% skew)) if (st2[i, "p-value"] < pg[num[i]]) {
            registra(F, "2.4", paste("forma di", num[i], "su scala log"), sprintf("GAM su log(x + 1): edf %.1f, p %.2g (scala grezza: edf %.1f, p %.2g)",
                     st2[i, "edf"], st2[i, "p-value"], edf[num[i]], pg[num[i]]), "x molto asimmetrica: il GAM sulla scala grezza schiaccia la coda",
                     "valuto le forme candidate", "AUTO")
            pg[num[i]] <- st2[i, "p-value"]; edf[num[i]] <- max(edf[num[i]], st2[i, "edf"]) } } }
      salva_tab(round(st, 4), "2_gam_edf")
      png_apri("2_gam_componenti", 11, 7)
      plot(g, pages = 1, se = TRUE, shade = TRUE, residuals = FALSE)
      dev.off()
    } else { edf <- setNames(rep(3, length(num)), num); pg <- setNames(rep(0, length(num)), num) }
  } else { edf <- setNames(rep(3, length(num)), num); pg <- setNames(rep(0, length(num)), num) }
  imposte <- cfg$decisioni$trasf_x                       # es. {"lead_time": "poly2"}
  tab <- NULL
  centro <- function(x) if (abs(mean(x)) > 3 * sd(x)) round(mean(x)) else 0
  pot <- function(v, cc, k) if (cc == 0) paste0("I(", v, "^", k, ")") else paste0("I((", v, " - ", cc, ")^", k, ")")
  costruisci <- function(v, tipo) switch(tipo,
    lin = v, log = if (min(d[[v]]) > 0) paste0("log(", v, ")") else paste0("log(", v, " + ", 1 - min(d[[v]]), ")"),
    # log + log^2: curva che sale e poi scende su una x con coda lunga (assenze); zero_log: massa di zeri
    # che si comporta come un gruppo a se' (conteggi: 0 = "mai", spesso un altro fenomeno) + log per il resto
    log2 = { lt <- if (min(d[[v]]) > 0) paste0("log(", v, ")") else paste0("log(", v, " + ", 1 - min(d[[v]]), ")"); c(lt, paste0("I(", lt, "^2)")) },
    zero_log = c(paste0("I(", v, " == 0)"), paste0("log(", v, " + 1)")),
    # potenze di una x lontana da zero (anno ~ 2000: anno^3 ~ 8e9) vanno centrate, altrimenti x, x^2 e x^3
    # sono collineari quasi alla perfezione (SE HC3 NaN, bootstrap in disaccordo: Spotify)
    poly2 = { cc <- centro(d[[v]]); c(v, pot(v, cc, 2)) }, poly3 = { cc <- centro(d[[v]]); c(v, pot(v, cc, 2), pot(v, cc, 3)) },
    sqrt = if (min(d[[v]]) >= 0) paste0("sqrt(", v, ")") else paste0("sqrt(", v, " + ", -min(d[[v]]), ")"),
    bin = paste0("I(", v, " != ", as.numeric(names(which.max(table(d[[v]])))), ")"))
  for (v in setdiff(num[order(-edf[num])], "trend")) {   # prima le piu' non lineari (greedy cumulativo); il trend resta lineare
    if (!is.null(imposte[[v]])) { termini_x[[v]] <- costruisci(v, imposte[[v]])
      registra(F, "2.4", paste("trasformazione", v), imposte[[v]], "scelta dell'utente", "applicata", "UTENTE"); next }
    if (edf[v] < 1.5 && pg[v] > 0.01) next              # quasi lineare: resta lineare
    moda <- as.numeric(names(which.max(table(d[[v]]))))
    if (mean(d[[v]] == moda) > 0.8) {                   # x quasi sempre uguale: binning, non polinomi
      tipi <- c("lin", "bin")
    } else tipi <- c("lin", "poly2", "poly3", if (min(d[[v]]) >= 0) c("log", "sqrt"),
                     # forme per le x con coda lunga: curva su scala log, zeri come gruppo a se' (lezione 134)
                     if (v %in% skew) "log2", if (v %in% skew && min(d[[v]]) == 0 && mean(d[[v]] == 0) >= 0.10) "zero_log")
    aics <- sapply(tipi, function(tp) { tt <- termini_x; tt[[v]] <- costruisci(v, tp)
      AIC(lmx(f_da_termini(yv, unlist(tt)), data = d)) })
    # preferisco la forma piu' semplice se entro 2 punti di AIC dalla migliore
    semplicita <- c(lin = 1, bin = 1.5, log = 2, sqrt = 2, zero_log = 2.5, poly2 = 3, log2 = 3, poly3 = 4)[tipi]
    ok <- tipi[aics <= min(aics) + 2]; scelta <- ok[which.min(semplicita[ok])]
    dA <- setNames(rep(NA_real_, 8), c("lin", "poly2", "poly3", "log", "sqrt", "bin", "log2", "zero_log"))
    dA[tipi] <- round(aics - aics["lin"], 1)
    tab <- rbind(tab, data.frame(x = v, edf_gam = round(edf[v], 2), t(dA), scelta = scelta))
    termini_x[[v]] <- costruisci(v, scelta)
    if (scelta != "lin") registra(F, "2.4", paste("trasformazione", v), paste0("edf GAM=", round(edf[v], 1)),
      "forma dal grafico GAM -> parametrica con AIC minimo (la piu' semplice entro 2 punti)",
      paste("uso", paste(termini_x[[v]], collapse = " + ")), "AUTO")
    if (scelta == "zero_log") registra(F, "2.4", paste("zeri di", v, "come gruppo a se'"), sprintf("%.0f%% di zeri; dAIC %.1f contro log", 100 * mean(d[[v]] == 0), aics["zero_log"] - aics["log"]),
      "uno zero che conta da solo puo' essere un artefatto di registrazione (lezione 42: chi abbandona ha 0 assenze)",
      "verificare il significato dello zero prima di interpretare l'effetto", "PROPOSTA")
  }
  if (!is.null(tab)) salva_tab(tab, "2_trasformazioni_x_dAIC")
  list(termini = termini_x, tab = tab)
}

# soglia dentro un'interazione fattore x numerica (regressione segmentata / binning): l'effetto della x
# puo' cambiare a gradino in un solo gruppo (assicurazioni: i fumatori con BMI >= 30 costano ~20.000 $ in
# piu', i non fumatori no). Cerco la soglia c sui quantili 10-90% (arrotondata a 2 cifre significative) e
# aggiungo I(x >= c) e fattore:I(x >= c) se l'SBC migliora di oltre 10 + ln(n) punti (ln(n) paga la ricerca di c)
soglie_interazioni <- function(d, yv, t_base, sel) {
  F <- "2_trasformazioni"; add <- character(); n <- nrow(d)
  for (it in sel) {
    v <- strsplit(it, ":")[[1]]; if (length(v) != 2) next
    v <- sapply(v, function(z) if (z %in% names(d)) z else { b <- var_base(z); if (length(b) == 1) b else z }, USE.NAMES = FALSE)   # log(bmi) -> bmi
    if (!all(v %in% names(d))) next
    fa <- v[sapply(d[v], is.factor)]; nu <- v[sapply(d[v], is.numeric)]
    if (length(fa) != 1 || length(nu) != 1 || length(unique(d[[nu]])) < 20) next
    m0 <- lmx(f_da_termini(yv, c(t_base, sel, add)), data = d); de <- deff_gruppi(m0)
    cc <- unique(signif(quantile(d[[nu]], seq(0.1, 0.9, 0.025), na.rm = TRUE), 2))
    bb <- sapply(cc, function(c0) { st <- sprintf("I(%s >= %s)", nu, format(c0))
      m <- tryCatch(lmx(f_da_termini(yv, c(t_base, sel, add, st, paste0(fa, ":", st))), data = d), error = function(e) NULL)
      if (is.null(m) || any(is.na(coef(m)))) Inf else d_sbc(m, m0, de) })
    if (!any(is.finite(bb))) next
    k <- which.min(bb); db <- bb[k]
    if (db < -10 - log(n / de)) {
      st <- sprintf("I(%s >= %s)", nu, format(cc[k])); add <- c(add, st, paste0(fa, ":", st))
      registra(F, "2.6", paste("soglia nell'interazione", it), sprintf("%s >= %s: dSBC=%.1f", nu, format(cc[k]), db),
               "gradino dell'effetto di una x in un gruppo; soglia cercata sui quantili 10-90%, accettata se dSBC < -10 - ln(n)",
               paste("aggiunti", st, "e", paste0(fa, ":", st), "- VERIFICARE IL SENSO SOSTANZIALE della soglia"), "PROPOSTA")
    }
  }
  add
}

interazioni <- function(d, yv, t_base, x, cfg) {
  F <- "2_trasformazioni"
  if (!is.null(cfg$decisioni$interazioni)) {
    it <- unlist(cfg$decisioni$interazioni)
    it <- sapply(it, function(i) paste(sapply(strsplit(i, ":")[[1]], function(v)
      if (v %in% x) v else if (paste0(v, "_og") %in% x) paste0(v, "_og") else v), collapse = ":"), USE.NAMES = FALSE)
    registra(F, "2.6", "interazioni", paste(it, collapse = ", "), "scelta dell'utente (senso sostanziale)",
             "inserite", "UTENTE"); return(list(it = it, tab = NULL))
  }
  m <- lmx(f_da_termini(yv, t_base), data = d)
  # importanza di ogni x = F del test che toglie TUTTI i suoi termini: una x trasformata (log(bmi), polinomi)
  # non compare con il suo nome tra i termini, e con drop1 restava fuori dallo screening (assicurazioni:
  # in un fold bmi diventava log(bmi) e l'interazione fumo x BMI, la piu' forte del dataset, spariva)
  tb <- termini(m)
  fv <- sapply(x, function(v) { tv <- tb[sapply(tb, function(tt) v %in% var_base(tt))]
    if (!length(tv)) return(NA_real_)
    m1 <- tryCatch(lmx(f_da_termini(yv, setdiff(tb, tv)), data = d), error = function(e) NULL)
    if (is.null(m1)) NA_real_ else tryCatch(anova(m1, m)$F[2], error = function(e) NA_real_) })
  imp <- names(sort(fv[is.finite(fv)], decreasing = TRUE))
  fat <- imp[sapply(d[imp], is.factor)]; num <- imp[sapply(d[imp], is.numeric)]
  top <- unique(c(head(fat, 4), head(num, 3)))
  if (length(top) < 2) return(list(it = character(), tab = NULL))
  cp <- combn(top, 2); n <- nrow(d)
  # nell'interazione una x entra nella forma del suo effetto principale se questo e' un solo termine (log(x),
  # sqrt(x)); con i polinomi entra lineare. Con log(budget) come effetto principale, anno:budget grezzo
  # estrapolava sui film piu' costosi (R2 fuori campione negativo)
  forma_it <- function(v) { tv <- tb[sapply(tb, function(tt) identical(var_base(tt), v)) & !grepl(":", tb)]
    if (length(tv) == 1 && !grepl("\\^", tv)) tv else v }
  aic0 <- AIC(m); de <- deff_gruppi(m)
  # per ogni coppia provo la forma dell'effetto principale e quella grezza e tengo la migliore per SBC
  # (calcestruzzo: eta':legante grezza batte log(eta'):legante; film: anno:log(budget) batte anno:budget)
  tab <- do.call(rbind, lapply(seq_len(ncol(cp)), function(j) {
    var_it <- unique(c(paste(sapply(cp[, j], forma_it), collapse = ":"), paste(cp[, j], collapse = ":")))
    alt <- do.call(rbind, lapply(var_it, function(it) { mi <- lmx(f_da_termini(yv, c(t_base, it)), data = d)
      data.frame(interazione = it, parametri_in_piu = length(coef(mi)) - length(coef(m)),
                 coef_NA = sum(is.na(coef(mi))), dAIC = AIC(mi) - aic0, dSBC = d_sbc(mi, m, de)) }))
    alt <- alt[order(alt$coef_NA > 0, alt$dSBC), ]
    # due forme valide: decide la convalida (RMSE sulla scala di lavoro). L'SBC in-sample non vede
    # l'estrapolazione: con log(y), anno:budget grezzo vinceva per SBC ma esplodeva sui film nuovi (R2 -0,25)
    if (nrow(alt) == 2 && all(alt$coef_NA == 0) && any(alt$dSBC < -10)) {
      # errori sulla scala ORIGINALE di y con la metrica guida: su scala log l'esplosione non si vede
      lam <- cfg$.lambda %||% 1; yo <- if (!is.null(cfg$y) && cfg$y %in% names(d)) d[[cfg$y]] else d[[yv]]; if (is.null(cfg$y) || !cfg$y %in% names(d)) lam <- 1
      cvf <- sapply(alt$interazione, function(it) { f <- f_da_termini(yv, c(t_base, it)); set.seed(5)
        fo <- fold_di(rownames(d), 10); e <- rep(NA_real_, nrow(d))
        for (k in 1:10) { mk <- tryCatch(lmx(f, d[fo != k, ]), error = function(e) NULL); if (is.null(mk)) next
          e[fo == k] <- yo[fo == k] - tryCatch(yinv(prevedi_sicuro(mk, d[fo == k, ]), lam, residuals(mk)), error = function(e) NA) }
        if (identical(cfg$metrica, "media")) sqrt(mean(e^2, na.rm = TRUE)) else mean(abs(e), na.rm = TRUE) })
      # la forma dell'effetto principale (prima riga di var_it) resta, salvo che la grezza sia meglio di oltre il 3%
      i_pr <- match(var_it[1], alt$interazione); i_gr <- 3 - i_pr
      alt <- if (cvf[i_gr] < 0.97 * cvf[i_pr]) alt[c(i_gr, i_pr), ] else alt[c(i_pr, i_gr), ] }
    alt[1, ] }))
  tab <- tab[order(tab$dSBC), ]; salva_tab(tab, "2_interazioni_screening")
  sel <- head(tab$interazione[tab$coef_NA == 0 & tab$dSBC < -10], 3)
  for (it in sel) registra(F, "2.6", paste("interazione", it),
    sprintf("dSBC=%.1f", tab$dSBC[tab$interazione == it]),
    "SBC migliora di oltre 10 punti, nessun coefficiente NA; VERIFICARE IL SENSO SOSTANZIALE",
    "inserita", "PROPOSTA")
  sel <- c(sel, soglie_interazioni(d, yv, t_base, sel))
  esatte <- tab$interazione[tab$coef_NA > 0]
  if (length(esatte)) registra(F, "2.6", "interazioni scartate", paste(esatte, collapse = ", "),
    "creano collinearita' esatta (combinazioni di livelli vuote)", "escluse")
  list(it = sel, tab = tab)
}

# ---------------------------------------------------------------------
# FASE 3 - MODEL SELECTION (stepwise AIC/SBC + approccio ibrido)
# ---------------------------------------------------------------------
ibrido_lm <- function(m, alpha = 0.05, deff = 1) {
  tolti <- character()
  repeat {
    a <- drop1(m, test = "F"); a <- a[-1, , drop = FALSE]; a <- a[!is.na(a$`Pr(>F)`), , drop = FALSE]
    # con unita' ripetute l'F conta le righe: lo si riporta all'informazione effettiva
    if (deff > 1 && nrow(a)) a$`Pr(>F)` <- pf(a$`F value` / deff, a$Df, df.residual(m) / deff, lower.tail = FALSE)
    t_att <- termini(m)
    # gerarchia: non tolgo x se resta un suo termine di ordine superiore (x^2, x:z)
    protetti <- sapply(rownames(a), function(tt) any(sapply(setdiff(t_att, tt), function(o)
      all(var_base(tt) %in% var_base(o)) && ((grepl(":", o) && !grepl(":", tt)) ||
        (!grepl(":", o) && !grepl(":", tt) && grado(o) > grado(tt))))))
    a <- a[!protetti, , drop = FALSE]
    if (!nrow(a) || max(a$`Pr(>F)`) <= alpha) break
    via <- rownames(a)[which.max(a$`Pr(>F)`)]; tolti <- c(tolti, via)
    m <- update(m, as.formula(paste(". ~ . -", via)))
  }
  list(m = m, tolti = tolti)
}

# un polinomio resta gerarchico: se c'e' I(x^3) devono esserci anche x e I(x^2)
completa_gerarchia <- function(m) {
  t <- termini(m); add <- character()
  for (tt in t[grepl("\\^", t) & !grepl(":", t)]) { v <- var_base(tt); if (length(v) != 1) next
    for (j in seq_len(grado(tt) - 1)) { c1 <- if (j == 1) v else sub("\\^[0-9]+", paste0("^", j), tt)   # stessa forma (anche centrata)
      if (!c1 %in% c(t, add)) add <- c(add, c1) } }
  if (length(add)) m <- update(m, as.formula(paste(". ~ . +", paste(add, collapse = " + "))))
  m
}

selezione <- function(m_full, d) {
  F <- "3_selezione"; reset_fase(F)
  n <- nobs(m_full); mse <- summary(m_full)$sigma^2; de <- deff_gruppi(m_full)
  # dividere la devianza per deff equivale a moltiplicare la penalita' per deff
  s_aic <- completa_gerarchia(stepAIC(m_full, direction = "both", k = 2 * de, trace = 0))
  s_sbc <- completa_gerarchia(stepAIC(m_full, direction = "both", k = de * log(n / de), trace = 0))
  if (de > 1) registra(F, "3.0", "unita' ripetute nella selezione", sprintf("effetto del disegno %.1f: informazione di circa %d osservazioni indipendenti su %d righe", de, round(n / de), n),
                       "le righe della stessa unita' non sono indipendenti: AIC, SBC e test F contano l'informazione effettiva", "penalita' moltiplicate per deff", "AUTO")
  ib <- ibrido_lm(s_aic, deff = de)
  tab <- rbind(completo = criteri_lm(m_full, mse), stepAIC_AIC = criteri_lm(s_aic, mse),
               stepAIC_SBC = criteri_lm(s_sbc, mse), ibrido = criteri_lm(ib$m, mse))
  salva_tab(round(tab, 4), "3_confronto_criteri")
  registra(F, "3.1", "tolti con AIC", paste(setdiff(termini(m_full), termini(s_aic)), collapse = ", "),
           "stepAIC both, penalita' 2", "-")
  registra(F, "3.1", "tolti con SBC", paste(setdiff(termini(m_full), termini(s_sbc)), collapse = ", "),
           "stepAIC both, penalita' ln(n)", "-")
  registra(F, "3.2", "ibrido: tolti dopo AIC", paste(ib$tolti, collapse = ", "),
           "dal modello AIC elimino i termini con p > 0.05 (rispettando la gerarchia)", "-")
  scelto <- ib$m
  registra(F, "3.4", "modello scelto fit3", sprintf("p=%d adjR2=%.4f AIC=%.1f PRESS=%.1f", length(coef(scelto)),
           summary(scelto)$adj.r.squared, AIC(scelto), press(scelto)),
           "approccio ibrido: parsimonia a parita' di adattamento, tutti i termini significativi", "scelto", "PROPOSTA")
  list(fit = scelto, aic = s_aic, sbc = s_sbc, tab = tab)
}

# ---------------------------------------------------------------------
# FASE 4 - OUTLIER, LEVERAGE, INFLUENZA, REGRESSIONE ROBUSTA
# ---------------------------------------------------------------------
influenza <- function(m, d, yv, cfg) {
  F <- "4_influenza"; reset_fase(F)
  n <- nobs(m); p <- length(coef(m))
  rs <- rstudent(m); h <- hatvalues(m); ck <- cooks.distance(m); df <- dffits(m)
  # leva = 1 (un livello con un solo caso): Cook e DFFITS non definiti -> trattate come influenti
  leva1 <- !is.finite(ck) | !is.finite(df) | !is.finite(rs)
  if (any(leva1)) { ck[leva1] <- Inf; df[leva1] <- Inf; rs[!is.finite(rs)] <- 0 }
  sc <- if (identical(cfg$soglie$cook, "4/n")) 4 / n else 4 / (n - p - 2)
  sog <- c(rstudent = 2.5, leverage = (2 * p + 2) / n, cook = sc, dffits = 2 * sqrt(p / n))
  cnt <- c(outlier = sum(abs(rs) > 2.5), leverage = sum(h > sog["leverage"]), cook = sum(ck > sog["cook"]),
           dffits = sum(abs(df) > sog["dffits"]), outlier_attesi_per_caso = round(n * 2 * pt(-2.5, n - p - 1)))
  salva_tab(data.frame(soglia = round(c(sog, NA), 5), conteggio = cnt), "4_conteggi_influenza")
  if (any(leva1)) registra("4_influenza", "4.1", "leva = 1", paste(sum(leva1), "osservazioni"),
    "un livello con un solo caso: l'osservazione determina da sola un coefficiente", "trattate come influenti")
  top <- order(-ck)[1:min(15, n)]
  salva_tab(data.frame(riga = top, y = d[[yv]][top], rstudent = round(rs[top], 2), leva = round(h[top], 3),
                       cook = round(ck[top], 4), dffits = round(df[top], 3)), "4_top_influenti")
  png_apri("4_influence_plot", 8, 6); try(influencePlot(m, id = list(n = 4)), silent = TRUE); dev.off()
  png_apri("4_cook_dffits", 10, 4); par(mfrow = c(1, 2))
  plot(ck, type = "h", main = "Cook's D", ylab = "D"); abline(h = sog["cook"], col = 2, lty = 2)
  plot(df, type = "h", main = "DFFITS", ylab = "DFFITS"); abline(h = c(-1, 1) * sog["dffits"], col = 2, lty = 2)
  dev.off()
  # strategia 1: regressione robusta (M-stimatori con IRLS) come verifica
  wr <- if (".w" %in% names(d)) d$.w else NULL
  hub <- tryCatch(rlm(formula(m), data = d, psi = psi.huber, maxit = 100, weights = wr), error = function(e) NULL)
  bis <- tryCatch(rlm(formula(m), data = d, psi = psi.bisquare, maxit = 100, weights = wr), error = function(e) NULL)
  if (!is.null(hub) && !is.null(bis)) {
    se <- sqrt(diag(vcov(m)))
    sp <- data.frame(OLS = coef(m), Huber = coef(hub)[names(coef(m))], Bisquare = coef(bis)[names(coef(m))])
    sp$spost_bisq_in_SE <- abs(sp$Bisquare - sp$OLS) / se
    salva_tab(round(sp[order(-sp$spost_bisq_in_SE), ], 4), "4_ols_vs_robusta")
    registra(F, "4.2", "regressione robusta", sprintf("spostamento mediano bisquare = %.2f SE, max = %.2f SE (%s)",
             median(sp$spost_bisq_in_SE), max(sp$spost_bisq_in_SE), rownames(sp)[which.max(sp$spost_bisq_in_SE)]),
             "Huber/bisquare (IRLS) come verifica di OLS", "confronto")
  }
  # strategia 2: elimino solo gli influenti (Cook se obiettivo interpretativo, DFFITS se previsivo)
  strat <- if (is.null(cfg$decisioni$influenti)) {
    if (identical(cfg$obiettivo, "previsivo")) "dffits" else "cook" } else cfg$decisioni$influenti
  tipo <- if (is.null(cfg$decisioni$influenti)) "PROPOSTA" else "UTENTE"
  via <- switch(strat, cook = ck > sog["cook"], dffits = abs(df) > sog["dffits"], nessuna = rep(FALSE, n))
  # coda unilaterale: se quasi tutti gli influenti stanno dallo stesso lato (residuo con lo stesso segno),
  # non sono errori ma la coda lunga del fenomeno (es. ritardi estremi). Toglierli sposta la media:
  # previsioni distorte verso il basso/alto (voli NYC: distorsione +5 min, RMSE peggiore dell'iniziale)
  if (sum(via) >= 20 && strat != "nessuna") {
    q_pos <- mean(residuals(m)[via] > 0)
    # sulla scala originale di y: quanto si sposta la media togliendo gli influenti (in unita' di sd)
    yo <- if (!is.null(cfg$y) && cfg$y %in% names(d)) d[[cfg$y]] else d[[yv]]
    spost <- (mean(yo[!via]) - mean(yo)) / sd(yo)
    registra(F, "4.3c", "spostamento della media di y", sprintf("%.3f sd togliendo gli influenti (%s)", spost, strat),
             "oltre 0.05 sd: gli influenti sono una coda del fenomeno", "-", "AUTO")
    if (abs(spost) > 0.05) {
      if (is.null(cfg$decisioni$influenti)) {
        registra(F, "4.3a", "influenti da una sola coda", sprintf("%.0f%% degli influenti (%s) ha residuo %s; media di y spostata di %.3f sd", 100 * max(q_pos, 1 - q_pos),
                 strat, if (q_pos > 0.5) "positivo" else "negativo", spost),
                 "influenti unilaterali = coda del fenomeno, non errori: eliminarli distorce la media prevista",
                 "non elimino (strategia 'nessuna'); la regressione robusta resta come verifica; forzare con decisioni.influenti", "PROPOSTA")
        strat <- "nessuna"; via <- rep(FALSE, n)
      } else registra(F, "4.3a", "influenti da una sola coda", sprintf("%.0f%% dallo stesso lato", 100 * max(q_pos, 1 - q_pos)),
                 "eliminarli distorce la media prevista", "eliminati per scelta dell'utente: controllare la distorsione in verifica", "UTENTE")
    }
  }
  # influenti in prevalenza dallo stesso lato anche con la media poco spostata: forse una coda del fenomeno che le
  # x non descrivono (studenti che crollano: 0,048 sd, 51 negativi su 74). Non si cambia il default: tenerli ha
  # aiutato in energia e calcestruzzo, peggiorato in California, studenti e film (lezioni 120, 129, 130). Lo si
  # segnala al checkpoint B; il percorso fuori campione (9.1d, proposta fino_a) mostra quanto pesa il passo 4
  if (is.null(cfg$decisioni$influenti) && strat != "nessuna" && sum(via) >= 20) {
    q_pos <- mean(residuals(m)[via] > 0); lato <- max(q_pos, 1 - q_pos)
    if (binom.test(sum(residuals(m)[via] > 0), sum(via))$p.value < 0.01)      # test, non una quota fissa
      registra(F, "4.3e", "influenti dallo stesso lato", sprintf("%d osservazioni (%.1f%%) con criterio %s; %.0f%% con residuo %s (test binomiale p < 0,01)",
               sum(via), 100 * mean(via), strat, 100 * lato, if (q_pos > 0.5) "positivo" else "negativo"),
               "in prevalenza dallo stesso lato: forse una coda del fenomeno, non errori",
               "eliminati (default); se descrivono un gruppo che interessa (chi crolla, chi ha risorse naturali) provare decisioni.influenti = 'nessuna' e confrontare in verifica", "PROPOSTA")
  }
  pct <- 100 * mean(via)
  registra(F, "4.3", "eliminazione influenti", sprintf("%d osservazioni (%.1f%%) con criterio %s", sum(via), pct, strat),
           "Cook: stabilita' dei parametri (interpretazione); DFFITS: previsione",
           if (pct > 5) "ATTENZIONE: oltre il 5% -> descrivere chi sono; valutare regressione robusta" else "eliminate", tipo)
  # un livello che scende sotto 10 casi SOLO per la pulizia: non tolgo i suoi influenti, altrimenti il modello
  # perde il livello e non copre piu' quei casi in verifica (diamanti: clarity I1, 693 righe di verifica scoperte)
  for (v in intersect(names(d)[sapply(d, is.factor)], all.vars(formula(m)))) {
    t0 <- table(d[[v]]); t1 <- table(factor(d[[v]][!via], levels = names(t0)))
    salva <- names(t0)[t0 >= 10 & t1 < 10]
    if (length(salva)) { k <- via & d[[v]] %in% salva; via[k] <- FALSE
      registra(F, "4.3d", paste("influenti tenuti", v), sprintf("%d righe dei livelli %s", sum(k), paste(salva, collapse = ",")),
               "togliendoli il livello scenderebbe sotto 10 casi e il modello non coprirebbe piu' quella categoria", "tenuti", "AUTO") }
  }
  dd <- droplevels(d[!via, ])
  # livelli rarissimi rimasti (< 10 casi): instabili -> tolgo quelle righe
  rari <- unlist(lapply(names(dd)[sapply(dd, is.factor)], function(v) {
    if (!v %in% all.vars(formula(m))) return(NULL)
    t <- table(dd[[v]]); if (any(t < 10)) paste0(v, "=", names(t)[t < 10]) }))
  if (length(rari)) {
    for (r in rari) { v <- sub("=.*", "", r); l <- sub("^[^=]*=", "", r); dd <- dd[as.character(dd[[v]]) != l, ] }
    dd <- droplevels(dd)
    registra(F, "4.3b", "livelli rarissimi", paste(rari, collapse = "; "),
             "livelli con < 10 casi: coefficiente stimato su pochissime osservazioni", "righe tolte", "PROPOSTA")
  }
  # fattori diventati costanti
  tt <- termini(m)
  cost <- names(dd)[sapply(dd, function(z) is.factor(z) && nlevels(z) < 2)]
  tt <- tt[!sapply(tt, function(t) any(var_base(t) %in% cost))]
  m4 <- lmx(f_da_termini(yv, tt), data = dd)
  # dopo la pulizia un termine puo' diventare non stimabile (coefficiente NA): lo tolgo
  repeat {
    al <- names(coef(m4))[is.na(coef(m4))]; if (!length(al)) break
    asg <- attr(model.matrix(m4), "assign"); t_na <- termini(m4)[unique(asg[match(al, colnames(model.matrix(m4)))])][1]
    m4 <- update(m4, as.formula(paste(". ~ . -", t_na)))
    registra(F, "4.4", paste("termine non stimabile", t_na), paste("coef NA:", paste(head(al, 3), collapse = ", ")),
             "dopo l'eliminazione degli influenti il termine e' collineare in modo esatto", "tolto", "AUTO")
  }
  ib <- ibrido_lm(m4)
  if (length(ib$tolti)) registra(F, "4.4", "termini non piu' significativi", paste(ib$tolti, collapse = ", "),
                                 "approccio ibrido dopo la pulizia", "tolti")
  m4 <- ib$m
  conf <- rbind(prima = c(n = n, adjR2 = summary(m)$adj.r.squared, sigma = summary(m)$sigma,
                          asimm = skew(residuals(m)), curt = kurt(residuals(m)), W = shapiro_w(residuals(m)),
                          RESET = reset_stat(m)),
                dopo = c(n = nobs(m4), adjR2 = summary(m4)$adj.r.squared, sigma = summary(m4)$sigma,
                         asimm = skew(residuals(m4)), curt = kurt(residuals(m4)), W = shapiro_w(residuals(m4)),
                         RESET = reset_stat(m4)))
  salva_tab(round(conf, 4), "4_prima_dopo")
  registra(F, "4", "modello fit4", sprintf("n=%d p=%d adjR2=%.4f", nobs(m4), length(coef(m4)), summary(m4)$adj.r.squared),
           "dopo eliminazione influenti e livelli rari", "modello di lavoro")
  list(fit = m4, d = dd, tolte = which(via), hub = hub, bis = bis, conf = conf, strat = strat)
}

# ---------------------------------------------------------------------
# FASE 5 - ETEROSCHEDASTICITA'
# ---------------------------------------------------------------------
eteroschedasticita <- function(m, d) {
  F <- "5_eteroschedasticita"; reset_fase(F)
  bp <- bptest(m); wh <- bptest(m, ~ yhat + I(yhat^2), data = data.frame(yhat = fitted(m)))
  vb <- unique(unlist(lapply(termini(m), var_base)))
  fonti <- t(sapply(vb, function(v) { tt <- tryCatch(ncvTest(m, as.formula(paste("~", v)), data = d),
    error = function(e) NULL); if (is.null(tt)) c(NA, NA, NA) else c(tt$ChiSquare, tt$Df, tt$p) }))
  colnames(fonti) <- c("Chisq", "df", "p"); fonti <- fonti[order(-fonti[, 1]), , drop = FALSE]
  salva_tab(round(fonti, 4), "5_fonti_eteroschedasticita")
  o <- coeftest(m); w <- coeftest(m, vcov = vc_rob(m))
  cs <- data.frame(b = coef(m), se_OLS = o[, 2], se_White = w[, 2], rapporto = w[, 2] / o[, 2],
                   p_OLS = o[, 4], p_White = w[, 4])
  salva_tab(cs, "5_se_ols_vs_white")
  cambia <- rownames(cs)[(cs$p_OLS < 0.05) != (cs$p_White < 0.05)]
  png_apri("5_residui", 10, 4); par(mfrow = c(1, 2))
  plot(fitted(m), rstudent(m), pch = 16, cex = .3, xlab = "previsti", ylab = "res. studentizzati"); abline(h = 0, col = 2)
  plot(fitted(m), sqrt(abs(rstandard(m))), pch = 16, cex = .3, xlab = "previsti", ylab = "sqrt|res std|")
  lines(lowess(fitted(m), sqrt(abs(rstandard(m)))), col = 2, lwd = 2); dev.off()
  registra(F, "5.2", "test", sprintf("BP=%.1f (p=%s); White ridotto=%.1f (p=%s); fonte principale: %s",
           bp$statistic, pval(bp$p.value), wh$statistic, pval(wh$p.value), rownames(fonti)[1]),
           "H0 omoschedasticita'", if (bp$p.value < 0.05) "eteroschedasticita' presente" else "omoschedasticita' non rifiutata")
  registra(F, "5.3", "rimedio", sprintf("rapporto mediano SE White/OLS = %.2f; cambiano decisione al 5%%: %s",
           median(cs$rapporto), if (length(cambia)) paste(cambia, collapse = ", ") else "nessuno"),
           "con eteroschedasticita' OLS resta corretto e consistente ma inefficiente; SE OLS distorti",
           "inferenza con SE di White HC3", if (bp$p.value < 0.05) "AUTO" else "AUTO")
  list(bp = bp, white = wh, fonti = fonti, se = cs, cambia = cambia)
}

# ---------------------------------------------------------------------
# FASE 6 - BOOTSTRAP
# ---------------------------------------------------------------------
bootstrap_lm <- function(m, R = 1999, seed = 2026) {
  F <- "6_bootstrap"; reset_fase(F)
  set.seed(seed)
  nc <- max(1, parallel::detectCores() - 1)
  g_b <- gruppi_di(names(residuals(m)))
  B <- if (is.null(g_b)) suppressWarnings(Boot(m, R = R, ncores = nc)) else boot_cluster(m, R, g_b)   # per unita' se c'e' un gruppo
  falliti <- sum(apply(is.na(B$t), 1, any))
  b <- coef(m); sb <- apply(B$t, 2, sd, na.rm = TRUE); bias <- colMeans(B$t, na.rm = TRUE) - b
  sw <- sqrt(diag(vc_rob(m))); so <- sqrt(diag(vcov(m)))
  ci <- tryCatch(Confint(B, type = "perc"), error = function(e) {
    q <- t(apply(B$t, 2, quantile, c(.025, .975), na.rm = TRUE)); cbind(Estimate = b, q) })
  sig <- data.frame(OLS = summary(m)$coefficients[, 4] < 0.05,
                    White = coeftest(m, vcov = vc_rob(m))[, 4] < 0.05,
                    boot_perc = !(ci[, 2] <= 0 & ci[, 3] >= 0))
  tab <- data.frame(b = b, bias = bias, bias_in_SE = abs(bias) / sb, se_OLS = so, se_White = sw, se_boot = sb,
                    IC_2.5 = ci[, 2], IC_97.5 = ci[, 3], b_dentro_IC = b >= ci[, 2] & b <= ci[, 3], sig)
  salva_tab(tab, "6_bootstrap")
  dis <- rownames(sig)[apply(sig, 1, function(r) length(unique(r)) > 1)]
  registra(F, "6", "bootstrap per casi", sprintf("R=%d, repliche fallite=%d; bias max=%.2f SE; SE boot/White mediano=%.2f; b dentro IC: %d/%d",
           R, falliti, max(tab$bias_in_SE, na.rm = TRUE), median(sb / sw, na.rm = TRUE), sum(tab$b_dentro_IC), length(b)),
           "inferenza robusta: confermo bias trascurabile e significativita'", "-")
  registra(F, "6.3", "disaccordo sulla significativita'", if (length(dis)) paste(dis, collapse = ", ") else "nessuno",
           "in caso di dubbio decide l'IC percentile", "-")
  png_apri("6_bootstrap_hist", 10, 6)
  sel <- names(sort(abs(b[-1] / so[-1]), decreasing = TRUE))[1:min(6, length(b) - 1)]
  par(mfrow = c(2, 3)); for (k in sel) { j <- match(k, names(b)); hist(B$t[, j], breaks = 40, main = k, xlab = "",
    col = "#cfe0ef"); abline(v = b[j], col = 2, lwd = 2); abline(v = ci[j, 2:3], lty = 2) }
  dev.off()
  list(B = B, tab = tab)
}

# ---------------------------------------------------------------------
# FASE 7 - CONTROLLO FINALE, EFFETTI SULLA SCALA ORIGINALE, VALIDAZIONE
# ---------------------------------------------------------------------
X_di <- function(m, dd) { tt <- delete.response(terms(m))
  model.matrix(tt, model.frame(tt, dd, xlev = m$xlevels), contrasts.arg = m$contrasts) }

effetti_originali <- function(m, dd, lambda, Bmat = NULL, x_orig) {
  res <- residuals(m); b <- coef(m)
  ok_b <- if (!is.null(Bmat)) Bmat[complete.cases(Bmat), , drop = FALSE] else NULL
  if (!is.null(ok_b) && nrow(ok_b) > 500) { set.seed(3); ok_b <- ok_b[sample(nrow(ok_b), 500), ] }
  eff <- function(alto, basso) {
    Xa <- X_di(m, alto); Xb <- X_di(m, basso)
    st <- mean(yinv(Xa %*% b, lambda, res) - yinv(Xb %*% b, lambda, res))
    if (is.null(ok_b)) return(c(effetto = st, IC_2.5 = NA, IC_97.5 = NA))
    Ea <- Xa %*% t(ok_b); Eb <- Xb %*% t(ok_b)
    # le repliche usano l'inversa "naive" (veloce); la correzione di smearing si riporta con il rapporto
    # stima smearing / stima naive (esatto per log, = 1 per sqrt; per 1/sqrt e inversa senza questo
    # rapporto l'effetto puntuale cadeva fuori dal proprio IC: voli NYC, hour)
    st_n <- mean(yinv_naive(Xa %*% b, lambda) - yinv_naive(Xb %*% b, lambda))
    rapp <- if (is.finite(st_n) && abs(st_n) > 1e-12) st / st_n else 1
    bt <- colMeans(yinv_naive(Ea, lambda) - yinv_naive(Eb, lambda)) * rapp
    bt <- bt[is.finite(bt)]                     # repliche con coefficienti NA (un caso con leva ~1 non estratto)
    if (length(bt) < 20) return(c(effetto = st, IC_2.5 = NA, IC_97.5 = NA))
    c(effetto = st, quantile(bt, c(.025, .975)))
  }
  out <- list()
  vb <- intersect(x_orig, unique(unlist(lapply(termini(m), var_base))))
  for (v in vb) {
    if (is.numeric(dd[[v]])) {
      u <- sort(unique(dd[[v]]))
      intera <- all(abs(u - round(u)) < 1e-8)
      delta <- if (length(u) <= 2) diff(range(u)) else if (intera && sd(dd[[v]]) < 5) 1 else if (IQR(dd[[v]]) > 0) signif(IQR(dd[[v]]) / 2, 1) else signif(sd(dd[[v]]), 1)
      if (delta == 0) delta <- 1
      # x entrata solo come log(x): l'effetto naturale e' moltiplicativo (eta' del calcestruzzo: +20 giorni a 3 giorni
      # o a 365 sono cose diverse; il raddoppio ha lo stesso effetto ovunque)
      tv <- termini(m); tv <- tv[sapply(tv, function(tt) v %in% var_base(tt)) & !grepl(":", tv)]
      if (length(tv) && all(grepl(paste0("^log\\(", v, "( \\+ 1)?\\)$"), tv)) && all(dd[[v]] > 0)) {
        a <- dd; a[[v]] <- a[[v]] * 2; out[[paste0(v, " x2")]] <- eff(a, dd); next }
      a <- dd; a[[v]] <- a[[v]] + delta
      out[[paste0(v, " +", delta)]] <- eff(a, dd)
    } else if (is.factor(dd[[v]])) {
      rif <- levels(dd[[v]])[1]
      for (l in levels(dd[[v]])[-1]) {
        a <- dd; a[[v]] <- factor(l, levels = levels(dd[[v]])); r <- dd; r[[v]] <- factor(rif, levels = levels(dd[[v]]))
        out[[paste0(v, ": ", l, " vs ", rif)]] <- eff(a, r)
      }
    }
  }
  # effetti condizionati: con un'interazione l'effetto medio nasconde effetti diversi tra gruppi (fumo:
  # +24.000 $ in media, ma ~+15.000 sotto BMI 30 e ~+35.000 sopra). Per ogni x in un'interazione con w,
  # l'effetto dentro i gruppi di w: livelli se w e' un fattore, la soglia se nel modello c'e' I(w >= c),
  # altrimenti sotto/sopra la mediana di w
  tm <- termini(m); coppie <- unique(do.call(rbind, lapply(tm[grepl(":", tm)], function(tt) { vv <- intersect(var_base(tt), vb)
    if (length(vv) == 2) rbind(vv, rev(vv)) else NULL })))
  if (!is.null(coppie)) for (i in seq_len(nrow(coppie))) {
    v <- coppie[i, 1]; w <- coppie[i, 2]
    gr <- if (is.factor(dd[[w]])) { g <- as.character(dd[[w]]); setNames(lapply(levels(dd[[w]]), function(l) which(g == l)), paste(w, "=", levels(dd[[w]]))) } else {
      sg <- regmatches(tm, regexpr(paste0("I\\(", w, " >= [-0-9.e]+\\)"), tm))
      c0 <- if (length(sg)) as.numeric(sub(".*>= ([-0-9.e]+)\\)", "\\1", sg[1])) else median(dd[[w]], na.rm = TRUE)
      setNames(list(which(dd[[w]] < c0), which(dd[[w]] >= c0)), c(sprintf("%s < %s", w, format(signif(c0, 3))), sprintf("%s >= %s", w, format(signif(c0, 3))))) }
    for (gn in names(gr)) { idx <- gr[[gn]]; if (length(idx) < 10) next
      if (is.numeric(dd[[v]])) {
        nm0 <- grep(paste0("^", v, " (\\+|x2$)"), names(out), value = TRUE)[1]; if (is.na(nm0)) next
        a <- dd[idx, ]; if (grepl("x2$", nm0)) a[[v]] <- a[[v]] * 2 else a[[v]] <- a[[v]] + as.numeric(sub(".*\\+", "", nm0))
        out[[paste0(nm0, " | ", gn)]] <- eff(a, dd[idx, ])
      } else if (is.factor(dd[[v]])) { rif <- levels(dd[[v]])[1]
        for (l in levels(dd[[v]])[-1]) { a <- dd[idx, ]; a[[v]] <- factor(l, levels = levels(dd[[v]])); r <- dd[idx, ]; r[[v]] <- factor(rif, levels = levels(dd[[v]]))
          out[[paste0(v, ": ", l, " vs ", rif, " | ", gn)]] <- eff(a, r) } }
    }
  }
  do.call(rbind, out)
}

cv_rmse <- function(form, dd, lambda, yorig, k = 10, seed = 11) {
  set.seed(seed); fold <- fold_di(rownames(dd), k); err <- numeric(nrow(dd))
  for (i in 1:k) {
    tr <- dd[fold != i, ]; te <- dd[fold == i, ]
    mm <- tryCatch(lmx(form, tr), error = function(e) NULL)
    if (is.null(mm)) return(NA)
    pr <- tryCatch(predict(mm, te), error = function(e) rep(NA, nrow(te)))
    err[fold == i] <- dd[[yorig]][fold == i] - yinv(pr, lambda, residuals(mm))
  }
  sqrt(mean(err^2, na.rm = TRUE))
}

finale <- function(fit0, m, dd, y, lambda, boot = NULL, x_orig) {
  F <- "7_finale"; reset_fase(F)
  f0 <- lmx(formula(fit0), dd)                                 # iniziale sulle stesse osservazioni
  yh0 <- fitted(f0); yh <- yinv(fitted(m), lambda, residuals(m)); yo <- dd[[y]]
  jac <- if (lambda == 1) 0 else if (lambda == 0) -sum(log(yo)) else sum(log(abs(lambda) * yo^(lambda - 1)))
  cf <- data.frame(modello = c("iniziale (y originale, tutte le x)", paste("finale,", nome_trasf(lambda))),
                   parametri = c(length(coef(f0)), length(coef(m))),
                   cor_y_yhat = c(cor(yo, yh0), cor(yo, yh)), R2_scala_y = c(cor(yo, yh0)^2, cor(yo, yh)^2),
                   RMSE = c(sqrt(mean((yo - yh0)^2)), sqrt(mean((yo - yh)^2))),
                   AIC_scala_y = c(AIC(f0), AIC(m) - 2 * jac),
                   RMSE_CV10 = c(cv_rmse(formula(fit0), dd, 1, y), cv_rmse(formula(m), dd, lambda, y)))
  if (requireNamespace("gvlma", quietly = TRUE)) {
    g0 <- tryCatch(gvlma::gvlma(f0)$GlobalTest$GlobalStat4$Value, error = function(e) NA)
    g1 <- tryCatch(gvlma::gvlma(m)$GlobalTest$GlobalStat4$Value, error = function(e) NA)
    cf$gvlma_globale <- c(g0, g1)
  }
  salva_tab(cf, "7_confronto_iniziale_finale")
  png_apri("7_osservati_vs_previsti", 10, 4.5); par(mfrow = c(1, 2)); lim <- quantile(c(yo, yh, yh0), c(0, .995))
  for (z in list(list(yh0, "Iniziale"), list(yh, "Finale"))) {
    plot(z[[1]], yo, pch = 16, cex = .3, col = adjustcolor(1, .3), xlim = lim, ylim = lim, main = z[[2]],
         xlab = "previsto", ylab = "osservato"); abline(0, 1, col = 4, lty = 2, lwd = 2)
    lines(lowess(z[[1]], yo), col = 2, lwd = 2) }
  dev.off()
  png_apri("7_residui_confronto", 9, 7); par(mfrow = c(2, 2))
  plot(f0, which = 1, main = "Iniziale", caption = "", cex = .4); plot(m, which = 1, main = "Finale", caption = "", cex = .4)
  plot(f0, which = 2, main = "Iniziale", caption = "", cex = .4); plot(m, which = 2, main = "Finale", caption = "", cex = .4)
  dev.off()
  ef <- effetti_originali(m, dd, lambda, if (!is.null(boot)) boot$B$t else NULL, x_orig)
  salva_tab(round(ef, 3), "7_effetti_scala_originale")
  w <- coeftest(m, vcov = vc_rob(m))
  tf <- data.frame(b = coef(m), SE_White = w[, 2], p_White = w[, 4])
  if (!is.null(boot)) { tf$IC_boot_2.5 <- boot$tab$IC_2.5; tf$IC_boot_97.5 <- boot$tab$IC_97.5 }
  salva_tab(tf, "7_tabella_finale")
  registra(F, "7", "iniziale vs finale", sprintf("cor y-yhat %.3f -> %.3f; RMSE %.2f -> %.2f; RMSE CV10 %.2f -> %.2f",
           cf$cor_y_yhat[1], cf$cor_y_yhat[2], cf$RMSE[1], cf$RMSE[2], cf$RMSE_CV10[1], cf$RMSE_CV10[2]),
           "stesse osservazioni, scala originale di y (previsioni corrette per la trasformazione)", "-")
  list(confronto = cf, effetti = ef, tabella = tf)
}

# ---------------------------------------------------------------------
# PARTE 1b - Y BINARIA (logistica)
# ---------------------------------------------------------------------
hosmer_lemeshow <- function(y, p, g = 10) {
  q <- unique(quantile(p, seq(0, 1, length.out = g + 1)))
  gr <- cut(p, q, include.lowest = TRUE)
  o1 <- tapply(y, gr, sum); e1 <- tapply(p, gr, sum); nn <- tapply(y, gr, length)
  st <- sum((o1 - e1)^2 / (e1 * (1 - e1 / nn)))
  c(chi2 = st, df = length(nn) - 2, p = pchisq(st, length(nn) - 2, lower.tail = FALSE))
}
# evento della logistica: y > soglia, oppure un'espressione dell'utente su piu' colonne (cfg$evento)
evento_binario <- function(d, y, soglia, evento = "") {
  if (!nzchar(evento)) return(as.integer(d[[y]] > soglia))
  e <- tryCatch(with(d, eval(parse(text = evento))), error = function(e) stop("evento '", evento, "' non calcolabile: ", conditionMessage(e),
                " (le colonne usate devono restare nei dati: escluderle dalle x con decisioni, non con escludi)"))
  as.integer(e)
}
nzv_flag <- function(x) { t <- sort(table(x), decreasing = TRUE)
  length(t) > 1 && t[1] / t[2] > 19 && length(t) / length(x) < 0.1 }

logistica <- function(dL, y, x, cfg, interazioni_lin = character(), R_boot = 999) {
  F <- "8_logistica"; reset_fase(F)
  soglia <- cfg$decisioni$soglia_binaria; tipo <- "UTENTE"
  if (is.null(soglia)) { soglia <- median(dL[[y]]); tipo <- "PROPOSTA" }
  evento <- cfg$evento %||% ""
  dL$.yb <- evento_binario(dL, y, soglia, evento)
  q <- quantile(dL[[y]], c(.5, .75, .9))
  if (nzchar(evento)) registra(F, "L.1", "evento binario", sprintf("%s -> %.1f%% eventi", evento, 100 * mean(dL$.yb)),
           "evento sostanziale definito dall'utente (puo' usare altre colonne, es. incasso > 2,5 x budget)", "applicato", "UTENTE")
  else registra(F, "L.1", "soglia y binaria", sprintf("y > %s -> %.1f%% eventi (mediana=%s, Q3=%s, P90=%s)",
           signif(soglia, 4), 100 * mean(dL$.yb), signif(q[1], 4), signif(q[2], 4), signif(q[3], 4)),
           "soglia motivata (mediana = classi bilanciate; soglia sostanziale se esiste)", "applicata", tipo)
  # controlli: varianza quasi nulla + separazione
  esc <- character(); xx <- x; mappe_b <- list()
  for (v in x) {
    z <- dL[[v]]
    if (is.numeric(z)) {
      # separazione: fuori dal valore piu' frequente ci sono solo eventi (o solo non-eventi)?
      # es. studenti: chi ha assenze > 0 ha sempre sostenuto l'esame (assenze non registrate per chi abbandona)
      mod <- as.numeric(names(which.max(table(z)))); fuori <- z != mod
      if (sum(fuori) >= 10 && sum(!fuori) >= 10) { ev <- sum(dL$.yb[fuori])
        if (ev == 0 || ev == sum(fuori)) { esc <- c(esc, v)
          registra(F, "L.2", paste("x tolta", v), sprintf("separazione: con %s diverso da %s %s", v, mod,
                   if (ev == 0) "mai l'evento" else "sempre l'evento"),
                   "separazione completa su una parte dei dati: MLE non esiste (coefficiente verso infinito)",
                   "eliminata; controllare se e' un artefatto di registrazione (valore non rilevato per un gruppo)", "PROPOSTA") } }
    } else {
      tb <- table(z, dL$.yb)
      if (nzv_flag(z) && nlevels(z) == 2 && any(tb == 0)) { esc <- c(esc, v)
        registra(F, "L.2", paste("x tolta", v), "separazione", "cella vuota in un fattore quasi costante", "eliminata") }
    }
  }
  xx <- setdiff(xx, esc)
  # optimal grouping per y binaria sui fattori (> 2 livelli), poi celle con < 5 eventi fuse col vicino
  uno <- xx[sapply(dL[xx], function(z) is.factor(z) && nlevels(droplevels(z)) < 2)]
  xx <- setdiff(xx, uno)
  for (v in xx[sapply(dL[xx], function(z) is.factor(z) && nlevels(z) > 2)]) {
    og <- og_merge(dL$.yb, dL[[v]], family = "binomial"); nv <- paste0(v, "_b")
    if (og$n_gruppi < 2) { xx <- setdiff(xx, v)
      registra(F, "L.2", paste("grouping binario", v), "1 gruppo", "AIC minimo senza il fattore", "x tolta", "PROPOSTA"); next }
    z <- og$nuovo(dL[[v]])
    repeat {
      tb <- table(z, dL$.yb); piccole <- rownames(tb)[apply(tb, 1, min) < 5]
      if (!length(piccole) || nlevels(z) <= 2) break
      pr <- tapply(dL$.yb, z, mean); l <- piccole[1]
      vic <- names(pr)[-match(l, names(pr))][which.min(abs(pr[-match(l, names(pr))] - pr[l]))]
      zz <- as.character(z); zz[zz %in% c(l, vic)] <- paste(l, vic, sep = "+"); z <- factor(zz)
    }
    if (nlevels(z) < 2) { xx <- setdiff(xx, v); next }
    dL[[nv]] <- relevel_max(z); xx[xx == v] <- nv
    mm <- unique(data.frame(o = as.character(dL[[v]]), g = as.character(dL[[nv]])))
    mappe_b[[nv]] <- list(var = v, mappa = setNames(mm$g, mm$o), livelli = levels(dL[[nv]]))
    registra(F, "L.2", paste("grouping binario", v), paste(nlevels(dL[[v]]), "->", nlevels(z), "gruppi"),
             "factorMerger-like con family binomial, AIC minimo; poi celle < 5 eventi fuse col livello piu' simile", "-")
  }
  # interazioni del modello lineare, se ancora stimabili
  it <- character()
  for (i in interazioni_lin) { vv <- strsplit(i, ":")[[1]]
    vv2 <- sapply(vv, function(a) if (a %in% xx) a else if (paste0(a, "_b") %in% xx) paste0(a, "_b") else NA)
    if (!any(is.na(vv2))) it <- c(it, paste(vv2, collapse = ":")) }
  form <- f_da_termini(".yb", c(xx, it))
  g <- withCallingHandlers(glmx(form, dL), warning = function(w) {
    registra(F, "L.2", "warning glm", conditionMessage(w), "possibile separazione", "controllare"); invokeRestart("muffleWarning") })
  se <- summary(g)$coefficients[, 2]
  if (any(se > 5)) { esplosi <- names(se)[se > 5]
    tt <- termini(g); via <- tt[sapply(tt, function(t) any(sapply(esplosi, function(e) startsWith(e, sub(":.*", "", t)))))]
    it <- setdiff(it, via[grepl(":", via)])
    if (length(via[grepl(":", via)])) {
      g <- suppressWarnings(glmx(f_da_termini(".yb", c(xx, it)), dL))
      registra(F, "L.2", "interazioni tolte", paste(via[grepl(":", via)], collapse = ", "),
               "coefficienti esplosi (SE > 5): quasi-separazione", "tolte") }
  }
  # forma delle numeriche sul logit PRIMA della selezione: una x con effetto non monotono (anno di uscita
  # sugli zeri di Spotify: pochi zeri prima del 2000 e dopo il 2018, molti in mezzo) ha pendenza lineare
  # ~0 e la selezione la scarterebbe. GAM logistico su tutte le numeriche, poi polinomio centrato se AIC -10
  numL <- xx[sapply(xx, function(v) is.numeric(dL[[v]]) && length(unique(dL[[v]])) >= 5)]   # anche conteggi (dimensione della famiglia)
  if (length(numL) && requireNamespace("mgcv", quietly = TRUE)) {
    fg0 <- as.formula(paste(".yb ~", paste(c(setdiff(c(xx, it), numL), sapply(numL, function(v)
      paste0("s(", v, ", k=", min(10, length(unique(dL[[v]])) - 1), ")"))), collapse = " + ")))
    gg0 <- tryCatch(mgcv::gam(fg0, data = dL, family = binomial, method = "REML"), error = function(e) NULL)
    if (!is.null(gg0)) { st0 <- summary(gg0)$s.table
      nl0 <- sub("^s\\((.*)\\)$", "\\1", rownames(st0)[st0[, "edf"] > 1.5 & st0[, "p-value"] < 0.05])
      for (v in nl0) {
        cc <- if (abs(mean(dL[[v]])) > 3 * sd(dL[[v]])) round(mean(dL[[v]])) else 0
        pw <- function(k) if (cc == 0) paste0("I(", v, "^", k, ")") else paste0("I((", v, " - ", cc, ")^", k, ")")
        cand <- list(poly2 = c(v, pw(2)), poly3 = c(v, pw(2), pw(3)))
        if (min(dL[[v]], na.rm = TRUE) > 0) cand <- c(list(log = paste0("log(", v, ")"), log2 = c(paste0("log(", v, ")"), paste0("I(log(", v, ")^2)"))), cand)
        base <- AIC(g); fits <- lapply(cand, function(tt) tryCatch(suppressWarnings(glmx(f_da_termini(".yb", c(setdiff(termini(g), v), tt)), dL)), error = function(e) NULL))
        a <- sapply(fits, function(f) if (is.null(f)) Inf else AIC(f)); best <- names(which.min(a))
        if (a[best] < base - 10) { g <- fits[[best]]
          registra(F, "L.2b", paste("forma sul logit", v), sprintf("GAM edf %.1f; AIC %.1f -> %.1f con %s", st0[paste0("s(", v, ")"), "edf"], base, a[best], best),
                   "non linearita' sul logit cercata prima della selezione (altrimenti una x non monotona viene scartata)", paste("uso", paste(cand[[best]], collapse = " + ")), "AUTO") }
      } }
  }
  # selezione: step AIC + ibrido LRT
  g_aic <- completa_gerarchia(step(g, trace = 0))
  gL <- g_aic; tolti <- character()
  repeat { a <- drop1(gL, test = "LRT"); a <- a[!is.na(a$`Pr(>Chi)`), , drop = FALSE]
    if (!nrow(a) || max(a$`Pr(>Chi)`) <= 0.05) break
    via <- rownames(a)[which.max(a$`Pr(>Chi)`)]; tolti <- c(tolti, via); gL <- update(gL, as.formula(paste(". ~ . -", via))) }
  repeat {     # coefficienti esplosi (SE > 5): quasi-separazione -> tolgo il termine e ristimo
    seL <- summary(gL)$coefficients[-1, 2, drop = TRUE]; if (!length(seL) || max(seL) <= 5) break
    peggiore <- names(seL)[which.max(seL)]
    tt <- termini(gL); via <- tt[sapply(tt, function(t) startsWith(peggiore, t) || startsWith(peggiore, sub(":.*", "", t)))][1]
    if (is.na(via)) break
    # gerarchia: se la x ha potenze piu' alte (x^2, x^3) si semplifica la forma togliendo la piu' alta, mai il
    # termine lineare lasciando le potenze (studenti, media G1-G2: restavano solo (x - 11)^2 e (x - 11)^3, lezione 138)
    fam <- tt[!grepl(":", tt) & sapply(tt, function(t) identical(var_base(t), var_base(via)))]
    if (length(fam) > 1) via <- fam[which.max(sapply(fam, grado))]
    gL <- suppressWarnings(update(gL, as.formula(paste(". ~ . -", via))))
    registra(F, "L.4", paste("termine tolto per quasi-separazione", via), sprintf("SE %.1f", max(seL)),
             "SE > 5: la stima non e' affidabile", "tolto e modello ristimato", "AUTO")
  }
  registra(F, "L.4", "selezione", sprintf("tolti con AIC: %s; poi con LRT: %s",
           paste(setdiff(termini(g), termini(g_aic)), collapse = ", "), paste(tolti, collapse = ", ")),
           "step AIC + approccio ibrido (LRT p > 0.05)", sprintf("modello gL: %d parametri, AIC %.1f", length(coef(gL)), AIC(gL)), "PROPOSTA")
  g0 <- glmx(.yb ~ 1, dL)
  hl <- hosmer_lemeshow(dL$.yb, fitted(gL))
  fitstat <- c(dev_nulla = g0$deviance, dev_residua = gL$deviance, McFadden = 1 - gL$deviance / g0$deviance,
               AIC = AIC(gL), HL_chi2 = hl[["chi2"]], HL_p = hl[["p"]], max_SE = max(summary(gL)$coefficients[, 2]))
  salva_tab(round(t(t(fitstat)), 4), "8_adattamento")
  seL <- summary(gL)$coefficients[-1, 2]   # l intercetta e' esclusa: con x non centrate (es. anni) e' imprecisa per costruzione
  if (any(seL > 5)) registra(F, "L.5", "coefficienti instabili", paste(names(seL)[seL > 5], collapse = ", "),
    "SE > 5: quasi-separazione (un livello con quasi solo eventi o non-eventi)",
    "unire quel livello al piu' simile (codice_preparazione) o togliere la x; non leggere quegli OR", "PROPOSTA")
  registra(F, "L.6", "adattamento", sprintf("McFadden=%.2f; Hosmer-Lemeshow chi2=%.1f p=%.3f", fitstat["McFadden"],
           hl[["chi2"]], hl[["p"]]), "HL: H0 buon adattamento (p > 0.05)", "-")
  ci <- tryCatch(suppressMessages(confint(gL)), error = function(e) confint.default(gL))
  or <- data.frame(b = coef(gL), OR = exp(coef(gL)), OR_2.5 = exp(ci[, 1]), OR_97.5 = exp(ci[, 2]),
                   p_Wald = summary(gL)$coefficients[, 4])
  if ("(Intercept)" %in% rownames(or)) or["(Intercept)", c("OR", "OR_2.5", "OR_97.5")] <- NA   # senza significato
  # OR per incrementi non unitari (corso): stesso incremento usato per gli effetti marginali
  or$incremento <- NA_real_; or$OR_incremento <- NA_real_; or$OR_inc_2.5 <- NA_real_; or$OR_inc_97.5 <- NA_real_
  for (k in rownames(or)) if (k %in% names(dL) && is.numeric(dL[[k]])) {
    u <- unique(dL[[k]]); dlt <- if (length(u) <= 2) 1 else if (sd(dL[[k]]) < 5) 1 else if (IQR(dL[[k]]) > 0) signif(IQR(dL[[k]]) / 2, 1) else signif(sd(dL[[k]]), 1)
    or[k, c("incremento", "OR_incremento", "OR_inc_2.5", "OR_inc_97.5")] <- c(dlt, exp(dlt * or[k, "b"]), exp(dlt * ci[k, 1]), exp(dlt * ci[k, 2])) }
  salva_tab(or, "8_odds_ratio")
  # OR per incrementi non unitari ed effetti marginali medi sulla probabilita'
  ame <- list()
  for (v in intersect(unique(unlist(lapply(termini(gL), var_base))), names(dL))) {
    if (is.numeric(dL[[v]])) { u <- unique(dL[[v]])
      delta <- if (length(u) <= 2) 1 else if (sd(dL[[v]]) < 5) 1 else if (IQR(dL[[v]]) > 0) signif(IQR(dL[[v]]) / 2, 1) else signif(sd(dL[[v]]), 1)
      a <- dL; a[[v]] <- a[[v]] + delta
      ame[[paste0(v, " +", delta)]] <- mean(predict(gL, a, type = "response") - fitted(gL))
    } else for (l in levels(dL[[v]])[-1]) { a <- dL; r <- dL
      a[[v]] <- factor(l, levels = levels(dL[[v]])); r[[v]] <- factor(levels(dL[[v]])[1], levels = levels(dL[[v]]))
      ame[[paste0(v, ": ", l, " vs ", levels(dL[[v]])[1])]] <- mean(predict(gL, a, type = "response") - predict(gL, r, type = "response")) }
  }
  salva_tab(data.frame(effetto_marginale_medio_punti_pct = round(100 * unlist(ame), 2)), "8_effetti_marginali")
  # classificazione (solo cenno)
  cl <- t(sapply(c(0.5, mean(dL$.yb)), function(s) { tb <- table(factor(dL$.yb, 0:1), factor(as.integer(fitted(gL) > s), 0:1))
    c(soglia = s, accuratezza = sum(diag(tb)) / sum(tb), sensibilita = tb[2, 2] / sum(tb[2, ]), specificita = tb[1, 1] / sum(tb[1, ])) }))
  salva_tab(round(cl, 3), "8_classificazione")
  # facoltativi: GAM logistico sulle numeriche, DIFCHISQ
  num <- intersect(termini(gL), names(dL)[sapply(dL, function(z) is.numeric(z) && length(unique(z)) >= 5)])
  if (length(num) && requireNamespace("mgcv", quietly = TRUE)) {
    fg <- as.formula(paste(".yb ~", paste(c(setdiff(termini(gL), num), sapply(num, function(v)
      paste0("s(", v, ", k=", min(10, length(unique(dL[[v]])) - 1), ")"))), collapse = " + ")))
    gg <- tryCatch(mgcv::gam(fg, data = dL, family = binomial, method = "REML"), error = function(e) NULL)
    if (!is.null(gg)) { st <- summary(gg)$s.table; salva_tab(round(st, 4), "8_gam_logistico_edf")
      nl <- rownames(st)[st[, "edf"] > 1.5 & st[, "p-value"] < 0.05]
      registra(F, "L.9", "GAM logistico", if (length(nl)) paste("non lineari sul logit:", paste(nl, collapse = ", ")) else
        "tutte lineari sul logit", "edf > 1.5 e p < 0.05", if (length(nl)) "valutare polinomio/binning" else "-",
        if (length(nl)) "PROPOSTA" else "AUTO") }
  }
  dch <- residuals(gL, type = "pearson")^2 / (1 - hatvalues(gL))
  registra(F, "L.9", "DIFCHISQ > 4", sum(dch > 4), "influenza nella logistica (soglia del corso)",
           "non eliminate di default: l'eliminazione puo' creare separazione", "PROPOSTA")
  BL <- NULL
  if (R_boot > 0) { set.seed(2026); BL <- suppressWarnings(Boot(gL, R = R_boot, ncores = max(1, parallel::detectCores() - 1)))
    # esplosione su scala standardizzata (b * sd della colonna): un polinomio grezzo di una x grande
    # (pressione ~1000, al cubo ~1e9) ha |b| > 10 per costruzione senza alcuna separazione
    sdX <- apply(model.matrix(gL), 2, sd); sdX[sdX == 0] <- NA        # intercetta esclusa
    espl <- sum(apply(abs(sweep(BL$t, 2, sdX[colnames(BL$t)], "*")) > 10, 1, any, na.rm = TRUE))
    q <- t(apply(BL$t, 2, quantile, c(.025, .975), na.rm = TRUE))
    salva_tab(data.frame(b = coef(gL), se = summary(gL)$coefficients[, 2], se_boot = apply(BL$t, 2, sd, na.rm = TRUE),
                         IC_2.5 = q[, 1], IC_97.5 = q[, 2]), "8_bootstrap")
    registra(F, "L.9", "bootstrap logit", sprintf("R=%d, repliche esplose (|b x sd(x)|>10): %d (%.0f%%)", R_boot, espl, 100 * espl / R_boot),
             "stabilita' dei parametri: repliche esplose = quasi-separazione nei ricampionamenti",
             if (espl > 0.1 * R_boot) "ATTENZIONE: coefficienti e OR non affidabili (pochi eventi o gruppi quasi separati); leggere solo segni e AUC" else "-",
             if (espl > 0.1 * R_boot) "PROPOSTA" else "AUTO") }
  list(g = gL, g_full = g, d = dL, fit = fitstat, or = or, ame = ame, class = cl, mappe_b = mappe_b, soglia = soglia, evento = evento)
}

######################################################################
# COSTRUZIONE / VERIFICA
# Il modello si costruisce su un campione di costruzione (dimensione adattiva) e si verifica
# sui dati mai visti: precisione predittiva, stabilita' dei coefficienti, e test "Pareto"
# per decidere se i coefficienti finali vanno riestimati su tutti i dati.
######################################################################
`%||%` <- function(a, b) if (is.null(a)) b else a

# --------------------------------------------------------------------
# costruzione del modello lineare (passi 0-7) come funzione riusabile.
# completo = FALSE: solo passi 0-4, senza file ne' grafici (per la convalida incrociata della procedura)
# --------------------------------------------------------------------
# passo 2 su una data scala di y: grouping dei fattori, forme delle x, interazioni, gerarchia
passo2 <- function(d1, y, x1, lambda, cfg) {
  cfg$.lambda <- lambda          # scala corrente: serve allo screening delle interazioni per valutare sulla scala di y
  d2 <- d1; d2$.yt <- ytrasf(d2[[y]], lambda)
  og <- grouping_fattori(d2, ".yt", x1, cfg); d2 <- og$d; x2 <- og$x
  tx <- trasforma_x(d2, ".yt", x2, cfg); t_base <- unlist(tx$termini, use.names = FALSE)
  it <- interazioni(d2, ".yt", t_base, x2, cfg)
  princ <- setdiff(unique(unlist(lapply(it$it, function(i) strsplit(i, ":")[[1]]))), unlist(lapply(t_base, var_base)))
  princ <- intersect(princ, names(d2))
  if (length(princ)) { t_base <- c(t_base, princ)          # gerarchia: un'interazione richiede i suoi effetti principali
    registra("2_trasformazioni", "2.6", "effetti principali aggiunti", paste(princ, collapse = ", "), "gerarchia delle interazioni", "aggiunti", "AUTO") }
  fit2 <- lmx(f_da_termini(".yt", c(t_base, it$it)), data = d2)
  list(d2 = d2, og = og, tx = tx, it = it, t_base = t_base, x2 = x2, fit2 = fit2)
}
# convalida incrociata di una formula fissa con previsione della media sulla scala originale di y
cv_scala <- function(fit, dd, y, lambda, met = "RMSE", seed = 1, K = 10) {
  set.seed(seed); n <- nrow(dd); fold <- fold_di(rownames(dd), K); f <- formula(fit); e <- rep(NA_real_, n)
  for (k in 1:K) { a <- dd[fold != k, ]; b <- dd[fold == k, ]
    m <- tryCatch(lmx(f, a), error = function(e) NULL); if (is.null(m)) next
    e[fold == k] <- b[[y]] - yinv(tryCatch(prevedi_sicuro(m, b), error = function(e) rep(NA_real_, nrow(b))), lambda, residuals(m)) }
  if (mean(is.na(e)) > 0.05) return(NA_real_)
  if (met == "RMSE") sqrt(mean(e^2, na.rm = TRUE)) else mean(abs(e), na.rm = TRUE)
}

costruisci_lineare <- function(dp, cfg, completo = TRUE) {
  y <- cfg$y
  if (!completo) { vecchio <- isTRUE(.RM$quiet); .RM$quiet <- TRUE; on.exit(.RM$quiet <- vecchio) }
  d <- prepara_apprendi(dp, cfg); sp <- attr(d, "stato_prep")
  if (!is.numeric(d[[y]])) stop("la y deve essere numerica")
  # pesi FGLS (Hill 8.5): con un voto medio la varianza e' tau^2 + sigma^2/n, non sigma^2/n. Stimo la
  # funzione di varianza dai residui OLS (e^2 = a + b/n) e peso con 1/(a + b/n): pesi 1/n puri danno
  # rapporti 1:1000 e spostano tutto verso le unita' piu' "pesanti" (giochi: R2 fuori campione 0,35 contro 0,42 OLS)
  pab <- NULL
  if (".w" %in% names(d) && identical(cfg$pesi_modo %||% "fgls", "fgls")) {
    d0 <- d; d0$.w <- NULL; f00 <- lm(f_da_termini(y, x_iniziali(d0, y)), data = d0)
    r2 <- residuals(f00)^2; nw <- d[names(r2), ".w"]
    vm <- lm(r2 ~ I(1 / nw)); a <- max(coef(vm)[1], 0.05 * mean(r2)); b <- max(coef(vm)[2], 0)
    w <- 1 / (a + b / d$.w); pab <- c(a = a, b = b, norm = mean(w)); d$.w <- w / pab[["norm"]]
    registra("0_preparazione", "0.1", "pesi FGLS", sprintf("e^2 = %.3g + %.3g / n (R2 %.2f); pesi da %.2f a %.2f (rapporto %.1f)", a, b,
             summary(vm)$r.squared, min(d$.w), max(d$.w), max(d$.w) / min(d$.w)),
             "funzione di varianza stimata dai residui OLS (FGLS): peso = 1 / varianza stimata",
             if (b == 0) "nessuna relazione tra varianza e n: equivale a OLS" else "WLS con pesi stimati", "AUTO")
  }
  aa <- separa_ausiliarie(d, cfg); d <- aa$d
  if (completo) { descrittive(d, y); fit0 <- modello_iniziale(d, y) } else
    fit0 <- lmx(f_da_termini(y, x_iniziali(d, y)), data = d)
  d_imp <- imputa_con_ausiliarie(aa, y, cfg)
  co <- collinearita(d_imp, y, cfg); x1 <- co$x; d1 <- co$d; fit1 <- co$fit
  reset_fase("2_trasformazioni")
  bc <- boxcox_scelta(d1, y, x1, cfg); lambda <- bc$lambda
  # scala di y: Box-Cox massimizza la verosimiglianza normale dei residui sulla scala trasformata, non
  # l'errore di previsione sulla scala di y. Con y bimodale o effetti additivi (assicurazioni: fumatori
  # obesi +20.000 $) log(y) storce la struttura e la previsione peggiora molto. Se Box-Cox propone una
  # trasformazione, rifaccio il passo 2 anche con y non trasformata e scelgo con la convalida incrociata
  # sulla scala originale, con la metrica guida (RMSE se metrica = media, MAE altrimenti)
  scala_cv <- FALSE
  if (lambda != 1 && is.null(cfg$decisioni$lambda)) {
    vq <- isTRUE(.RM$quiet); .RM$quiet <- TRUE          # due passi 2 silenziosi, poi quello scelto con il registro attivo
    p2 <- passo2(d1, y, x1, lambda, cfg)
    p2b <- tryCatch(passo2(d1, y, x1, 1, cfg), error = function(e) NULL)
    .RM$quiet <- vq
    if (is.null(p2b)) { if (!vq) p2 <- passo2(d1, y, x1, lambda, cfg) } else {
      met <- if (identical(cfg$metrica, "media")) "RMSE" else "MAE"
      ea <- cv_scala(p2$fit2, p2$d2, y, lambda, met, cfg$seed %||% 1); eb <- cv_scala(p2b$fit2, p2b$d2, y, 1, met, cfg$seed %||% 1)
      # soglia: con pochi dati la convalida e' rumorosa (grasso corporeo, 71 righe: la scelta cambiava tra fold)
      soglia_sc <- if (nrow(p2$d2) < 300) 0.95 else 0.98
      scelta_1 <- is.finite(eb) && is.finite(ea) && eb < soglia_sc * ea
      registra("2_trasformazioni", "2.2b", "scala di y per la previsione", sprintf("%s in convalida (10 fold, scala di y): %s %.4g, y non trasformata %.4g",
               met, nome_trasf(lambda), ea, eb), "Box-Cox guarda la normalita' dei residui, non la previsione: confronto le due scale rifacendo il passo 2 su ciascuna",
               if (scelta_1) sprintf("uso y non trasformata (%s -%.0f%%); config decisioni.lambda per imporre", met, 100 * (1 - eb / ea)) else
                 sprintf("tengo %s", nome_trasf(lambda)), if (scelta_1) "PROPOSTA" else "AUTO")
      if (scelta_1) { lambda <- 1; scala_cv <- TRUE; p2 <- p2b }
      if (!vq) p2 <- passo2(d1, y, x1, lambda, cfg)                # rifatto con il registro e i grafici attivi
    }
  } else p2 <- passo2(d1, y, x1, lambda, cfg)
  d2 <- p2$d2; og <- p2$og; tx <- p2$tx; it <- p2$it; t_base <- p2$t_base; x2 <- p2$x2; fit2 <- p2$fit2
  if (any(is.na(coef(fit2)))) registra("2_trasformazioni", "2.7", "coef NA nel modello 2",
    paste(names(coef(fit2))[is.na(coef(fit2))], collapse = ", "), "collinearita' esatta", "da controllare", "PROPOSTA")
  if (completo) {
    vp <- tryCatch(vif(fit2, type = "predictor"), error = function(e) NULL)
    if (!is.null(vp)) salva_tab(vp, "2_vif_dopo_trasformazioni")
    registra("2_trasformazioni", "2.7", "modello fit2", sprintf("p=%d adjR2=%.4f; RESET %.1f -> %.1f; asimmetria res %.2f -> %.2f; W Shapiro %.3f -> %.3f",
             length(coef(fit2)), summary(fit2)$adj.r.squared, reset_stat(fit1), reset_stat(fit2), skew(residuals(fit1)),
             skew(residuals(fit2)), shapiro_w(residuals(fit1)), shapiro_w(residuals(fit2))),
             "RESET prima/dopo, normalita' e simmetria dei residui", "modello di lavoro")
    png_apri("2_residui_prima_dopo", 9, 7); par(mfrow = c(2, 2))
    plot(fit1, which = 1, main = "Passo 1", caption = "", cex = .4); plot(fit2, which = 1, main = "Passo 2", caption = "", cex = .4)
    plot(fit1, which = 2, main = "Passo 1", caption = "", cex = .4); plot(fit2, which = 2, main = "Passo 2", caption = "", cex = .4)
    dev.off()
  }
  se <- selezione(fit2, d2); fit3 <- se$fit
  inf <- influenza(fit3, d2, ".yt", cfg); fit4 <- inf$fit; d4 <- inf$d
  fa <- cfg$fino_a %||% "finale"
  if (fa %in% c("collinearita", "trasformazioni", "selezione")) {
    # la verifica ha mostrato che i passi successivi peggiorano fuori campione: modello finale = quel passo
    if (fa == "collinearita") { d4 <- d1; d4$.yt <- d4[[y]]; fit4 <- lmx(f_da_termini(".yt", termini(fit1)), d4); lambda <- 1 }
    else if (fa == "trasformazioni") { fit4 <- fit2; d4 <- d2 } else { fit4 <- fit3; d4 <- d2 }
    inf$strat <- "nessuna"
    registra("4_influenza", "4.0", "procedura fermata", paste("fino_a =", fa), "scelta dell'utente dopo il percorso fuori campione",
             paste("modello finale = modello del passo", fa, "(passi successivi non applicati)"), "UTENTE")
  }
  st <- list(cfg = cfg, y = y, stato_prep = sp, d = d, d_imp = d_imp, d1 = d1, x1 = x1, quote_def = co$quote_def,
             lambda = lambda, mappe = og$mappe, d2 = d2, d4 = d4, fit0 = fit0, fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4, strat_infl = inf$strat, pesi_ab = pab)
  if (!completo) return(st)
  if (lambda != 1 && !scala_cv && all(d4[[y]] > 0)) {
    bc4 <- boxcox(lmx(f_da_termini(y, termini(fit4)), data = d4, y = TRUE, qr = TRUE), lambda = seq(-1, 1.5, 0.01), plotit = FALSE)
    l4 <- bc4$x[which.max(bc4$y)]
    registra("4_influenza", "4.5", "Box-Cox dopo la pulizia", round(l4, 2), "la trasformazione regge?",
             if (abs(l4 - lambda) > 0.2) "lambda cambiato: valutare (decisione dell'utente)" else "confermata",
             if (abs(l4 - lambda) > 0.2) "PROPOSTA" else "AUTO")
  }
  st$et <- eteroschedasticita(fit4, d4)
  R <- cfg$R_boot %||% 1999
  st$boot <- if (R > 0) bootstrap_lm(fit4, R) else NULL
  t0f <- termini(fit0); t0f <- t0f[sapply(t0f, function(v) !(is.factor(d4[[v]]) && nlevels(droplevels(d4[[v]])) < 2))]
  st$fin <- finale(lmx(f_da_termini(y, t0f), data = d4), fit4, d4, y, lambda, st$boot, names(d4))
  st$perc <- data.frame(passo = c("0 iniziale", "1 collinearita'", "2 trasformazioni", "3 selezione", "4 influenti"),
    y = c(y, y, nome_trasf(lambda), nome_trasf(lambda), nome_trasf(lambda)),
    n = sapply(list(fit0, fit1, fit2, fit3, fit4), nobs), p = sapply(list(fit0, fit1, fit2, fit3, fit4), function(m) length(coef(m))),
    adjR2 = sapply(list(fit0, fit1, fit2, fit3, fit4), function(m) summary(m)$adj.r.squared),
    RESET = sapply(list(fit0, fit1, fit2, fit3, fit4), reset_stat),
    BP = sapply(list(fit0, fit1, fit2, fit3, fit4), function(m) unname(bptest(m)$statistic)))
  salva_tab(st$perc, "7_percorso")
  st
}

# --------------------------------------------------------------------
# DIVISIONE: dimensione del campione di costruzione = osservazioni per parametro candidato
# (Harrell: almeno 15-20 per parametro; default 40, minimo 2000, massimo 10000)
# --------------------------------------------------------------------
conta_parametri <- function(d, y, min_liv = 30) {
  x <- setdiff(names(d), c(y, ".w"))
  sum(sapply(x, function(v) { z <- d[[v]]
    if (is.numeric(z)) return(1)
    t <- table(z); k <- sum(t >= min_liv) + any(t < min_liv); max(k - 1, 1) }))
}

campione_strat <- function(idx, k, yv) {         # stratificato per decili di y: stessa distribuzione di y
  if (k >= length(idx)) return(idx)
  g <- cut(rank(yv, ties.method = "first"), 10, labels = FALSE)
  unlist(lapply(split(seq_along(idx), g), function(ii) idx[ii[sample.int(length(ii), max(1, round(k * length(ii) / length(idx))))]]))
}

dividi <- function(d, cfg) {
  F <- "0_divisione"; reset_fase(F); y <- cfg$y; n <- nrow(d)
  set.seed(cfg$seed %||% 1234)
  p <- conta_parametri(d[setdiff(names(d), c(".tempo", ".w"))], y, cfg$livelli_rari_min %||% 30)
  opp <- cfg$oss_per_parametro %||% 40
  n_t <- cfg$n_costruzione %||% as.integer(min(max(opp * p, 2000), 10000))
  capv <- cfg$max_verifica %||% 50000
  cap <- function(i) if (length(i) > capv) sample(i, capv) else i
  val <- list(); tv <- list()
  if (!is.null(cfg$tempo) && nzchar(cfg$tempo)) {
    tt <- d$.tempo
    taglio <- cfg$taglio_tempo %||% unname(quantile(tt, 0.8, type = 1))
    passato <- which(tt < taglio); futuro <- which(tt >= taglio)
    build <- if (length(passato) > 1.25 * n_t) campione_strat(passato, n_t, d[[y]][passato]) else passato
    stessa <- setdiff(passato, build)
    min_v <- max(30, round(0.05 * n))     # serie corte (728 giorni): anche 150 giorni di futuro sono una verifica utile
    if (length(stessa) >= max(200, min_v)) { i <- cap(stessa); val$stessa_epoca <- i; tv$stessa_epoca <- tt[i] }
    if (length(futuro) >= min_v) { i <- cap(futuro); val$futuro <- i; tv$futuro <- tt[i] }
    modo <- "temporale"
    descr <- sprintf("costruzione: %d righe del periodo < %s (su %d); verifica stessa epoca: %d; verifica futuro (>= %s): %d",
                     length(build), format(taglio), length(passato), length(val$stessa_epoca), format(taglio), length(val$futuro))
  } else if (!is.null(.RM$gruppi) && n / length(unique(gruppi_di(rownames(d)))) >= 3 &&
             (function() { g <- gruppi_di(rownames(d)); length(unique(g)) - opp * p < max(50, 0.2 * length(unique(g))) })()) {
    # pannello (3+ righe per unita'): il campione si misura in unita', non in righe. Energia: 2.000 righe
    # erano 101 paesi su 174, e aggiungere paesi abbassava l'errore dell'11% (lezione 127). Se le unita' non
    # bastano per 40 unita' per parametro piu' una verifica, si usano tutte con la convalida della procedura
    # per unita' (fold_di tiene insieme le righe di un'unita')
    build <- seq_len(n); modo <- "cv"
    descr <- sprintf("%d unita' ripetute (%.0f righe per unita'): troppo poche per dividere (%d parametri candidati x %d unita'); si costruisce su tutte e la precisione si stima con la convalida della procedura per unita'",
                     length(unique(gruppi_di(rownames(d)))), n / length(unique(gruppi_di(rownames(d)))), p, opp)
  } else if (!is.null(.RM$gruppi) && n / length(unique(gruppi_di(rownames(d)))) >= 3) {
    # pannello con molte unita': si estraggono 40 unita' per parametro, il resto e' verifica su unita' mai viste
    g <- gruppi_di(rownames(d)); u <- sample(unique(g)); ub <- head(u, opp * p)
    build <- which(g %in% ub); i <- cap(setdiff(seq_len(n), build)); val$verifica <- i
    modo <- "casuale"; descr <- sprintf("costruzione: %d righe (%d unita' intere, %d per parametro); verifica: %d righe di unita' mai viste", length(build), length(ub), opp, length(i))
  } else if (!is.null(.RM$gruppi) && n - n_t >= max(1000, 0.2 * n)) {
    # per unita': si estraggono unita' intere fino a circa n_t righe
    g <- gruppi_di(rownames(d)); u <- sample(unique(g)); cum <- cumsum(table(g)[u]); ub <- u[cum <= n_t]
    build <- which(g %in% ub); i <- cap(setdiff(seq_len(n), build)); val$verifica <- i
    modo <- "casuale"; descr <- sprintf("costruzione: %d righe (%d unita' intere); verifica: %d righe di unita' mai viste", length(build), length(ub), length(i))
  } else if (n - n_t >= max(1000, 0.2 * n)) {
    build <- campione_strat(seq_len(n), n_t, d[[y]]); i <- cap(setdiff(seq_len(n), build)); val$verifica <- i
    modo <- "casuale"; descr <- sprintf("costruzione: %d righe (campione stratificato per y); verifica: %d righe mai viste", length(build), length(i))
  } else if (0.8 * n >= 15 * p && 0.2 * n >= (cfg$min_verifica %||% 1000) && !is.null(.RM$gruppi)) {
    g <- gruppi_di(rownames(d)); u <- unique(g); ub <- sample(u, round(0.8 * length(u)))
    build <- which(g %in% ub); i <- setdiff(seq_len(n), build); val$verifica <- i
    modo <- "casuale"; descr <- sprintf("costruzione: %d righe (80%% delle unita'); verifica: %d righe di unita' mai viste", length(build), length(i))
  } else if (0.8 * n >= 15 * p && 0.2 * n >= (cfg$min_verifica %||% 1000)) {
    # una sola verifica 80/20 e' affidabile solo se il 20% e' grande: con 270 righe (caffe') le metriche
    # oscillano piu' delle differenze tra modelli. Sotto le 1.000 righe di verifica: convalida della procedura
    build <- campione_strat(seq_len(n), round(0.8 * n), d[[y]]); i <- setdiff(seq_len(n), build); val$verifica <- i
    modo <- "casuale"; descr <- sprintf("costruzione: %d righe (80%%); verifica: %d righe (20%%)", length(build), length(i))
  } else {
    build <- seq_len(n); modo <- "cv"
    descr <- sprintf("n = %d troppo piccolo per dividere (%.0f oss. per parametro candidato): si costruisce su tutto e la precisione si stima con la convalida incrociata della procedura", n, n / p)
  }
  # pannello in cui y cambia quasi solo tra unita': la domanda e' sulle differenze tra unita', e un'unita'
  # per riga (media nel periodo) toglie le righe ripetute alla radice (energia: MAE 15,4 -> 11,1, lezione 128)
  if (!is.null(.RM$gruppi)) { g <- gruppi_di(rownames(d))
    if (n / length(unique(g)) >= 3) {
      a <- tryCatch(anova(lm(d[[y]] ~ factor(g))), error = function(e) NULL)
      q_tra <- if (is.null(a)) NA else a[1, 2] / sum(a[, 2])
      if (is.finite(q_tra) && q_tra > 0.9)
        registra(F, "0.0b", "y cambia quasi solo tra unita'", sprintf("%.0f%% della varianza di y e' tra unita'", 100 * q_tra),
                 "le righe della stessa unita' ripetono quasi la stessa y: per spiegare le differenze tra unita' basta una riga per unita'",
                 "PROPOSTA: aggregare per unita' (media del periodo) in un file a parte; per prevedere gli anni futuri serve invece il valore precedente di y", "PROPOSTA") } }
  # copertura dei livelli (minimum allocation): un livello frequente nel dataset intero ma raro nel campione
  # (diamanti: I1 = 1,4% di 54.000 pietre, ma 28 nelle 2.000 di costruzione) viene integrato con righe in piu'
  # di quel livello, prese da quelle non usate. Selezionare sulle x non distorce l'OLS non pesato
  # (Winship & Radbill 1994); i conteggi del dataset intero decidono poi quali livelli sono davvero rari
  min_liv <- cfg$livelli_rari_min %||% 30
  fat <- setdiff(names(d)[sapply(d, function(z) (is.character(z) || is.factor(z) || is.logical(z)) &&
                                   length(unique(z)) <= max(2, 0.3 * n))], c(y, ".tempo", ".w"))
  conteggi <- lapply(d[fat], function(z) table(as.character(z)))
  if (modo != "cv" && length(fat) && is.null(.RM$gruppi)) {   # con unita' ripetute si aggiungerebbero righe, non unita' intere
    pool <- setdiff(if (modo == "temporale") passato else seq_len(n), build); aggiunte <- character(); n_agg <- 0
    for (v in fat) {
      tb <- table(factor(as.character(d[[v]][build]), levels = names(conteggi[[v]])))
      for (l in names(conteggi[[v]])) {
        tot <- conteggi[[v]][[l]]; obiettivo <- min(min_liv, floor(tot / 2))   # meta' resta per la verifica
        if (tot < min_liv || tb[[l]] >= obiettivo) next
        cand <- pool[!is.na(d[[v]][pool]) & as.character(d[[v]][pool]) == l]
        k <- min(length(cand), obiettivo - tb[[l]]); if (k <= 0) next
        nuove <- cand[sample.int(length(cand), k)]; build <- c(build, nuove); pool <- setdiff(pool, nuove)
        n_agg <- n_agg + k; aggiunte <- c(aggiunte, sprintf("%s=%s (+%d)", v, l, k))
      }
    }
    if (n_agg) {
      for (nm in names(val)) { keep <- !val[[nm]] %in% build; if (length(tv[[nm]])) tv[[nm]] <- tv[[nm]][keep]; val[[nm]] <- val[[nm]][keep] }
      registra(F, "0.0b", "copertura dei livelli", sprintf("%d righe aggiunte alla costruzione per %d livelli: %s%s", n_agg, length(aggiunte),
               paste(head(aggiunte, 8), collapse = ", "), if (length(aggiunte) > 8) ", ..." else ""),
               sprintf("ogni livello con almeno %d casi nel dataset intero ne ha almeno %d (o la meta') in costruzione", min_liv, min_liv),
               "aggiunte (selezione sulle x: OLS non distorto, nessun peso)", "AUTO")
    }
  }
  registra(F, "0.0", "divisione dei dati", descr,
           sprintf("%d parametri candidati x %d oss./parametro = %d (min 2000, max 10000)", p, opp, as.integer(opp * p)),
           paste("modo:", modo), if (modo == "temporale") "UTENTE" else "AUTO")
  if (nrow(d[build, ]) / p < 15) registra(F, "0.0", "osservazioni per parametro", round(length(build) / p, 1),
    "Harrell: almeno 15-20 per parametro candidato", "pochi dati per parametro: raggruppare livelli rari, meno x candidate", "PROPOSTA")
  d$.tempo <- NULL
  b <- d[build, ]; attr(b, "conteggi_pieni") <- conteggi
  list(build = b, val = lapply(val, function(i) d[i, ]), t_val = tv, modo = modo, p = p, n_t = n_t, descr = descr, conteggi = conteggi)
}

# --------------------------------------------------------------------
# applicare ai dati nuovi cio' che si e' imparato sulla costruzione (senza ristimare nulla: niente leakage)
# --------------------------------------------------------------------
# colonne ausiliarie (cfg$ausiliarie): fuori dal modello per scelta di analisi (collinearita', domanda) ma note
# prima di y, quindi utili a mice per stimare i mancanti delle altre x. `escludi` invece toglie la colonna da
# tutto, imputazione compresa: e' per cio' che non deve contare (leakage, info successive a y, identificativi).
# Imputare prima di togliere le variabili conserva l'informazione (lezione 137)
separa_ausiliarie <- function(d, cfg) {
  aux <- setdiff(intersect(unlist(cfg$ausiliarie), names(d)), cfg$y)
  list(d = d[setdiff(names(d), aux)], aux = if (length(aux)) d[aux] else NULL)
}
imputa_con_ausiliarie <- function(aa, y, cfg) {
  met <- cfg$imputazione %||% "mice"; pg <- cfg$imputa_per_gruppo
  if (is.null(aa$aux) || !any(is.na(aa$d[setdiff(names(aa$d), y)]))) return(imputa(aa$d, y, met, per_gruppo = pg))
  # imputare PRIMA di togliere le ausiliarie (mice le usa) o DOPO (senza)? Decide la convalida a 10 fold sulla
  # scala di y, con la metrica guida, sul modello con tutte le x candidate: le ausiliarie si tengono solo se
  # migliorano oltre il 2% (5% sotto le 300 righe); a parita' vince l'imputazione piu' semplice (lezione 137)
  vq <- isTRUE(.RM$quiet); .RM$quiet <- TRUE
  d_dopo <- imputa(aa$d, y, met, per_gruppo = pg)
  d_prima <- imputa(cbind(aa$d, aa$aux), y, met, per_gruppo = pg)[names(aa$d)]
  .RM$quiet <- vq
  mt <- if (identical(cfg$metrica, "media")) "RMSE" else "MAE"
  bin <- y_binaria(aa$d[[y]], cfg)
  err <- function(dd) { x <- x_iniziali(dd, y); if (bin) return(NA_real_)
    tryCatch(cv_scala(lmx(f_da_termini(y, x), dd), dd, y, 1, mt, cfg$seed %||% 1), error = function(e) NA_real_) }
  e_dopo <- err(d_dopo); e_prima <- err(d_prima)
  sg <- if (nrow(aa$d) < 300) 0.95 else 0.98
  usa <- is.finite(e_prima) && is.finite(e_dopo) && e_prima < sg * e_dopo
  registra("0_preparazione", "0.8b", "imputazione prima o dopo aver tolto le ausiliarie",
           sprintf("ausiliarie %s; %s in convalida (10 fold, tutte le x): imputando dopo %.4g, imputando prima (con le ausiliarie) %.4g",
                   paste(names(aa$aux), collapse = ", "), mt, e_dopo, e_prima),
           sprintf("le colonne ausiliarie (note prima di y, fuori dal modello) entrano in mice solo se migliorano la previsione di oltre il %.0f%%", 100 * (1 - sg)),
           if (usa) sprintf("imputo PRIMA: %s -%.1f%%; le ausiliarie restano fuori dal modello", mt, 100 * (1 - e_prima / e_dopo)) else
             "imputo DOPO: le ausiliarie non migliorano l'imputazione abbastanza", "AUTO")
  imputa(if (usa) cbind(aa$d, aa$aux) else aa$d, y, met, per_gruppo = pg)[names(aa$d)]   # rifatta con il registro attivo
}

applica_stato <- function(dn, st) {
  y <- st$y; sp <- st$stato_prep
  for (v in names(sp$effetti_gruppo)) { eg <- sp$effetti_gruppo[[v]]   # effetti di gruppo stimati sulla costruzione
    m <- if (v %in% names(dn)) unname(eg$dev[as.character(dn[[v]])]) else rep(NA_real_, nrow(dn)); m[is.na(m)] <- 0; dn[[eg$nome]] <- m }
  if (!is.null(st$pesi_ab) && ".w" %in% names(dn))          # stessi pesi FGLS della costruzione
    dn$.w <- (1 / (st$pesi_ab[["a"]] + st$pesi_ab[["b"]] / dn$.w)) / st$pesi_ab[["norm"]]
  dn <- dn[intersect(c(sp$colonne, ".w"), names(dn))]
  for (v in names(sp$livelli)) {
    x <- as.character(dn[[v]]); lv <- sp$livelli[[v]]
    nuovo <- !is.na(x) & !x %in% lv; x[nuovo] <- if ("Altro" %in% lv) "Altro" else NA
    dn[[v]] <- factor(x, levels = lv)
  }
  di <- st$d_imp; pg <- st$cfg$imputa_per_gruppo
  for (v in intersect(names(pg), names(dn))) { g <- pg[[v]]; k <- is.na(dn[[v]])
    if (!any(k) || !g %in% names(dn) || !is.numeric(di[[v]])) next
    m <- tapply(di[[v]], di[[g]], median); dn[[v]][k] <- m[as.character(dn[[g]][k])] }
  for (v in setdiff(names(dn), c(y, ".w"))) { k <- is.na(dn[[v]]); if (!any(k) || !v %in% names(di)) next
    dn[[v]][k] <- if (is.numeric(di[[v]])) median(di[[v]]) else names(which.max(table(di[[v]]))) }
  for (qn in names(st$quote_def)) { pp <- st$quote_def[[qn]]
    dn[[qn]] <- ifelse(dn[[pp[2]]] > 0, dn[[pp[1]]] / dn[[pp[2]]], 0) }
  for (nv in names(st$mappe)) { m <- st$mappe[[nv]]
    g <- unname(m$mappa[as.character(dn[[m$var]])]); g[is.na(g)] <- m$livelli[1]
    dn[[nv]] <- factor(g, levels = m$livelli) }
  if (!is.null(st$lambda)) dn$.yt <- ytrasf(dn[[y]], st$lambda)
  dn
}


# previsioni dei passi intermedi della procedura sulla scala originale di y: dicono quale passo
# aiuta e quale peggiora FUORI campione (il percorso in-sample migliora sempre per costruzione)
prev_passi <- function(st, dv) {
  out <- list()
  out[["passo 1 (collinearita')"]] <- tryCatch(prevedi_sicuro(st$fit1, dv), error = function(e) rep(NA_real_, nrow(dv)))
  for (k in c("fit2", "fit3")) {
    nm <- if (k == "fit2") "passo 2 (trasformazioni)" else "passo 3 (selezione)"
    out[[nm]] <- tryCatch(yinv(prevedi_sicuro(st[[k]], dv), st$lambda, residuals(st[[k]])), error = function(e) rep(NA_real_, nrow(dv)))
  }
  out
}
passi_codice <- c("passo 1 (collinearita')" = "collinearita", "passo 2 (trasformazioni)" = "trasformazioni", "passo 3 (selezione)" = "selezione")
registra_percorso_fuori <- function(F, tab, righe_nomi, prefisso, cfg, tolte = NULL) {
  met <- if (identical(cfg$metrica, "media")) "RMSE" else "MAE"
  togli <- function(x) if (nzchar(prefisso)) sub(prefisso, "", x, fixed = TRUE) else x
  cand <- intersect(c(paste0(prefisso, c("iniziale", names(passi_codice))), paste0(prefisso, "finale")), rownames(tab))
  if (length(cand) < 3) return(invisible(NULL))
  v <- tab[cand, met]; best <- cand[which.min(v)]; fin <- paste0(prefisso, "finale")
  registra(F, "9.1d", paste0("percorso fuori campione", if (nzchar(prefisso)) paste0(" (", sub(" \\| $", "", prefisso), ")") else ""),
           paste(sprintf("%s %.4g", togli(cand), v), collapse = "; "),
           sprintf("%s fuori campione dopo ogni passo: un passo che peggiora va tolto (config fino_a)", met),
           if (best != fin && v[best == cand] < 0.98 * tab[fin, met]) {
             b <- togli(best)
             sprintf("PROPOSTA: il modello migliore fuori campione e' '%s'; %s", b,
                     if (b %in% names(passi_codice)) sprintf("rilanciare con fino_a = \"%s\"", passi_codice[[b]]) else {
                       p1 <- paste0(prefisso, "passo 1 (collinearita')")
                       if (p1 %in% rownames(tab) && tab[p1, met] > 1.02 * tab[best, met] && length(tolte))
                         sprintf("la collinearita' toglie informazione utile per prevedere (passo 1 peggiora di %.0f%%): proteggere le x tolte, decisioni.tieni = [%s]",
                                 100 * (tab[p1, met] / tab[best, met] - 1), paste0("\"", tolte, "\"", collapse = ", "))
                       else "i passi della procedura non aiutano: rilanciare con fino_a = \"collinearita\" (il piu' vicino all'iniziale)" })
           } else "la procedura completa e' la migliore (o entro il 2%)",
           if (best != fin && v[best == cand] < 0.98 * tab[fin, met]) "PROPOSTA" else "AUTO")
}

# righe identiche in tutte le colonne, contate solo se esiste una colonna numerica quasi continua
# (con sole colonne discrete righe uguali sono normali: due passeggeri con stesse classe, sesso, eta')
righe_doppie <- function(d) {
  cont <- any(sapply(d, function(z) is.numeric(z) && length(unique(z[!is.na(z)])) >= 0.5 * sum(!is.na(z))))
  if (!cont) return(0L)
  sum(duplicated(d))
}


# gruppo (unita') di ogni riga, se definito
gruppi_di <- function(rn) { g <- .RM$gruppi; if (is.null(g)) NULL else { x <- g[rn]; x[is.na(x)] <- rn[is.na(x)]; unname(x) } }
# effetto del disegno con unita' ripetute: le righe della stessa unita' ripetono in gran parte la stessa
# informazione (energia: 20 anni quasi identici per paese). deff = 1 + (m - 1) * ICC dei residui, m = righe
# per unita'. Le penalita' di selezione si calcolano sull'informazione effettiva n / deff: con le righe
# contate come indipendenti entravano cubi e interazioni che inseguivano singoli paesi (lezione 128).
# Sotto deff 2 (calcestruzzo: 2,5 prove per miscela) si resta sulle righe
deff_gruppi <- function(m) {
  r <- residuals(m); g <- gruppi_di(names(r)); if (is.null(g)) return(1)
  mm <- length(r) / length(unique(g)); if (mm < 1.5) return(1)
  a <- tryCatch(anova(lm(r ~ factor(g))), error = function(e) NULL); if (is.null(a)) return(1)
  msb <- a[1, 3]; msw <- a[2, 3]; icc <- max(0, (msb - msw) / (msb + (mm - 1) * msw))
  de <- 1 + (mm - 1) * icc; if (de < 2) 1 else de
}
# differenza di SBC calcolata sull'informazione effettiva (con deff = 1 e' BIC(m1) - BIC(m0))
d_sbc <- function(m1, m0, deff = 1) {
  n <- nobs(m0); dk <- attr(logLik(m1), "df") - attr(logLik(m0), "df")
  ddev <- (BIC(m1) - BIC(m0)) - dk * log(n)
  ddev / deff + dk * log(n / deff)
}
# fold della convalida: per unita' se c'e' un gruppo (tutte le righe di un'unita' nello stesso fold)
fold_di <- function(rn, K) { g <- gruppi_di(rn)
  if (is.null(g)) return(sample(rep_len(seq_len(K), length(rn))))
  u <- unique(g); fu <- setNames(sample(rep_len(seq_len(K), length(u))), u); unname(fu[g]) }
# matrice di covarianza robusta: HC3, oppure cluster-robusta per unita'
vc_rob <- function(m) { g <- gruppi_di(names(residuals(m)))
  if (is.null(g)) vcovHC(m, type = "HC3") else sandwich::vcovCL(m, cluster = g, type = "HC1") }
# bootstrap per unita' (cluster bootstrap): si ricampionano le unita', non le righe
boot_cluster <- function(m, R, g) {
  # sulla matrice del modello (le trasformazioni sono gia' calcolate): si ricampionano le unita' e si rifa' l'OLS
  X <- model.matrix(m); yy <- model.response(model.frame(m)); w <- weights(m); u <- unique(g); idx <- split(seq_along(g), g)
  t <- t(sapply(seq_len(R), function(r) { ii <- unlist(idx[sample(u, length(u), replace = TRUE)], use.names = FALSE)
    f <- if (is.null(w)) lm.fit(X[ii, , drop = FALSE], yy[ii]) else lm.wfit(X[ii, , drop = FALSE], yy[ii], w[ii])
    f$coefficients }))
  colnames(t) <- names(coef(m)); list(t = t, t0 = coef(m), R = R)
}


# ---------------------------------------------------------------------
# QUALE MODELLO SI CONSEGNA: vince chi prevede meglio sui dati mai visti; a parita' (entro il 2%) il piu' semplice
# (caffe': il finale era un po' peggiore dell'iniziale e lo si consegnava lo stesso)
# ---------------------------------------------------------------------
righe_ini_fin <- function(ver, cvr) {
  if (!is.null(ver) && !is.null(ver$tab)) { tab <- ver$tab; rn <- rownames(tab); f <- rn[grepl("\\| finale$", rn) & !grepl("^costruzione", rn)][1]
    if (is.na(f)) return(NULL); g <- sub(" \\|.*", "", f)
    return(list(ini = tab[paste(g, "| iniziale"), ], fin = tab[f, ], dove = g)) }
  if (!is.null(cvr)) return(list(ini = cvr$tab["iniziale", ], fin = cvr$tab["finale", ], dove = "convalida incrociata"))
  NULL
}
decidi_consegna <- function(e_ini, e_fin, p_ini, p_fin, nome_err, tol = 0.02) {
  d <- e_fin / e_ini - 1
  if (!is.finite(d)) return(list(chi = "finale", motivo = "confronto non disponibile"))
  if (d < -tol) list(chi = "finale", motivo = sprintf("prevede meglio dell'iniziale sui dati mai visti (%s %.0f%% piu' basso)", nome_err, -100 * d))
  else if (d > tol) list(chi = "iniziale", motivo = sprintf("l'iniziale prevede meglio sui dati mai visti (%s del finale %.0f%% piu' alto): i passi della procedura non hanno aiutato", nome_err, 100 * d))
  else if (p_fin <= p_ini) list(chi = "finale", motivo = sprintf("prevedono allo stesso modo (%s entro il 2%%) e il finale e' piu' semplice (%d parametri contro %d) e ha passato i controlli di robustezza", nome_err, p_fin, p_ini))
  else list(chi = "iniziale", motivo = sprintf("prevedono allo stesso modo (%s entro il 2%%) e l'iniziale e' piu' semplice (%d parametri contro %d)", nome_err, p_ini, p_fin))
}
scelta_consegna <- function(st, ver, cvr, cfg) {
  r <- righe_ini_fin(ver, cvr); if (is.null(r)) return(NULL)
  met <- if (identical(cfg$metrica, "media")) "RMSE" else "MAE"
  sc <- decidi_consegna(r$ini[[met]], r$fin[[met]], length(coef(st$fit0)), length(coef(st$fit4)), met)
  registra("9_verifica", "9.9", "modello consegnato", sprintf("%s: iniziale %.4g, finale %.4g (%s)", met, r$ini[[met]], r$fin[[met]], r$dove),
           "vince chi prevede meglio sui dati mai visti; a parita' (entro il 2%) il piu' semplice", paste0("consegno il modello ", sc$chi, ": ", sc$motivo),
           if (sc$chi == "iniziale") "PROPOSTA" else "AUTO")
  c(sc, list(ini = r$ini, fin = r$fin, dove = r$dove, met = met))
}
scelta_consegna_bin <- function(tab, p_ini, p_fin) {
  ri <- grep("tutte le x|iniziale", rownames(tab))[1]; rf <- which(rownames(tab) == "finale")[1]
  if (is.na(ri) || is.na(rf)) return(NULL)
  sc <- decidi_consegna(tab[ri, "Brier"], tab[rf, "Brier"], p_ini, p_fin, "errore di Brier")
  registra("8_logistica", "L.11", "modello consegnato", sprintf("Brier: iniziale %.4f, finale %.4f; accuratezza %.1f%% contro %.1f%%", tab[ri, "Brier"], tab[rf, "Brier"],
           100 * tab[ri, "acc_0.5"], 100 * tab[rf, "acc_0.5"]), "vince chi prevede meglio sui dati mai visti (errore di Brier sulle probabilita'); a parita' (entro il 2%) il piu' semplice",
           paste0("consegno il modello ", sc$chi, ": ", sc$motivo), if (sc$chi == "iniziale") "PROPOSTA" else "AUTO")
  c(sc, list(ini = tab[ri, ], fin = tab[rf, ], naive = if ("naive (prevalenza)" %in% rownames(tab)) tab["naive (prevalenza)", ] else NULL))
}
# precisione del modello iniziale su dati mai visti, gia' nel report 1 (stesso metro del report finale):
# sulla verifica se c'e', altrimenti con la convalida incrociata a 5 fold (stessi fold della procedura)
fuori_iniziale <- function(fit0, d, div, cfg, binaria = FALSE) {
  y <- cfg$y; f <- formula(fit0)
  # stesse righe del report finale: i mancanti delle x si imputano come nella procedura (altrimenti il confronto
  # sarebbe su 705 passeggeri con eta' nota contro 891)
  vq <- isTRUE(.RM$quiet); .RM$quiet <- TRUE
  d <- tryCatch(imputa(d, y, cfg$imputazione %||% "mice", per_gruppo = cfg$imputa_per_gruppo), error = function(e) d)
  .RM$quiet <- vq
  stima <- function(a) if (binaria) suppressWarnings(glm(f, data = a, family = binomial)) else lmx(f, a)
  prev <- function(m, b) tryCatch(as.numeric(prevedi_sicuro(m, b, type = "response")), error = function(e) rep(NA_real_, nrow(b)))
  if (length(div$val)) {
    st0 <- list(y = y, stato_prep = attr(d, "stato_prep"), d_imp = d, cfg = cfg)
    dv <- applica_stato(div$val[[1]], st0); m <- stima(d); p <- prev(m, dv); yo <- dv[[y]]; dove <- names(div$val)[1]; ytr <- d[[y]]
  } else {
    set.seed(cfg$seed %||% 1234); fo <- fold_di(rownames(d), cfg$K_cv %||% 5); p <- rep(NA_real_, nrow(d))
    for (k in unique(fo)) { m <- tryCatch(stima(d[fo != k, ]), error = function(e) NULL); if (!is.null(m)) p[fo == k] <- prev(m, d[fo == k, ]) }
    yo <- d[[y]]; dove <- "convalida incrociata"; ytr <- d[[y]]
  }
  ok <- !is.na(p) & !is.na(yo)
  if (binaria) { pc <- mean(ytr, na.rm = TRUE); pr <- as.integer(p[ok] > 0.5)
    return(list(binaria = TRUE, dove = dove, n = sum(ok), acc = mean(pr == yo[ok]), acc_naive = max(mean(yo[ok]), 1 - mean(yo[ok])),
                brier = mean((yo[ok] - p[ok])^2), brier0 = mean((yo[ok] - pc)^2), quota = mean(yo[ok]),
                met = tryCatch(metriche_logit(yo[ok], p[ok], pc, pc), error = function(e) NULL), p = length(coef(fit0)),
                modello = tryCatch(stima(d), error = function(e) NULL))) }
  e <- yo[ok] - p[ok]; med <- median(ytr, na.rm = TRUE)
  list(binaria = FALSE, dove = dove, n = sum(ok), MAE = mean(abs(e)), RMSE = sqrt(mean(e^2)), MAE_naive = mean(abs(yo[ok] - med)),
       R2 = 1 - sum(e^2) / sum((yo[ok] - mean(ytr, na.rm = TRUE))^2), mediana = med)
}

# previsione che non si rompe sui livelli tolti in costruzione (restituisce NA per quelle righe)
prevedi_sicuro <- function(fit, dn, type = "response", non_bloccare = "trend") {   # il trend e' fatto per proiettare
  ok <- rep(TRUE, nrow(dn))
  for (v in names(fit$xlevels)) if (v %in% names(dn)) ok <- ok & as.character(dn[[v]]) %in% fit$xlevels[[v]]
  # x numeriche fuori dall'intervallo di costruzione: le riporto al bordo. Un polinomio (o un exp di un
  # polinomio) fuori dal campo osservato esplode (diamanti, log(prezzo) con carat cubico: RMSE 2,3 milioni)
  dc <- tryCatch(eval(fit$call$data), error = function(e) NULL); n_cl <- 0
  if (!is.null(dc)) for (v in setdiff(intersect(all.vars(formula(fit))[-1], names(dn)), non_bloccare)) {
    if (!is.numeric(dn[[v]]) || !v %in% names(dc) || !is.numeric(dc[[v]])) next
    # dentro una potenza (x^2, x^3) anche le zone rade della coda sono pericolose: un solo punto estremo in costruzione
    # rende il polinomio enorme tra lui e il grosso dei dati (California, persone per famiglia fino a 1.200): limite all'1 per mille
    pot <- any(grepl(paste0("\\b", v, "\\b.*\\^|\\^.*\\b", v, "\\b"), grep("\\^", attr(terms(fit), "term.labels"), value = TRUE)))
    r <- if (pot) quantile(dc[[v]], c(.001, .999), na.rm = TRUE, names = FALSE) else range(dc[[v]], na.rm = TRUE)
    fuori <- !is.na(dn[[v]]) & (dn[[v]] < r[1] | dn[[v]] > r[2])
    if (any(fuori)) { n_cl <- n_cl + sum(fuori); dn[[v]] <- pmin(pmax(dn[[v]], r[1]), r[2]) } }
  p <- rep(NA_real_, nrow(dn))
  if (any(ok)) p[ok] <- suppressWarnings(predict(fit, dn[ok, , drop = FALSE], type = type))
  attr(p, "fuori_campo") <- n_cl
  p
}

# metriche di previsione. MASE = MAE / MAE del previsore "naive" (la mediana di y in costruzione):
# < 1 meglio di nessun modello, 0.5 = errore dimezzato; senza unita' di misura, confrontabile tra dataset
metriche_reg <- function(yo, yh, mediana_c, media_c) {
  ok <- !is.na(yh) & !is.na(yo); yo <- yo[ok]; e <- yo - yh[ok]
  q <- quantile(abs(e), 0.99)
  c(n = length(e), RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)), MASE = mean(abs(e)) / mean(abs(yo - mediana_c)),
    R2_fuori_campione = 1 - sum(e^2) / sum((yo - media_c)^2), distorsione_media = mean(e),
    RMSE_senza_1pct_estremi = sqrt(mean(e[abs(e) <= q]^2)))
}

pulisci_cook <- function(form, dd, strat = "cook") {   # stessa regola della costruzione (cook, dffits o nessuna)
  m <- lmx(form, dd); if (identical(strat, "nessuna")) return(m)
  p <- sum(!is.na(coef(m))); n <- nobs(m)
  keep <- if (identical(strat, "dffits")) { df <- dffits(m); is.na(df) | abs(df) <= 2 * sqrt(p / n) } else {
    ck <- cooks.distance(m); is.na(ck) | ck <= 4 / (n - p - 2) }
  lmx(form, droplevels(dd[keep, , drop = FALSE]))
}

confronta_coef <- function(m1, m2) {          # stabilita': la formula congelata, stimata su dati diversi
  s1 <- coeftest(m1, vcov = vc_rob(m1)); s2 <- coeftest(m2, vcov = vc_rob(m2))
  k <- setdiff(intersect(rownames(s1), rownames(s2)), "(Intercept)")
  z <- (s2[k, 1] - s1[k, 1]) / sqrt(s1[k, 2]^2 + s2[k, 2]^2)
  data.frame(b_costruzione = s1[k, 1], b_verifica = s2[k, 1], stesso_segno = sign(s1[k, 1]) == sign(s2[k, 1]),
             z_differenza = z, signif_costruzione = s1[k, 4] < 0.05, signif_verifica = s2[k, 4] < 0.05)
}

verifica_lineare <- function(st, div, cfg) {
  F <- "9_verifica"; reset_fase(F)
  y <- st$y; fit4 <- st$fit4; lam <- st$lambda; res <- residuals(fit4)
  med_c <- median(st$d[[y]]); mean_c <- mean(st$d[[y]])
  fit0i <- lmx(formula(st$fit0), st$d_imp)                     # modello iniziale, stessi dati imputati
  # secondo termine di paragone: la sola x piu' legata a y (quanto aggiungono tutte le altre?)
  num_x <- setdiff(names(st$d_imp)[sapply(st$d_imp, is.numeric)], y)
  x_top <- if (length(num_x)) num_x[which.max(abs(sapply(num_x, function(v) cor(st$d_imp[[v]], st$d_imp[[y]]))))] else NULL
  fit1x <- if (!is.null(x_top)) lmx(f_da_termini(y, x_top), st$d_imp) else NULL
  righe <- list(); dv_list <- list(); cal <- list()
  # in-sample su TUTTE le righe di costruzione (anche quelle tolte come influenti): confronto alla pari con la verifica
  righe[["costruzione (in-sample) | finale"]] <- metriche_reg(st$d2[[y]], yinv(prevedi_sicuro(fit4, st$d2), lam, res), med_c, mean_c)
  for (nm in names(div$val)) {
    dv <- applica_stato(div$val[[nm]], st); dv_list[[nm]] <- dv
    p4 <- prevedi_sicuro(fit4, dv); yh4 <- yinv(p4, lam, res); yh0 <- prevedi_sicuro(fit0i, dv)
    if (attr(p4, "fuori_campo") > 0) registra(F, "9.1", paste("x fuori campo", nm), sprintf("%d valori di x oltre l'intervallo di costruzione", attr(p4, "fuori_campo")),
      "un polinomio fuori dal campo osservato esplode: le x vengono riportate al bordo (nessuna estrapolazione)", "riportate al bordo", "AUTO")
    if (!is.null(fit1x)) righe[[paste0(nm, " | solo ", x_top)]] <- metriche_reg(dv[[y]], prevedi_sicuro(fit1x, dv), med_c, mean_c)
    righe[[paste(nm, "| iniziale")]] <- metriche_reg(dv[[y]], yh0, med_c, mean_c)
    if (any(grepl("trend", termini(fit4)))) {
      fit_nt <- lmx(f_da_termini(".yt", termini(fit4)[!grepl("trend", termini(fit4))]), st$d4)
      righe[[paste(nm, "| finale senza trend")]] <- metriche_reg(dv[[y]], yinv(prevedi_sicuro(fit_nt, dv), lam, residuals(fit_nt)), med_c, mean_c)
      # alternativa intermedia: trend congelato all'ultimo livello osservato (se la crescita ha rallentato)
      righe[[paste(nm, "| finale con trend congelato")]] <- metriche_reg(dv[[y]], yinv(prevedi_sicuro(fit4, dv, non_bloccare = character()), lam, res), med_c, mean_c)
    }
    pp <- prev_passi(st, dv); for (kp in names(pp)) righe[[paste(nm, "|", kp)]] <- metriche_reg(dv[[y]], pp[[kp]], med_c, mean_c)
    righe[[paste(nm, "| finale")]] <- metriche_reg(dv[[y]], yh4, med_c, mean_c)
    if (lam != 1) {                    # stessa formula, y non trasformata: insegue la media invece del valore tipico
      fit_y <- tryCatch(lmx(f_da_termini(y, termini(fit4)), st$d4), error = function(e) NULL)
      if (!is.null(fit_y)) righe[[paste(nm, "| finale, y non trasformata")]] <- metriche_reg(dv[[y]], prevedi_sicuro(fit_y, dv), med_c, mean_c)
    }
    np <- sum(is.na(yh4))
    if (np) registra(F, "9.1", paste("non prevedibili", nm), paste(np, "righe"),
                     "livelli tolti in costruzione (rarissimi o influenti)", "escluse dalle metriche: il modello non li copre")
    ok <- !is.na(yh4); dec <- cut(rank(yh4[ok], ties.method = "first"), 10, labels = FALSE)
    cal[[nm]] <- data.frame(decile = 1:10, previsto = tapply(yh4[ok], dec, mean), osservato = tapply(dv[[y]][ok], dec, mean))
  }
  tab <- do.call(rbind, righe); salva_tab(round(tab, 4), "9_verifica_predittiva")
  tolte_coll <- tryCatch(setdiff(x_iniziali(st$d, y), st$x1), error = function(e) NULL)
  for (nm in names(div$val)) registra_percorso_fuori(F, tab, rownames(tab), paste(nm, "| "), cfg, tolte_coll)
  if (length(cal)) {
    png_apri("9_calibrazione", 5 * length(cal), 4.5); par(mfrow = c(1, length(cal)))
    for (nm in names(cal)) { cc <- cal[[nm]]; lim <- range(c(cc$previsto, cc$osservato))
      plot(cc$previsto, cc$osservato, pch = 19, xlim = lim, ylim = lim, main = paste("Calibrazione:", nm),
           xlab = paste(y, "previsto (media per decile)"), ylab = "osservato"); abline(0, 1, lty = 2, col = 4) }
    dev.off(); salva_tab(do.call(rbind, lapply(names(cal), function(nm) cbind(set = nm, cal[[nm]]))), "9_calibrazione")
  }
  for (nm in names(div$val)) {
    a <- tab[paste(nm, "| iniziale"), ]; b <- tab[paste(nm, "| finale"), ]
    if (!is.null(fit1x)) { u <- tab[paste0(nm, " | solo ", x_top), ]
      registra(F, "9.1", paste("confronto con la sola", x_top, "su", nm), sprintf("MAE %.4g con la sola %s vs %.4g con il modello finale (%.0f%% in meno)",
               u["MAE"], x_top, b["MAE"], 100 * (1 - b["MAE"] / u["MAE"])), "quanto aggiungono tutte le altre x alla x piu' forte",
               if (b["MAE"] > 0.95 * u["MAE"]) "le altre x aggiungono poco: un modello minimo potrebbe bastare" else "-",
               if (b["MAE"] > 0.95 * u["MAE"]) "PROPOSTA" else "AUTO") }
    met <- if (identical(cfg$metrica, "media")) "RMSE" else "MAE"
    if (b[met] > 1.02 * a[met])
      registra(F, "9.1b", paste("finale peggiore dell'iniziale su", nm), sprintf("%s %.4g contro %.4g dell'iniziale (%+.1f%%)", met, b[met], a[met], 100 * (b[met] / a[met] - 1)),
               sprintf("metrica guida: %s (config metrica = tipico -> MAE, media -> RMSE)", met),
               "il modello finale non batte il termine di confronto sulla metrica scelta: rivedere trasformazione di y e pulizia", "PROPOSTA")
    ky <- paste(nm, "| finale, y non trasformata")
    if (ky %in% rownames(tab)) { cy <- tab[ky, ]
      registra(F, "9.1c", paste("y trasformata o no su", nm), sprintf("%s %.4g con y = %s, %.4g con y non trasformata (stessa formula); MAE %.4g / %.4g, RMSE %.4g / %.4g",
               met, b[met], nome_trasf(lam), cy[met], b["MAE"], cy["MAE"], b["RMSE"], cy["RMSE"]),
               "la trasformazione insegue il valore tipico (MAE), la y originale la media (RMSE): decide la metrica guida",
               if (cy[met] < 0.98 * b[met]) "PROPOSTA: decisioni.lambda = 1 (la y non trasformata prevede meglio sulla metrica scelta)" else "la trasformazione regge sulla metrica scelta",
               if (cy[met] < 0.98 * b[met]) "PROPOSTA" else "AUTO") }
    registra(F, "9.1", paste("precisione su", nm), sprintf("n=%d; MAE %.4g -> %.4g; RMSE %.4g -> %.4g; MASE %.2f -> %.2f; R2 fuori campione %.3f -> %.3f",
             as.integer(b["n"]), a["MAE"], b["MAE"], a["RMSE"], b["RMSE"], a["MASE"], b["MASE"], a["R2_fuori_campione"], b["R2_fuori_campione"]),
             "dati mai usati per costruire il modello (iniziale -> finale)", "-")
  }
  # trend: crescita annua stimata e quanto riduce la deriva sul futuro
  if ("trend" %in% termini(fit4)) {
    b <- coef(fit4)["trend"]; se_b <- sqrt(diag(vc_rob(fit4)))["trend"]
    cresc <- if (lam == 0) sprintf("%+.1f%% all'anno (IC95 %+.1f%%; %+.1f%%)", 100 * (exp(b) - 1), 100 * (exp(b - 1.96 * se_b) - 1), 100 * (exp(b + 1.96 * se_b) - 1)) else
      sprintf("coefficiente %.4g per anno sulla scala %s", b, nome_trasf(lam))
    registra(F, "9.1", "trend stimato", cresc, "crescita di y a parita' delle altre x (stimata sul periodo di costruzione)", "-")
    for (nm in names(div$val)) if (paste(nm, "| finale senza trend") %in% rownames(tab)) {
      a <- tab[paste(nm, "| finale senza trend"), ]; b2 <- tab[paste(nm, "| finale"), ]
      registra(F, "9.1", paste("effetto del trend su", nm), sprintf("MAE %.4g senza trend -> %.4g con trend; errore medio %+.4g -> %+.4g",
               a["MAE"], b2["MAE"], a["distorsione_media"], b2["distorsione_media"]), "il trend riduce la deriva sul periodo nuovo?",
               if (b2["MAE"] < a["MAE"]) "si': tenere il trend per prevedere" else "no: la crescita non e' costante (rallenta o accelera), valutare una ristima periodica", "AUTO")
      cg <- tab[paste(nm, "| finale con trend congelato"), ]
      if (cg["MAE"] < 0.98 * b2["MAE"])
        registra(F, "9.1", paste("trend congelato su", nm), sprintf("MAE %.4g proiettando il trend, %.4g congelandolo all'ultimo livello; errore medio %+.4g -> %+.4g",
                 b2["MAE"], cg["MAE"], b2["distorsione_media"], cg["distorsione_media"]), "la crescita stimata continua anche sul periodo nuovo?",
                 "no: la crescita ha rallentato; per prevedere usare il livello raggiunto (trend congelato) e ristimare periodicamente", "PROPOSTA")
    }
  }
  # deriva temporale: sul futuro il modello sbaglia sistematicamente in una direzione?
  if ("futuro | finale" %in% rownames(tab)) {
    b <- tab["futuro | finale", ]
    if (abs(b["distorsione_media"]) > 0.25 * b["MAE"]) registra(F, "9.1", "deriva temporale",
      sprintf("sul futuro errore medio %+.4g (%.0f%% del MAE): il modello %s sistematicamente", b["distorsione_media"],
              100 * abs(b["distorsione_media"]) / b["MAE"], if (b["distorsione_media"] > 0) "sottostima" else "sovrastima"),
      "y nel periodo nuovo ha cambiato livello (prezzi, inflazione, mercato)",
      "per prevedere il futuro: aggiungere un trend/anno, ristimare periodicamente o modellare la variazione; le relazioni (coefficienti) vanno lette sulla tabella di stabilita'",
      "PROPOSTA")
  }
  rif <- if ("stessa_epoca | finale" %in% rownames(tab)) "stessa_epoca | finale" else rownames(tab)[nrow(tab)]
  ins <- tab["costruzione (in-sample) | finale", ]; ult <- tab[rif, ]
  registra(F, "9.1", "ottimismo in-sample", sprintf("MAE in costruzione %.4g vs %.4g fuori campione (%+.0f%%)", ins["MAE"], ult["MAE"],
           100 * (ult["MAE"] / ins["MAE"] - 1)), "quanto il modello si e' adattato ai dati di costruzione",
           if (ult["MAE"] > 1.2 * ins["MAE"]) "oltre +20%: sovradattamento o cambiamento tra periodi" else "contenuto")
  # stabilita' dei coefficienti: formula congelata, stessa pulizia, dati mai visti
  stab <- list()
  for (nm in names(dv_list)) {
    dv <- dv_list[[nm]]; dv <- dv[!is.na(prevedi_sicuro(fit4, dv)), ]
    if (nrow(dv) < 3 * length(coef(fit4))) next
    mv <- tryCatch(pulisci_cook(formula(fit4), dv, st$strat_infl %||% "cook"), error = function(e) NULL); if (is.null(mv)) next
    cc <- confronta_coef(fit4, mv); stab[[nm]] <- cc; salva_tab(round(cc, 4), paste0("9_stabilita_", nm))
    cc <- cc[complete.cases(cc), , drop = FALSE]      # SE NaN (HC3 con leva ~1): fuori dal conteggio
    if (!nrow(cc)) next
    sg <- cc$signif_costruzione
    registra(F, "9.2", paste("stabilita' coefficienti su", nm),
             sprintf("stesso segno: %d/%d (tra i significativi in costruzione: %d/%d); |z| < 2: %d%%; significativita' confermata: %d/%d",
                     sum(cc$stesso_segno), nrow(cc), sum(cc$stesso_segno & sg), sum(sg), as.integer(round(100 * mean(abs(cc$z_differenza) < 2))),
                     sum(sg & cc$signif_verifica), sum(sg)),
             "formula congelata riestimata sui dati di verifica (stessa pulizia Cook)",
             if (sum(cc$stesso_segno & sg) / max(1, sum(sg)) >= 0.9) "coefficienti stabili" else "ATTENZIONE: coefficienti instabili, leggere la tabella", "AUTO")
  }
  # livelli uniti in "Altro": qualcuno andava tenuto? Sulla verifica stimo la formula congelata con e senza
  # un fattore .split che separa da "Altro" i livelli con almeno 30 casi: test F parziale (nessuna regola
  # pubblicata: e' il confronto tra modelli annidati applicato alla decisione di unire)
  for (nm in names(dv_list)) {
    raw <- div$val[[nm]]; dv <- dv_list[[nm]]
    for (v in names(st$stato_prep$livelli)) {
      if (!"Altro" %in% st$stato_prep$livelli[[v]] || !v %in% names(raw) || !v %in% names(dv)) next
      orig <- as.character(raw[[v]]); inA <- !is.na(dv[[v]]) & dv[[v]] == "Altro" & !is.na(orig)
      tt <- table(orig[inA]); grandi <- names(tt)[tt >= 30]; if (!length(grandi)) next
      dv$.split <- factor(ifelse(inA & orig %in% grandi, orig, "resto")); dv$.split <- relevel(dv$.split, "resto")
      ok <- !is.na(prevedi_sicuro(fit4, dv)); dd <- dv[ok, ]
      m0 <- tryCatch(lmx(formula(fit4), dd), error = function(e) NULL)
      m1 <- tryCatch(lmx(update(formula(fit4), . ~ . + .split), dd), error = function(e) NULL)
      if (is.null(m0) || is.null(m1)) next
      a <- anova(m0, m1); pv <- a$`Pr(>F)`[2]
      b <- coef(m1)[grepl("^\\.split", names(coef(m1)))]; names(b) <- sub("^\\.split", "", names(b))
      salva_tab(data.frame(livello = names(b), effetto_vs_resto = round(b, 4), casi = as.integer(tt[names(b)])), paste0("9_livelli_altro_", v, "_", nm))
      registra(F, "9.4", paste("livelli in Altro:", v, "su", nm), sprintf("%d livelli con >= 30 casi in Altro; F parziale p = %s; scarti piu' grandi: %s",
               length(grandi), pval(pv), paste(head(sprintf("%s %+.3g", names(b)[order(-abs(b))], b[order(-abs(b))]), 3), collapse = ", ")),
               "i livelli uniti per rarita' si comportano come il resto? (modelli annidati sulla verifica)",
               if (isTRUE(pv < 0.01)) "NO: qualche livello va tenuto separato -> livelli_rari_min piu' basso o n_costruzione piu' grande" else "si': l'unione regge",
               if (isTRUE(pv < 0.01)) "PROPOSTA" else "AUTO")
    }
  }
  # test "Pareto": aggiungere dati migliora la previsione su dati mai visti?
  par_res <- NULL; decisione <- "costruzione"
  if (length(dv_list)) {
    pool <- do.call(rbind, lapply(names(dv_list), function(nm) { z <- dv_list[[nm]]; z$.ord <- if (!is.null(div$t_val[[nm]])) div$t_val[[nm]] else runif(nrow(z)); z }))
    pool <- pool[order(pool$.ord), ]; pool$.ord <- NULL
    if (nrow(pool) >= 1000) {
      h <- floor(nrow(pool) / 2); H1 <- pool[seq_len(h), ]; H2 <- pool[(h + 1):nrow(pool), ]
      cols <- unique(c(all.vars(formula(fit4)), y))
      comb <- rbind(st$d4[cols], H1[!is.na(prevedi_sicuro(fit4, H1)), cols])
      fitB <- tryCatch(pulisci_cook(formula(fit4), comb, st$strat_infl %||% "cook"), error = function(e) NULL)
      if (!is.null(fitB)) {
        mA <- metriche_reg(H2[[y]], yinv(prevedi_sicuro(fit4, H2), lam, res), med_c, mean_c)
        mB <- metriche_reg(H2[[y]], yinv(prevedi_sicuro(fitB, H2), lam, residuals(fitB)), med_c, mean_c)
        par_res <- rbind(solo_costruzione = mA, costruzione_piu_H1 = mB); salva_tab(round(par_res, 4), "9_pareto_H1_H2")
        guad <- (mA["MAE"] - mB["MAE"]) / mA["MAE"]
        decisione <- if (guad > (cfg$soglia_pareto %||% 0.01)) "tutti" else "costruzione"
        registra(F, "9.3", "coefficienti finali: costruzione o tutti i dati?",
                 sprintf("su H2 (%d righe mai viste): MAE %.4g con soli dati di costruzione, %.4g aggiungendo H1 (%+.1f%%)",
                         nrow(H2), mA["MAE"], mB["MAE"], -100 * guad),
                 sprintf("piu' dati migliorano la previsione oltre l'%d%%? (Pareto)", round(100 * (cfg$soglia_pareto %||% 0.01))),
                 if (decisione == "tutti") "coefficienti finali stimati su tutti i dati" else "coefficienti finali = quelli della costruzione",
                 "AUTO")
      }
    } else registra(F, "9.3", "coefficienti finali", "verifica < 1000 righe", "test H1/H2 poco affidabile",
                    "coefficienti finali = costruzione")
  }
  fit_fin <- fit4; dd_fin <- st$d4
  # il campione serve a SCEGLIERE (velocita' + verifica onesta delle scelte); stimare e fare inferenza sulla
  # formula ormai fissata conviene su tutti i dati: SE ~ 1/sqrt(n), costa un solo fit. Le metriche di verifica
  # restano quelle del modello di costruzione (onesto); la riestima su tutti i dati e' almeno altrettanto buona
  modo_cf <- cfg$coef_finali %||% "tutti"
  if (identical(modo_cf, "tutti") && length(dv_list) && decisione != "tutti") {
    decisione <- "tutti"
    registra(F, "9.3b", "coefficienti finali", sprintf("formula fissata sul campione, coefficienti e SE stimati su tutti i dati (%d righe)",
             nrow(st$d4) + sum(sapply(dv_list, nrow))), "scelte sul campione, inferenza su tutti i dati (config coef_finali = tutti | pareto)",
             "SE piu' piccoli di circa sqrt(n_tot / n_costruzione); con n grande guardare la grandezza degli effetti, non solo la significativita'", "AUTO")
  }
  if (decisione == "tutti") {
    cols <- unique(c(all.vars(formula(fit4)), y, intersect(".w", names(st$d4))))
    tutti <- rbind(st$d4[cols], do.call(rbind, lapply(dv_list, function(z) z[!is.na(prevedi_sicuro(fit4, z)), cols])))
    fit_fin <- pulisci_cook(formula(fit4), tutti, st$strat_infl %||% "cook"); dd_fin <- fit_fin$call$data   # lmx incorpora i dati nella chiamata
  }
  w <- coeftest(fit_fin, vcov = vc_rob(fit_fin))
  cf <- data.frame(b = w[, 1], SE_HC3 = w[, 2], p = w[, 4])
  if (decisione == "tutti") cf$b_costruzione <- coef(fit4)[rownames(cf)]
  salva_tab(cf, "9_coefficienti_finali")
  Bm <- if (decisione == "costruzione" && !is.null(st$boot)) st$boot$B$t else {
    # modello riestimato su tutti i dati: n grande, IC da estrazioni da N(b, V_HC3) (bootstrap parametrico)
    set.seed(cfg$seed %||% 1234); bb <- coef(fit_fin); ok <- !is.na(bb)
    tryCatch({ Z <- matrix(NA_real_, 1999, length(bb), dimnames = list(NULL, names(bb)))
      Z[, ok] <- MASS::mvrnorm(1999, bb[ok], vc_rob(fit_fin)[names(bb)[ok], names(bb)[ok]]); Z }, error = function(e) NULL) }
  ef <- tryCatch(effetti_originali(fit_fin, dd_fin, lam, Bm, names(dd_fin)), error = function(e) NULL)
  if (!is.null(ef)) salva_tab(round(ef, 3), "9_effetti_finali")
  list(tab = tab, stab = stab, pareto = par_res, decisione = decisione, fit_finale = fit_fin, n_finale = nobs(fit_fin), effetti = ef)
}

# --------------------------------------------------------------------
# n piccolo: convalida incrociata DELLA PROCEDURA (ogni fold ripete tutte le scelte da capo)
# --------------------------------------------------------------------
cv_procedura <- function(dp, cfg, K = 5) {
  F <- "9_verifica"; reset_fase(F); y <- cfg$y
  set.seed(cfg$seed %||% 1234)
  fold <- fold_di(rownames(dp), K)
  if (!is.null(.RM$gruppi)) registra(F, "9.0", "convalida per unita'", sprintf("%d unita' divise in %d fold", length(unique(gruppi_di(rownames(dp)))), K),
    "tutte le righe di un'unita' nello stesso fold: si stima l'errore su unita' nuove, non su righe nuove di unita' gia' viste", "-", "AUTO")
  yh4 <- yh0 <- yh1 <- rep(NA_real_, nrow(dp)); sel <- list(); st_k <- list(); x_top <- NULL; yhp <- list()
  for (k in seq_len(K)) {
    cat(format(Sys.time(), "%H:%M:%S"), "  convalida della procedura: fold", k, "di", K, "\n")
    s <- tryCatch(costruisci_lineare(dp[fold != k, ], cfg, completo = FALSE), error = function(e) { message(e$message); NULL })
    if (is.null(s)) next
    dv <- applica_stato(dp[fold == k, ], s)
    yh4[fold == k] <- yinv(prevedi_sicuro(s$fit4, dv), s$lambda, residuals(s$fit4))
    pp <- prev_passi(s, dv); for (kp in names(pp)) { if (is.null(yhp[[kp]])) yhp[[kp]] <- rep(NA_real_, nrow(dp)); yhp[[kp]][fold == k] <- pp[[kp]] }
    yh0[fold == k] <- prevedi_sicuro(lmx(formula(s$fit0), s$d_imp), dv)
    nx <- setdiff(names(s$d_imp)[sapply(s$d_imp, is.numeric)], y)
    if (length(nx)) { xt <- nx[which.max(abs(sapply(nx, function(v) cor(s$d_imp[[v]], s$d_imp[[y]]))))]
      yh1[fold == k] <- prevedi_sicuro(lmx(f_da_termini(y, xt), s$d_imp), dv); x_top <- xt }
    sel[[k]] <- unique(sub("_og$", "", unlist(lapply(termini(s$fit4), var_base))))
    st_k[[k]] <- list(st = s, idx = which(fold == k))
  }
  med <- median(dp[[y]]); mu <- mean(dp[[y]])
  tab <- rbind(iniziale = metriche_reg(dp[[y]], yh0, med, mu),
               do.call(rbind, lapply(yhp, function(v) metriche_reg(dp[[y]], v, med, mu))),
               finale = metriche_reg(dp[[y]], yh4, med, mu))
  if (!is.null(x_top)) { tab <- rbind(metriche_reg(dp[[y]], yh1, med, mu), tab); rownames(tab)[1] <- paste("solo", x_top) }
  if (tab["finale", "MAE"] > 1.02 * tab["iniziale", "MAE"]) registra(F, "9.1", "finale peggiore dell'iniziale",
    sprintf("MAE %.4g (finale) vs %.4g (iniziale, tutte le x senza accorgimenti)", tab["finale", "MAE"], tab["iniziale", "MAE"]),
    "su dati mai visti la procedura non ha migliorato la previsione",
    "controllare le x tolte e le trasformazioni; con n piccolo preferire il modello piu' semplice se l'obiettivo e' previsivo", "PROPOSTA")
  salva_tab(round(tab, 4), "9_verifica_cv_procedura")
  registra_percorso_fuori(F, tab, rownames(tab), "", cfg)
  n_ok <- sum(!sapply(sel, is.null)); if (n_ok < K) registra(F, "9.1", "fold falliti", K - n_ok,
    "un fold non e' stato stimato", "le sue righe restano senza previsione", "PROPOSTA")
  freq <- sort(table(unlist(sel)) / max(1, n_ok), decreasing = TRUE)
  # forma scelta in ogni fold: scala di y e termini (una forma che cambia tra fold e' una scelta fragile)
  ok <- which(!sapply(st_k, is.null))
  if (length(ok)) { ft <- do.call(rbind, lapply(ok, function(k) { s <- st_k[[k]]$st
      data.frame(fold = k, y = nome_trasf(s$lambda), parametri = length(coef(s$fit4)), termini = paste(termini(s$fit4), collapse = " + ")) }))
    salva_tab(ft, "9_forma_per_fold")
    norm_t <- function(t) sapply(t, function(z) { z <- gsub("\\(([A-Za-z_.][A-Za-z0-9_.]*) - [-0-9.e]+\\)", "(\\1 - c)", z)   # centro della potenza: varia tra fold
      paste(sort(strsplit(z, ":", fixed = TRUE)[[1]]), collapse = ":") })   # bmi:smoker = smoker:bmi
    tt <- table(unlist(lapply(ok, function(k) unique(norm_t(termini(st_k[[k]]$st$fit4)))))) / length(ok)
    fr <- names(tt)[tt < 0.6]
    registra(F, "9.2b", "forma per fold", sprintf("scala di y: %s; termini in meno del 60%% dei fold: %s",
             paste(names(table(ft$y)), table(ft$y), sep = " x", collapse = ", "), if (length(fr)) paste(fr, collapse = ", ") else "nessuno"),
             "la procedura rifatta su ogni fold sceglie la stessa forma?", if (length(fr) || length(unique(ft$y)) > 1) "forma in parte fragile: leggere quei termini con cautela" else "forma stabile",
             if (length(fr) || length(unique(ft$y)) > 1) "PROPOSTA" else "AUTO") }
  salva_tab(data.frame(quota_fold_selezionata = round(as.numeric(freq), 2), row.names = names(freq)), "9_stabilita_selezione")
  registra(F, "9.1", "precisione (convalida della procedura)", sprintf("%d fold; MAE iniziale %.4g -> finale %.4g; MASE %.2f -> %.2f; R2 fuori campione %.3f -> %.3f",
           K, tab["iniziale", "MAE"], tab["finale", "MAE"], tab["iniziale", "MASE"], tab["finale", "MASE"], tab["iniziale", "R2_fuori_campione"], tab["finale", "R2_fuori_campione"]),
           "ogni fold ripete da capo tutte le scelte (trasformazioni, grouping, selezione, pulizia): stima onesta",
           "coefficienti finali = modello costruito su tutti i dati")
  instab <- names(freq)[freq > 0 & freq < 0.6]
  registra(F, "9.2", "stabilita' della selezione", sprintf("variabili scelte in tutti i fold: %d; scelte in meno del 60%% dei fold: %s",
           sum(freq == 1), if (length(instab)) paste(instab, collapse = ", ") else "nessuna"),
           "una variabile scelta solo in alcuni fold e' un effetto fragile", if (length(instab)) "leggerle con cautela" else "-",
           if (length(instab)) "PROPOSTA" else "AUTO")
  list(tab = tab, freq = freq, st_k = st_k, fold = fold, yh4 = yh4)
}

# --------------------------------------------------------------------
# verifica della logistica sui dati mai visti
# --------------------------------------------------------------------
auc <- function(yb, p) { ok <- !is.na(p); yb <- yb[ok]; r <- rank(p[ok]); n1 <- sum(yb == 1); n0 <- sum(yb == 0)
  if (!n1 || !n0) return(NA); (sum(r[yb == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
metriche_logit <- function(yb, p, prev_c, soglia2) {
  ok <- !is.na(p); yb <- yb[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  br <- mean((yb - p)^2); br0 <- mean((yb - prev_c)^2); hl <- tryCatch(hosmer_lemeshow(yb, p), error = function(e) c(p = NA))
  acc <- function(s) { pr <- as.integer(p > s); c(mean(pr == yb), mean(pr[yb == 1] == 1), mean(pr[yb == 0] == 0)) }
  a5 <- acc(0.5); a2 <- acc(soglia2)
  c(n = length(yb), eventi_pct = 100 * mean(yb), AUC = auc(yb, p), Brier = br, Brier_skill = 1 - br / br0,
    log_loss = -mean(yb * log(p) + (1 - yb) * log(1 - p)), HL_p = unname(hl["p"]),
    acc_0.5 = a5[1], sens_0.5 = a5[2], spec_0.5 = a5[3], acc_prev = a2[1], sens_prev = a2[2], spec_prev = a2[3])
}
applica_logit <- function(dn, st, lo) {
  dv <- applica_stato(dn, st); dv$.yb <- evento_binario(dv, st$y, lo$soglia, lo$evento %||% "")
  for (nv in names(lo$mappe_b)) { m <- lo$mappe_b[[nv]]
    g <- unname(m$mappa[as.character(dv[[m$var]])]); g[is.na(g)] <- m$livelli[1]; dv[[nv]] <- factor(g, levels = m$livelli) }
  dv
}
verifica_logistica <- function(st, lo, div) {
  F <- "8_logistica"; g <- lo$g; prev <- mean(lo$d$.yb)
  righe <- list(); righe[["costruzione (in-sample)"]] <- metriche_logit(lo$d$.yb, fitted(g), prev, prev)
  for (nm in names(div$val)) {
    dv <- applica_logit(div$val[[nm]], st, lo); p <- prevedi_sicuro(g, dv)
    righe[[nm]] <- metriche_logit(dv$.yb, p, prev, prev)
    dv2 <- dv[!is.na(p), ]
    gv <- tryCatch(suppressWarnings(glmx(formula(g), dv2)), error = function(e) NULL)
    if (!is.null(gv)) {
      s1 <- summary(g)$coefficients; s2 <- summary(gv)$coefficients
      k <- setdiff(intersect(rownames(s1), rownames(s2)), "(Intercept)"); sg <- s1[k, 4] < 0.05
      cc <- data.frame(b_costruzione = s1[k, 1], b_verifica = s2[k, 1], stesso_segno = sign(s1[k, 1]) == sign(s2[k, 1]),
                       signif_costruzione = sg, signif_verifica = s2[k, 4] < 0.05)
      salva_tab(round(cc, 4), paste0("8_stabilita_logit_", nm))
      registra(F, "L.10", paste("stabilita' logit su", nm), sprintf("stesso segno tra i significativi: %d/%d; significativita' confermata: %d/%d",
               sum(cc$stesso_segno & sg), sum(sg), sum(sg & cc$signif_verifica), sum(sg)), "formula congelata riestimata sulla verifica", "-")
    }
  }
  tab <- do.call(rbind, righe); salva_tab(round(tab, 4), "8_verifica_predittiva")
  for (nm in names(div$val)) registra(F, "L.10", paste("precisione su", nm),
    sprintf("AUC %.3f (in-sample %.3f); Brier skill %.3f; HL p %.3f; eventi %.1f%% (costruzione %.1f%%)", tab[nm, "AUC"], tab[1, "AUC"],
            tab[nm, "Brier_skill"], tab[nm, "HL_p"], tab[nm, "eventi_pct"], tab[1, "eventi_pct"]),
    "dati mai usati: AUC = probabilita' che un evento abbia punteggio piu' alto di un non-evento",
    if (tab[nm, "n"] > 5000) "con n > 5000 Hosmer-Lemeshow rifiuta anche scostamenti irrilevanti: giudicare la calibrazione dal grafico" else "-")
  png_apri("8_calibrazione_logit", 5 * length(div$val), 4.5); par(mfrow = c(1, length(div$val)))
  for (nm in names(div$val)) { dv <- applica_logit(div$val[[nm]], st, lo); p <- prevedi_sicuro(g, dv); ok <- !is.na(p)
    dec <- cut(rank(p[ok], ties.method = "first"), 10, labels = FALSE)
    plot(tapply(p[ok], dec, mean), tapply(dv$.yb[ok], dec, mean), pch = 19, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "probabilita' prevista (decili)", ylab = "quota osservata", main = paste("Calibrazione:", nm)); abline(0, 1, lty = 2, col = 4) }
  dev.off()
  tab
}
cv_logistica <- function(cvr, dp, cfg, it_lin) {
  y <- cfg$y; p <- rep(NA_real_, nrow(dp)); yb <- rep(NA_integer_, nrow(dp)); prev <- NA
  vecchio <- isTRUE(.RM$quiet); .RM$quiet <- TRUE; on.exit(.RM$quiet <- vecchio)
  for (k in seq_along(cvr$st_k)) { sk <- cvr$st_k[[k]]; if (is.null(sk)) next
    lo <- tryCatch(logistica(sk$st$d1, y, sk$st$x1, cfg, it_lin, 0), error = function(e) NULL); if (is.null(lo)) next
    dv <- applica_logit(dp[sk$idx, ], sk$st, lo); p[sk$idx] <- prevedi_sicuro(lo$g, dv); yb[sk$idx] <- dv$.yb }
  .RM$quiet <- vecchio
  prev <- mean(yb, na.rm = TRUE)
  tab <- rbind(convalida_procedura = metriche_logit(yb, p, prev, prev))
  salva_tab(round(tab, 4), "8_verifica_cv_procedura")
  registra("8_logistica", "L.10", "precisione (convalida della procedura)", sprintf("AUC %.3f; Brier skill %.3f", tab[1, "AUC"], tab[1, "Brier_skill"]),
           "ogni fold ripete tutte le scelte", "-")
  tab
}

# --------------------------------------------------------------------
# la parte binaria serve davvero? (soglia sostanziale, y bimodale, massa di zeri)
# coefficiente di bimodalita' (Pfister et al. 2013): BC > 0.555 suggerisce due gruppi
# --------------------------------------------------------------------
serve_logistica <- function(yv, cfg) {
  yv <- yv[!is.na(yv)]; n <- length(yv)
  g <- skew(yv); k <- kurt(yv)
  bc <- (g^2 + 1) / (k + 3 * (n - 1)^2 / ((n - 2) * (n - 3)))
  zeri <- mean(yv == 0); distinti <- length(unique(yv))
  scelta <- cfg$logistica %||% "auto"
  motivi <- c(if (!is.null(cfg$decisioni$soglia_binaria)) "soglia sostanziale indicata", if (nzchar(cfg$evento %||% "")) paste("evento indicato:", cfg$evento),
              if (bc > 0.555) sprintf("y bimodale (coefficiente di bimodalita' %.2f > 0.555)", bc),
              if (zeri >= 0.10 && zeri <= 0.90) sprintf("%.0f%% di zeri: domanda 'zero o positivo?'", 100 * zeri))
  if (distinti <= 10) motivi <- c(motivi, sprintf("y con %d valori: fasce ordinate -> logistica ordinale (estensione)", distinti))
  esegui <- if (identical(scelta, "si") || isTRUE(scelta)) TRUE else if (identical(scelta, "no") || isFALSE(scelta)) FALSE else length(motivi) > 0
  list(esegui = esegui, motivi = motivi, bc = bc, zeri = zeri, distinti = distinti, scelta = scelta)
}

# --------------------------------------------------------------------
# perche' il modello non prevede di piu'? (tetto di informazione contro forma funzionale)
#   (a) benchmark flessibile: GAM con spline su tutte le numeriche, stesse x -> se fa poco meglio,
#       il limite non e' la forma ma l'informazione che manca
#   (b) errore relativo tipico (mediana di |y - yhat| / y): piu' leggibile del MAE quando y spazia
#       su ordini di grandezza
#   (c) informazione nelle colonne escluse ad alta cardinalita' (regista, attore, editore):
#       media "leave-one-out" dei residui del modello finale nello stesso gruppo -> R2 > 0 = il gruppo
#       sa qualcosa che il modello non sa (da usare con shrinkage / effetti casuali, mai la media grezza)
# --------------------------------------------------------------------
# formula del GAM di confronto: spline sulle quantitative + fattori + le interazioni e le soglie del modello
# finale come termini parametrici (senza, il GAM "flessibile" perde le interazioni e il confronto e' ingiusto:
# assicurazioni, GAM 0,75 contro 0,87 del lineare solo per l'interazione fumo x BMI)
gam_formula <- function(num, oth, d, fit) {
  ex <- termini(fit); ex <- ex[grepl(":", ex) | grepl(">=", ex)]
  ex <- ex[vapply(ex, function(tt) all(var_base(tt) %in% names(d)), logical(1))]
  as.formula(paste(".yt ~", paste(c(sapply(num, function(v) sprintf("s(%s, k=%d)", v, min(10, length(unique(d[[v]])) - 1))), oth, ex), collapse = " + ")))
}
diagnosi_tetto <- function(st, div, cfg, cvr = NULL) {
  F <- "9_verifica"; y <- cfg$y; lam <- st$lambda; out <- list()
  if (!length(div$val) && !is.null(cvr)) {
    # n piccolo: GAM flessibile stimato in ogni fold sulle righe del modello di quel fold, previsto sul fold escluso
    dp <- div$build; pg <- rep(NA_real_, nrow(dp))
    for (sk in cvr$st_k) { s <- sk$st; x <- intersect(s$x1, names(s$d4)); d4 <- s$d4
      num <- x[sapply(d4[x], function(z) is.numeric(z) && length(unique(z)) >= 10)]; oth <- setdiff(x, num); if (!length(num)) next
      fg <- gam_formula(num, oth, d4, s$fit4)
      g <- tryCatch(mgcv::gam(fg, data = d4, method = "REML"), error = function(e) NULL); if (is.null(g)) next
      dv <- applica_stato(dp[sk$idx, ], s)
      for (v in num) { r <- range(d4[[v]], na.rm = TRUE); dv[[v]] <- pmin(pmax(dv[[v]], r[1]), r[2]) }
      ok <- rep(TRUE, nrow(dv)); for (v in oth) if (is.factor(d4[[v]])) { ok <- ok & as.character(dv[[v]]) %in% levels(droplevels(d4[[v]]))
        dv[[v]] <- factor(as.character(dv[[v]]), levels = levels(d4[[v]])) }
      pr <- rep(NA_real_, nrow(dv)); pr[ok] <- tryCatch(yinv(as.numeric(predict(g, dv[ok, ])), s$lambda, residuals(g, type = "response")), error = function(e) NA)
      pg[sk$idx] <- pr }
    med_c <- median(dp[[y]]); mean_c <- mean(dp[[y]]); k <- !is.na(pg) & !is.na(cvr$yh4)
    if (sum(k) > 30) {
      mg <- metriche_reg(dp[[y]][k], pg[k], med_c, mean_c); ml <- metriche_reg(dp[[y]][k], cvr$yh4[k], med_c, mean_c)
      out$gam <- rbind(modello_finale = ml, GAM_flessibile = mg); salva_tab(round(out$gam, 4), "9_tetto_gam")
      dR2 <- mg[["R2_fuori_campione"]] - ml[["R2_fuori_campione"]]; dMAE <- 1 - mg[["MAE"]] / ml[["MAE"]]
      registra(F, "9.5", "forma o informazione?", sprintf("GAM flessibile (convalida): R2 fuori campione %.3f contro %.3f della procedura; MAE %+.1f%%",
               mg[["R2_fuori_campione"]], ml[["R2_fuori_campione"]], -100 * dMAE),
               "se un modello molto piu' flessibile non fa meglio, il limite e' l'informazione nelle x, non la forma",
               if (dR2 < 0.02 && dMAE < 0.03) "la forma non e' il limite: mancano informazioni (variabili) per prevedere di piu'"
               else "la forma conta ancora: rivedere le trasformazioni (grafico GAM) o le interazioni", "AUTO")
    }
    yy <- dp[[y]]; p <- cvr$yh4; k <- !is.na(p) & yy > 0
    if (sum(k) > 30) { rel <- abs(yy[k] - p[k]) / yy[k]
      out$rel <- c(errore_relativo_mediano = median(rel), quota_entro_25pct = mean(rel <= .25), quota_entro_50pct = mean(rel <= .5),
                   quota_oltre_doppio = mean(p[k] > 2 * yy[k] | p[k] < yy[k] / 2))
      registra(F, "9.6", "errore relativo tipico", sprintf("meta' dei casi entro +-%.0f%% del valore vero; %.0f%% entro +-25%%, %.0f%% entro +-50%%; %.0f%% sbagliati di oltre il doppio",
               100 * out$rel[1], 100 * out$rel[2], 100 * out$rel[3], 100 * out$rel[4]), "errore in percentuale (convalida)", "-", "AUTO") }
  }
  val <- if (length(div$val)) applica_stato(div$val[[1]], st) else NULL
  # (a) GAM benchmark
  x <- intersect(st$x1, names(st$d4)); d2 <- st$d4          # stesse righe del modello finale (dopo la pulizia): confronto alla pari
  num <- x[sapply(d2[x], function(z) is.numeric(z) && length(unique(z)) >= 10)]
  oth <- setdiff(x, num)
  if (length(num) && !is.null(val) && requireNamespace("mgcv", quietly = TRUE)) {
    fg <- gam_formula(num, oth, d2, st$fit4)
    g <- tryCatch(if (nrow(d2) > 3000) mgcv::bam(fg, data = d2, discrete = TRUE) else mgcv::gam(fg, data = d2, method = "REML"), error = function(e) NULL)
    if (!is.null(g)) {
      ok <- complete.cases(val[c(x, y)])
      for (v in oth) if (is.factor(d2[[v]])) ok <- ok & as.character(val[[v]]) %in% levels(droplevels(d2[[v]]))
      vv <- val[ok, ]; for (v in oth) if (is.factor(d2[[v]])) vv[[v]] <- factor(as.character(vv[[v]]), levels = levels(d2[[v]]))
      vg <- vv; for (v in num) { r <- range(d2[[v]], na.rm = TRUE); vg[[v]] <- pmin(pmax(vg[[v]], r[1]), r[2]) }   # niente estrapolazione delle spline
      pg <- tryCatch(yinv(as.numeric(predict(g, vg)), lam, residuals(g, type = "response")), error = function(e) NULL)
      pl <- yinv(prevedi_sicuro(st$fit4, vv), lam, residuals(st$fit4))
      if (!is.null(pg)) {
        med_c <- median(st$d2[[y]]); mean_c <- mean(st$d2[[y]])
        mg <- metriche_reg(vv[[y]], pg, med_c, mean_c); ml <- metriche_reg(vv[[y]], pl, med_c, mean_c)
        out$gam <- rbind(modello_finale = ml, GAM_flessibile = mg)
        salva_tab(round(out$gam, 4), "9_tetto_gam")
        dR2 <- mg[["R2_fuori_campione"]] - ml[["R2_fuori_campione"]]; dMAE <- 1 - mg[["MAE"]] / ml[["MAE"]]
        registra(F, "9.5", "forma o informazione?", sprintf("GAM flessibile sulle stesse x: R2 fuori campione %.3f contro %.3f del modello finale; MAE %+.1f%%",
                 mg[["R2_fuori_campione"]], ml[["R2_fuori_campione"]], -100 * dMAE),
                 "se un modello molto piu' flessibile non fa meglio, il limite e' l'informazione nelle x, non la forma",
                 if (dR2 < 0.02 && dMAE < 0.03) "la forma non e' il limite: mancano informazioni (variabili) per prevedere di piu'"
                 else "la forma conta ancora: rivedere le trasformazioni (grafico GAM) o le interazioni", "AUTO")
      }
    }
  }
  # (a2) boosting sulle stesse x (facoltativo, se xgboost e' installato): coglie da solo forme e interazioni di
  # ogni ordine. Se nemmeno lui fa meglio, il limite sono i dati (studenti: boosting 0,305 contro 0,319 del
  # lineare, lezione 121); se fa molto meglio, c'e' struttura che il modello lineare non vede (fase ML)
  if (length(x) && !is.null(val) && requireNamespace("xgboost", quietly = TRUE)) {
    ok <- complete.cases(val[c(x, y)]); vv <- val[ok, ]
    tutto <- rbind(d2[x], vv[x]); for (v in names(tutto)) if (is.character(tutto[[v]])) tutto[[v]] <- factor(tutto[[v]])
    mm <- tryCatch(model.matrix(~ . - 1, tutto), error = function(e) NULL)
    if (!is.null(mm) && nrow(mm) == nrow(tutto) && nrow(vv) >= 30) {
      itr <- seq_len(nrow(d2)); ite <- nrow(d2) + seq_len(nrow(vv)); yb <- d2[[y]]
      obj <- if (identical(cfg$metrica, "media")) "reg:squarederror" else "reg:absoluteerror"
      set.seed(cfg$seed %||% 1234)
      b <- tryCatch(xgboost::xgboost(x = mm[itr, , drop = FALSE], y = yb, nrounds = 400, learning_rate = 0.03, max_depth = 4, subsample = 0.8,
                                     colsample_bytree = 0.8, min_child_weight = 10, objective = obj, nthreads = 1),
                    error = function(e) tryCatch(xgboost::xgboost(data = mm[itr, , drop = FALSE], label = yb, nrounds = 400, eta = 0.03, max_depth = 4,
                                     subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 10, objective = obj, verbose = 0, nthread = 1),
                                     error = function(e) NULL))
      pb <- if (is.null(b)) NULL else tryCatch(as.numeric(predict(b, mm[ite, , drop = FALSE])), error = function(e) NULL)
      if (!is.null(pb)) {
        pl <- yinv(prevedi_sicuro(st$fit4, vv), lam, residuals(st$fit4))
        med_c <- median(st$d2[[y]]); mean_c <- mean(st$d2[[y]])
        mb <- metriche_reg(vv[[y]], pb, med_c, mean_c); ml <- metriche_reg(vv[[y]], pl, med_c, mean_c)
        out$boost <- rbind(modello_finale = ml, boosting = mb); salva_tab(round(out$boost, 4), "9_tetto_boosting")
        dMAE <- 1 - mb[["MAE"]] / ml[["MAE"]]
        registra(F, "9.5b", "confronto con il boosting", sprintf("boosting (xgboost) sulle stesse x: R2 fuori campione %.3f contro %.3f del modello finale; MAE %+.1f%%",
                 mb[["R2_fuori_campione"]], ml[["R2_fuori_campione"]], -100 * dMAE),
                 "il boosting trova da solo forme e interazioni: e' il tetto pratico con queste x",
                 if (dMAE < 0.03) "neanche il boosting fa meglio: per prevedere di piu' servono altri dati, non un altro modello"
                 else "il boosting prevede meglio: ci sono forme o interazioni che il lineare non vede (fase machine learning)", "AUTO")
      }
    }
  }
  # (b) errore relativo tipico
  if (!is.null(val)) {
    p <- yinv(prevedi_sicuro(st$fit4, val), lam, residuals(st$fit4)); yy <- val[[y]]; k <- !is.na(p) & yy > 0
    if (sum(k) > 30) {
      rel <- abs(yy[k] - p[k]) / yy[k]
      out$rel <- c(errore_relativo_mediano = median(rel), quota_entro_25pct = mean(rel <= .25), quota_entro_50pct = mean(rel <= .5),
                   quota_oltre_doppio = mean(p[k] > 2 * yy[k] | p[k] < yy[k] / 2))
      registra(F, "9.6", "errore relativo tipico", sprintf("meta' dei casi entro +-%.0f%% del valore vero; %.0f%% entro +-25%%, %.0f%% entro +-50%%; %.0f%% sbagliati di oltre il doppio",
               100 * out$rel[1], 100 * out$rel[2], 100 * out$rel[3], 100 * out$rel[4]),
               "quando y spazia su ordini di grandezza l'errore in percentuale e' piu' leggibile del MAE", "-", "AUTO")
    }
  }
  # (b2) sottogruppo nascosto: molti residui grandi tutti dello stesso segno (assicurazioni: ~6% di non
  # fumatori con spese di 10-25.000 $ sopra il previsto) = una variabile che manca, non una forma sbagliata
  rs <- rstandard(st$fit4); rs <- rs[is.finite(rs)]
  if (length(rs) > 100) { pos <- mean(rs > 3); neg <- mean(rs < -3)
    if (max(pos, neg) >= 0.02 && max(pos, neg) >= 4 * max(min(pos, neg), 0.0025)) {
      lato <- if (pos > neg) "sopra" else "sotto"
      out$coda <- c(quota = max(pos, neg), lato = lato)
      registra(F, "9.8", "sottogruppo nascosto nei residui", sprintf("%.1f%% dei casi ha residuo standardizzato oltre 3 (tutti %s il previsto; dall'altra parte %.1f%%)",
               100 * max(pos, neg), lato, 100 * min(pos, neg)), "con errori normali oltre 3 sd cade lo 0,1% per lato: una coda unilaterale e' un gruppo che le x non distinguono",
               "manca una variabile che identifichi questi casi (vedi 'cosa manca'); non e' un problema di forma ne' di outlier da togliere", "AUTO") } }
  # (c) segnale nelle colonne escluse
  esc <- unlist(cfg$escludi)
  if (length(esc)) {
    vq <- .RM$quiet; .RM$quiet <- TRUE
    raw <- tryCatch(leggi_dati(cfg), error = function(e) NULL)
    dA <- tryCatch(prepara_base(raw, cfg), error = function(e) NULL)
    .RM$quiet <- vq
    if (!is.null(dA)) {
      dv <- applica_stato(dA, st); eta <- prevedi_sicuro(st$fit4, dv)
      r <- ytrasf(dv[[y]], lam) - eta; names(r) <- rownames(dv)
      righe <- list()
      # la colonna che definisce le unita' non si propone: la verifica e' su unita' mai viste, per le quali
      # l'effetto di gruppo non esiste (energia: "Entity spiega il 57% dei residui", lezione 128)
      v_gr <- if (!is.null(cfg$gruppo) && nzchar(cfg$gruppo)) tryCatch(all.vars(parse(text = cfg$gruppo)), error = function(e) character()) else character()
      for (v in setdiff(intersect(esc, names(raw)), v_gr)) {
        z <- raw[rownames(dv), v]; if (!is.character(z) && !is.factor(z)) next
        z <- as.character(z); lista <- mean(grepl(",", z), na.rm = TRUE) > 0.3
        if (lista) z <- trimws(sub(",.*", "", z))                    # lista (attori, paesi): il primo nome
        z[!nzchar(z)] <- NA
        if (mean(grepl("^[$€£]?\\s*-?[0-9][0-9.,]*(\\s+[A-Za-z]{1,5}|\\s*%)?$", na.omit(z))) > 0.9) next   # numeri scritti come testo
        tt <- table(z); if (length(tt) < 30 || length(tt) > 0.9 * sum(!is.na(z))) next
        ok <- !is.na(z) & !is.na(r) & z %in% names(tt)[tt >= 2]
        if (sum(ok) < 50 || mean(!is.na(z)) < 0.3) next
        rr <- r[ok]; zz <- z[ok]
        s <- ave(rr, zz, FUN = sum); n <- ave(rr, zz, FUN = length); loo <- (s - rr) / (n - 1)
        # shrinkage alla Gelman-Hill: peso n/(n + k), k = var entro / var tra gruppi
        vw <- mean(tapply(rr, zz, var), na.rm = TRUE); vb <- max(var(tapply(rr, zz, mean)) - vw / mean(table(zz)), 1e-12)
        kk <- vw / vb; loo_s <- loo * (n - 1) / (n - 1 + kk)
        righe[[v]] <- data.frame(colonna = v, primo_nome_della_lista = lista, gruppi_ripetuti = length(unique(zz)),
                                 quota_righe = round(sum(ok) / length(r), 3),
                                 R2_residui_loo_grezzo = round(1 - sum((rr - loo)^2) / sum(rr^2), 3),
                                 R2_residui_loo_shrink = round(1 - sum((rr - loo_s)^2) / sum(rr^2), 3))
      }
      if (length(righe)) {
        out$escluse <- do.call(rbind, righe); out$escluse <- out$escluse[order(-out$escluse$R2_residui_loo_shrink), ]
        salva_tab(out$escluse, "9_tetto_colonne_escluse")
        top <- out$escluse[out$escluse$R2_residui_loo_shrink > 0.02, ]
        registra(F, "9.7", "informazione nelle colonne escluse", if (nrow(top)) paste(sprintf("%s: R2 dei residui %.2f su %.0f%% delle righe", top$colonna,
                 top$R2_residui_loo_shrink, 100 * top$quota_righe), collapse = "; ") else "nessuna colonna esclusa spiega i residui",
                 "media dei residui degli ALTRI casi dello stesso gruppo, ristretta verso zero (shrinkage): R2 > 0 = il gruppo sa qualcosa che il modello non sa",
                 if (nrow(top)) sprintf("PROPOSTA: farle entrare come effetto di gruppo ristretto (config effetti_gruppo = [%s]), mai con la media grezza", paste0("\"", top$colonna, "\"", collapse = ", ")) else "-",
                 if (nrow(top)) "PROPOSTA" else "AUTO")
      }
    }
  }
  out
}

# --------------------------------------------------------------------
# y binaria nativa (sopravvive si'/no, abbandona si'/no): preparazione, imputazione e collinearita'
# come per il lineare (il VIF dipende solo dalle x; l'F parziale usa il modello lineare di probabilita'
# solo per scegliere tra due x collineari), poi direttamente la logistica
# --------------------------------------------------------------------
y_binaria <- function(yv, cfg) identical(cfg$tipo_y, "binaria") ||
  (is.numeric(yv) && length(unique(na.omit(yv))) == 2 && all(na.omit(yv) %in% c(0, 1)))

costruisci_binaria <- function(dp, cfg, completo = TRUE) {
  y <- cfg$y
  if (!completo) { vecchio <- isTRUE(.RM$quiet); .RM$quiet <- TRUE; on.exit(.RM$quiet <- vecchio) }
  d <- prepara_apprendi(dp, cfg); sp <- attr(d, "stato_prep")
  aa <- separa_ausiliarie(d, cfg); d <- aa$d
  if (completo) descrittive(d, y)
  d_imp <- imputa_con_ausiliarie(aa, y, cfg)
  co <- collinearita(d_imp, y, cfg)
  list(cfg = cfg, y = y, stato_prep = sp, d = d, d_imp = d_imp, d1 = co$d, x1 = co$x, quote_def = co$quote_def,
       mappe = list(), lambda = NULL, binaria = TRUE)
}

# confronti per la parte binaria: naive (AUC 0,5), la sola x piu' forte, tutte le x prima della selezione, finale
confronti_binaria <- function(st, lo, div) {
  out <- list(); prev <- mean(lo$d$.yb)
  xx <- intersect(st$x1, names(lo$d))
  aic1 <- sapply(xx, function(v) tryCatch(AIC(suppressWarnings(glmx(f_da_termini(".yb", v), lo$d))), error = function(e) Inf))
  x_top <- names(which.min(aic1))
  g1 <- suppressWarnings(glmx(f_da_termini(".yb", x_top), lo$d))
  for (nm in names(div$val)) {
    dv <- applica_logit(div$val[[nm]], st, lo)
    righe <- list()
    righe[["naive (prevalenza)"]] <- metriche_logit(dv$.yb, rep(prev, nrow(dv)), prev, prev)
    righe[[paste("solo", x_top)]] <- metriche_logit(dv$.yb, prevedi_sicuro(g1, dv), prev, prev)
    righe[["tutte le x (prima della selezione)"]] <- metriche_logit(dv$.yb, prevedi_sicuro(lo$g_full, dv), prev, prev)
    righe[["finale"]] <- metriche_logit(dv$.yb, prevedi_sicuro(lo$g, dv), prev, prev)
    out[[nm]] <- do.call(rbind, righe)
    salva_tab(round(out[[nm]], 4), paste0("8_confronti_", nm))
  }
  out
}

# y binaria con n piccolo: convalida incrociata della procedura (ogni fold ripete preparazione,
# collinearita', grouping, forma sul logit e selezione), poi il modello finale su tutti i dati
cv_binaria <- function(dp, cfg, K = 5) {
  F <- "8_logistica"; y <- cfg$y; set.seed(cfg$seed %||% 1234)
  fold <- integer(nrow(dp)); for (cl in unique(dp[[y]])) { i <- which(dp[[y]] == cl); fold[i] <- sample(rep(seq_len(K), length.out = length(i))) }  # strati per classe
  p_fin <- p_tut <- p_1 <- rep(NA_real_, nrow(dp)); yb <- rep(NA_integer_, nrow(dp)); scelti <- list()
  for (k in seq_len(K)) {
    cat(format(Sys.time(), "%H:%M:%S"), "  convalida della procedura (binaria): fold", k, "di", K, "\n")
    vq <- .RM$quiet; .RM$quiet <- TRUE
    r <- tryCatch({ st <- costruisci_binaria(dp[fold != k, ], cfg, completo = FALSE)
      lo <- logistica(st$d1, y, st$x1, cfg, character(), 0); list(st = st, lo = lo) }, error = function(e) { message(e$message); NULL })
    .RM$quiet <- vq; if (is.null(r)) next
    dv <- applica_logit(dp[fold == k, ], r$st, r$lo); idx <- which(fold == k)
    yb[idx] <- dv$.yb; p_fin[idx] <- prevedi_sicuro(r$lo$g, dv); p_tut[idx] <- prevedi_sicuro(r$lo$g_full, dv)
    scelti[[k]] <- unique(unlist(lapply(attr(terms(r$lo$g), "term.labels"), var_base)))
  }
  prev <- mean(yb, na.rm = TRUE)
  tab <- rbind(`naive (prevalenza)` = metriche_logit(yb, rep(prev, length(yb)), prev, prev),
               `tutte le x (prima della selezione)` = metriche_logit(yb, p_tut, prev, prev),
               finale = metriche_logit(yb, p_fin, prev, prev))
  salva_tab(round(tab, 4), "8_confronti_convalida")
  ok <- !is.na(p_fin); dec <- cut(rank(p_fin[ok], ties.method = "first"), 10, labels = FALSE)
  png_apri("8_calibrazione_logit", 5, 4.5)
  plot(tapply(p_fin[ok], dec, mean), tapply(yb[ok], dec, mean), pch = 19, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "probabilita' prevista fuori fold (decili)", ylab = "quota osservata", main = "Calibrazione: convalida"); abline(0, 1, lty = 2, col = 4)
  dev.off()
  fr <- sort(table(unlist(scelti)) / max(1, length(scelti)), decreasing = TRUE)
  salva_tab(data.frame(quota_fold = round(as.numeric(fr), 2), row.names = names(fr)), "8_stabilita_selezione")
  registra(F, "L.10", "precisione (convalida della procedura)", sprintf("AUC %.3f (tutte le x %.3f); Brier skill %.3f; %d fold",
           tab["finale", "AUC"], tab["tutte le x (prima della selezione)", "AUC"], tab["finale", "Brier_skill"], length(scelti)),
           "ogni fold ripete da capo tutte le scelte: stima onesta su n piccolo", "-")
  fragili <- names(fr)[fr < 0.6]
  if (length(fragili)) registra(F, "L.10", "variabili scelte in pochi fold", paste(sprintf("%s (%.0f%%)", fragili, 100 * fr[fragili]), collapse = ", "),
           "scelte in meno del 60% dei fold: effetto fragile", "leggerne gli OR con cautela", "PROPOSTA")
  list(tab = tab, freq = fr)
}
