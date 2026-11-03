######################################################################
# rm_suite.R - controllo automatico delle regressioni sui collaudi gia' validati
#   Rscript rm_suite.R <cartella_collaudi> <cartella_uscita> [--aggiorna] [--solo a,b] [--completo] [--esci-se-peggiora]
# I percorsi "dati" relativi si leggono rispetto alla cartella che contiene <cartella_collaudi> (la radice del repository).
# --esci-se-peggiora: codice di uscita 1 se un collaudo peggiora o va in errore (per la CI)
# Per ogni config della cartella: esecuzione in modalita' veloce (bootstrap 199, verifica <= 10.000
# righe, niente grafici e report), al massimo 2 processi alla volta, BLAS a un thread.
# Estrae le grandezze chiave e le confronta con <cartella_collaudi>/riferimento.json:
# termini del modello, lambda, metriche di verifica (MAE, RMSE, R2, MASE), AUC della logistica.
# --aggiorna riscrive il riferimento con i risultati correnti (dopo aver accettato le differenze).
######################################################################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("uso: Rscript rm_suite.R <cartella_collaudi> <cartella_uscita> [--aggiorna] [--solo a,b] [--completo]")
qui <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
dir_c <- normalizePath(args[1]); dir_o <- args[2]; dir.create(dir_o, recursive = TRUE, showWarnings = FALSE); dir_o <- normalizePath(dir_o)
aggiorna <- "--aggiorna" %in% args; completo <- "--completo" %in% args; rigido <- "--esci-se-peggiora" %in% args
radice <- dirname(dir_c)
percorso_dati <- function(x) if (grepl("^(/|[A-Za-z]:)", x)) x else file.path(radice, x)
solo <- if (any(grepl("^--solo", args))) strsplit(args[which(args == "--solo") + 1], ",")[[1]] else NULL
cfgs <- list.files(dir_c, pattern = "\\.json$", full.names = TRUE); cfgs <- cfgs[basename(cfgs) != "riferimento.json"]
if (!is.null(solo)) cfgs <- cfgs[sub("\\.json$", "", basename(cfgs)) %in% solo]
`%||%` <- function(a, b) if (is.null(a)) b else a

esegui <- function(f) {
  nm <- sub("\\.json$", "", basename(f)); c0 <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  c0$output <- file.path(dir_o, nm); c0$dati <- percorso_dati(c0$dati)
  if (!completo) { c0$veloce <- TRUE; c0$R_boot <- 199; c0$R_boot_logit <- 199; c0$max_verifica <- min(c0$max_verifica %||% 50000, 10000) }
  unlink(c0$output, recursive = TRUE); dir.create(c0$output, recursive = TRUE)
  fc <- file.path(dir_o, paste0(nm, ".json")); jsonlite::write_json(c0, fc, auto_unbox = TRUE, pretty = TRUE, null = "null")
  t0 <- Sys.time()
  rc <- system2("Rscript", c(file.path(qui, "rm_run.R"), fc, "tutto"), stdout = file.path(dir_o, paste0(nm, ".log")), stderr = file.path(dir_o, paste0(nm, ".log")),
                env = c("OPENBLAS_NUM_THREADS=1", "OMP_NUM_THREADS=1", "LANG=C.UTF-8"))
  list(nome = nm, rc = rc, minuti = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), out = c0$output)
}

estrai <- function(r) {
  e <- list(nome = r$nome, esito = if (r$rc == 0) "ok" else "ERRORE", minuti = r$minuti)
  f <- file.path(r$out, "stato_lineare.rds"); if (!file.exists(f)) return(e)
  s <- readRDS(f); st <- s$st
  if (isTRUE(st$binaria)) {              # y binaria nativa: termini e AUC della logistica
    l <- readRDS(file.path(r$out, "stato_logistica.rds")); e$termini <- sort(attr(terms(l$lo$g), "term.labels"))
    v <- l$ver; if (!is.null(v)) { i <- setdiff(rownames(v), "costruzione (in-sample)")[1]; e$AUC <- unname(v[i, "AUC"]) }
    return(e) }
  e$lambda <- st$lambda; e$parametri <- length(coef(st$fit4)); e$n_costruzione <- nrow(st$d2)
  e$termini <- sort(attr(terms(st$fit4), "term.labels"))
  tab <- if (!is.null(s$ver)) s$ver$tab else if (!is.null(s$cvr)) s$cvr$tab else NULL
  if (!is.null(tab)) {
    rn <- rownames(tab); g <- if (any(grepl(" \\| ", rn))) sub(" \\|.*", "", rn[grepl("\\| finale$", rn) & !grepl("^costruzione", rn)][1]) else ""   # mai la riga in-sample
    pick <- function(k) { i <- if (nzchar(g)) which(rn == paste(g, "|", k)) else which(rn == k); if (length(i)) tab[i[1], ] else NULL }
    fi <- pick("finale"); ini <- pick("iniziale")
    if (!is.null(fi)) { e$MAE <- unname(fi["MAE"]); e$RMSE <- unname(fi["RMSE"]); e$R2 <- unname(fi["R2_fuori_campione"]); e$MASE <- unname(fi["MASE"]) }
    if (!is.null(ini)) { e$MAE_iniziale <- unname(ini["MAE"]); e$R2_iniziale <- unname(ini["R2_fuori_campione"]) }
  }
  fl <- file.path(r$out, "stato_logistica.rds")
  if (file.exists(fl)) { v <- readRDS(fl)$ver; if (!is.null(v)) { i <- if ("verifica" %in% rownames(v)) "verifica" else rownames(v)[nrow(v)]; e$AUC <- unname(v[i, "AUC"]) } }
  reg <- tryCatch(read.csv(file.path(r$out, "registro.csv")), error = function(e) NULL)
  if (!is.null(reg)) e$proposte <- sum(reg$tipo == "PROPOSTA")
  e
}

# i piu' lunghi per primi, 2 processi alla volta
ord <- order(-file.size(sapply(cfgs, function(f) percorso_dati(jsonlite::fromJSON(f)$dati))))
cat(format(Sys.time(), "%H:%M:%S"), "collaudi:", length(cfgs), "\n")
ris <- parallel::mclapply(cfgs[ord], function(f) estrai(esegui(f)), mc.cores = 2, mc.preschedule = FALSE)
names(ris) <- sapply(ris, `[[`, "nome"); ris <- ris[order(names(ris))]
jsonlite::write_json(ris, file.path(dir_o, "risultati.json"), auto_unbox = TRUE, pretty = TRUE, digits = 6)

# confronto con il riferimento
f_rif <- file.path(dir_c, "riferimento.json")
rif <- if (file.exists(f_rif)) jsonlite::fromJSON(f_rif, simplifyVector = FALSE) else list()
num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
righe <- lapply(names(ris), function(nm) {
  a <- ris[[nm]]; b <- rif[[nm]]
  if (is.null(b)) return(data.frame(collaudo = nm, esito = a$esito, minuti = a$minuti, MAE = num(a$MAE), dMAE_pct = NA, R2 = num(a$R2), dR2 = NA,
                                   AUC = num(a$AUC), dAUC = NA, termini = "(nuovo)", giudizio = "nuovo"))
  tb <- unlist(b$termini); ta <- unlist(a$termini)
  dm <- 100 * (num(a$MAE) / num(b$MAE) - 1); dr <- num(a$R2) - num(b$R2); da <- num(a$AUC) - num(b$AUC)
  piu <- setdiff(ta, tb); meno <- setdiff(tb, ta)
  ter <- paste(c(if (length(piu)) paste0("+", piu), if (length(meno)) paste0("-", meno)), collapse = " ")
  if (!identical(num(a$lambda), num(b$lambda))) ter <- paste0(ter, sprintf(" lambda %s->%s", b$lambda, a$lambda))
  giu <- if (a$esito != "ok") "ERRORE" else if (isTRUE(dm > 2) || isTRUE(dr < -0.01) || isTRUE(da < -0.01)) "PEGGIORA" else
         if (isTRUE(dm < -2) || isTRUE(dr > 0.01) || isTRUE(da > 0.01)) "migliora" else if (nzchar(ter)) "cambia forma" else "uguale"
  data.frame(collaudo = nm, esito = a$esito, minuti = a$minuti, MAE = num(a$MAE), dMAE_pct = round(dm, 2), R2 = round(num(a$R2), 4), dR2 = round(dr, 4),
             AUC = round(num(a$AUC), 3), dAUC = round(da, 3), termini = ter, giudizio = giu)
})
cf <- do.call(rbind, righe)
write.csv(cf, file.path(dir_o, "confronto.csv"), row.names = FALSE)
print(cf[, c("collaudo", "esito", "minuti", "dMAE_pct", "R2", "dR2", "AUC", "dAUC", "giudizio")], row.names = FALSE)
cat("\ndifferenze di forma:\n"); for (i in which(nzchar(cf$termini) & cf$termini != "(nuovo)")) cat(" ", cf$collaudo[i], ":", cf$termini[i], "\n")
if (aggiorna) {        # unisce: con --solo si aggiornano solo i collaudi rilanciati, gli altri restano
  nuovo <- rif; for (nm in names(ris)) if (ris[[nm]]$esito == "ok") nuovo[[nm]] <- ris[[nm]]
  jsonlite::write_json(nuovo, f_rif, auto_unbox = TRUE, pretty = TRUE, digits = 6); cat("\nriferimento aggiornato:", f_rif, "(", sum(sapply(ris, function(r) r$esito == "ok")), "collaudi )\n") }
n_pegg <- sum(cf$giudizio %in% c("PEGGIORA", "ERRORE"))
cat(format(Sys.time(), "%H:%M:%S"), "fine; peggioramenti:", n_pegg, "\n")
if (rigido && n_pegg > 0) quit(status = 1)
