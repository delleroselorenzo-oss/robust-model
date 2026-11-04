######################################################################
# rm_report.R - pagine HTML per l'utente (una per checkpoint)
#   report_esplorazione(): prima delle scelte (checkpoint A) - dizionario, descrittive, legame con y
#   report_iniziale():     modello iniziale (termine di confronto) e suoi parametri
#   report_finale():       guadagni del modello finale, parametri finali, robustezza, decisioni
# Pagine autosufficienti (grafici incorporati in base64): si aprono ovunque e si stampano.
######################################################################

COL <- list(blu = "#2a78d6", arancio = "#eb6834", grigio = "#a3a29c", ink = "#0b0b0b", ink2 = "#52514e",
            griglia = "#e4e3df")

h <- function(x) { x <- as.character(x); x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }

fmt1 <- function(x, d = 3) {
  if (length(x) != 1 || is.na(x)) return("")
  if (!is.numeric(x)) return(h(x))
  if (abs(x) >= 1e9) return(paste(formatC(x / 1e9, format = "f", digits = 2, decimal.mark = ","), "mld"))
  if (abs(x) >= 1e6) return(paste(formatC(x / 1e6, format = "f", digits = if (abs(x) >= 1e8) 0 else 1, decimal.mark = ","), "mln"))
  if (x != 0 && abs(x) < 1e-3) return(formatC(x, format = "g", digits = d, decimal.mark = ","))
  dd <- if (abs(x - round(x)) < 1e-9 || abs(x) >= 100) 0 else if (abs(x) >= 10) 1 else d
  formatC(x, format = "f", digits = dd, big.mark = ".", decimal.mark = ",")
}
fmt <- function(x, d = 3) { if (!is.numeric(x)) return(h(x)); vapply(x, fmt1, "", d = d) }
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 1e-4, "&lt;0,0001", formatC(p, format = "f", digits = 4, decimal.mark = ",")))

img64 <- function(path, larg = "100%") {
  if (!file.exists(path)) return("")
  b <- base64enc::base64encode(path)
  sprintf('<img src="data:image/png;base64,%s" style="width:%s">', b, larg)
}

tab_html <- function(df, num = NULL, classi = NULL, max_righe = 200, evidenzia = NULL) {
  if (is.null(df) || !nrow(df)) return("<p class='cap'>(nessun dato)</p>")
  df <- head(df, max_righe)
  num <- num %||% names(df)[sapply(df, is.numeric)]
  th <- paste0("<tr>", paste0(sprintf("<th%s>%s</th>", ifelse(names(df) %in% num, " class='n'", ""), h(names(df))), collapse = ""), "</tr>")
  righe <- vapply(seq_len(nrow(df)), function(i) {
    cel <- vapply(names(df), function(v) { x <- df[[v]][i]
      if (v %in% num) sprintf("<td class='n'>%s</td>", if (grepl("^p$|^p_", v)) fmt_p(x) else fmt(x))
      else sprintf("<td>%s</td>", h(ifelse(is.na(x), "", x))) }, "")
    cl <- if (!is.null(evidenzia) && isTRUE(evidenzia[i])) " class='fin'" else ""
    paste0("<tr", cl, ">", paste0(cel, collapse = ""), "</tr>") }, "")
  paste0("<table>", th, paste0(righe, collapse = ""), "</table>")
}

tiles <- function(v, l, hi = NULL) {
  hi <- hi %||% rep(FALSE, length(v))
  paste0("<div class='tiles'>", paste0(sprintf("<div class='tile%s'><div class='v'>%s</div><div class='l'>%s</div></div>",
         ifelse(hi, " hi", ""), v, h(l)), collapse = ""), "</div>")
}

pagina <- function(file, titolo, sotto, corpo) {
  css <- "
  :root{--ink:#0b0b0b;--ink2:#52514e;--muted:#8a8984;--rule:#e4e3df;--blu:#2a78d6;--fondo:#f6f6f4;--bg:#ffffff}
  @media (prefers-color-scheme: dark){:root:not([data-theme='light']){--ink:#f0efec;--ink2:#c3c2b7;--muted:#9a998f;--rule:#3a3a37;--blu:#3987e5;--fondo:#24241f;--bg:#1a1a19}
    img{background:#fff;border-radius:4px}}
  :root[data-theme='dark']{--ink:#f0efec;--ink2:#c3c2b7;--muted:#9a998f;--rule:#3a3a37;--blu:#3987e5;--fondo:#24241f;--bg:#1a1a19}
  @page{size:A4;margin:14mm}
  body{font-family:Inter,'Helvetica Neue',Arial,sans-serif;color:var(--ink);background:var(--bg);font-size:14px;line-height:1.5;margin:0 auto;max-width:980px;padding:24px 16px}
  h1{font-size:26px;margin:0 0 4px;letter-spacing:-.01em} h2{font-size:18px;margin:28px 0 8px;padding-bottom:4px;border-bottom:1px solid var(--rule)}
  h3{font-size:15px;margin:18px 0 6px}
  .sotto{color:var(--ink2);margin:0 0 16px}
  .tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:10px;margin:14px 0}
  .tile{background:var(--fondo);border-radius:8px;padding:10px 12px}.tile .v{font-size:22px;font-weight:600;font-variant-numeric:tabular-nums}
  .tile .l{color:var(--ink2);font-size:12px;line-height:1.3}.tile.hi .v{color:var(--blu)}
  .scroll{overflow-x:auto}
  table{border-collapse:collapse;width:100%;font-size:12.5px;margin:6px 0}
  th,td{text-align:left;padding:4px 7px;border-bottom:1px solid var(--rule);vertical-align:top}
  th{color:var(--ink2);font-weight:600} td.n,th.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
  tr.fin td{font-weight:600}
  .cap{color:var(--muted);font-size:12px;margin:2px 0 10px}
  .nota{background:var(--fondo);border-left:3px solid var(--blu);padding:9px 13px;margin:12px 0}
  .due{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px;align-items:start}
  code{font-size:12px;background:var(--fondo);padding:1px 4px;border-radius:3px;word-break:break-word}
  ul{margin:4px 0;padding-left:20px} li{margin:2px 0}
  .piede{color:var(--muted);font-size:11px;margin-top:24px;border-top:1px solid var(--rule);padding-top:6px}
  .verdetto{border:2px solid var(--blu);border-radius:10px;padding:12px 16px;margin:14px 0 18px;font-size:15px}
  .verdetto h2{border:0;margin:0 0 6px;padding:0;font-size:17px} .verdetto p{margin:6px 0}
  details.leggi{background:var(--fondo);border-radius:8px;padding:8px 12px;margin:10px 0} details.leggi summary{cursor:pointer;font-weight:600}"
  writeLines(c("<!doctype html><html lang='it'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
               sprintf("<title>%s</title><style>%s</style></head><body>", h(titolo), css),
               sprintf("<h1>%s</h1><p class='sotto'>%s</p>", h(titolo), sotto), corpo,
               sprintf("<p class='piede'>Routine semi-automatica di modellazione robusta - generato il %s</p></body></html>",
                       format(Sys.time(), "%d/%m/%Y %H:%M"))), file, useBytes = TRUE)
  invisible(file)
}

leggi_tab <- function(nome) { f <- file.path(.RM$out, "tabelle", paste0(nome, ".csv"))
  if (!file.exists(f)) return(NULL); x <- read.csv(f, check.names = FALSE); names(x)[1] <- "voce"; x }
graf <- function(nome) file.path(.RM$out, "grafici", paste0(nome, ".png"))
png_r <- function(nome, w = 9, h = 5) png(graf(nome), width = w, height = h, units = "in", res = 110)
par_base <- function(...) par(mar = c(4, 4, 2.5, 1), col.axis = COL$ink2, col.lab = COL$ink2, fg = COL$ink2, las = 1,
                              bty = "n", cex.axis = .85, ...)

# barre orizzontali ordinate (una sola serie: un colore, il titolo la nomina)
barre_h <- function(val, etich, titolo, xlab, col = COL$blu, evid = NULL) {
  o <- order(val); val <- val[o]; etich <- etich[o]; cols <- rep(col, length(val))
  if (!is.null(evid)) cols[evid[o]] <- COL$arancio
  par_base(mar = c(4, max(8, min(22, max(nchar(etich)) * .55)), 2.5, 2))
  b <- barplot(val, horiz = TRUE, names.arg = etich, col = cols, border = NA, main = "", xlab = xlab,
               cex.names = .8, xlim = c(0, max(val, na.rm = TRUE) * 1.15), axes = FALSE)
  at <- pretty(c(0, max(val, na.rm = TRUE))); axis(1, at = at, labels = fmt(at))
  abline(v = pretty(c(0, max(val, na.rm = TRUE))), col = COL$griglia, lwd = .8)
  barplot(val, horiz = TRUE, col = cols, border = NA, add = TRUE, axes = FALSE, names.arg = rep("", length(val)))
  text(val, b, fmt(val), pos = 4, cex = .75, col = COL$ink2, xpd = TRUE)
  title(titolo, adj = 0, cex.main = 1, font.main = 2, col.main = COL$ink)
}

# --------------------------------------------------------------------
# 0. ESPLORAZIONE (prima delle scelte)
# --------------------------------------------------------------------
report_esplorazione <- function(d, pr, cfg) {
  y <- cfg$y; ha_y <- !is.null(y) && nzchar(y) && y %in% names(d) && is.numeric(d[[y]])
  n <- nrow(d)
  num <- names(d)[sapply(d, function(z) is.numeric(z) && length(unique(z[!is.na(z)])) > 2)]
  cat_v <- names(d)[sapply(d, function(z) !is.numeric(z) || length(unique(z[!is.na(z)])) <= 2)]
  testo <- pr$variabile[grepl("testo libero|identificativo|UNITA' RIPETUTE", pr$segnalazione)]
  # dizionario con descrittive
  diz <- do.call(rbind, lapply(names(d), function(v) { z <- d[[v]]
    descr <- if (is.numeric(z) && length(unique(z[!is.na(z)])) > 2) {
      q <- quantile(z, c(0, .25, .5, .75, 1), na.rm = TRUE)
      sprintf("media %s, sd %s | min %s, Q1 %s, mediana %s, Q3 %s, max %s", fmt(mean(z, na.rm = TRUE)), fmt(sd(z, na.rm = TRUE)),
              fmt(q[1]), fmt(q[2]), fmt(q[3]), fmt(q[4]), fmt(q[5]))
    } else { t <- sort(table(z), decreasing = TRUE); k <- min(3, length(t))
      paste0(paste(sprintf("%s (%s%%)", substr(names(t)[1:k], 1, 25), round(100 * t[1:k] / n)), collapse = ", "),
             if (length(t) > 3) sprintf(" ... %d livelli", length(t)) else "") }
    r2 <- NA
    if (ha_y && v != y && !v %in% testo) r2 <- tryCatch({
      if (is.numeric(z) && length(unique(z[!is.na(z)])) > 2) cor(z, d[[y]], use = "pairwise.complete.obs")^2 else {
        zz <- factor(z); if (nlevels(zz) < 2 || nlevels(zz) > 200) NA else summary(lm(d[[y]] ~ zz))$r.squared } }, error = function(e) NA)
    data.frame(variabile = v, tipo = if (is.numeric(z)) "quantitativa" else "qualitativa", distinti = length(unique(z[!is.na(z)])),
               NA_pct = round(100 * mean(is.na(z)), 1), descrittive = descr, R2_con_y = r2) }))
  diz$segnalazione <- pr$segnalazione[match(diz$variabile, pr$variabile)]
  corpo <- c()
  corpo <- c(corpo, tiles(c(format(n, big.mark = ".", decimal.mark = ","), ncol(d), length(num), length(cat_v), sum(pr$segnalazione != "")),
                          c("righe", "colonne", "quantitative", "qualitative o binarie", "colonne con segnalazioni")))
  if (!is.null(cfg$consegna) && nzchar(cfg$consegna)) corpo <- c(corpo, sprintf("<div class='nota'><strong>Domanda di partenza.</strong> %s</div>", h(cfg$consegna)))
  if ((nd <- righe_doppie(d)) > 0) corpo <- c(corpo, sprintf("<div class='nota'><strong>Righe doppie.</strong> %d righe sono identiche a un'altra in tutte le colonne: quasi certamente record doppi, verranno tolte.</div>", nd))
  if (ha_y) {
    yy <- d[[y]]
    png_r("00_y", 9, 3.6); layout(matrix(1:2, 1), widths = c(3, 1)); par_base()
    hist(yy, breaks = 50, col = COL$blu, border = "white", main = "", xlab = y, ylab = "frequenza")
    title(paste("Distribuzione di", y), adj = 0, cex.main = 1, col.main = COL$ink)
    par_base(); boxplot(yy, col = COL$blu, border = COL$ink2, outcol = COL$grigio, pch = 16, cex = .4, main = "", frame = FALSE)
    dev.off()
    corpo <- c(corpo, sprintf("<h2>La variabile da spiegare: %s</h2>", h(y)),
      tiles(c(fmt(mean(yy, na.rm = TRUE)), fmt(median(yy, na.rm = TRUE)), fmt(sd(yy, na.rm = TRUE)),
              sprintf("%s / %s", fmt(min(yy, na.rm = TRUE)), fmt(max(yy, na.rm = TRUE))), fmt(skew(yy), 2),
              sprintf("%s%%", round(100 * mean(yy == 0, na.rm = TRUE), 1))),
            c("media", "mediana", "deviazione standard", "minimo / massimo", "asimmetria (0 = simmetrica)", "valori pari a zero")),
      img64(graf("00_y")))
    # quanto ogni x da sola spiega di y
    k <- !is.na(diz$R2_con_y)
    if (any(k)) {
      top <- head(diz[k, ][order(-diz$R2_con_y[k]), ], 25)
      png_r("00_r2_univariato", 9, max(3, .28 * nrow(top) + 1))
      barre_h(top$R2_con_y, top$variabile, sprintf("Quanto spiega di %s ciascuna variabile da sola (R², 0-1)", y), "R² univariato")
      dev.off()
      corpo <- c(corpo, "<h2>Quali variabili sono legate alla y</h2>",
        "<p>R² di <em>y</em> su una sola variabile alla volta (correlazione al quadrato per le quantitative, ANOVA a una via per le qualitative). Valori sopra 0,8 vanno controllati: possono essere la y stessa sotto altro nome o una sua parte (leakage).</p>",
        img64(graf("00_r2_univariato")))
      sc <- head(top$variabile[top$variabile %in% num], 6)
      if (length(sc)) {
        png_r("00_scatter", 9, 2.9 * ceiling(length(sc) / 3)); par(mfrow = c(ceiling(length(sc) / 3), 3)); par_base(mar = c(4, 4, 2, 1))
        set.seed(1); ii <- if (n > 4000) sample(n, 4000) else seq_len(n)
        for (v in sc) { plot(d[[v]][ii], yy[ii], pch = 16, cex = .45, col = adjustcolor(COL$blu, .35), xlab = v, ylab = y, main = "")
          ok <- complete.cases(d[[v]][ii], yy[ii]); if (sum(ok) > 20) lines(lowess(d[[v]][ii][ok], yy[ii][ok]), col = COL$arancio, lwd = 2)
          title(v, adj = 0, cex.main = .95, col.main = COL$ink) }
        dev.off()
        corpo <- c(corpo, "<h3>La y contro le quantitative piu' legate</h3>", img64(graf("00_scatter")),
                   "<p class='cap'>Punti: un campione di al massimo 4.000 righe. Linea arancione: andamento medio (lowess); se curva, la relazione non e' lineare.</p>")
      }
      bx <- head(top$variabile[top$variabile %in% cat_v & top$distinti <= 30], 4)
      if (length(bx)) {
        png_r("00_box", 9, 3.2 * ceiling(length(bx) / 2)); par(mfrow = c(ceiling(length(bx) / 2), 2)); par_base(mar = c(6, 4, 2, 1))
        for (v in bx) { z <- factor(d[[v]]); z <- reorder(z, yy, median, na.rm = TRUE)
          boxplot(yy ~ z, col = COL$blu, border = COL$ink2, outline = FALSE, las = 2, xlab = "", ylab = y, main = "", cex.axis = .75, frame = FALSE)
          title(v, adj = 0, cex.main = .95, col.main = COL$ink) }
        dev.off()
        corpo <- c(corpo, "<h3>La y per livello delle qualitative piu' legate</h3>", img64(graf("00_box")))
      }
    }
  } else {
    cand <- leggi_tab("00_y_candidate")
    if (!is.null(cand)) corpo <- c(corpo, "<h2>Variabili candidate a y</h2><p>Quantitative continue, non identificativi, NA sotto il 20%. La scelta dipende dalla domanda di ricerca.</p>",
                                   tab_html(cand[, -1]))
  }
  # distribuzioni di tutte le quantitative (piccoli multipli)
  nv <- head(setdiff(num, c(y, testo)), 24)
  if (length(nv)) {
    righe <- ceiling(length(nv) / 4)
    png_r("00_distribuzioni", 9, 1.9 * righe); par(mfrow = c(righe, 4)); par_base(mar = c(2.5, 4, 2, .5), cex.axis = .7)
    for (v in nv) { hist(d[[v]], breaks = 30, col = COL$blu, border = "white", main = "", xlab = "", ylab = "")
      title(v, adj = 0, cex.main = .85, col.main = COL$ink) }
    dev.off()
    corpo <- c(corpo, "<h2>Distribuzioni delle quantitative</h2>", img64(graf("00_distribuzioni")),
               "<p class='cap'>Code lunghe a destra suggeriscono il logaritmo; picchi isolati (zeri, 999) possono essere codici di 'mancante'.</p>")
  }
  corpo <- c(corpo, "<h2>Dizionario delle variabili</h2><div class='scroll'>",
             tab_html(diz[, c("variabile", "tipo", "distinti", "NA_pct", "descrittive", if (ha_y) "R2_con_y", "segnalazione")]), "</div>")
  seg <- pr[pr$segnalazione != "", c("variabile", "segnalazione")]
  if (nrow(seg)) corpo <- c(corpo, "<h2>Cose da decidere insieme</h2><p>Segnalazioni automatiche: ognuna corrisponde a una scelta che la routine non fa da sola.</p>",
                            tab_html(seg))
  pagina(file.path(.RM$out, "report_0_esplorazione.html"), paste("Esplorazione:", basename(cfg$dati)),
         "Prima delle scelte: cosa contiene il dataset e cosa e' legato alla variabile da spiegare", corpo)
}


# --------------------------------------------------------------------
# VERDETTO IN PAROLE (in cima ai report): quanto sbaglia su casi nuovi, contro chi, quale modello si tiene
# --------------------------------------------------------------------
giudizio_r2 <- function(r2) if (!is.finite(r2)) "non valutabile" else if (r2 < 0.1) "molto debole" else if (r2 < 0.3) "debole" else
  if (r2 < 0.5) "moderato" else if (r2 < 0.7) "buono" else "forte"
frase_r2 <- function(r2) if (!is.finite(r2)) "" else if (r2 < 0.1) "le variabili disponibili dicono pochissimo su y: nessun modello di questo tipo puo' fare molto meglio; per prevedere servono dati diversi" else
  if (r2 < 0.3) "le variabili colgono una parte piccola delle differenze tra i casi: utili per capire le tendenze, poco per prevedere il singolo caso" else
  if (r2 < 0.5) "le variabili colgono una parte importante delle differenze, ma resta molto di non spiegato" else
  if (r2 < 0.7) "le variabili colgono la maggior parte delle differenze tra i casi" else "le variabili colgono quasi tutte le differenze tra i casi"
giudizio_auc <- function(a) if (!is.finite(a)) "non valutabile" else if (a < 0.6) "molto debole" else if (a < 0.7) "debole" else if (a < 0.8) "discreto" else if (a < 0.9) "buono" else "eccellente"
maiusc <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
n_it <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

verdetto_lineare <- function(sc, y, st) {
  if (is.null(sc)) return(character())
  fin <- sc$fin; ini <- sc$ini; med <- median(st$d[[y]], na.rm = TRUE); naive <- fin[["MAE"]] / fin[["MASE"]]
  alt <- if (sc$chi == "iniziale") fin else ini; fin <- if (sc$chi == "iniziale") ini else fin   # in testa il modello consegnato
  r2 <- fin[["R2_fuori_campione"]]
  c("<div class='verdetto'><h2>In breve</h2>",
    sprintf("<p><strong>Su casi che il modello non ha mai visto, il modello consegnato sbaglia in media di %s</strong> (unita' di %s). Il valore tipico di %s e' %s: l'errore e' circa il %s%% del valore tipico.</p>",
            fmt(fin[["MAE"]]), h(y), h(y), fmt(med), round(100 * fin[["MAE"]] / abs(med))),
    sprintf("<p>Senza modello, rispondendo sempre con il valore tipico, si sbaglierebbe di %s: il modello toglie il <strong>%s%%</strong> dell'errore. %s sbagliava di %s.</p>",
            fmt(naive), round(100 * (1 - fin[["MAE"]] / naive)), if (sc$chi == "iniziale") "Il modello finale (con trasformazioni e selezione)" else "Il modello iniziale (tutte le variabili, nessun accorgimento)", fmt(alt[["MAE"]])),
    sprintf("<p><strong>Quanto spiega:</strong> R&sup2; su dati nuovi %s, segnale <strong>%s</strong>: %s.</p>", fmt(r2), giudizio_r2(r2), frase_r2(r2)),
    sprintf("<p><strong>Modello consegnato: %s.</strong> %s.%s</p>", sc$chi, h(maiusc(sc$motivo)),
            if (sc$chi == "iniziale") " La sezione 'Modello consegnato' qui sotto ne riporta i coefficienti; il resto della pagina descrive il modello finale esaminato e scartato." else ""),
    sprintf("<p class='cap'>Numeri misurati su %s (%s casi mai usati per costruire il modello).</p>", h(sc$dove), n_it(fin[["n"]])),
    "</div>", leggi_numeri(FALSE))
}
verdetto_binaria <- function(sc, y, vl) {
  if (is.null(sc)) return(character())
  fin <- sc$fin; ini <- sc$ini; q <- fin[["eventi_pct"]] / 100
  acc_n <- if (!is.null(sc$naive)) sc$naive[["acc_0.5"]] else max(q, 1 - q)
  auc <- if (sc$chi == "iniziale" && is.finite(ini[["AUC"]])) ini[["AUC"]] else fin[["AUC"]]
  c("<div class='verdetto'><h2>In breve</h2>",
    if (sc$chi == "iniziale") sprintf("<p><strong>Su 100 casi mai visti il modello consegnato (l'iniziale, tutte le variabili) ne classifica correttamente %s</strong>; il modello finale %s. Rispondendo sempre \"%s\" (la risposta piu' frequente) se ne indovinerebbero %s.</p>",
            round(100 * ini[["acc_0.5"]]), round(100 * fin[["acc_0.5"]]), if (q < 0.5) paste(h(y), "= 0") else paste(h(y), "= 1"), round(100 * acc_n)) else
    sprintf("<p><strong>Su 100 casi mai visti ne classifica correttamente %s.</strong> Rispondendo sempre \"%s\" (la risposta piu' frequente) se ne indovinerebbero %s. Il modello iniziale (tutte le variabili) ne indovinava %s.</p>",
            round(100 * fin[["acc_0.5"]]), if (q < 0.5) paste(h(y), "= 0") else paste(h(y), "= 1"), round(100 * acc_n), round(100 * ini[["acc_0.5"]])),
    sprintf("<p><strong>Quanto distingue:</strong> AUC %s, potere di distinzione <strong>%s</strong>: preso a caso un caso con %s = 1 e uno con %s = 0, nel %s%% delle coppie il modello da' al primo una probabilita' piu' alta.</p>",
            fmt(auc), giudizio_auc(auc), h(y), h(y), round(100 * auc)),
    sprintf("<p><strong>Modello consegnato: %s.</strong> %s.</p>", sc$chi, h(maiusc(sc$motivo))),
    "<p class='cap'>Il confronto tra modelli usa l'errore di Brier (quanto le probabilita' previste sono vicine a cio' che succede), piu' stabile della percentuale di classificati; classificare bene e' tema della fase di machine learning.</p>",
    "</div>", leggi_numeri(TRUE))
}
leggi_numeri <- function(binaria) {
  voci <- if (binaria) c(
    "<strong>Classificati correttamente</strong>: su 100 casi nuovi, quanti il modello mette nella classe giusta (probabilita' prevista sopra o sotto 0,5).",
    "<strong>AUC</strong>: 0,5 = a caso, 1 = perfetto; sopra 0,8 il modello distingue bene chi ha l'evento da chi non lo ha.",
    "<strong>Brier skill</strong>: quanto le probabilita' previste migliorano rispetto a dare a tutti la stessa probabilita' (0 = nessun guadagno, 1 = perfetto).",
    "<strong>Sensibilita' / specificita'</strong>: quota degli eventi (e dei non-eventi) riconosciuti.",
    "<strong>Odds ratio (OR)</strong>: sopra 1 la x rende l'evento piu' probabile, sotto 1 meno probabile.",
    "<strong>Dati mai visti</strong>: casi tenuti da parte e mai usati per costruire il modello. Sui dati usati per stimarlo un modello con piu' parametri sembra sempre migliore: per questo si confronta solo qui.") else c(
    "<strong>MAE (errore medio assoluto)</strong>: di quanto sbaglia in media, nelle unita' di y.",
    "<strong>RMSE</strong>: come il MAE ma pesa di piu' gli errori grandi; conta quando interessa la media o il totale.",
    "<strong>MASE</strong>: errore del modello diviso l'errore di chi risponde sempre il valore tipico (la mediana); sotto 1 il modello serve, 0,5 = errore dimezzato.",
    "<strong>R&sup2; su dati nuovi</strong>: quota delle differenze tra i casi che il modello riesce a prevedere (0 = niente, 1 = tutto); puo' essere negativo se il modello sbaglia piu' della media.",
    "<strong>Effetto</strong>: di quanto cambia in media y se la x cambia della quantita' indicata, a parita' delle altre x; IC = intervallo di confidenza al 95%.",
    "<strong>Dati mai visti</strong>: casi tenuti da parte e mai usati per costruire il modello. Sui dati usati per stimarlo un modello con piu' parametri sembra sempre migliore: per questo si confronta solo qui.")
  c("<details class='leggi'><summary>Come si leggono i numeri di questa pagina</summary><ul>", paste0("<li>", voci, "</li>"), "</ul></details>")
}

# --------------------------------------------------------------------
# 1. MODELLO INIZIALE (termine di confronto)
# --------------------------------------------------------------------
report_iniziale <- function(fit0, d, cfg, div = NULL, fo = NULL) {
  y <- cfg$y
  box <- character()
  if (!is.null(fo)) box <- if (isTRUE(fo$binaria)) c("<div class='verdetto'><h2>Quanto prevede gia' il modello iniziale</h2>",
      sprintf("<p><strong>Su 100 casi mai visti ne classifica correttamente %s.</strong> Rispondendo sempre con la classe piu' frequente se ne indovinerebbero %s.</p>", round(100 * fo$acc), round(100 * fo$acc_naive)),
      if (!is.null(fo$met) && is.finite(fo$met[["AUC"]]))
        sprintf("<p><strong>Quanto distingue:</strong> AUC %s, potere di distinzione <strong>%s</strong>. Brier skill %s: le probabilita' previste migliorano del %s%% rispetto a dare a tutti la stessa probabilita'.</p>",
                fmt(fo$met[["AUC"]]), giudizio_auc(fo$met[["AUC"]]), fmt(fo$met[["Brier_skill"]]), round(100 * fo$met[["Brier_skill"]])),
      sprintf("<p>E' il numero da battere: il report finale lo confronta, sugli stessi casi mai visti, con il modello finale. Misurato su %s (%s casi).</p>", h(fo$dove), n_it(fo$n)),
      "<p class='cap'>I numeri piu' sotto (pseudo-R&sup2;, coefficienti) sono calcolati sui dati usati per stimare il modello: descrivono il modello, non servono a confrontarlo con il finale.</p></div>", leggi_numeri(TRUE)) else
    c("<div class='verdetto'><h2>Quanto prevede gia' il modello iniziale</h2>",
      sprintf("<p><strong>Su casi mai visti sbaglia in media di %s</strong> (unita' di %s; valore tipico %s). Rispondendo sempre con il valore tipico si sbaglierebbe di %s: il modello iniziale toglie gia' il %s%% dell'errore. R&sup2; su dati nuovi %s (segnale %s).</p>",
              fmt(fo$MAE), h(y), fmt(fo$mediana), fmt(fo$MAE_naive), round(100 * (1 - fo$MAE / fo$MAE_naive)), fmt(fo$R2), giudizio_r2(fo$R2)),
      sprintf("<p>E' il numero da battere: il report finale lo confronta, sugli stessi casi mai visti, con il modello finale. Misurato su %s (%s casi).</p>", h(fo$dove), n_it(fo$n)),
      "<p class='cap'>R&sup2; e test piu' sotto sono calcolati sui dati usati per stimare il modello: descrivono il modello, non servono a confrontarlo con il finale.</p></div>", leggi_numeri(FALSE))
  if (y_binaria(d[[y]], cfg)) {           # y binaria: il termine di confronto e' una logistica con tutte le x
    fit0 <- suppressWarnings(glm(formula(fit0), data = model.frame(fit0), family = binomial))
    s <- summary(fit0); ct <- as.data.frame(s$coefficients); names(ct) <- c("b (logit)", "SE", "z", "p"); ct$termine <- rownames(ct)
    ct$OR <- exp(ct[["b (logit)"]]); ct$OR[ct$termine == "(Intercept)"] <- NA   # l'OR dell'intercetta non ha significato
    ct <- ct[, c("termine", "b (logit)", "OR", "SE", "z", "p")]
    mcf <- 1 - fit0$deviance / fit0$null.deviance
    tt <- ct[ct$termine != "(Intercept)", ]; tt <- head(tt[order(-abs(tt$z)), ], 25)
    png_r("01_t_iniziale", 9, max(3, .26 * nrow(tt) + 1))
    barre_h(abs(tt$z), tt$termine, "Peso dei termini nella logistica iniziale (|z|; oltre 2 = significativo al 5%)", "|z|", evid = tt$p >= 0.05); dev.off()
    corpo <- c(box, tiles(c(format(nobs(fit0), big.mark = ".", decimal.mark = ","), length(coef(fit0)), fmt(mcf), sprintf("%d%%", as.integer(round(100 * mean(fit0$y)))),
                       sprintf("%d / %d", sum(ct$p[-1] < 0.05, na.rm = TRUE), nrow(ct) - 1)),
                     c("righe usate (costruzione, righe complete)", "parametri", "pseudo-R² di McFadden", "quota di eventi", "termini significativi al 5%"), c(FALSE, FALSE, TRUE, FALSE, FALSE)),
      "<div class='nota'>y binaria: il termine di confronto e' una <strong>logistica</strong> con tutte le x candidate, cosi' come sono. OR > 1 = l'evento diventa piu' probabile.</div>",
      "<h2>Peso dei termini</h2>", img64(graf("01_t_iniziale")), "<h2>Coefficienti</h2><div class='scroll'>", tab_html(ct), "</div>")
    return(pagina(file.path(.RM$out, "report_1_iniziale.html"), paste("Modello iniziale:", y), "Il termine di confronto: logistica con tutte le x candidate", corpo))
  }
  s <- summary(fit0)
  ct <- as.data.frame(s$coefficients); names(ct) <- c("b", "SE", "t", "p"); ct$termine <- rownames(ct)
  ct <- ct[, c("termine", "b", "SE", "t", "p")]
  ct$segno <- ifelse(ct$p < 0.05, ifelse(ct$b > 0, "+ significativo", "- significativo"), "non significativo")
  fs <- s$fstatistic; pF <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  rs <- tryCatch(reset_stat(fit0), error = function(e) NA); bp <- tryCatch(unname(lmtest::bptest(fit0)$statistic), error = function(e) NA)
  vt <- tryCatch(vif_tab(fit0), error = function(e) NULL)
  tt <- ct[ct$termine != "(Intercept)", ]; tt <- head(tt[order(-abs(tt$t)), ], 25)
  png_r("01_t_iniziale", 9, max(3, .26 * nrow(tt) + 1))
  barre_h(abs(tt$t), tt$termine, "Peso dei termini nel modello iniziale (|t|; oltre 2 = significativo al 5%)", "|t|", evid = tt$p >= 0.05)
  abline(v = 1.96, lty = 2, col = COL$ink2); dev.off()
  corpo <- c(box, tiles(c(format(nobs(fit0), big.mark = ".", decimal.mark = ","), length(coef(fit0)), fmt(s$r.squared), fmt(s$adj.r.squared), fmt(s$sigma),
                     sprintf("%d / %d", sum(ct$p[-1] < 0.05, na.rm = TRUE), nrow(ct) - 1)),
                   c("righe usate (campione di costruzione, righe complete)", "parametri", "R²", "R² corretto", "errore standard dei residui",
                     "termini significativi al 5%"), c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
    "<div class='nota'>Il modello iniziale usa <strong>tutte</strong> le x candidate, cosi' come sono, con i minimi quadrati. Non e' il risultato: e' il termine di confronto. Ogni passo successivo (collinearita', trasformazioni, selezione, influenti) deve fare meglio di lui sui dati mai visti.</div>",
    sprintf("<p>Test F globale: F = %s su %d e %d gradi di liberta', p %s. RESET di Ramsey (forma funzionale): %s. Breusch-Pagan (eteroschedasticita'): %s. VIF massimo: %s.</p>",
            fmt(fs[1]), fs[2], fs[3], fmt_p(pF), fmt(rs), fmt(bp), if (is.null(vt)) "-" else fmt(max(vt$VIF_equiv))),
    "<h2>Peso dei termini</h2>", img64(graf("01_t_iniziale")),
    "<p class='cap'>Barre arancioni: termini non significativi al 5%. La linea tratteggiata e' |t| = 1,96.</p>",
    "<h2>Coefficienti del modello iniziale</h2><p>b = variazione media di y per un'unita' in piu' della x (per le qualitative: differenza rispetto al livello di riferimento), a parita' delle altre x.</p><div class='scroll'>",
    tab_html(ct), "</div>",
    "<h2>Diagnostica</h2><div class='due'>", img64(graf("0_diagnostica_fit0")), "</div>",
    "<p class='cap'>Residui contro valori previsti: una forma a U indica non linearita', un ventaglio indica eteroschedasticita'. QQ-plot: i punti lontani dalla diagonale indicano code pesanti.</p>",
    if (file.exists(graf("0_avplots_fit0"))) c("<h3>Partial plot (effetto di ogni x al netto delle altre)</h3>", img64(graf("0_avplots_fit0"))))
  pagina(file.path(.RM$out, "report_1_iniziale.html"), paste("Modello iniziale:", y), "Il termine di confronto: tutte le x candidate, nessuna trasformazione", corpo)
}

# --------------------------------------------------------------------
# 2. MODELLO FINALE (guadagni, parametri, robustezza, decisioni)
# --------------------------------------------------------------------
report_finale <- function(st, ver, cvr, cfg, logit = NULL, tetto = NULL) {
  y <- cfg$y; reg <- .RM$reg
  sc <- st$consegna %||% tryCatch(scelta_consegna(st, ver, cvr, cfg), error = function(e) NULL)
  corpo <- verdetto_lineare(sc, y, st)
  if (!is.null(sc) && sc$chi == "iniziale") {       # si consegna l'iniziale: i suoi coefficienti per primi
    w <- lmtest::coeftest(st$fit0, vcov = vc_rob(st$fit0))
    corpo <- c(corpo, "<h2>Modello consegnato: l'iniziale</h2><p>Tutte le variabili candidate, senza trasformazioni, stimate con i minimi quadrati; errori standard robusti. Sui dati mai visti prevede meglio (o uguale, ed e' piu' semplice) del modello finale descritto piu' sotto.</p>",
               sprintf("<p><code>%s</code></p>", h(paste(deparse(formula(st$fit0), width.cutoff = 500), collapse = " "))),
               "<div class='scroll'>", tab_html(data.frame(termine = rownames(w), b = w[, 1], SE_robusto = w[, 2], p = w[, 4], check.names = FALSE)), "</div>",
               "<h2 style='color:var(--muted)'>Il modello finale esaminato (non consegnato)</h2>")
  }
  val_reg <- function(passo, ogg = NULL) { r <- reg[reg$passo == passo & (is.null(ogg) | grepl(ogg %||% "", reg$oggetto)), ]
    if (nrow(r)) r$valore[nrow(r)] else NA }
  vp <- if (!is.null(ver)) leggi_tab("9_verifica_predittiva") else NULL      # solo se questa esecuzione ha una verifica
  if (!is.null(vp)) {
    vv <- vp[grepl("^(verifica|stessa_epoca|futuro) \\|", vp$voce), ]
    grp <- sub(" \\|.*", "", vv$voce)
    g1 <- grp[1]; v1 <- vv[grp == g1, ]
    fin <- v1[grepl("\\| finale$", v1$voce), ]; ini <- v1[grepl("\\| iniziale$", v1$voce), ]
    mae_naive <- fin$MAE / fin$MASE
    corpo <- c(corpo, tiles(
      c(sprintf("%s%%", round(100 * (1 - fin$MASE))), fmt(fin$R2_fuori_campione), fmt(fin$MAE), sprintf("%+.0f%%", 100 * (fin$MAE / ini$MAE - 1)),
        sprintf("%+.0f%%", 100 * (fin$RMSE / ini$RMSE - 1)), sprintf("%d &rarr; %d", length(coef(st$fit0)), length(coef(st$fit4)))),
      c(sprintf("errore in meno rispetto al naive (MASE %s)", fmt(fin$MASE)), "R² su dati mai visti", sprintf("errore medio assoluto (MAE) su %s righe mai viste", format(fin$n, big.mark = ".", decimal.mark = ",")),
        "MAE rispetto al modello iniziale (valore tipico)", "RMSE rispetto al modello iniziale (media, pesa i casi estremi)", "parametri: iniziale -> finale"),
      c(TRUE, TRUE, FALSE, !identical(cfg$metrica, "media"), identical(cfg$metrica, "media"), FALSE)))
    # grafico dei guadagni
    lab <- c("naive (mediana)", sub("^[^|]*\\| ", "", v1$voce)); mae <- c(mae_naive, v1$MAE)
    png_r("02_guadagni", 9, max(2.6, .45 * length(mae) + 1))
    barre_h(mae, lab, sprintf("Errore medio assoluto su %s righe mai viste (piu' basso = meglio)", format(fin$n, big.mark = ".", decimal.mark = ",")), paste("MAE in unita' di", y),
            col = COL$grigio, evid = lab == "finale")
    dev.off()
    tv <- vv; tv$voce <- sub(" \\| ", " - ", tv$voce)
    tv <- tv[, intersect(c("voce", "n", "MAE", "RMSE", "MASE", "R2_fuori_campione", "distorsione_media"), names(tv))]
    ins <- vp[grepl("costruzione", vp$voce), ]
    corpo <- c(corpo, "<h2>Quanto guadagna il modello finale</h2>", img64(graf("02_guadagni")),
      "<p class='cap'>Naive = prevedere sempre la mediana di y (nessun modello). Arancione: il modello finale.</p>",
      tab_html(tv, evidenzia = grepl("- finale$", tv$voce)),
      sprintf("<p>Sul campione di costruzione l'errore medio era %s, sui dati mai visti %s: %s.</p>", fmt(ins$MAE[1]), fmt(fin$MAE),
              if (fin$MAE / ins$MAE[1] < 1.1) "nessun sovradattamento rilevante" else "c'e' un po' di sovradattamento (oltre il 10%)"))
  } else if (!is.null(cvr)) {
    tb <- as.data.frame(cvr$tab); fin <- tb[grepl("finale", rownames(tb)), ][1, ]; ini <- tb[grepl("iniziale", rownames(tb)), ][1, ]
    corpo <- c(corpo, tiles(c(sprintf("%s%%", round(100 * (1 - fin$MASE))), fmt(fin$R2_fuori_campione), fmt(fin$MAE), sprintf("%+.0f%%", 100 * (fin$MAE / ini$MAE - 1)),
        sprintf("%d &rarr; %d", length(coef(st$fit0)), length(coef(st$fit4)))),
      c(sprintf("errore in meno rispetto al naive (MASE %s)", fmt(fin$MASE)), "R² stimato fuori campione (convalida incrociata)", "errore medio assoluto (MAE)",
        "errore rispetto al modello iniziale", "parametri: iniziale -> finale"), c(TRUE, TRUE, FALSE, FALSE, FALSE)))
    corpo <- c(corpo, "<h2>Quanto guadagna il modello finale</h2><p>Dataset piccolo: la precisione e' stimata con la convalida incrociata della procedura (ogni fold ripete tutte le scelte).</p>",
               tab_html(data.frame(voce = rownames(cvr$tab), as.data.frame(cvr$tab), check.names = FALSE)))
  }
  # percorso
  pc <- leggi_tab("7_percorso")
  if (!is.null(pc)) corpo <- c(corpo, "<h2>Il percorso: cosa ha fatto ogni passo</h2>",
    "<p>Sul campione di costruzione. RESET misura i problemi di forma funzionale, BP l'eteroschedasticita' (piu' bassi = meglio).</p>",
    tab_html(setNames(pc[, intersect(c("passo", "y", "n", "p", "adjR2", "RESET", "BP"), names(pc))],
                      c("passo", "y", "n", "parametri", "R² corretto", "RESET", "BP")[seq_along(intersect(c("passo", "y", "n", "p", "adjR2", "RESET", "BP"), names(pc)))])))
  # formula e coefficienti
  corpo <- c(corpo, "<h2>Il modello finale</h2>", sprintf("<p><code>%s</code></p>", h(paste(deparse(formula(ver$fit_finale %||% st$fit4), width.cutoff = 500), collapse = " "))))
  if (st$lambda != 1) corpo <- c(corpo, sprintf("<p>La y e' trasformata (%s): i coefficienti sono su quella scala; gli <strong>effetti</strong> qui sotto sono riportati in unita' di %s.</p>", nome_trasf(st$lambda), h(y)))
  ef <- leggi_tab("9_effetti_finali") %||% leggi_tab("7_effetti_scala_originale")
  if (!is.null(ef)) {
    names(ef)[1:4] <- c("se la x cambia di...", "effetto medio su y", "IC 2,5%", "IC 97,5%")
    ef$significativo <- ifelse(ef[[3]] > 0 | ef[[4]] < 0, "si'", "no")
    corpo <- c(corpo, sprintf("<h3>Effetti in unita' di %s (a parita' delle altre x)</h3>", h(y)), tab_html(ef))
  }
  cf <- leggi_tab("9_coefficienti_finali")
  if (is.null(cf)) { w <- lmtest::coeftest(st$fit4, vcov = sandwich::vcovHC(st$fit4, type = "HC3"))
    cf <- data.frame(voce = rownames(w), b = w[, 1], SE_HC3 = w[, 2], p = w[, 4]) }
  if (!is.null(cf)) { names(cf)[1] <- "termine"; corpo <- c(corpo, sprintf("<h3>Coefficienti (stimati su %s righe, errori standard robusti HC3)</h3>", format(ver$n_finale %||% nobs(st$fit4), big.mark = ".", decimal.mark = ",")),
                                                       "<div class='scroll'>", tab_html(cf), "</div>") }
  corpo <- c(corpo, "<h2>Previsti contro osservati</h2><div class='due'>", img64(graf("7_osservati_vs_previsti")), img64(graf("9_calibrazione")), "</div>",
             "<p class='cap'>Calibrazione: media osservata contro media prevista per decili di previsione; sulla diagonale = previsioni non distorte.</p>",
             "<h3>Residui: iniziale contro finale</h3>", img64(graf("7_residui_confronto")))
  # perche' non prevede di piu'
  if (!is.null(tetto) || length(cfg$cosa_manca)) {
    corpo <- c(corpo, "<h2>Perche' non prevede di piu' (e cosa manca)</h2>")
    if (!is.null(tetto$rel)) corpo <- c(corpo, tiles(c(sprintf("&plusmn;%d%%", round(100 * tetto$rel[["errore_relativo_mediano"]])),
        sprintf("%d%%", round(100 * tetto$rel[["quota_entro_25pct"]])), sprintf("%d%%", round(100 * tetto$rel[["quota_entro_50pct"]])),
        sprintf("%d%%", round(100 * tetto$rel[["quota_oltre_doppio"]]))),
      c("errore relativo mediano: meta' dei casi e' piu' vicina di cosi' al valore vero", "casi previsti entro +-25%", "casi previsti entro +-50%",
        "casi sbagliati di oltre il doppio (o la meta')")))
    if (!is.null(tetto$gam)) {
      g <- data.frame(modello = c("modello finale (lineare)", "GAM flessibile, stesse x"), tetto$gam[, c("MAE", "RMSE", "R2_fuori_campione")], check.names = FALSE)
      dR2 <- tetto$gam[2, "R2_fuori_campione"] - tetto$gam[1, "R2_fuori_campione"]
      corpo <- c(corpo, "<h3>La forma o l'informazione?</h3><p>Un GAM con spline su tutte le quantitative (e le stesse interazioni del modello) e' molto piu' flessibile del modello lineare: se non fa meglio sui dati mai visti, il limite non e' la forma delle relazioni ma <strong>l'informazione che manca nelle x</strong>.</p>",
        tab_html(g), sprintf("<p>%s</p>", if (dR2 < 0.02) "Il GAM non fa meglio: per prevedere di piu' servono variabili nuove, non un modello piu' complicato." else
          sprintf("Il GAM guadagna %s di R²: c'e' ancora forma da catturare (vedi il grafico GAM e le interazioni).", fmt(dR2))))
    }
    if (!is.null(tetto$coda)) corpo <- c(corpo, sprintf("<h3>Un gruppo che le x non vedono</h3><p>Il %s%% dei casi ha un residuo oltre 3 deviazioni standard, tutti <strong>%s</strong> il previsto (con errori normali sarebbero lo 0,1%%). E' un sottogruppo che le variabili disponibili non distinguono: serve un'informazione nuova che lo identifichi, non un modello piu' complicato e non l'eliminazione di quei casi.</p>",
                                       fmt1(100 * as.numeric(tetto$coda[["quota"]]), 1), tetto$coda[["lato"]]))
    if (!is.null(tetto$escluse)) corpo <- c(corpo, "<h3>Informazione nelle colonne escluse</h3><p>Per ogni colonna esclusa con gruppi ripetuti (regista, attore, editore...), quanto la media dei residui degli <em>altri</em> casi dello stesso gruppo spiega il residuo di un caso. Valori sopra zero: il gruppo sa qualcosa che il modello non sa. Va usato con effetti casuali o medie ristrette calcolate sulla sola costruzione, mai con la media grezza (la colonna 'grezzo' mostra quanto si sbaglierebbe).</p>",
                                        tab_html(tetto$escluse))
    if (length(cfg$cosa_manca)) corpo <- c(corpo, "<h3>Cosa manca nei dati (lettura dell'analista)</h3><ul>",
                                           paste0("<li>", h(unlist(cfg$cosa_manca)), "</li>", collapse = ""), "</ul>")
  }
  # robustezza
  pr_rb <- c("4.2", "4.3", "4.3a", "5.2", "5.3", "6", "6.3", "9.2", "9.3", "9.3b", "9.4")
  if (any(reg$passo == "9.3b")) pr_rb <- setdiff(pr_rb, "9.3")      # il test H1/H2 e' solo informativo se si stima su tutti i dati
  rb <- reg[reg$passo %in% pr_rb, c("oggetto", "valore", "azione")]
  if (nrow(rb)) corpo <- c(corpo, "<h2>Quanto e' robusto</h2>", tab_html(rb))
  dec <- reg[reg$tipo == "PROPOSTA", c("fase", "oggetto", "valore", "azione")]
  if (nrow(dec)) corpo <- c(corpo, "<h2>Scelte da confermare</h2><p>Dove la regola del corso non basta: la routine propone, decidi tu (si cambiano nel config, sezione <code>decisioni</code>).</p>", tab_html(dec))
  ut <- reg[reg$tipo == "UTENTE", c("oggetto", "valore", "azione")]
  if (nrow(ut)) corpo <- c(corpo, "<h3>Scelte gia' fatte da te</h3>", tab_html(ut))
  # logistica
  if (!is.null(logit)) {
    lv <- leggi_tab("8_verifica_predittiva"); or <- leggi_tab("8_odds_ratio"); am <- leggi_tab("8_effetti_marginali")
    corpo <- c(corpo, "<h2>Parte binaria: regressione logistica</h2>",
      sprintf("<p>%s</p>", h(val_reg("L.1"))),
      sprintf("<p><code>%s</code></p>", h(paste(deparse(formula(logit$g), width.cutoff = 500), collapse = " "))),
      "<p>AUC = probabilita' che un evento riceva una probabilita' prevista piu' alta di un non-evento (0,5 = a caso, 1 = perfetto). Brier skill = miglioramento rispetto a prevedere sempre la stessa probabilita'.</p>")
    if (!is.null(lv)) corpo <- c(corpo, tab_html(lv))
    if (!is.null(am)) { names(am)[1] <- "x"; corpo <- c(corpo, "<h3>Effetti marginali medi (punti percentuali di probabilita')</h3>", tab_html(am)) }
    if (!is.null(or)) { names(or)[1] <- "termine"; corpo <- c(corpo, "<h3>Odds ratio</h3><div class='scroll'>", tab_html(or), "</div>") }
    corpo <- c(corpo, img64(graf("8_calibrazione_logit"), "60%"))
  }
  pagina(file.path(.RM$out, "report_2_finale.html"), paste("Modello finale:", y),
         sprintf("%s - guadagni sui dati mai visti, parametri finali, robustezza e scelte da confermare", basename(cfg$dati)), corpo)
}

# --------------------------------------------------------------------
# 2b. y binaria nativa: guadagni, modello, odds ratio, effetti marginali, robustezza
# --------------------------------------------------------------------
report_binaria <- function(st, lo, vl, cfr, cfg) {
  y <- cfg$y; reg <- .RM$reg
  sc <- st$consegna %||% (if (length(cfr)) tryCatch(scelta_consegna_bin(cfr[[1]], length(coef(lo$g_full)), length(coef(lo$g))), error = function(e) NULL))
  corpo <- verdetto_binaria(sc, y, vl)
  if (!is.null(sc) && sc$chi == "iniziale") {
    g0 <- sc$modello0 %||% lo$g_full; s0 <- summary(g0)$coefficients
    corpo <- c(corpo, "<h2>Modello consegnato: l'iniziale</h2><p>Logistica con tutte le variabili candidate, cosi' come sono (lo stesso modello del report 1), stimata su tutte le righe con i mancanti imputati: sui dati mai visti prevede meglio (o uguale, ed e' piu' semplice) del modello finale descritto piu' sotto. Coefficienti con errori standard enormi (centinaia) indicano separazione: quel coefficiente non e' interpretabile, anche se il modello prevede bene.</p>",
               sprintf("<p><code>%s</code></p>", h(paste(deparse(formula(g0), width.cutoff = 500), collapse = " "))),
               "<div class='scroll'>", tab_html(data.frame(termine = rownames(s0), b = s0[, 1], OR = ifelse(rownames(s0) == "(Intercept)", NA, exp(s0[, 1])), SE = s0[, 2], p = s0[, 4], check.names = FALSE)), "</div>",
               "<h2 style='color:var(--muted)'>Il modello finale esaminato (non consegnato)</h2>")
  }
  if (!is.null(vl)) {
    nm <- setdiff(rownames(vl), "costruzione (in-sample)")[1]; v <- vl[nm, ]
    corpo <- c(corpo, tiles(c(fmt(v[["AUC"]]), fmt(v[["Brier_skill"]]), sprintf("%d%%", as.integer(round(100 * v[["acc_0.5"]]))),
                              sprintf("%d%%", as.integer(round(v[["eventi_pct"]]))), sprintf("%d &rarr; %d", length(coef(lo$g_full)), length(coef(lo$g)))),
      c(sprintf("AUC su %s righe mai viste (0,5 = a caso, 1 = perfetto)", format(v[["n"]], big.mark = ".", decimal.mark = ",")),
        "Brier skill: miglioramento rispetto a prevedere sempre la prevalenza", "classificati correttamente (soglia 0,5)",
        sprintf("quota di eventi (%s = 1)", y), "parametri: tutte le x -> finale"), c(TRUE, FALSE, FALSE, FALSE, FALSE)))
  }
  if (length(cfr)) {
    c1 <- cfr[[1]]; lab <- rownames(c1)
    png_r("02_guadagni_binaria", 9, max(2.6, .45 * nrow(c1) + 1))
    barre_h(c1[, "AUC"], lab, "AUC su dati mai visti (piu' alto = meglio; 0,5 = a caso)", "AUC", col = COL$grigio, evid = lab == "finale")
    dev.off()
    corpo <- c(corpo, "<h2>Quanto guadagna il modello finale</h2>", img64(graf("02_guadagni_binaria")),
      tab_html(data.frame(modello = lab, c1[, c("AUC", "Brier_skill", "log_loss", "acc_0.5", "sens_0.5", "spec_0.5")], check.names = FALSE), evidenzia = lab == "finale"),
      "<p class='cap'>Sensibilita' = eventi riconosciuti; specificita' = non-eventi riconosciuti (soglia 0,5). Classificare bene e' tema della fase di machine learning: qui conta capire cosa cambia la probabilita'.</p>")
  }
  corpo <- c(corpo, "<h2>Il modello finale</h2>", sprintf("<p><code>%s</code></p>", h(paste(deparse(formula(lo$g), width.cutoff = 500), collapse = " "))))
  am <- leggi_tab("8_effetti_marginali")
  if (!is.null(am)) { names(am)[1] <- "se la x cambia di..."; corpo <- c(corpo, "<h3>Effetti marginali medi (punti percentuali di probabilita')</h3>", tab_html(am)) }
  or <- leggi_tab("8_odds_ratio")
  if (!is.null(or)) { names(or)[1] <- "termine"; corpo <- c(corpo, "<h3>Odds ratio (con IC al 95%)</h3><p>OR > 1: l'evento diventa piu' probabile; per le qualitative, rispetto al livello di riferimento.</p><div class='scroll'>", tab_html(or), "</div>") }
  corpo <- c(corpo, "<h2>Calibrazione</h2>", img64(graf("8_calibrazione_logit"), "60%"),
             "<p class='cap'>Probabilita' prevista contro quota osservata per decili: sulla diagonale le probabilita' sono affidabili.</p>")
  if (!is.null(vl)) corpo <- c(corpo, "<h3>Costruzione contro verifica</h3>", tab_html(data.frame(insieme = rownames(vl), vl, check.names = FALSE)))
  rb <- reg[reg$fase == "8_logistica" & reg$passo %in% c("L.6", "L.9", "L.10"), c("oggetto", "valore", "azione")]
  if (nrow(rb)) corpo <- c(corpo, "<h2>Quanto e' robusto</h2>", tab_html(rb))
  dec <- reg[reg$tipo == "PROPOSTA", c("fase", "oggetto", "valore", "azione")]
  if (nrow(dec)) corpo <- c(corpo, "<h2>Scelte da confermare</h2>", tab_html(dec))
  ut <- reg[reg$tipo == "UTENTE", c("oggetto", "valore", "azione")]
  if (nrow(ut)) corpo <- c(corpo, "<h3>Scelte gia' fatte da te</h3>", tab_html(ut))
  if (length(cfg$cosa_manca)) corpo <- c(corpo, "<h2>Cosa manca nei dati (lettura dell'analista)</h2><ul>", paste0("<li>", h(unlist(cfg$cosa_manca)), "</li>", collapse = ""), "</ul>")
  pagina(file.path(.RM$out, "report_2_finale.html"), paste("Modello finale:", y, "(binaria)"),
         sprintf("%s - regressione logistica: guadagni sui dati mai visti, effetti, robustezza", basename(cfg$dati)), corpo)
}
