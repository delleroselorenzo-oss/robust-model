# robust-model (in italiano)

Pipeline semi-automatica in R che costruisce il miglior **modello lineare robusto** (e, quando serve,
una **logistica**) su un dataset qualsiasi, misura quanto prevede su dati mai visti e spiega ogni
decisione in tre report HTML. Il lavoro meccanico è automatico; le scelte di merito restano
all'utente, in tre checkpoint.

- **Procedura completa** (anche per Claude): [`SKILL.md`](SKILL.md)
- **Istruzioni per qualsiasi agente AI**: [`AGENTS.md`](AGENTS.md) (in inglese)
- **Panoramica, risultati sui 16 dataset di prova, avvio rapido**: [`README.md`](README.md) (in inglese)
- **Le 119 lezioni apprese** sui dataset di prova: [`references/lezioni_apprese.md`](references/lezioni_apprese.md)

Avvio rapido:
```bash
cp scripts/config_template.json mio.json            # "dati", "y", "output"
Rscript scripts/rm_run.R mio.json profilo           # report 0: esplorazione
Rscript scripts/rm_run.R mio.json iniziale          # report 1: modello iniziale
Rscript scripts/rm_run.R mio.json tutto             # report 2: modello robusto (+ logistica se serve)
```

Nato come elaborato per il corso di Data Mining e sviluppato insieme a un agente AI (Claude).
Licenza MIT per il codice; i dataset in `data/` mantengono le loro licenze (`data/SOURCES.md`).
