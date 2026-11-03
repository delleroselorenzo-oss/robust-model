# Contributing

Thanks for helping. The project improves mostly in one way: **run it on a new dataset, find where it
goes wrong, fix it without breaking the other datasets.**

## Ways to contribute

- **A new test dataset.** Run the full workflow (see `AGENTS.md`), then add its config to `collaudi/`
  (`"dati"` relative to the repository root, data file in `data/` with its source in
  `data/SOURCES.md`). Prefer datasets that bring a situation not yet covered (see `docs/piano_collaudi.md`).
- **A bug or a better rule.** Open an issue with the dataset, the config and the row of
  `registro.csv` that looks wrong.
- **Translation.** Code comments, reports and lessons are in Italian; English versions are welcome,
  one file at a time.

## Before opening a pull request

1. Never edit a script while an R process that uses it is running.
2. Run the regression suite from the repository root:
   ```bash
   Rscript scripts/rm_suite.R collaudi /tmp/suite
   ```
   It prints, for every test case, `uguale` (same), `migliora` (better), `cambia forma`
   (different terms, same accuracy) or `PEGGIORA` (worse). The same suite runs on GitHub Actions.
3. Explain every `PEGGIORA` and every change of form in the pull request. If the change is accepted,
   update the reference with `--aggiorna` (optionally `--solo name1,name2`).
4. Add a numbered lesson to `references/lezioni_apprese.md`: what went wrong, on which dataset, what
   rule fixes it. Distinguish a **bug** (always fixed) from a **new rule** (added only if it helps
   beyond the dataset that revealed it).

## Style

- R, base + the packages listed in the README; no new dependency without a reason.
- Every automatic decision goes through `registra()` with its type (AUTO / PROPOSTA / UTENTE).
- Comments explain *why*, with the dataset that motivated the rule.
