# robust-model

[![regression-suite](https://github.com/delleroselorenzo-oss/robust-model/actions/workflows/suite.yml/badge.svg)](https://github.com/delleroselorenzo-oss/robust-model/actions/workflows/suite.yml)

**A semi-automatic pipeline, in R, that builds the best robust linear model (and, when it makes
sense, a logistic model) for any tabular dataset, proves how well it predicts on data it has never
seen, and explains every decision in three HTML reports.**

The mechanical work is automatic: collinearity, transformations, interactions, model selection,
outliers, heteroskedasticity, bootstrap, out-of-sample verification. The substantive choices stay
with a human, at three checkpoints. It works on its own from the command line, and it is designed to
be driven by an AI agent (Claude, Codex, Cursor, Copilot, Gemini CLI...) that talks to the user at the
checkpoints.

> Reports, code comments and the lesson log are in Italian for now (the project started as a Data
> Mining course assignment). The pipeline itself is language-independent; translations are welcome.

## Why

A linear model done properly takes an analyst many hours: check collinearity, try Box-Cox, find the
shape of each predictor, test interactions, deal with influential points, robust standard errors,
bootstrap, and then verify on held-out data. Most of it is mechanical, and most of the mistakes are
silent: a model that looks good on the data used to fit it and predicts worse than the naive one.

This pipeline does that work in minutes and is honest about the result:

- every number that compares models is measured on **data never used to build them**;
- the model delivered is **the one that predicts better on unseen data**, not the last one built: on a
  tie (within 2%) the simpler one wins, and if the initial model wins, the initial model is delivered;
- weak signal is reported as such, with what data is probably missing.

## How it works

```
 report 0  ── explore ──► checkpoint A: y, goal, leakage, metric, time, groups
 report 1  ── initial model (all x, no transformation), already measured on unseen data
           ── steps 0-7 on a build sample ─────────────────────────────────────────────
              missing data (mice) · collinearity (VIF, chi-norm, exact aliases)
              scale of y (Box-Cox, then chosen by cross-validation on the scale of y)
              shapes of x (optimal grouping, GAM → parametric forms, thresholds)
              interactions (ΔSBC screening, form chosen out of sample)
              selection (AIC/SBC hybrid) · influence (Cook / DFFITS, with safeguards)
              heteroskedasticity (BP, White, HC3 / cluster-robust) · bootstrap
           ── verification on unseen data (hold-out, future period, or CV of the whole procedure)
 report 2  ── "In breve": error on unseen cases, gain over no model and over the initial model,
              which model is delivered and why · effects in units of y · what is missing
           ── checkpoint B: confirm or change the proposals ── (logistic model if needed)
```

Every decision is logged in `registro.csv` as **AUTO** (clear rule), **PROPOSTA** (needs judgement:
a default is chosen and shown to the user) or **UTENTE** (the user's choice, written in the config).

## Results on the test datasets

The same 16 public datasets are re-run on every change (GitHub Actions). R² is measured on data never
used to build the model (fast mode of the suite).

| Dataset | n | y | Main difficulty | R² unseen: initial → final | Delivered |
|---|---:|---|---|---|---|
| Palmer penguins | 344 | body mass | small n, missing values | 0.86 → 0.86 | final (tie, simpler) |
| CEO salaries | 177 | salary | very skewed y | 0.13 → 0.15 | final |
| Student performance | 395 | final grade | weak signal, impossible zeros | 0.11 → 0.10 | final (tie, simpler) |
| Body fat | 71 | DEXA body fat | tiny n, twin predictors | 0.90 → 0.89 | initial |
| San Francisco rents | 63k | rent | time drift, 140-level factors | 0.64 → 0.74 | final |
| NYC flights + weather | 337k | arrival delay | weak signal, joined tables | 0.07 → 0.14 | final |
| Diamonds | 54k | price | non-linearity, extreme collinearity | 0.92 → 0.97 | final (error −49%) |
| Spotify songs | 28k | popularity | duplicated units, zero-inflated y | 0.12 → 0.12 | final (tie, simpler) |
| Board games | 10.5k | average rating | tag lists, ratings as averages (FGLS) | 0.38 → 0.41 | final |
| Bechdel movies | 1.8k | worldwide gross | numbers stored as text, post-release leakage | 0.38 → 0.39 | final |
| Titanic | 891 | survived (binary) | separation, missing ages | AUC 0.87 vs 0.86 | initial |
| Coffee ratings | 1.3k | cup points | narrow y, high-cardinality origin | 0.25 → 0.23 | final (tie, simpler) |
| California housing | 20.6k | house value | censoring at $500k, coordinates | 0.65 → 0.70 | final (+ logistic above the cap, AUC 0.95) |
| Bike sharing | 731 | registered rentals | forecasting the future, lagged leakage | 0.81 → 0.85 | final (next 5 months) |
| Medical insurance | 1.3k | charges | bimodal y, smoker × BMI threshold found automatically | 0.75 → 0.87 | final (error −44%) |
| Concrete strength | 1k | compressive strength | repeated mixes, total = sum of parts | 0.59 → 0.85 | final (error −42%) |

Where the initial model wins or ties, the pipeline says so and delivers it: the point is not that
every step always helps, but that you always know what helps on unseen data.

Example reports (open them in a browser): [`examples/`](examples/) — insurance, Titanic, concrete,
bike sharing, coffee.

## Quick start

Requirements: R ≥ 4.1 and
```r
install.packages(c("car", "MASS", "lmtest", "sandwich", "mice", "leaps", "mgcv",
                   "robustbase", "boot", "knitr", "jsonlite", "base64enc"))
```

```bash
git clone https://github.com/delleroselorenzo-oss/robust-model.git
cd robust-model
cp scripts/config_template.json my_config.json      # set "dati" (csv path), "y", "output"
Rscript scripts/rm_run.R my_config.json profilo     # report 0: exploration
Rscript scripts/rm_run.R my_config.json iniziale    # report 1: initial model
Rscript scripts/rm_run.R my_config.json tutto       # report 2: robust model (+ logistic if needed)
```

A minimal config:
```json
{
  "dati": "data/15_insurance.csv.gz",
  "y": "charges",
  "output": "output/insurance",
  "obiettivo": "interpretativo",
  "metrica": "media"
}
```
All the keys (time ordering, groups, weights, two-part models, decisions to force) are documented in
`scripts/config_template.json` and `SKILL.md`. The configs in `collaudi/` are worked examples.

## Using it with an AI agent

- **Claude**: the repository is a Claude skill. Copy it into your skills folder (or upload `SKILL.md`
  with the scripts) and ask for "/modello-robusto" or "the best robust model for this dataset".
  `SKILL.md` is the full procedure, with the questions to ask at each checkpoint.
- **Other agents** (Codex, Cursor, Copilot, Gemini CLI, ...): `AGENTS.md` contains the same workflow
  in agent-neutral form: commands, checkpoints, rules, how to change the code safely.
- **No agent**: run the scripts yourself; the reports tell you what to decide.

## Built with AI

This project was developed in collaboration with an AI agent (Claude, by Anthropic), as a deliberate
test of how far AI can automate expert analytical work. The division of labour is the one the
pipeline itself follows: the human sets the goal, judges the trade-offs and decides at the
checkpoints; the AI implements, tests on real datasets and records what it learns.

The process is documented: `references/lezioni_apprese.md` holds **119 numbered lessons**, each from a
real failure on a real dataset (a rule that removed the strongest interaction, a model that
extrapolated to RMSE in the millions, a report that compared in-sample with out-of-sample numbers...),
with the fix. `references/regole_e_soglie.md` lists every threshold with its source (course material,
literature or heuristic).

## Limitations and roadmap

- Linear and logistic models only (by design). Machine-learning models are a separate future module.
- One table at a time; joins and feature engineering go in `codice_preparazione` (R code in the config).
- Reports and comments in Italian; English translation to do.
- To do: ordinal logistic regression, splitting `rm_lib.R` by phase, recording package versions in
  the outputs, a "high-cardinality factor" option as true mixed models.

## Repository layout

```
SKILL.md, AGENTS.md, CLAUDE.md   instructions for AI agents (Claude / any agent)
scripts/      rm_run.R (entry point), rm_lib.R (pipeline), rm_report.R (HTML), rm_suite.R (tests)
collaudi/     configs of the 16 test datasets + riferimento.json (reference results)
data/         test datasets (see data/SOURCES.md for sources and licenses)
references/   lessons learned and thresholds
examples/     example HTML reports
docs/         test plan
```

## License

Code: MIT (see `LICENSE`). Datasets in `data/` keep their original licenses (`data/SOURCES.md`).
