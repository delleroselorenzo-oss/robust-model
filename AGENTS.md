# AGENTS.md: instructions for AI coding agents

This file is for any AI agent (Codex, Cursor, Copilot, Gemini CLI, Claude Code, ...) asked to build a
model with this repository. Claude users get the same procedure, in more detail, in `SKILL.md`
(Italian). The R scripts also work without any agent: see `README.md`.

## What the user wants

The best **robust linear model** for a quantitative y (and, when it makes sense, a **logistic model**
for a binary y), with honest evidence on data the model has never seen. The agent does the mechanical
work; **the user makes the substantive choices**. Never skip a checkpoint by guessing silently: if the
user is not available, take the most prudent option and write it at the top of your reply.

## Workflow

Run everything from the repository root. One JSON config per dataset (start from
`scripts/config_template.json`; the keys are documented there and in `SKILL.md`).

1. **Explore**: `Rscript scripts/rm_run.R config.json profilo`
   Send the user `output/report_0_esplorazione.html`.
   **Checkpoint A**: ask the user (at most 4 questions, recommended option first):
   which y and research goal; which x are known only *after* y (leakage) and must be excluded;
   interpretation or prediction; guiding metric (`"tipico"` = MAE, `"media"` = RMSE);
   time ordering (`tempo`, `trend`), repeated units (`gruppo`), totals and parts, high-cardinality
   categories (`effetti_gruppo`), whether a binary event is of interest. Write the answers in the config.
2. **Initial model**: `Rscript scripts/rm_run.R config.json iniziale`
   Send `output/report_1_iniziale.html` with its "In breve" sentence: how well the initial model
   (all x, no transformation) already predicts unseen cases.
3. **Robust model (+ logistic if needed)**: `Rscript scripts/rm_run.R config.json tutto`
   Look at `output/report_2_finale.html` before sending it (render it or take a screenshot).
   **Checkpoint B**: present only the PROPOSTA rows of `output/registro.csv` that change the model,
   each with your recommendation; write the user's choices under `decisioni` and run again.
4. **Report to the user in plain words**, reusing the sentences of the "In breve" box: error on unseen
   cases in units of y, gain over no model and over the initial model, how much the x explain,
   **which model is delivered and why** (the one that predicts better out of sample; on a tie, the
   simpler one, which may be the initial model). Always send all three reports.

## Rules

- Effects are associations, never causal claims.
- Every removed observation or variable must have a row in `registro.csv`: never drop data by hand.
- Weak signal is a result: say so plainly and say what data is missing (`cosa_manca` in the config).
- Stay within linear and logistic models (robust regression, GAM, bootstrap are in scope;
  random forests, boosting and neural networks are not).

## Changing the code

- Read `references/lezioni_apprese.md` first: 119 numbered lessons, each from a real failure.
- **Never edit a script while an R process that uses it is running**: Rscript reads the file as it
  executes, and a half-edited file produces meaningless errors.
- After any change to `scripts/`, run the regression suite and compare with the reference:
  `Rscript scripts/rm_suite.R collaudi /tmp/suite` (about 17 minutes, 2 processes).
  Accept differences with `--aggiorna` only when they are improvements or justified changes.
- Record each new trap as a numbered lesson, and new thresholds in `references/regole_e_soglie.md`
  with their source ([C] course, [F] literature, [E] heuristic).
