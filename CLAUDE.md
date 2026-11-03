# CLAUDE.md

This repository is a Claude skill: the procedure is in `SKILL.md` (Italian). Follow it step by step,
asking the user at the checkpoints with AskUserQuestion. Agent-neutral instructions are in `AGENTS.md`.

When changing `scripts/`: never edit a script while an R process that uses it is running, then run the
regression suite (`Rscript scripts/rm_suite.R collaudi /tmp/suite`) and record any new trap as a numbered
lesson in `references/lezioni_apprese.md`.
