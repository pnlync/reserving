# CLAUDE.md

General insurance reserving study (portfolio project 02): a US commercial auto insurer at 2007-12-31 from CAS / NAIC Schedule P data, blind-locked, then back-tested on the 2008–2016 run-off. **SPEC.md is the build contract. Read it in full before any work, and follow §1 (how we work).** After M0, also read `CONTRACT.md`.

## Key rules (from SPEC, repeated because they are the ones most easily broken)
- One module at a time (M0 → M9). Before coding a module, explain it in Chinese (max 300 words) and wait for the owner's OK. Write the acceptance tests first. Stop after the teach-back and do not start the next module until the owner replies "continue".
- Hindsight firewall (§5): only `R/01_load.R` reads `data/raw/`, only `R/reveal.R` reads `data/holdout/`, and `reveal()` errors unless the git tag `selection-locked` exists. Never read, print or summarise lower-triangle (cy ≥ 2008) values before the tag, including in ad-hoc exploration.
- The tag `selection-locked` is the only irreversible step. Never create it yourself; the owner tags after passing Gate 3. After the tag, only pure code bugs may be fixed (publish both numbers in `reports/post_lock_changes.md`). Judgement choices never change.
- Never edit the golden values in SPEC §11 or the tests that check them. A failing golden test means the implementation is wrong.
- No insurer names in any output (NAIC codes only). Never write "IFRS 17 compliant", "SCR" (as a claim about our number), or "validated model".
- Every public number comes from `outputs/` (ultimately `outputs/cv_numbers.json`); no hand-typed numbers in the README, memo or CV.
- Commit once per module with the message `M<n>: <module name>`.
- If SPEC is unclear, contradicts itself or looks wrong: stop and ask. Don't guess.

## Owner context
- The owner is learning GI reserving through this project (IFoA CM2 background, working towards SP7-level ideas) and must be able to explain every line in interviews. Prefer readable, explicit R over clever code. The companion guide explains concepts in Chinese with a 4×4 toy triangle; use the toy triangle as the worked example when explaining.
- `private_notes/` holds the owner's planning guides (`claude_guide.pdf` = final "GI Reserving Project Guide", `chatgpt_guide.pdf` = ChatGPT's review of an earlier draft, already merged into the final guide and SPEC). It is gitignored: never commit it, quote it into public files or publish it.

## Environment
- R 4.5 via renv (set up in M0); tests with testthat. Excel check recalculated with LibreOffice headless (M3); reports and site with Quarto (M9).
- GitHub: git@github.com:pnlync/reserving.git (SPEC §10 calls the repo `gi-reserving`; the directory layout inside is as in §10).

## Decision authority
The owner has given the agent full discretion over design decisions (28 Sep 2026). Make the call, record it in SPEC.md (§14 change log) or the relevant report, and tell the owner what was decided and why. Still: never alter golden values in SPEC §11, and never create the `selection-locked` tag (the owner chose to tag after reviewing the M4 selection).
