# Agent Workflow Guardrails

These rules are mandatory for AI agents working in this repo.

## Scope control
- Keep diffs tightly scoped to the user request.
- Do not modify unrelated files.
- Before committing, verify changed paths match intended work.
- For hard-to-reproduce bugs or perf issues, explicitly ask for a record/replay capture when available.

## Formatting policy
- Run `cargo fmt` before every commit.
- Use standard Rust formatting as-is; do not hand-format against `rustfmt`.
- If formatting-only changes are present, that is acceptable when they come from `cargo fmt`.

## Commit hygiene
- Prefer small, focused commits with clear purpose.
- If there are unrelated local changes, leave them untouched unless asked.

## Pre-commit checklist
- `cargo fmt` has been run successfully.
- `git status --short` shows intended files (plus any `cargo fmt` output).
- Tests run for the modified area (or explain why not run).
- Diff is reviewed before commit.
