# Agent Workflow Guardrails

These rules are mandatory for AI agents working in this repo.

## Scope control
- Keep diffs tightly scoped to the user request.
- Do not modify unrelated files.
- Before committing, verify changed paths match intended work.

## Formatting policy
- Do not run repo-wide `cargo fmt` unless explicitly requested.
- Do not make formatting-only commits.
- If formatting is needed, format only files you intentionally changed.
- If a formatter touched unrelated files, revert those files immediately.

## Commit hygiene
- Prefer small, focused commits with clear purpose.
- Never mix behavior changes with broad style churn.
- If there are unrelated local changes, leave them untouched unless asked.

## Pre-commit checklist
- `git status --short` shows only intended files.
- Tests run for the modified area (or explain why not run).
- Diff contains behavioral intent, not style-only noise.
