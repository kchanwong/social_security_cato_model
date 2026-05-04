---
paths:
  - "Slides/**/*.tex"
  - "Quarto/**/*.qmd"
---

# Beamer → Quarto Auto-Sync Rule (MANDATORY)

**Every edit to a Beamer `.tex` file MUST be immediately synced to the corresponding Quarto `.qmd` file — automatically, without the user asking.** This is non-negotiable.

## The Rule

When you modify a Beamer `.tex` file, you MUST also apply the equivalent change to the Quarto `.qmd` (if it exists) **in the same task**, before reporting completion. Do NOT wait to be asked. Do NOT just "flag the drift." Just do it.

## Lecture Mapping

<!-- Customize this table for your lectures -->
| Lecture | Beamer | Quarto |
|---------|--------|--------|
| 1 | `Slides/Lecture1_Topic.tex` | `Quarto/Lecture1_Topic.qmd` |
| 2 | `Slides/Lecture2_Topic.tex` | `Quarto/Lecture2_Topic.qmd` |
<!-- Add rows as you create lectures -->

## Workflow (Every Time)

1. Apply fix to Beamer `.tex`
2. **Immediately** apply equivalent fix to Quarto `.qmd`
3. Compile Beamer (3-pass xelatex)
4. Render Quarto (`./scripts/sync_to_docs.sh LectureN`)
5. Only then report task complete

## LaTeX → Quarto Translation Reference

| Beamer | Quarto Equivalent |
| ------ | ----------------- |
| `\muted{text}` | `[text]{style="color: #525252;"}` |
| `\key{text}` | `[**text**]{.emorygold}` |
| `\textcolor{positive}{text}` | `[text]{.positive}` |
| `\textcolor{negative}{text}` | `[text]{.negative}` |
| `\item text` | `- text` |
| `\begin{highlightbox}` | `::: {.highlightbox}` |
| `\begin{methodbox}` | `::: {.methodbox}` |
| `$formula$` | `$formula$` (same) |

## When NOT to Sync

- Quarto file doesn't exist yet
- Change is LaTeX-only infrastructure (preamble, theme files)
- Explicitly told to skip Quarto sync

## Precedence when the Quarto file has manual post-translation edits

This rule (auto-sync) and [`single-source-of-truth.md`](single-source-of-truth.md) (Beamer is authoritative) can conflict after a human has hand-edited the Quarto file. Resolution:

1. **Beamer remains authoritative.** Hand-edits to Quarto that add *content* (new slides, different equations) are a violation of SSOT and should be backported to Beamer first, then re-synced down.
2. **Presentation-only divergence is allowed.** HTML-specific callouts (e.g., `.smaller`, `{.scrollable}`, plotly embeds) can live only in Quarto. Auto-sync should not delete them when propagating Beamer edits — diff before overwriting.
3. **On ambiguity, regenerate the Quarto file from Beamer** (e.g. `/translate-to-quarto [file]` into a scratch path, then diff against the existing Quarto) so you can compare structurally. Merge manually, keeping HTML-only decorations.
4. **If the two files have drifted structurally** (slide count mismatch, reordered sections), treat as a bug and fix Beamer first, then regenerate Quarto from scratch via `/translate-to-quarto`.

## Enforcement

Before marking any Beamer editing task as complete, check:
> "Did I also update the Quarto file?"

If the answer is no and a Quarto file exists, **you are NOT done.**

## When to Update This Table

After creating a new Quarto translation, add it to the mapping table above.
