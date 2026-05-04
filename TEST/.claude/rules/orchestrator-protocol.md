# Orchestrator Protocol: Contractor Mode

**This rule describes the contract that skills implement.** The 6-step loop is a **pattern**, not a runtime — the harness does not run it automatically after plan approval. Specific skills (listed below) implement the pattern; others invoke parts of it. Users bridge the gap by invoking the right skill.

## The Loop (pattern)

```
Task starts (plan approved or skill invoked)
  │
  Step 1: IMPLEMENT — Execute plan steps
  │
  Step 2: VERIFY — Compile, render, check outputs
  │         If verification fails → fix → re-verify
  │
  Step 3: REVIEW — Run review agents (by file type)
  │
  Step 4: FIX — Apply fixes (critical → major → minor)
  │
  Step 5: RE-VERIFY — Confirm fixes are clean
  │
  Step 6: SCORE — Apply quality-review rubric
  │
  └── Score >= threshold?
        YES → Present summary to user
        NO  → Loop back to Step 3 (max 5 rounds)
              After max rounds → present with remaining issues
```

## Where the pattern is actually implemented

| Skill | Steps covered | Notes |
|-------|---------------|-------|
| `/commit` | 2 (verifier agent), 6 (quality_score.py) | Halts on failure; user can override with explicit reason |
| `/qa-quarto` | 3–5 (critic-fixer loop, up to 5 rounds) | Parity-focused, Beamer vs Quarto |
| `/review-paper --adversarial` | 3–5 (critic-fixer loop, up to 5 rounds) | Manuscript review |
| `/slide-excellence` | 3 (multi-agent parallel review) | Does not auto-fix |
| `/create-lecture` | 1, 2, 3 (Pre-Flight → draft → review) | Fresh-fork fallback in Phase 0 |
| `/data-analysis` | 1, 2 (Pre-Flight → analysis → verify) | Pre-Flight is required |
| `/review-paper --peer` | 3 (editor + 2 referees), with 6b cross-artifact | Full pipeline |

## What is NOT automatic

- **No post-plan-approval trigger.** Exiting plan mode does not launch this loop. The user (or a skill invocation) starts it.
- **No repo-wide orchestrator service.** Skills implement the loop within their own scope; they do not chain into each other without an explicit invocation.
- **No git-hook enforcement of Step 6.** `quality_score.py` runs inside `/commit`; a direct `git commit` bypasses the review.

To get the full loop on a non-trivial task: start in plan mode, write the plan, approve it, then invoke the skill that implements the pattern for your artifact type.

## Limits (when the pattern is running)

- **Main loop:** max 5 review-fix rounds
- **Critic-fixer sub-loop:** max 5 rounds
- **Verification retries:** max 2 attempts
- Never loop indefinitely

## "Just Do It" Mode

When user says "just do it" / "handle it" (within an already-invoked skill):
- Skip the final approval pause for the currently invoked skill
- Still run the full verify-review-fix loop (within that skill)
- Still present the summary
- **Do NOT treat this phrase alone as commit authorization.** Commits still require an explicit `/commit` invocation or an unambiguous user request to commit — see [`.claude/skills/commit/SKILL.md`](../skills/commit/SKILL.md) for the allowed trigger phrases.

## Cross-references

- `.claude/rules/plan-first-workflow.md` — when to enter plan mode before invoking a skill.
- `.claude/rules/quality-gates.md` — threshold definitions.
- `.claude/rules/cross-artifact-review.md` — paper ↔ code dependency-graph pattern.
