---
name: create-issue
trigger: create GitHub issues
description: Creates GitHub issues for the package repository. Use when asked to create, file, or open a GitHub issue, or when planning new features or functions that need to be tracked.
compatibility: Requires the `gh` CLI and an authenticated GitHub session.
---

# Create a GitHub issue

Create issues with `gh issue create`, conveying the issue's category with a **label**.

If `gh` is not authenticated, stop and ask the user to authenticate before continuing.

> **Why labels rather than GitHub issue types?** Issue types are an organization-only feature — they are configured under *Organization → Settings → Planning → Issue types*, and there is no equivalent for repositories owned by a personal account. `pbulsink/f1predicter` is owned by the user `pbulsink` (verified 2026-09-17: `repositoryOwner.__typename` is `"User"`, and `/orgs/pbulsink/issue-types` returns `404`), so `issueTypes` will always be `null` for this repo and the GraphQL `createIssue` mutation's non-nullable `issueTypeId` cannot be supplied. Labels are the supported route here. If the repo is ever transferred to an organization, this skill should be revisited.

## Category labels

Map the issue's category onto one of the repository's labels. Check what currently exists with `gh label list --limit 40`; as of 2026-09-17 the relevant ones are:

| Category | Label | Conventional prefix |
|---|---|---|
| Bug | `bug` | `fix:` |
| Feature | `enhancement` | `feat:` |
| Documentation | `documentation` | `docs:` |
| Task | `enhancement`, or omit the label | `chore:` |

## Issue title

Titles use conventional commit prefixes:

- `feat: my_function()` — new exported function or feature
- `fix: short description` — bug fix
- `docs: short description` — documentation
- `chore: short description` — maintenance or task

## Issue body structure

Which sections to include depends on the issue's category:

| Section | Feature | Bug | Documentation | Task |
|---|---|---|---|---|
| `## Summary` | ✓ | ✓ | ✓ | ✓ |
| `## Details` | optional | optional | optional | optional |
| `## Proposed signature` | ✓ | — | — | — |
| `## Behavior` | ✓ | ✓ | — | — |
| `## References` | optional | optional | optional | optional |

### `## Summary` (all types)

A single user story sentence (no other content in this section):

```markdown
> As a [role], in order to [goal], I would like to [feature].
```

Example:

```markdown
## Summary

> As a package developer, in order to set up agent skills quickly, I would like to generate a skill template from a single function call.
```

### `## Details` (optional, all types)

For information that's important to capture but doesn't fit naturally into any other section, including implementation details such as packages to add to `Imports` in `DESCRIPTION` or files to add to `inst`. Use sparingly — if the content belongs in `## Behavior`, `## Proposed signature`, or `## References`, put it there instead.

### `## Proposed signature` (Feature only)

The proposed R function signature, arguments table, and return value description:

````markdown
## Proposed signature

```r
function_name(arg1, arg2)
```

**Arguments**

- `arg1` (`TYPE`) — Description.
- `arg2` (`TYPE`) — Description.

**Returns** a `TYPE` with description.
````

### `## Behavior` (Feature and Bug)

- **Feature**: bullet points describing expected behavior, edge cases, and any internal helpers to implement as part of this issue.
- **Bug**: describe the current (broken) behavior, the expected behavior, and steps to reproduce if known.

### `## References` (optional, all types)

Only include when there are specific reference implementations, external URLs, or related code to link to. Omit it entirely when there are none.

## Creating the issue

Write the body to a temp file first, then pass it with `--body-file`. Note that the `write` tool may refuse paths outside the workspace — if so, write the temp file with a shell heredoc:

```bash
cat > /tmp/issue_body.md <<'ISSUEEOF'
## Summary

> As a package developer, in order to ..., I would like to ...
ISSUEEOF
```

Then create the issue:

```bash
gh issue create \
  --title "fix: short description" \
  --label "bug" \
  --body-file /tmp/issue_body.md
```

`gh issue create` prints the new issue's URL on success.

To file several issues in one pass, write each body to its own temp file and loop, using `set -e` so a failure partway through stops the run rather than silently skipping issues.

Cross-references between issues (`#26`) can only be written once the referenced issue exists. When filing a batch with dependencies, create the referenced issues first, then substitute the real numbers into the remaining bodies before creating those.
