---
description: Fetch completed CERN Todoist tasks for the last 7 days and draft a weekly report email for supervisors, categorized by Frontier and GlideinWMS Factory.
argument-hint: [since DD-MM-YYYY] [until DD-MM-YYYY]
allowed-tools: mcp__todoist__find-projects mcp__todoist__find-sections mcp__todoist__find-completed-tasks AskUserQuestion
---

Fetch my completed Todoist tasks for the CERN project and write a weekly report email to send to my supervisors.

Arguments (optional):
- `since DD-MM-YYYY`: start date (inclusive). If omitted, default to last Monday (the most recent Monday on or before today).
- `until DD-MM-YYYY`: end date (inclusive). If omitted, default to today.

Steps:
1. Determine the date range:
   - If `since` is provided, parse it; otherwise compute the most recent Monday on or before today.
   - If `until` is provided, parse it; otherwise use today.
   - Convert both to YYYY-MM-DD format for API calls.
2. Find the CERN project ID using find-projects.
3. Find the project sections using find-sections — use the section IDs to categorize tasks as Frontier or Factory.
4. Fetch completed tasks for the date range using find-completed-tasks.
5. Skip personal/admin tasks (e.g. "schedule day off", "update address", "update UBS").
6. Collect all tasks that have no section and whose category (Frontier vs Factory) is ambiguous from the title/description. Use AskUserQuestion to ask about all of them at once in a single prompt — list each task clearly and ask whether it belongs to Factory, Frontier, or should be excluded. Do not ask task by task; batch them all in one question.
7. Group related tasks into themes — do NOT list every task as its own bullet.
8. Write the email following this structure:

---

Subject: Weekly Report – DD/MM – DD/MM/YYYY

Hello folks,

This is my weekly report for the week DD/MM/YYYY - DD/MM/YYYY.

# Activities

## GlideinWMS Factory

[One short summary sentence of the week's focus — must NOT restate the bullets. Then bullet points grouping related work by theme.]

## Frontier

[One short summary sentence. Then bullet points grouping related work by theme.]

## Miscellaneous (only if there are cross-cutting items such as CVEs affecting both teams)

[Bullets for items that don't belong to either section above.]

Best regards,
Luís.

---

Guidelines:
- Summary sentence per section states the overall thrust only — never repeats what the bullets say.
- Use past tense, active voice.
- Bullets describe themes or outcomes, not individual task completions.
- When in doubt about a task's category or relevance, ask the user rather than guessing.
- Do not invent details not present in the task titles or descriptions.
