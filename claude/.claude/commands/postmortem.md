---
description: Draft a blameless postmortem in ~/projects/postmortems from the current debugging session.
argument-hint: [short title hint]
allowed-tools: Read Write Glob AskUserQuestion
---

Write a blameless postmortem for the incident we just debugged in this session, using everything already in the conversation.

Optional argument: a short title hint. If omitted, derive the title from the incident itself.

Steps:

1. Check the session actually contains incident debugging — pasted logs, symptoms, hypotheses, a fix. If it does not (e.g. this is a fresh session), say so and stop. Do not interview the user from nothing.
2. Read all existing postmortems in `~/projects/postmortems/` (`*.md`, excluding `README.md`) to absorb voice and phrasing before writing anything. Where an individual document conflicts with the template below, follow the template.
3. Extract from the conversation: symptoms, affected hosts and services, every timestamped log line, hypotheses tried and eliminated, the fix applied, and the evidence blocks that support the conclusion.
4. Sanity-check the analysis before drafting. Is the stated root cause an actual cause or a symptom? Is the trigger separated from the latent condition that made it harmful? Were contributing factors left implicit? Anything questionable becomes a question in step 6 — never resolve it silently in the document.
5. Write the draft to `~/projects/postmortems/YYYY-MM-DD-<slug>.md`, where the date is the day the incident started and the slug is kebab-case from the title. If that file already exists, stop and ask instead of overwriting. Mark anything unknown inline as `**TBD**`.
6. Interview on the gaps with a single batched AskUserQuestion. Ask only about what the conversation genuinely cannot tell you — typically impact magnitude in cores/slots, when and how the incident was detected, whether any communications went out, and the analysis concerns from step 4. Do not ask about things already established in the session.
7. Patch the draft with the answers. Report the file path and briefly list what remains `**TBD**`. Do not print the whole document back. Do not commit.

Template — emit every section, always:

---

# YYYY-MM-DD: <Title>

**Status:** Draft<br>
**Author:** Luis Simas<br>
**Incident:** <Day YYYY-MM-DD ~HH:MM -> Day YYYY-MM-DD ~HH:MM (duration)><br>
**Affected:** <hosts and services; pools><br>
**Audience:** CMS Submission Infrastructure team

This is a blameless [postmortem](https://sre.google/sre-book/postmortem-culture/) of an incident concerning the Submission Infrastructure team. The goal is to support discussion, learning and improvement.

## TL;DR
## Impact
## Timeline (CERN time)
## Root causes
### Trigger
## Fix
## Communications
## What went well
## What went poorly
## Where we got lucky
## Action items
## Extra information

---

Section rules:

- **TL;DR** — one paragraph: what broke, why, how it was detected, how it was fixed, and that recovery followed.
- **Impact** — bullets with hard numbers: duration, cores before and after, percentage. Follow them with `<!-- TODO: graph of ... -->` placeholder comments where a Grafana screenshot belongs. Do not write image links for files that do not exist.
- **Timeline (CERN time)** — a `| Time | Event |` table. Every row must trace to a timestamp actually present in the conversation. Convert non-CERN-time sources (syslog, UTC from OpenStack) and state the source-timezone assumption rather than converting silently. Prefix genuinely approximate times with `~`. Human events that appear in no log (detection, fix applied, communications) are asked for in step 6, never inferred from conversation flow. The markers **Trigger**, **Detected**, **Fix applied** and **Resolved** are mandatory — if one cannot be placed from evidence, ask for it.
- **Root causes** — a lead-in sentence, then a numbered list with a **bold lead phrase** per cause when there is more than one. `### Trigger` nested underneath: a short paragraph naming the specific event and time that started the impact.
- **Communications** — bullets, or `- None` when nothing went out.
- **What went well / What went poorly / Where we got lucky** — always emitted and always populated. Open questions phrased as questions are welcome; the existing postmortems use them.
- **Action items** — header row only, left empty for the user to fill:
  ```
  | # | Action | Owner | Priority | Notes | Jira Ticket |
  |---|--------|-------|----------|-------|-------------|
  ```
- **Extra information** — one `###` heading per evidence block, a sentence of framing, then the output verbatim in a fenced block with the shell prompt line kept. Include only evidence that supports the root cause, trigger, or fix — typically 2 to 4 blocks. Drop dead-end investigation output. Trim to the relevant lines; never edit, reformat, or reconstruct log content.

Guidelines:

- Blameless throughout. Describe systems, configurations and decisions; never fault a person. "The off-boarding process didn't include a Puppet review", not "X forgot to re-enable Puppet".
- Never invent timestamps, log lines, numbers, links, hostnames, or root causes. Anything not in the conversation is either asked for or left `**TBD**`.
- Write the analysis the session reached. Do not author new causal claims of your own — a confidently wrong root cause is worse than a thin one.
- Match the voice of the existing postmortems: plain past tense, active voice, concrete and specific, naming the hosts and services involved. No preamble, no filler, no loaded words ("critical", "catastrophic", "major", "dominated").
- Batch every question into one AskUserQuestion. Do not ask section by section.
- Backtick hostnames, service names, config parameters, ports, and commands.
