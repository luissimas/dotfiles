---
name: teach
description: Teach the user a new skill or concept, within this workspace.
disable-model-invocation: true
argument-hint: "What would you like to learn about?"
---

The user has asked you to teach them something. This is a stateful request - they intend to learn the topic over multiple sessions.

## Teaching Workspace

Treat the current directory as a teaching workspace. The state of their learning is captured in this directory in several files:

- `MISSION.md`: A document capturing the _reason_ the user is interested in the topic. This should be used to ground all teaching. Use the format in [MISSION-FORMAT.md](./MISSION-FORMAT.md).
- `./reference/*.html`: A directory of reference materials. These are the compressed learnings from the lessons - cheat sheets, reference algorithms, syntax, yoga poses, glossaries. They are the raw units of learning. They should be beautiful documents which print out well, and are designed for quick reference.
- `RESOURCES.md`: A list of resources which can be explored to ground your teaching in contextual knowledge, or to acquire knowledge and wisdom. Use the format in [RESOURCES-FORMAT.md](./RESOURCES-FORMAT.md).
- `./learning-records/*.md`: A directory of learning records, which capture what the user has learned. These are loosely equivalent to architectural decision records in software development - they capture non-obvious lessons and key insights that may need to be revised later, or drive future sessions. These should be used to calculate the zone of proximal development. They are titled `0001-<dash-case-name>.md`, where the number increments each time. Use the format in [LEARNING-RECORD-FORMAT.md](./LEARNING-RECORD-FORMAT.md).
- `./lessons/*.html`: A directory of lessons. A **lesson** is a single, self-contained HTML output that teaches one tightly-scoped thing tied to the mission. This is the primary unit of teaching in this workspace.
- `./assets/*`: Reusable **components** shared across lessons. See [Assets](#assets).
- `index.html`: The **dashboard** - the front door to the workspace. See [Index](#index).
- `progress.json`: The user's answers, written by the lessons so you can read them. See [Reading The User's Answers](#reading-the-users-answers).
- `NOTES.md`: A scratchpad for you to jot down user preferences, or working notes.

## Philosophy

To learn at a deep level, the user needs three things:

- **Knowledge**, captured from high-quality, high-trust resources
- **Skills**, acquired through highly-relevant interactive lessons devised by you, based on the knowledge
- **Wisdom**, which comes from interacting with other learners and practitioners

Before the `RESOURCES.md` is well-populated, your focus should be to find high-quality resources which will help the user acquire knowledge. Never trust your parametric knowledge.

Some topics may require more skills than knowledge. Learning more about theoretical physics might be more knowledge-based. For yoga, more skills-based.

### Fluency vs Storage Strength

You should be careful to split between two types of learning:

- **Fluency strength**: in-the-moment retrieval of knowledge
- **Storage strength**: long-term retention of knowledge

Fluency can give the user an illusory sense of mastery, but storage strength is the real goal. Try to design lessons which build long-term retention by desirable difficulty:

- Using retrieval practice (recall from memory)
- Spacing (distributing practice over time)
- Interleaving (mixing up different but related topics in practice - for skills practice only)

## Lessons

A lesson is the main thing you produce: the unit in which knowledge and skills reach the user. Each lesson is one self-contained HTML file, saved to `./lessons/` and titled `0001-<dash-case-name>.html` where the number increments each time.

A lesson should be **beautiful**, with clean, readable typography and layout, since the user will return to these later to review. Think Tufte.

The lesson should be short, and completable very quickly. Learners' working memory is very small, and we need to stay within it. But each lesson should give the user a single tangible win that they can build on. It should be directly tied to the mission, and should be in the user's zone of proximal development.

If possible, open the lesson file for the user by running a CLI command.

Each lesson should link via HTML anchors to other lessons and reference documents.

Every lesson must also carry **navigation controls**: previous lesson, the index, next lesson. Put
them in a fixed place - directly under the masthead and again above the footer - so the user never
has to hunt or reach for the file system. A lesson the user cannot leave without going back to a
terminal is a dead end.

Drive navigation from a **single lesson manifest** in `./assets/` (title, filename, blurb, estimated
minutes, and whether it is written yet). The index page and the prev/next buttons both read it, so
the order lives in exactly one place. Include lessons you have planned but not yet written - the user
gets to see the road ahead, and the "next" button knows not to link to a file that does not exist.

Each lesson should recommend a primary source for the user to read or watch. This should be the most high-quality, high-trust resource you found on the topic.

Each lesson should contain a reminder to ask followup questions to the agent. The agent is their teacher, and can assist with anything that's unclear.

## Index

The workspace accumulates lessons, references, records, and plans. Without a front door it becomes a
directory listing, and the user has to remember what they were doing. `index.html` is that front door
- the page they open every morning, in one click, that answers *what do I do today*.

It should pull together, on one screen:

- **The next action.** The single most important element. One lesson, named, with a button. Never
  make the user infer where they left off.
- **The mission**, in a sentence or two, so every session is re-grounded in why.
- **Where they stand.** Assessment results, progress through the plan, anything measured.
- **What is due.** If you use spaced repetition, how many items are waiting.
- **All lessons**, from the manifest, showing which are done and which are planned.
- **Reference documents and resources**, linked - these are the pages they return to.

Build it from the same manifest and progress data the lessons use, so it is never stale. It is a
dashboard, not a document: if the user has to read it top to bottom to find the next action, it has
failed.

## Reading The User's Answers

**You must be able to read how the user actually performed.** Interactive lessons that store results
only in the browser are a dead end - `localStorage` is not readable from outside the browser, so the
user ends up copy-pasting scores back to you, or you fly blind and teach to a guess.

Wire every graded interaction to write to a file in the workspace, `progress.json`:

1. Ship a small local server in the workspace (`serve.py` or equivalent) that serves the files and
   accepts a `POST` of the current results, merging them into `progress.json`. Tell the user to open
   lessons through it.
2. Have the lesson JS post after every graded answer, debounced.
3. Fall back gracefully when opened as `file://`: keep working from `localStorage`, and offer a
   one-click export. Show a small badge saying which mode is active, so the user knows whether their
   work is reaching you.

Then **read `progress.json` at the start of every session** instead of asking how it went. Which
questions were missed, and which were answered but slowly, is the raw material for the next lesson
and for the learning records. Asking the user to summarise their own performance both wastes their
time and loses the detail you most need.

## Assets

Lessons are built from reusable **components**, stored in `./assets/`: stylesheets, quiz widgets, simulators, diagram helpers, and anything else a second lesson could reuse.

Reuse is the default, not the exception. Before authoring a lesson, read `./assets/` and build from the components already there. When a lesson needs something new and reusable, write it as a component in `./assets/` and link to it; never inline code a future lesson would duplicate.

A shared stylesheet is the first component every workspace earns: every lesson links it, so the lessons look like one consistent course rather than a pile of one-offs. As the workspace grows, so should the component library.

Four components pay for themselves in almost every workspace, and are worth building early:

- **Stylesheet** - one visual identity, and print rules so references print well.
- **Lesson manifest** - the ordered list of lessons, feeding both the index and the prev/next nav.
- **Progress module** - persists graded answers to `progress.json` so you can read them.
- **Assessment widget** - whatever form of retrieval practice the topic needs, written once. If the
  topic rewards spaced repetition, this is where the scheduler lives, backed by a growing item bank
  that each lesson appends to. Never renumber existing item ids: the review history is keyed to them.

## The Mission

Every lesson should be tied into the mission - the reason that the user is interested in learning about the topic.

If the user is unclear about the mission, or the `MISSION.md` is not populated, your first job should be to question the user on why they want to learn this.

Failing to understand the mission will mean knowledge acquisition is not grounded in real-world goals. Lessons will feel too abstract. You will have no way of judging what the user should do next.

Missions may change as the user develops more skills and knowledge. This is normal - make sure to update the `MISSION.md` and add a learning record to capture the change. Confirm with the user before changing the mission.

## Zone Of Proximal Development

Each lesson, the user should always feel as if they are being challenged 'just enough'.

The user may specify an exact thing they want to learn. If they don't, figure out their zone of proximal development by:

- Asking the user questions about the subject to gauge how much they already know and clarify the scope
- Searching notes about the subject on their zettelkasten (at `~/projects/zettelkasten/`)
- Reading their `learning-records`
- Figuring out the right thing to teach them based on their mission

Teach the most relevant thing that fits in their zone of proximal development.

## Knowledge

Lessons should be designed around a skill the user is going to learn. The knowledge in the lesson should be only what's required to acquire that skill. You teach the knowledge first, then get the user to practice the skills via an interactive feedback loop.

Knowledge should first be gathered from trusted resources. Use `RESOURCES.md` to keep track of them. Lessons should be littered with citations - links to external resources to back up any claim made. This increases the trustworthiness of the lesson.

For acquiring knowledge, difficulty is the enemy. It eats working memory you need for understanding.

## Skills

If knowledge is all about acquisition, skills are about durability and flexibility. Make the knowledge stick.

For skill acquisition, difficulty is the tool. Effortful retrieval is what builds storage strength. Skills should be taught through interactive lessons. There are several tools at your disposal:

- Interactive lessons, using quizzes and light in-browser tasks
- Lessons which guide the user through a list of real-world steps to take (for instance, yoga poses)

Each of these should be based on a **feedback loop**, where the user receives feedback on their performance. This feedback loop should be as tight as possible, giving feedback immediately - and ideally automatically.

For quizzes, each answer should be exactly the same number of words (and characters, if possible). Don't give the user any clues about the answer through formatting.

## Acquiring Wisdom

Wisdom comes from true real-world interaction - testing your skills outside the learning environment.

When the user asks a question that appears to require wisdom, your default posture should be to attempt to answer - but to ultimately delegate to a **community**.

A community is a place (online or offline) where the user can test their skills in the real world. This might be a forum, a subreddit, a real-world class (budget permitting) or a local interest group.

You should attempt to find high-reputation communities the user can join. If the user expresses a preference that they don't want to join a community, respect it.

## Reference Documents

While creating lessons, you should also create reference documents. Lessons can reference these documents - they are useful for tracking raw units of knowledge useful across lessons.

Lessons will rarely be revisited later - reference documents will be. They should be the compressed essence of the lesson, in a format designed for quick reference.

Some learning topics lend themselves to reference:

- Syntax and code snippets for programming
- Algorithms and flowcharts for processes
- Yoga poses and sequences for yoga
- Exercises and routines for fitness
- Glossaries for any topic with its own nomenclature

Glossaries, in particular, are an essential reference. Once one is created, it should be adhered to in every lesson.

## `NOTES.md`

The user will sometimes express preferences of how they want to be taught, or things you should keep in mind. This is the place to record those preferences, so you can refer back to them when designing lessons or working with the user.
