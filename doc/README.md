# Journaling TUI for recording SCUBA dives

## Journal Formatting

Saves each dive as a Markdown (`.md`) file.
The Markdown format utilizes the `compact_definition_lists`.


## Functionality

The TUI app solicits the user to fill out a *form* to record pertinent information regarding the dive.
This information is stored in a "flat" data-structure called a `SCUBA.Form`.
Upon (partial) completion of the `SCUBA.Form`, the "flat" data-structure is converted to a well-typed, hierarchical data-structure called a `SCUBA.Dive`.
A `SCUBA.Dive` can have each of its sub-components partitioned into a `SCUBA.Entry.Section`.
One or more `SCUBA.Entry.Section` can be compined into a `SCUBA.Entry`.
Once finalized, the `SCUBA.Entry` can be transformed to a `Pandoc` document.
The `Pandoc` document can be serialized to the desired document format.


### Data-flow Diagram
```

  ┌─────────────────────┐
  │ Input I/O           │
  └──────────┬──────────┘
             │
             ▼
  ┌─────────────────────┐
  │ SCUBA.Form          │
  └──────────┬──────────┘
             │
             ▼
  ┌─────────────────────┐
  │ SCUBA.Entry.Section │
  └──────────┬──────────┘
             │
             ▼
  ┌─────────────────────┐
  │ SCUBA.Entry         │
  └──────────┬──────────┘
             │
             ▼
  ┌─────────────────────┐
  │ Pandoc              │
  └──────────┬──────────┘
             │
             ▼
  ┌─────────────────────┐
  │ File I/O            │
  └─────────────────────┘

```


## Example Formatting

```bash
pandoc \
  --from=markdown+compact_definition_lists \
  --to=html \
  -s format.md \
  --output=format.html
```
