# Historical otherpk archive

`dp_otherpk` is the active project. On October 10, 2026, the ten files in the
former sibling `../otherpk` were preserved unchanged in
[`otherpk-legacy.tar.gz`](otherpk-legacy.tar.gz). Their original file dates span
2009–2011. The sibling had no Git repository; the initial `dp_otherpk` commit
(`1c63704`) explicitly left historical writing there. This documents why the two
directories coexisted, but does not establish when or how the older files were
copied onto this machine.

[`otherpk-manifest.json`](otherpk-manifest.json) records every original path,
byte count, and SHA-256 hash, plus the archive hash. Every archived file was
checked against its local original before the sibling directory was removed.
The user also reports Google Drive archives; those copies were not inspected
and were not needed for this preservation.

## Substantive comparison

The current manuscript retains the central question: equally knowledgeable
people may know different facts, and deliberation may increase knowledge of
facts that favor an initially opposed position. It also retains the distinction
between learning and agreement, compares settings, and discusses limitations of
the original Danish case. The questions about who learns, attitude change,
and issue salience remain possible future work in `dp_otherpk`; recording them
does not commit the project to answering them now. Misinformation and response
transitions belong in the sibling [`dp_misinformation`](../../dp_misinformation/README.md)
project. The comparison below distinguishes these responsibilities:

| Original idea and source | Status in the current project |
| --- | --- |
| Who learns uncongenial facts: attitude extremity, attitudes inconsistent with values or interests, and socioeconomic inconsistency (`APSA 11.doc`, `Awful Truth APSA 11.doc`, `the_other_side.doc`) | Not tested. Preserve as hypotheses about individual differences; the comparative learning estimates do not answer them. |
| Whether uncongenial learning predicts attitude change, and for which people and attitudes (all three drafts; `diagnostic.txt`) | Discussed conceptually, but no attitude-change model is estimated. The diagnostic note's implication that congenial learning means no attitude change is a hypothesis, not an established result. |
| Whether misinformation becomes an explicit “don't know” (`the_other_side.doc`) | Covered in `dp_misinformation`, whose manuscript and transition analysis distinguish wrong-to-right, wrong-to-wrong, and wrong-to-DK outcomes. Its older source release combines explicit don't-know and no-answer responses, an accepted data limitation. This is not an outstanding analysis for `dp_otherpk`. |
| Whether issue salience and issue type condition learning (`the_other_side.doc`, closing notes) | Cross-study variation is described, but salience and issue-type moderation are not estimated. |
| Distinguish recruitment, arrival, and exit knowledge; include party-position knowledge (`denmark.R`, `res/tab1.know.csv`) | The old table reports three waves, seven referendum items, thirteen party-placement items, and three aggregate scores. The current primary comparison uses recruitment to exit; the full historical three-wave, party-placement analysis is not reproduced by that comparison. |
| Learning from diverse sources whose errors differ (`lit.review/learning.from.others.pdf`, Andrew Healy, 2009) | The source is preserved. Its information-aggregation mechanism is not directly tested in the current paper. |

The drafts also proposed assigning an item's political direction from initial
left–right knowledge gaps. The current manuscript explicitly rejects that
procedure because it confounds substantive direction with initial accuracy and
room to improve. Preserve the proposal as intellectual history, not as a
missing analysis to restore. The early description of the Danish battery as
half pro-Euro and half anti-Euro is likewise not an established coding fact:
the current appendix records disagreements over particular items.

The archived Gaines, Kuklinski, Quirk, Peyton, and Verkuilen manuscript,
`lit.review/Same Facts KUKLINSKI.pdf`, addresses different interpretations of
shared facts. The current manuscript already cites the published paper and
retains the distinction between factual learning and agreement.

## File review

- `APSA 11.doc` and `Awful Truth APSA 11.doc` are nearly identical short proposals;
  both originals are retained.
- `the_other_side.doc` contains the earlier proposal and additional research notes.
- `denmark.R` and `res/tab1.know.csv` preserve the historical analysis and table.
  The script depends on an external `data/CDD/Denmark/den.rdata` file and helper
  functions `kros` and `stars`. It is not part of the current build. Its first
  party-placement row uses `t1pk1` between `t0ppk1` and `t2ppk1`, an apparent
  variable mismatch. Its displayed means use complete cases across all three
  waves, while its paired tests can use different pair-specific samples. Treat
  the table as a historical output, not as validated current evidence.
- `diagnostic.txt` contains the short attitude-change hypothesis noted above.
- `otherpk.fin.tabs.docx` has no document text, tables, or embedded media in its
  OOXML contents. It is retained despite being empty.
- `lit.review/` contains the two literature PDFs discussed above.
- `.project` is the original Eclipse/StatET project configuration.

## Inspect or recover

From the repository root, list the original paths with:

```sh
tar -tzf archive/otherpk-legacy.tar.gz
```

Extract into a separate destination when needed:

```sh
mkdir -p /tmp/otherpk-recovery
tar -xzf archive/otherpk-legacy.tar.gz -C /tmp/otherpk-recovery
```

The archive includes the original `otherpk/` directory and relative paths.
Historical code is stored in the compressed archive so it remains unchanged
and outside the active project's linting and execution targets.
