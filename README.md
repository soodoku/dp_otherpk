# The Awful Truth: Learning Facts That Favor the Other Side

[Manuscript](ms/main.pdf) · [LaTeX source](ms/main.tex) · [References](ms/references.bib)

A comparative analysis of congenial and uncongenial factual accuracy across
Deliberative Polls, with separate policy-position and party-alignment measures.
The paper distinguishes gains on facts favoring the other side, their difference
from gains on facts favoring one's own side, and the knowledge gap remaining
afterward. The paper contains the data description, research design, coding
protocol, sensitivity analyses, and limitations. Its appendix
follows the references. The [historical archive](archive/README.md) preserves
the original writing, code, and literature, with a review of outstanding ideas.

## Reproduce

With the sibling [`dp_data`](https://github.com/soodoku/dp_data) checkout and its
canonical outputs available, including the baseline export added in
[`a193c68`](https://github.com/soodoku/dp_data/commit/a193c68):

```sh
make check
```

This runs tests and linting, rebuilds the baseline export, classifications and
analysis, generates the tables, numerical macros and figures, and compiles
`ms/main.pdf`. `make paper` rebuilds without linting and tests; `make manuscript`
regenerates and compiles from existing analysis outputs. Set `DP_DATA` to use a
different upstream checkout. Requirements are R, the packages in `DESCRIPTION`,
Python with `black`, `isort`, and `flake8`, and TeX with XeLaTeX, BibTeX, and
`latexmk`. Python tools can be installed with `uv tool install` or `pip`.

The tested R package versions are in `renv.lock`. To use a separate library:

```sh
Rscript --vanilla -e 'renv::restore(library = ".library", prompt = FALSE)'
R_LIBS_USER="$PWD/.library" make check
```

Install `renv` first if needed. Commands disable startup profiles; the explicit
library setting is required. To recreate the source inventory:

```sh
uv run --with pyarrow==23.0.1 scripts/inventory.py
```

## Files

- `R/` and `scripts/`: classification, statistical analysis, and manuscript build.
- `coding/`: original item packet, independent coding records, and exposure notes.
- `metadata/`: source inventory and reference-validation evidence.
- `archive/`: unchanged legacy files, checksums, and substantive comparison.
- `output/`: generated respondent-item records, paired data, estimates and manifests.
- `tabs/`, `figs/`, `ms/`: generated exhibits and manuscript sources.

Unclassifiable respondent-item rows remain in the data. The full inventory contains 260 questions across 32 studies. Adjudicated
coding covers 16 studies; 14 enter the primary policy comparison. Coding is
model-assisted and provisional. The paper reports descriptive paired changes,
source and response-rule sensitivities, and a separate US party comparison.

<!-- adjacent:start -->

## 🔗 Adjacent Repositories

- [soodoku/dp_data](https://github.com/soodoku/dp_data) — Deliberative Poll data
- [soodoku/partisan_campaigns](https://github.com/soodoku/partisan_campaigns) — Coming to Dislike Your Opponents
- [soodoku/dp_distortions](https://github.com/soodoku/dp_distortions) — Replication Data and Scripts for Deliberative Distortions
- [soodoku/dp_learning](https://github.com/soodoku/dp_learning) — Deliberation and Learning: Evidence From Deliberative Polls \(Luskin, Sood, Fishkin\)
- [soodoku/poor_browser](https://github.com/soodoku/poor_browser) — Do people on older browsers abandon surveys at higher rates?

_Powered by [Adjacent](https://github.com/gojiplus/adjacent)_

<!-- adjacent:end -->
