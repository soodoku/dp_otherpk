RSCRIPT ?= Rscript --vanilla
DP_DATA ?= ../dp_data
export DP_DATA

.PHONY: baseline build analysis manuscript paper test lint check clean

baseline:
	cd "$(DP_DATA)" && $(RSCRIPT) scripts/build_baseline_positions.R

build: baseline
	$(RSCRIPT) scripts/build_classification.R

analysis: build
	$(RSCRIPT) scripts/analyze.R
	$(RSCRIPT) scripts/build_comparative.R
	$(RSCRIPT) scripts/analyze_comparative.R

manuscript:
	$(RSCRIPT) scripts/build_manuscript.R
	$(RSCRIPT) scripts/build_comparative_manuscript.R
	cd ms && latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex

paper: analysis
	$(MAKE) manuscript

test:
	$(RSCRIPT) -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

lint:
	$(RSCRIPT) -e 'x <- lintr::lint_dir(); print(x); stopifnot(length(x) == 0L)'
	black --check --line-length 79 scripts/inventory.py
	isort --check-only --profile black --line-length 79 scripts/inventory.py
	flake8 scripts/inventory.py

check: test lint paper

clean:
	cd ms && latexmk -C main.tex
