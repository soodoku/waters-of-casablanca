.PHONY: restore analysis figures tables manuscript paper format lint test check ci-docker clean

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	Rscript scripts/run_all.R

figures: analysis
	Rscript scripts/figures.R

tables: analysis
	Rscript scripts/tables.R

manuscript:
	cd ms && latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex

paper: figures tables
	$(MAKE) manuscript

format:
	Rscript -e 'styler::style_dir("R"); styler::style_dir("scripts"); styler::style_dir("tests")'

lint:
	Rscript -e 'l <- unlist(lapply(c("R", "scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -v "$(PWD):/project" -w /project rocker/verse:4.6.0 \
		bash -lc "Rscript -e 'install.packages(\"renv\", repos = \"https://cloud.r-project.org\")' && make restore check"

clean:
	cd ms && latexmk -C main.tex
