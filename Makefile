
.PHONY : all
all : install

check:
	R -q -e "devtools::check()"

docs:
	R -q -e "devtools::document()"

install: docs
	R -q -e "devtools::install()"

