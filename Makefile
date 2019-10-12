
.PHONY: document build register_native_routines clean
.SILENT: clean

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

clean:
	$(RM) -r *.Rcheck/ ;\
	cd ..; \
	$(RM) -r $(PKGNAME)_$(PKGVERS).tar.gz

register_native_routines: 
	R -q -e "pkgbuild::compile_dll(force = TRUE, register_routines = TRUE)"

check: install
	R CMD check .

install: build
	R CMD INSTALL .

build: 
	cd ..; \
	R CMD build $(PKGNAME)

document:
	R -q -e "devtools::document()"

