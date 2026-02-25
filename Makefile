# Makefile for iaw R package

PKGNAME := iaw
VERSION := $(shell sed -n 's/^Version: *//p' DESCRIPTION)

# All library .R files (excluding Rprofile.R)
LIB_SOURCES := $(filter-out Rprofile.R, $(wildcard *.R))

.PHONY: lib man build check install clean

## Populate inst/lib/ with copies of library .R files
lib: $(LIB_SOURCES)
	@mkdir -p inst/lib
	@cp -f $(LIB_SOURCES) inst/lib/
	@echo "Copied $(words $(LIB_SOURCES)) files to inst/lib/"

## Generate man pages from roxygen-style comments
man: $(LIB_SOURCES) tools/generate-man.R
	@Rscript --no-init-file tools/generate-man.R

## Build the package tarball
build: lib man
	R CMD build .

## Run R CMD check
check: build
	R CMD check $(PKGNAME)_$(VERSION).tar.gz

## Install the package
install: lib man
	R CMD INSTALL .

## Clean build artifacts
clean:
	rm -rf inst/lib
	rm -rf $(PKGNAME).Rcheck
	rm -f $(PKGNAME)_*.tar.gz
