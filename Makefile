# BFO 2.0 classes only ontology Makefile
# Jie Zheng
#
# This Makefile is used to build artifacts for the BFO 2.0 classes only ontology.
#

### Configuration
#
# prologue:
# <http://clarkgrubb.com/makefile-style-guide#toc2>

MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

### Definitions
OBO     := http://purl.obolibrary.org/obo
BFO 	:= $(OBO)/BFO_
TODAY   := $(shell date +%Y-%m-%d)

### Directories
#
# This is a temporary place to put things.
build:
	mkdir -p $@


### ROBOT
#
# We use the official released version of ROBOT
build/robot.jar: | build
	curl -L -o $@ https://github.com/ontodev/robot/releases/download/v1.4.1/robot.jar

ROBOT := java -jar build/robot.jar

### Build
#
# Here we create a standalone OWL file appropriate for release.
# This involves annotating.

bfo_classes_only.owl: src/ontology/owl-group/bfo_classes_only_dev.owl | build/robot.jar
	$(ROBOT) reason \
	--input $< \
	--reasoner HermiT \
	annotate \
	--ontology-iri "$(OBO)/bfo.owl" \
	--version-iri "$(OBO)/bfo/$(TODAY)/bfo.owl" \
	--output $@

### 
#
# Full build
.PHONY: all
all: bfo_classes_only.owl

# Remove generated files
.PHONY: clean
clean:
	rm -rf build
