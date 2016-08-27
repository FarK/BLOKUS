SRC_DIR=src
DOC_DIR=doc

.PHONY: all clean doc blokus

all: doc blokus

blokus:
	@cd $(SRC_DIR) && $(MAKE)

doc:
	@cd $(DOC_DIR) && $(MAKE)

clean:
	@-cd $(SRC_DIR) && $(MAKE) clean
	@-cd $(DOC_DIR) && $(MAKE) clean
