PDF_NAME = Minimax_para_Blokus.pdf
TMP_DIR = ./tmp.latex
IMG_DIR = img

IMGS= $(wildcard $(IMG_DIR)/*.png)

PDFLATEX_FLAGS = -interaction=nonstopmode -file-line-error -output-directory=$(TMP_DIR)


.PHONY: all clean

all: $(PDF_NAME)

$(PDF_NAME) : $(TMP_DIR)/main.pdf | $(PDF_DIR)
	cp "${TMP_DIR}/$(notdir $<)" $@

$(TMP_DIR)/main.pdf : *.tex $(IMGS) | $(TMP_DIR)
	pdflatex $(PDFLATEX_FLAGS) main.tex | grep '.*:.*:.*' -A 4
	@test -e $@ # Force fail if pdf no exits

$(TMP_DIR) :
	@-mkdir -pv $(TMP_DIR)

$(PDF_DIR) :
	@-mkdir -pv $(PDF_DIR)

clean:
	@-rm -fv $(PDF_NAME) 2>/dev/null; true
	@-rm -frv $(TMP_DIR) 2>/dev/null; true

# vim:filetype=make
