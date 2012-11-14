all: schemas toc

schemas:
	emacs --batch \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(load-file \"ifc-mode-generator.el\")" \
	--eval "(ifc-mode-download-syntax-files \"schemas\")" \
	--eval "(ifc-mode-generate-syntax-from-schema \"schemas/ifc2x2_add1.exp\" \"generated/ifc2x2_add1.el\")" \
	--eval "(ifc-mode-generate-syntax-from-schema \"schemas/ifc2x3_tc1.exp\" \"generated/ifc2x3_tc1.el\")"

toc:
	emacs --batch \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(load-file \"ifc-mode-generator.el\")" \
	--eval "(ifc-mode-download-toc-files \"html\")" \
	--eval "(ifc-mode-generate-tocs \"generated/ifc2x3_tc1.toc.el\")"
