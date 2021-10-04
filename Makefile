.POSIX:
.PHONY: all compile test clean
.SUFFIXES: .el .elc

EMACS = lemacs
BYTEC = gnu-apl-documentation.elc \
	gnu-apl-editor.elc \
	gnu-apl-finnapl.elc \
	gnu-apl-follow.elc \
	gnu-apl-input.elc \
	gnu-apl-interactive.elc \
	gnu-apl-mode.elc \
	gnu-apl-network.elc \
	gnu-apl-osx-workaround.elc \
	gnu-apl-plot.elc \
	gnu-apl-refdocs-bsd-license.elc \
	gnu-apl-spreadsheet.elc \
	gnu-apl-symbols.elc \
	gnu-apl-util.elc

all: compile

compile: $(BYTEC)

clean:
	rm -f $(BYTEC)

.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $^

