# canvas-mindmap -- a mind map on an Emacs 32 canvas, built on canvas-diagram.

EMACS   ?= emacs
DIAGRAM ?= ../canvas-diagram
KEYS    ?= ../canvas-keys
CONSULT ?= $(HOME)/.emacs.d/elpaca/builds/consult

.PHONY: test acceptance module clean

test: module
	$(EMACS) -Q --batch -L . -L $(DIAGRAM) -L $(KEYS) -L tests \
	  -l tests/canvas-mindmap-tests.el \
	  --eval '(ert-run-tests-batch-and-exit)'

# The real commands in a real window, on a private display.  The plain
# tests run with no consult on the load path.
ACCEPTANCE = timeout 120 xvfb-run -a -s "-screen 0 1400x900x24" $(EMACS) -Q \
	  --eval '(setq load-prefer-newer t)' -L . -L $(DIAGRAM) -L $(KEYS) -L tests

acceptance: module
	status=0; \
	$(ACCEPTANCE) -l tests/canvas-mindmap-acceptance.el \
	  --eval '(canvas-mindmap-acceptance-run "acceptance-plain-")' || status=1; \
	$(ACCEPTANCE) -L $(CONSULT) -l tests/canvas-mindmap-acceptance.el \
	  --eval '(canvas-mindmap-acceptance-run "acceptance-consult-")' || status=1; \
	exit $$status

# The cairo and pango module lives with canvas-diagram.
module:
	$(MAKE) -C $(DIAGRAM)

clean:
	rm -f *.elc tests/*.elc
