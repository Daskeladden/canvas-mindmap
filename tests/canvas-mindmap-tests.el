;;; canvas-mindmap-tests.el --- tests -*- lexical-binding: t -*-
(require 'ert)
(require 'cl-lib)
(require 'canvas-mindmap)
(require 'lisp-mnt)
(require 'package)

;; The canvas, the boxes, the keys and the mouse are canvas-diagram's,
;; tested there.  These tests cover the tree: reading it, laying it
;; out, folding it and walking it.

;;;; Helpers

(defun canvas-mindmap-test--canvas (w h)
  "A fresh W x H canvas spec."
  (list 'image :type 'canvas :id (make-symbol "test-canvas")
        :data-width w :data-height h))

(defmacro canvas-mindmap-test--with-context (var w h &rest body)
  "Run BODY with VAR bound to a context on a fresh W x H canvas."
  (declare (indent 3))
  `(let ((,var (canvas-cairo-context (canvas-mindmap-test--canvas ,w ,h))))
     (unwind-protect (progn ,@body)
       (canvas-cairo-destroy ,var))))

(defun canvas-mindmap-test--measure (label)
  "Ten pixels per character, twenty tall: a fake text measure."
  (cons (* 10 (length label)) 20))

(defmacro canvas-mindmap-test--rendering (&rest body)
  "Run BODY with fixed font and colours, so pixels are predictable."
  `(let ((canvas-diagram-font "Sans 12px")
         (canvas-diagram-margin 10)
         (canvas-mindmap-branch-colors '("red" "green"))
         (canvas-diagram-kinds '(("todo" . "red")))
         (canvas-diagram-colors '(:background "white" :node "blue"
                                  :text "black" :edge "gray"))
         (canvas-mindmap-depth nil)
         (canvas-mindmap-direction 'right)
         (canvas-mindmap-edge-style 'curve)
         (canvas-diagram-shape 'rounded)
         (canvas-diagram-spacing 'normal)
         (canvas-diagram-family nil)
         (canvas-diagram-palette "derived")
         (canvas-diagram-show-kinds t)
         (canvas-diagram-show-legend t)
         (canvas-diagram-show-icons nil)
         (canvas-diagram-paper nil))
     ,@body))

(defun canvas-mindmap-test--laid-out (spec ctx)
  "A mind map diagram built from SPEC and laid out on CTX."
  (let ((diagram (canvas-mindmap--diagram)))
    (setf (canvas-diagram-spec diagram) spec
          (canvas-diagram-model diagram) (canvas-mindmap--build diagram spec)
          (canvas-diagram-nodes diagram) (canvas-mindmap--lay-out diagram ctx))
    diagram))

(defmacro canvas-mindmap-test--in-map-buffer-at-depth (depth spec size &rest body)
  "Run BODY in a map buffer showing SPEC on a canvas of SIZE, the map
opened with DEPTH as `canvas-mindmap-depth'."
  (declare (indent 3))
  `(canvas-mindmap-test--rendering
    (let ((canvas-mindmap-depth ,depth))
      (with-temp-buffer
        (canvas-mindmap-mode)
        (canvas-diagram-adopt (canvas-mindmap--diagram) ,spec)
        (plist-put (cdr canvas-diagram--canvas) :data-width (car ,size))
        (plist-put (cdr canvas-diagram--canvas) :data-height (cdr ,size))
        (unwind-protect (progn ,@body)
          (canvas-diagram--release))))))

(defmacro canvas-mindmap-test--in-map-buffer (spec size &rest body)
  "Run BODY in a map buffer showing SPEC on a canvas of SIZE, all of it open."
  (declare (indent 2))
  `(canvas-mindmap-test--in-map-buffer-at-depth nil ,spec ,size ,@body))

(defun canvas-mindmap-test--labelled (label)
  "The node called LABEL in this buffer's map, shown or folded away."
  (let (found)
    (canvas-mindmap--walk-all (canvas-diagram-current-model)
                              (lambda (n) (when (equal (canvas-mindmap-node-label n) label)
                                            (setq found n))))
    found))

(defun canvas-mindmap-test--selected ()
  "The label of the selected node."
  (canvas-mindmap-node-label (canvas-diagram-selected)))

(defun canvas-mindmap-test--shown-labels ()
  "The labels of the nodes this buffer's map shows, in reading order."
  (mapcar #'canvas-mindmap-node-label (canvas-diagram-nodes-shown)))

(defun canvas-mindmap-test--inside (node)
  "A canvas pixel just inside NODE's top-left corner, at a ten pixel margin."
  (cons (+ 10 (floor (canvas-mindmap-node-x node)) 3)
        (+ 10 (floor (canvas-mindmap-node-y node)) 3)))

(defun canvas-mindmap-test--png-p (file)
  "Whether FILE starts with the PNG signature."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 8)
    (equal (buffer-string) "\x89PNG\r\n\x1a\n")))

;;;; The tree

(ert-deftest canvas-mindmap-spec-options-become-kind-and-note ()
  ;; GIVEN a spec whose nodes carry :note and :kind options before their children
  ;; WHEN it is read
  ;; THEN each node has its option AND the children are still found after them
  (let* ((root (canvas-mindmap--tree '("Root" :note "why" ("A" :kind "todo" ("A1")))))
         (a (car (canvas-mindmap-node-children root))))
    (should (equal (canvas-mindmap-node-note root) "why"))
    (should (null (canvas-mindmap-node-kind root)))
    (should (equal (canvas-mindmap-node-kind a) "todo"))
    (should (equal (canvas-mindmap-node-label (car (canvas-mindmap-node-children a))) "A1"))
    ;; AND a mind map node is a diagram node too
    (should (canvas-diagram-node-p a))))

(ert-deftest canvas-mindmap-unknown-spec-option-is-an-error ()
  ;; GIVEN a spec with an option nobody defined
  ;; WHEN it is read
  ;; THEN that is an error, not a silently dropped child
  (should-error (canvas-mindmap--tree '("Root" :colour "red" ("A")))))

(ert-deftest canvas-mindmap-spec-options-become-status-and-done ()
  ;; GIVEN a spec whose nodes carry :status, one of them :done too
  ;; WHEN it is read, then a spec whose status is no string
  ;; THEN each node has its status, only the done one is done, AND the
  ;;      status that is no string is an error
  (let* ((root (canvas-mindmap--tree '("Root" :status "TODO" ("A" :status "DONE" :done t) ("B"))))
         (a (car (canvas-mindmap-node-children root))))
    (should (equal (canvas-mindmap-node-status root) "TODO"))
    (should-not (canvas-mindmap-node-done root))
    (should (equal (canvas-mindmap-node-status a) "DONE"))
    (should (canvas-mindmap-node-done a))
    (should-not (canvas-mindmap-node-status (cadr (canvas-mindmap-node-children root)))))
  (should-error (canvas-mindmap--tree '("Root" :status todo))))

(ert-deftest canvas-mindmap-branches-are-numbered-from-the-root ()
  ;; GIVEN a tree with two top-level branches, one of them deeper
  ;; WHEN the tree is built
  ;; THEN the root has no branch, each child of the root its own number,
  ;;      AND a grandchild carries its parent's
  (let* ((root (canvas-mindmap--tree-of '("R" ("A" ("A1")) ("B"))))
         (a (car (canvas-mindmap-node-children root)))
         (b (cadr (canvas-mindmap-node-children root))))
    (should (null (canvas-mindmap-node-branch root)))
    (should (= (canvas-mindmap-node-branch a) 0))
    (should (= (canvas-mindmap-node-branch (car (canvas-mindmap-node-children a))) 0))
    (should (= (canvas-mindmap-node-branch b) 1))))

(ert-deftest canvas-mindmap-path-runs-from-the-root-to-a-node ()
  ;; GIVEN a tree with a grandchild
  ;; WHEN the path to it is asked for
  ;; THEN it lists the labels from the root down
  ;;      AND a node from elsewhere has no path
  (let* ((root (canvas-mindmap--tree-of '("R" ("A" ("A1")) ("B"))))
         (a1 (car (canvas-mindmap-node-children (car (canvas-mindmap-node-children root))))))
    (should (equal (canvas-mindmap--path root a1) '("R" "A" "A1")))
    (should (equal (canvas-mindmap--path root root) '("R")))
    (should-not (canvas-mindmap--path root (canvas-mindmap--tree '("X"))))))

(ert-deftest canvas-mindmap-key-is-the-label-path-without-repeated-labels ()
  ;; GIVEN a tree whose siblings all have labels of their own
  ;; WHEN the keys of a grandchild and of the root are asked for
  ;; THEN each key is the plain label path, AND a node from elsewhere is an error
  (let* ((root (canvas-mindmap--tree-of '("R" ("A" ("A1")) ("B"))))
         (a1 (car (canvas-mindmap-node-children (car (canvas-mindmap-node-children root))))))
    (should (equal (canvas-mindmap--key root a1) '("R" "A" "A1")))
    (should (equal (canvas-mindmap--key root root) '("R")))
    (should-error (canvas-mindmap--key root (canvas-mindmap--tree '("X"))))))

(ert-deftest canvas-mindmap-key-counts-a-label-that-repeats-among-siblings ()
  ;; GIVEN a branch whose children are labelled "x", "y", "x" and "x"
  ;; WHEN the keys of the children are asked for, and nodes are found by them
  ;; THEN the first "x" and "y" keep their labels, the later "x" carry their
  ;;      place among the "x", each key finds its own node, AND a key whose
  ;;      node is gone finds the last node on the way
  (let* ((root (canvas-mindmap--tree-of '("R" ("A" ("x") ("y") ("x" ("x1")) ("x")))))
         (a (car (canvas-mindmap-node-children root)))
         (children (canvas-mindmap-node-children a)))
    (should (equal (mapcar (lambda (child) (car (last (canvas-mindmap--key root child)))) children)
                   '("x" "y" ("x" . 2) ("x" . 3))))
    (dolist (child children)
      (should (eq (canvas-mindmap--node-by-key root (canvas-mindmap--key root child)) child)))
    (should (eq (canvas-mindmap--node-by-key root '("R" "A" ("x" . 4))) a))))

(ert-deftest canvas-mindmap-org-headings-become-a-tree ()
  ;; GIVEN an org buffer with one top-level heading, nested children and
  ;;       body text under the last one
  ;; WHEN its headings are read
  ;; THEN the top-level heading is the root, nesting follows the stars,
  ;;      AND the body text is that heading's note
  (with-temp-buffer
    (insert "* Root\n** A\n*** A1\n*** A2\n** B\nsome body text\n")
    (should (equal (canvas-mindmap--headings-tree)
                   '("Root" ("A" ("A1") ("A2")) ("B" :note "some body text"))))))

(ert-deftest canvas-mindmap-org-keywords-tags-and-bodies-become-statuses-kinds-and-notes ()
  ;; GIVEN headings with a todo keyword and a tag, a done keyword, a tag
  ;;       alone, and body text over two lines
  ;; WHEN they are read
  ;; THEN the keyword is the status, done when org counts it done, the tag
  ;;      the kind beside it, AND the body, trimmed, is the note
  (with-temp-buffer
    (insert "* TODO Ship it :risk:\nsome body\nmore\n\n** DONE Sub\n** Plain :idea:\n")
    (should (equal (canvas-mindmap--headings-tree)
                   '("Ship it" :kind "risk" :status "TODO" :note "some body\nmore"
                     ("Sub" :status "DONE" :done t) ("Plain" :kind "idea"))))))

(defvar org-todo-keywords)

(ert-deftest canvas-mindmap-todo-keywords-come-from-the-buffer-else-org ()
  ;; GIVEN a buffer with #+TODO and #+SEQ_TODO lines, fast-access keys on
  ;;       some keywords; then org's own sequences without a bar; then none
  ;; WHEN its todo keywords are asked for, each time
  ;; THEN those after a bar are done, the keys left off, AND with no bar
  ;;      the last keyword is the done one, TODO and DONE when nothing says
  (with-temp-buffer
    (insert "#+TODO: TODO NEXT(n) | DONE(d!) COMPLETE\n#+seq_todo: WAIT HOLD | GAVEUP\n* NEXT a\n* COMPLETE b\n")
    (should (equal (canvas-mindmap--todo-keywords)
                   '(("TODO") ("NEXT") ("DONE" . t) ("COMPLETE" . t) ("WAIT") ("HOLD") ("GAVEUP" . t))))
    (should (equal (canvas-mindmap--headings-tree "r")
                   '("r" ("a" :status "NEXT") ("b" :status "COMPLETE" :done t)))))
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "A(a)" "B" "C") (type "X" "|" "Y"))))
      (should (equal (canvas-mindmap--todo-keywords) '(("A") ("B") ("C" . t) ("X") ("Y" . t)))))
    (let ((org-todo-keywords nil))
      (should (equal (canvas-mindmap--todo-keywords) '(("TODO") ("DONE" . t)))))))

(ert-deftest canvas-mindmap-todo-keywords-of-an-org-buffer-are-org-s ()
  ;; GIVEN an org-mode buffer whose #+TODO line org has read
  ;; WHEN its keywords and its headings are read
  ;; THEN they are org's, split as org splits them
  (with-temp-buffer
    (insert "#+TODO: OPEN | SHUT\n* OPEN a\n* SHUT b\n")
    (org-mode)
    (should (equal (canvas-mindmap--todo-keywords) '(("OPEN") ("SHUT" . t))))
    (should (equal (canvas-mindmap--headings-tree "r")
                   '("r" ("a" :status "OPEN") ("b" :status "SHUT" :done t))))))

(ert-deftest canvas-mindmap-plain-lists-become-nodes-under-their-heading ()
  ;; GIVEN a heading with an intro line, a nested plain list and a sub-heading
  ;; WHEN the headings are read
  ;; THEN the intro is the note, the items nest by indentation under the
  ;;      heading, and the sub-heading follows them
  (with-temp-buffer
    (insert "* H\nintro line\n- a\n  - a1\n    - a11\n  - a2\n- b\n** Sub\n")
    (should (equal (canvas-mindmap--headings-tree)
                   '("H" :note "intro line" ("a" ("a1" ("a11")) ("a2")) ("b") ("Sub"))))))

(ert-deftest canvas-mindmap-list-items-read-checkboxes-links-and-numbers ()
  ;; GIVEN numbered items with checkboxes and an org link, and a keyword line
  ;; WHEN the headings are read
  ;; THEN a ticked box is done, an empty one todo, the link shows its
  ;;      description, AND the keyword line is neither note nor node
  (with-temp-buffer
    (insert "* H\n#+ATTR_REVEAL: :frag (appear)\n1. [X] done thing\n2. [ ] open [[file:x.org][thing]]\n3) bare [[https://e.org]]\n")
    (should (equal (canvas-mindmap--headings-tree)
                   '("H" ("done thing" :kind "done") ("open thing" :kind "todo")
                     ("bare https://e.org"))))))

(ert-deftest canvas-mindmap-heading-links-are-cleaned-too ()
  ;; GIVEN a heading whose title is a link with a description
  ;; WHEN the headings are read
  ;; THEN the label is the description alone
  (with-temp-buffer
    (insert "* About [[file:fpga.org][FPGAs]]  today\n")
    (should (equal (canvas-mindmap--headings-tree) '("About FPGAs today")))))

(ert-deftest canvas-mindmap-markdown-headings-and-lists-become-a-tree ()
  ;; GIVEN a markdown buffer with # headings, a nested list with a ticked box
  ;;       and a link, and a fenced code block holding a fake heading
  ;; WHEN it is read as markdown
  ;; THEN the headings nest by their #s, the list by its indentation, the
  ;;      box is done, the link shows its text, AND the fence is skipped
  (with-temp-buffer
    (insert "# Root\n\nsome prose\n\n## A\n- x\n  - [x] y [see](http://e)\n```\n# not a heading\n- not an item\n```\n## B ##\n")
    (setq canvas-mindmap-dialect 'markdown)
    (should (equal (canvas-mindmap--headings-tree)
                   '("Root" :note "some prose" ("A" ("x" ("y see" :kind "done"))) ("B"))))))

(ert-deftest canvas-mindmap-org-code-blocks-are-skipped ()
  ;; GIVEN an org heading whose body has a source block with star lines in it
  ;; WHEN it is read
  ;; THEN nothing in the block becomes a heading, an item or the note
  (with-temp-buffer
    (insert "* Root\n#+begin_src markdown\n* fake\n- fake item\n#+end_src\n- real item\n")
    (should (equal (canvas-mindmap--headings-tree) '("Root" ("real item"))))))

(ert-deftest canvas-mindmap-dialect-comes-from-the-file-name ()
  ;; GIVEN a markdown file and an org file
  ;; WHEN each is read by name
  ;; THEN the markdown one nests by #s and the org one by stars
  (let ((md (make-temp-file "canvas-mindmap-test" nil ".md" "# Top\n## Under\n"))
        (org (make-temp-file "canvas-mindmap-test" nil ".org" "* Top\n** Under\n")))
    (unwind-protect
        (progn
          (should (equal (canvas-mindmap--file-tree md) '("Top" ("Under"))))
          (should (equal (canvas-mindmap--file-tree org) '("Top" ("Under")))))
      (delete-file md)
      (delete-file org))))

(ert-deftest canvas-mindmap-code-buffers-map-their-imenu-index ()
  ;; GIVEN an Emacs Lisp buffer with a function and a variable
  ;; WHEN its outline is read, with positions
  ;; THEN the root is the given name, the function hangs off it with its
  ;;      position, AND the variable sits under a category whose kind it takes
  (with-temp-buffer
    (insert "(defun foo ())\n\n(defvar bar 1)\n")
    (emacs-lisp-mode)
    (let* ((canvas-mindmap--with-positions t)
           (spec (canvas-mindmap--headings-tree "code.el"))
           (foo (assoc "foo" (cdr spec)))
           (vars (assoc "Variables" (cdr spec))))
      (should (eq (canvas-mindmap--dialect) 'imenu))
      (should (equal (car spec) "code.el"))
      (should (equal (plist-get (cdr foo) :pos) 1))
      (should (equal (plist-get (cdr vars) :kind) "variable"))
      (should (equal (assoc "bar" (cdr (canvas-mindmap--options (cdr vars))))
                     '("bar" :kind "variable" :pos 17))))))

(ert-deftest canvas-mindmap-imenu-entries-with-a-kind-property-take-it ()
  ;; GIVEN an imenu index whose names carry imenu's kind property, one of
  ;;       them a category with a region, and one plain category
  ;; WHEN it is turned into specs, with positions
  ;; THEN the property's kind serves, in this package's words, a category
  ;;      node takes its region's start, AND a plain category names its kind
  (let* ((module (propertize "widgets" 'imenu-kind "Module" 'imenu-region '(5 . 90)))
         (index (list (cons module (list (cons (propertize "draw" 'imenu-kind "Function") 10)
                                         (cons (propertize "count" 'imenu-kind "Variable") 20)))
                      (cons "Classes" (list (cons "Widget" 30)))))
         (canvas-mindmap--with-positions t))
    (should (equal (canvas-mindmap--imenu-specs index nil)
                   '(("widgets" :kind "module" :pos 5 ("draw" :kind "function" :pos 10) ("count" :kind "variable" :pos 20))
                     ("Classes" :kind "class" ("Widget" :kind "class" :pos 30)))))))

(defun canvas-mindmap-test--code-buffer (mode)
  "Make the current buffer one of MODE, a prog mode, without loading it."
  (put mode 'derived-mode-parent 'prog-mode)
  (setq major-mode mode))

(ert-deftest canvas-mindmap-imenu-items-of-its-own-are-left-out ()
  ;; GIVEN an index holding, around one real entry, the items imenu keeps
  ;;       for itself: the nil it marks an empty index with, and *Rescan*
  ;; WHEN it is turned into specs
  ;; THEN the real entry alone becomes one
  (should (equal (canvas-mindmap--imenu-specs (list '("*Rescan*" . -99) nil (cons "draw" 10)) "function")
                 '(("draw" :kind "function")))))

(ert-deftest canvas-mindmap-an-imenu-entry-named-by-no-string-is-an-error ()
  ;; GIVEN an index entry whose name is no string
  ;; WHEN it is turned into specs
  ;; THEN the error names the entry, in place of one from deep within
  (should (string-match-p
           "odd imenu entry"
           (cadr (should-error (canvas-mindmap--imenu-specs (list (cons 'draw 10)) nil))))))

(ert-deftest canvas-mindmap-an-empty-imenu-index-reads-nothing ()
  ;; GIVEN a prog-mode buffer whose imenu finds nothing, which imenu
  ;;       hands back as an index holding one nil entry
  ;; WHEN the buffer's outline is read from it
  ;; THEN nothing is read, in place of an error over that entry
  (with-temp-buffer
    (canvas-mindmap-test--code-buffer 'no-symbols-mode)
    (setq-local imenu-create-index-function #'ignore)
    (should (equal (let ((imenu-auto-rescan t)) (imenu--make-index-alist t)) '(nil)))
    (should-not (canvas-mindmap--imenu-tree "empty.cpp"))))

(ert-deftest canvas-mindmap-sources-are-kept-by-priority-and-replaced-by-name ()
  ;; GIVEN the built-in sources
  ;; WHEN a package defines one, then defines it again with another priority,
  ;;      and one without a way to read is attempted
  ;; THEN the sources stay best first, the name is held once with the
  ;;      later definition, AND the faulty one is an error
  (let ((canvas-mindmap-sources canvas-mindmap-sources))
    (canvas-mindmap-define-source 'test-lang :modes '(prog-mode) :read #'ignore)
    (should (eq (canvas-mindmap-source-name (car canvas-mindmap-sources)) 'test-lang))
    (canvas-mindmap-define-source 'test-lang :modes '(prog-mode) :read #'ignore :priority 5)
    (should (equal (mapcar #'canvas-mindmap-source-name canvas-mindmap-sources)
                   '(markdown org treesit imenu test-lang)))
    (should (equal (mapcar #'canvas-mindmap-source-priority canvas-mindmap-sources) '(50 40 20 10 5)))
    (should-error (canvas-mindmap-define-source 'broken :modes '(prog-mode)))
    (should-error (canvas-mindmap-define-source 'broken :read #'ignore))))

(ert-deftest canvas-mindmap-the-best-source-that-reads-something-wins ()
  ;; GIVEN an org buffer, a package source for every buffer that reads
  ;;       nothing, and one below it that reads a tree of its own
  ;; WHEN the outline is read, and then with the buffer told to be org
  ;; THEN the buffer's dialect is the first source, the tree is the second's,
  ;;      the org headings serving when neither reads, AND the buffer's own
  ;;      word picks org whatever applies
  (let ((canvas-mindmap-sources canvas-mindmap-sources))
    (canvas-mindmap-define-source 'test-none :when #'always :read #'ignore :priority 200)
    (canvas-mindmap-define-source 'test-own :when #'always :priority 150
                                  :read (lambda (root) (list root '("own" :kind "test"))))
    (with-temp-buffer
      (insert "* Heading\n")
      (org-mode)
      (should (eq (canvas-mindmap--dialect) 'test-none))
      (should (equal (canvas-mindmap--headings-tree "r") '("r" ("own" :kind "test"))))
      (setq canvas-mindmap-dialect 'org)
      (should (equal (canvas-mindmap--headings-tree "r") '("Heading")))
      (setq canvas-mindmap-dialect 'no-such-source)
      (should-error (canvas-mindmap--headings-tree "r")))))

(ert-deftest canvas-mindmap-tree-sitter-rules-name-annotate-rename-and-add ()
  ;; GIVEN a VHDL buffer with an unlabelled process and a signal, and the
  ;;       rules a language package would pass
  ;; WHEN the tree is read with them
  ;; THEN the rule names the process the grammar could not, the note rule
  ;;      annotates the signal, the kind is renamed, AND the extra nodes
  ;;      hang below the process after the grammar's own
  (skip-unless (and (fboundp 'treesit-language-available-p) (treesit-language-available-p 'vhdl)))
  (with-temp-buffer
    (insert "architecture rtl of e is\n  signal st : integer;\nbegin\n"
            "  process (clk) is\n    variable v : integer;\n  begin\n  end process;\nend architecture;\n")
    (canvas-mindmap-test--code-buffer 'vhdl-mode)
    (should (equal (canvas-mindmap-treesit-tree
                    "x" :language 'vhdl
                    :kind-names '(("signal" . "wire"))
                    :name (lambda (node) (and (equal (treesit-node-type node) "process_statement") "proc"))
                    :note (lambda (node) (and (equal (treesit-node-type node) "signal_declaration") "an integer"))
                    :below (lambda (node) (and (equal (treesit-node-type node) "process_statement") '(("extra" :kind "x")))))
                   '("x" ("rtl" :kind "architecture"
                          ("st" :kind "wire" :note "an integer")
                          ("proc" :kind "process" ("v" :kind "variable") ("extra" :kind "x"))))))
    (should (equal (canvas-mindmap-treesit-tree "x")
                   '("x" ("rtl" :kind "architecture" ("st" :kind "signal") ("v" :kind "variable")))))))

(ert-deftest canvas-mindmap-tree-sitter-outlines-a-buffer-whose-language-has-a-grammar ()
  ;; GIVEN a VHDL buffer, its mode a prog mode without tree-sitter, the vhdl grammar installed
  ;; WHEN its outline is read
  ;; THEN tree-sitter gives it: the entity with its generic and ports, the
  ;;      architecture with its type, signals, labelled process and
  ;;      instantiation, the process with its variable, each kind the
  ;;      grammar's own word for the node, AND a statement with no label
  ;;      is no node
  (skip-unless (and (fboundp 'treesit-language-available-p) (treesit-language-available-p 'vhdl)))
  (with-temp-buffer
    (insert "entity e is\n  generic (N : integer := 2);\n  port (clk : in std_logic; q : out std_logic);\nend entity;\n"
            "architecture rtl of e is\n  type t_state is (A, B);\n  signal st, nx : t_state;\nbegin\n"
            "  p_main : process (clk) is\n    variable v : integer;\n  begin\n    if clk = '1' then st <= nx; end if;\n  end process;\n"
            "  u_inst : component_x port map (a => b);\nend architecture;\n")
    (canvas-mindmap-test--code-buffer 'vhdl-mode)
    (should (eq (canvas-mindmap--treesit-language) 'vhdl))
    (should (eq (canvas-mindmap--dialect) 'treesit))
    (should (equal (canvas-mindmap--headings-tree "x.vhd")
                   '("x.vhd"
                     ("e" :kind "entity"
                      ("N" :kind "constant interface") ("clk" :kind "signal interface") ("q" :kind "signal interface"))
                     ("rtl" :kind "architecture"
                      ("t_state" :kind "full type") ("st, nx" :kind "signal")
                      ("p_main" :kind "process" ("v" :kind "variable"))
                      ("u_inst" :kind "component instantiation")))))
    (let ((canvas-mindmap--with-positions t))
      (should (equal (plist-get (cdr (nth 2 (canvas-mindmap--headings-tree "x.vhd"))) :pos)
                     (1+ (string-search "architecture rtl" (buffer-string))))))
    (let ((canvas-mindmap-treesit-kind-names '(("full type" . "type"))))
      (should (equal (plist-get (cdr (nth 3 (nth 2 (canvas-mindmap--headings-tree "x.vhd")))) :kind) "type")))))

(ert-deftest canvas-mindmap-tree-sitter-names-by-field-and-skips-parameters ()
  ;; GIVEN a C buffer in plain c-mode with the c grammar installed
  ;; WHEN its outline is read
  ;; THEN a struct is named by its name field with its fields under it, a
  ;;      typedef and a function by what their declarators come down to,
  ;;      AND parameters, a struct named in passing and an unlabelled
  ;;      return statement are no nodes
  (skip-unless (and (fboundp 'treesit-language-available-p) (treesit-language-available-p 'c)))
  (with-temp-buffer
    (insert "struct ctx { int n; };\ntypedef struct ctx ctx_t;\n"
            "static int *twice (struct ctx *p, int n) { struct ctx *c = p; return c; }\n")
    (canvas-mindmap-test--code-buffer 'c-mode)
    (should (equal (canvas-mindmap--headings-tree "x.c")
                   '("x.c" ("ctx" :kind "struct" ("n" :kind "field")) ("ctx_t" :kind "type")
                     ("twice" :kind "function"))))))

(ert-deftest canvas-mindmap-modes-without-a-grammar-keep-imenu ()
  ;; GIVEN a buffer in a prog mode named for no grammar
  ;; WHEN its language and dialect are asked for
  ;; THEN there is no language and the dialect is imenu
  (with-temp-buffer
    (put 'no-such-language-mode 'derived-mode-parent 'prog-mode)
    (setq major-mode 'no-such-language-mode)
    (should-not (canvas-mindmap--treesit-language))
    (should (eq (canvas-mindmap--dialect) 'imenu))))

(ert-deftest canvas-mindmap-modes-with-their-own-parser-keep-imenu ()
  ;; GIVEN a buffer whose mode brought a tree-sitter parser of its own
  ;; WHEN its language and dialect are asked for
  ;; THEN the language is the parser's, but the dialect is imenu, the
  ;;      mode's own tree-sitter view of the buffer
  (skip-unless (and (fboundp 'treesit-language-available-p) (treesit-language-available-p 'python)))
  (with-temp-buffer
    (put 'some-ts-mode 'derived-mode-parent 'prog-mode)
    (setq major-mode 'some-ts-mode)
    (setq-local treesit-primary-parser (treesit-parser-create 'python))
    (should (eq (canvas-mindmap--treesit-language) 'python))
    (should (eq (canvas-mindmap--dialect) 'imenu))))

(ert-deftest canvas-mindmap-focus-cycles-through-the-kinds ()
  ;; GIVEN a map with three kinds
  ;; WHEN the focus moves to the next kind three times, then once more
  ;; THEN each time one kind alone shows and the others are hidden, in
  ;;      order, AND after the last it comes round to the first
  (canvas-mindmap-test--in-map-buffer '("R" ("a" :kind "todo") ("b" :kind "done") ("c" :kind "risk") ("d")) '(600 . 300)
    (canvas-mindmap-focus-next-kind)
    (should (equal (canvas-mindmap--focused-kind) "todo"))
    (should (equal (mapcar #'canvas-mindmap-node-label (canvas-diagram-nodes-shown)) '("R" "a" "d")))
    (canvas-mindmap-focus-next-kind)
    (should (equal (canvas-mindmap--focused-kind) "done"))
    (canvas-mindmap-focus-next-kind)
    (should (equal (canvas-mindmap--focused-kind) "risk"))
    (canvas-mindmap-focus-next-kind)
    (should (equal (canvas-mindmap--focused-kind) "todo"))
    (canvas-mindmap-toggle-kind "done")
    (should-not (canvas-mindmap--focused-kind))
    (should (equal (funcall (plist-get (cddr (cl-find "K o" (canvas-mindmap--kind-suffixes) :key #'car :test #'equal)) :description))
                   "only the next kind"))))

(ert-deftest canvas-mindmap-imenu-categories-become-kinds ()
  ;; GIVEN category names as imenu modes spell them
  ;; WHEN they are turned into kinds
  ;; THEN known plurals map to their kind and an unknown one loses its s
  (should (equal (canvas-mindmap--category-kind "Classes") "class"))
  (should (equal (canvas-mindmap--category-kind "Function") "function"))
  (should (equal (canvas-mindmap--category-kind "Widgets") "widget")))

(ert-deftest canvas-mindmap-several-top-level-headings-share-a-root ()
  ;; GIVEN an org buffer with two top-level headings
  ;; WHEN its headings are read with a name for the root
  ;; THEN both headings hang off a root of that name
  (with-temp-buffer
    (insert "* One\n* Two\n** Two-a\n")
    (should (equal (canvas-mindmap--headings-tree "notes.org")
                   '("notes.org" ("One") ("Two" ("Two-a")))))))

(ert-deftest canvas-mindmap-headings-skip-levels-gracefully ()
  ;; GIVEN a heading three levels deep directly under a top-level one
  ;; WHEN the headings are read
  ;; THEN it becomes a child of the nearest shallower heading
  (with-temp-buffer
    (insert "* Root\n*** Deep\n** Shallow\n")
    (should (equal (canvas-mindmap--headings-tree)
                   '("Root" ("Deep") ("Shallow"))))))

(ert-deftest canvas-mindmap-empty-buffer-has-no-tree ()
  ;; GIVEN a buffer without headings
  ;; WHEN its headings are read
  ;; THEN there is no tree, rather than an empty root
  (with-temp-buffer
    (insert "just prose\n")
    (should-not (canvas-mindmap--headings-tree "x"))))

(ert-deftest canvas-mindmap-positions-are-read-when-asked-for ()
  ;; GIVEN an org buffer with a heading, a list item under it and a sub-heading
  ;; WHEN it is read with positions wanted, and without, and as a source
  ;; THEN each node knows where its line starts, without, specs are plain,
  ;;      AND a source is always read with positions
  (with-temp-buffer
    (insert "* Root\n- item\n** Sub\n")
    (let ((canvas-mindmap--with-positions t))
      (should (equal (canvas-mindmap--headings-tree)
                     '("Root" :pos 1 ("item" :pos 8) ("Sub" :pos 15)))))
    (should (equal (canvas-mindmap--headings-tree) '("Root" ("item") ("Sub"))))
    (should (equal (canvas-mindmap--read-source (current-buffer))
                   '("Root" :pos 1 ("item" :pos 8) ("Sub" :pos 15))))))

(ert-deftest canvas-mindmap-icon-option-is-read ()
  ;; GIVEN a spec with an :icon option
  ;; WHEN it is read
  ;; THEN the node carries the icon's name
  (should (equal (canvas-mindmap-node-icon (canvas-mindmap--tree '("R" :icon "material:brain")))
                 "material:brain")))

;;;; The layout

(ert-deftest canvas-mindmap-layout-puts-children-right-of-the-parent ()
  ;; GIVEN a root with two children of equal size
  ;; WHEN the tree is laid out with a horizontal gap of 30 and a vertical of 10
  ;; THEN both children start one gap right of the root's right edge
  ;;      AND they are stacked without overlap, one gap apart
  ;;      AND the root is centred on the span of its children
  (let* ((root (canvas-mindmap--tree '("Root" ("AA") ("BB"))))
         (kids (canvas-mindmap-node-children root)))
    (canvas-mindmap--layout root #'canvas-mindmap-test--measure 30 10)
    (should (= (canvas-mindmap-node-x root) 0))
    (should (= (canvas-mindmap-node-w root) 40))
    (dolist (k kids)
      (should (= (canvas-mindmap-node-x k) 70)))
    (let ((a (car kids)) (b (cadr kids)))
      (should (= (canvas-mindmap-node-y a) 0))
      (should (= (canvas-mindmap-node-y b) 30))
      (should (= (canvas-mindmap-node-y root)
                 (/ (- (+ (canvas-mindmap-node-y b) 20) 20) 2.0))))))

(ert-deftest canvas-mindmap-layout-centres-a-lone-child-on-a-tall-parent ()
  ;; GIVEN a parent whose own height exceeds its children's span
  ;; WHEN the tree is laid out
  ;; THEN the child is centred on the parent instead of the parent on the child
  (let* ((root (canvas-mindmap--tree '("Root" ("A"))))
         (child (car (canvas-mindmap-node-children root))))
    (canvas-mindmap--layout root (lambda (l) (if (equal l "Root") '(40 . 60) '(10 . 20))) 30 10)
    (should (= (canvas-mindmap-node-y root) 0))
    (should (= (canvas-mindmap-node-y child) 20))))

(ert-deftest canvas-mindmap-layout-keeps-cousins-apart ()
  ;; GIVEN two siblings that each have two children
  ;; WHEN the tree is laid out
  ;; THEN no two grandchildren overlap vertically
  ;;      AND the bounds cover every node
  (let* ((root (canvas-mindmap--tree '("R" ("A" ("A1") ("A2")) ("B" ("B1") ("B2")))))
         (grand (cl-mapcan (lambda (k) (copy-sequence (canvas-mindmap-node-children k)))
                           (canvas-mindmap-node-children root))))
    (canvas-mindmap--layout root #'canvas-mindmap-test--measure 30 10)
    (cl-loop for (a b) on grand while b
             do (should (>= (canvas-mindmap-node-y b)
                            (+ (canvas-mindmap-node-y a) (canvas-mindmap-node-h a) 10))))
    (let ((bounds (canvas-mindmap--bounds root)))
      (should (= (car bounds) (+ 10 30 10 30 20)))
      (should (= (cdr bounds) (+ (* 4 20) (* 3 10)))))))

(ert-deftest canvas-mindmap-left-layout-mirrors-the-right-one ()
  ;; GIVEN a root with two children
  ;; WHEN it is laid out to the left
  ;; THEN the children sit left of the root, the root at the right edge,
  ;;      and the map is as wide as the rightward one
  (let ((root (canvas-mindmap--tree '("Root" ("AA") ("BB"))))
        (mirror (canvas-mindmap--tree '("Root" ("AA") ("BB")))))
    (canvas-mindmap--layout root #'canvas-mindmap-test--measure 30 10 'right)
    (canvas-mindmap--layout mirror #'canvas-mindmap-test--measure 30 10 'left)
    (should (equal (canvas-mindmap--bounds root) (canvas-mindmap--bounds mirror)))
    (should (= (canvas-mindmap-node-x mirror) (- (car (canvas-mindmap--bounds mirror)) 40)))
    (dolist (child (canvas-mindmap-node-children mirror))
      (should (= (canvas-mindmap-node-x child) 0)))))

(ert-deftest canvas-mindmap-both-layout-puts-half-the-branches-each-side ()
  ;; GIVEN a root with four branches
  ;; WHEN it is laid out to both sides
  ;; THEN two branches lie right of the root and two left of it,
  ;;      AND the map starts at the origin
  (let ((root (canvas-mindmap--tree '("Root" ("A") ("B") ("C") ("D")))))
    (canvas-mindmap--layout root #'canvas-mindmap-test--measure 30 10 'both)
    (let* ((rx (canvas-mindmap-node-x root))
           (kids (canvas-mindmap-node-children root))
           (right (cl-count-if (lambda (k) (> (canvas-mindmap-node-x k) rx)) kids))
           (left (cl-count-if (lambda (k) (< (+ (canvas-mindmap-node-x k) (canvas-mindmap-node-w k)) rx)) kids)))
      (should (= right 2))
      (should (= left 2))
      (should (= 4 (length kids)))
      (should (= 0 (apply #'min (mapcar #'canvas-mindmap-node-x (cons root kids)))))
      (should (= 0 (apply #'min (mapcar #'canvas-mindmap-node-y (cons root kids))))))))

(ert-deftest canvas-mindmap-edges-leave-from-the-side-facing-the-child ()
  ;; GIVEN a parent with one child to its right and one to its left
  ;; WHEN the edge ends are computed
  ;; THEN the right child's edge runs from the parent's right side to its left,
  ;;      AND the left child's from the parent's left side to its right
  (let ((parent (canvas-mindmap-node-create :label "P" :x 100 :y 0 :w 40 :h 20))
        (right (canvas-mindmap-node-create :label "R" :x 200 :y 0 :w 30 :h 20))
        (left (canvas-mindmap-node-create :label "L" :x 0 :y 0 :w 30 :h 20)))
    (should (equal (canvas-mindmap--edge-ends parent right) '(140 10.0 200 10.0)))
    (should (equal (canvas-mindmap--edge-ends parent left) '(100 10.0 30 10.0)))))

(ert-deftest canvas-mindmap-spacing-scales-the-gaps ()
  ;; GIVEN the three spacings
  ;; WHEN the gaps are asked for
  ;; THEN airy is wider than normal, which is wider than compact, in each
  (let ((canvas-mindmap-gap-x 48) (canvas-mindmap-gap-y 12))
    (cl-flet ((measures (spacing)
                (let ((canvas-diagram-spacing spacing))
                  (list (canvas-mindmap--gap-x) (canvas-mindmap--gap-y)))))
      (should (equal (measures 'normal) '(48 12)))
      (cl-mapc (lambda (a n c) (should (> a n)) (should (> n c)))
               (measures 'airy) (measures 'normal) (measures 'compact)))))

(ert-deftest canvas-mindmap-layout-callback-lists-the-shown-nodes-in-reading-order ()
  ;; GIVEN a map buffer whose branch "A" has just been folded
  ;; WHEN the diagram is laid out again through the callback
  ;; THEN the nodes are the shown ones, parents before children, AND the
  ;;      layout marks the branch as folded
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1")) ("B")) '(600 . 300)
    (canvas-diagram-set-fold '("R" "A") 'folded)
    (let ((nodes (canvas-mindmap--lay-out canvas-diagram--diagram canvas-diagram--context)))
      (should (equal (mapcar #'canvas-mindmap-node-label nodes) '("R" "A" "B")))
      (should (> (canvas-mindmap-node-x (cadr nodes)) 0))
      (should (canvas-mindmap-node-collapsed (cadr nodes))))))

;;;; Colours and the legend

(ert-deftest canvas-mindmap-branch-colours-cycle-the-configured-list ()
  ;; GIVEN two configured branch colours
  ;; WHEN branches 0, 1 and 2 ask for theirs
  ;; THEN they get the first, the second and the first again,
  ;;      AND no branch, the root, gets the node colour
  (let ((canvas-mindmap-branch-colors '("red" "green"))
        (canvas-diagram-colors '(:node "blue")))
    (should (equal (canvas-mindmap--branch-rgb 0) '(1.0 0.0 0.0)))
    (should (equal (canvas-mindmap--branch-rgb 1) '(0.0 1.0 0.0)))
    (should (equal (canvas-mindmap--branch-rgb 2) '(1.0 0.0 0.0)))
    (should (equal (canvas-mindmap--branch-rgb nil) '(0.0 0.0 1.0)))))

(ert-deftest canvas-mindmap-palette-colours-the-branches ()
  ;; GIVEN no explicit branch colours and a named palette
  ;; WHEN the first branch's colour is asked for
  ;; THEN it is the palette's first colour
  (let ((canvas-mindmap-branch-colors nil)
        (canvas-diagram-palettes '(("derived") ("two" "red" "green")))
        (canvas-diagram-palette "two"))
    (should (equal (canvas-mindmap--branch-rgb 0) '(1.0 0.0 0.0)))
    (should (equal (canvas-mindmap--branch-rgb 1) '(0.0 1.0 0.0)))))

(ert-deftest canvas-mindmap-legend-lists-branches-then-kinds ()
  ;; GIVEN a tree with two branches and one node of kind "todo"
  ;; WHEN the legend's entries are made through the diagram
  ;; THEN both branches come first, filled, then the kind, outlined
  ;;      AND a tree with one branch and no kinds has no legend
  (canvas-mindmap-test--rendering
   (canvas-mindmap-test--with-context ctx 4 4
     (let ((canvas-diagram-kinds '(("todo" . "blue"))))
       (should (equal (canvas-diagram--legend-entries
                       (canvas-mindmap-test--laid-out '("R" ("A" ("A1" :kind "todo")) ("B")) ctx))
                      '(("A" (1.0 0.0 0.0) fill)
                        ("B" (0.0 1.0 0.0) fill)
                        ("todo" (0.0 0.0 1.0) outline))))
       (should-not (canvas-diagram--legend-entries (canvas-mindmap-test--laid-out '("R" ("A")) ctx)))))))

(ert-deftest canvas-mindmap-render-paints-branches-and-kinds ()
  ;; GIVEN a root with two children, one of kind "todo", and a canvas
  ;; WHEN the map is rendered
  ;; THEN the root carries the node colour, each child its branch colour,
  ;;      the todo child's left edge the kind's red, AND the far corner the background
  (canvas-mindmap-test--rendering
   (canvas-mindmap-test--with-context ctx 300 200
     (let* ((canvas-mindmap-branch-colors '("green" "green"))
            (diagram (canvas-mindmap-test--laid-out '("Root" ("A" :kind "todo") ("B")) ctx))
            (root (canvas-diagram-model diagram))
            (kids (canvas-mindmap-node-children root))
            (a (car kids)))
       (canvas-diagram--render diagram ctx '(300 . 200))
       (pcase-let ((`(,x . ,y) (canvas-mindmap-test--inside root)))
         (should (= (canvas-cairo-pixel ctx x y) #xFF0000FF)))
       (pcase-let ((`(,x . ,y) (canvas-mindmap-test--inside (cadr kids))))
         (should (= (canvas-cairo-pixel ctx x y) #xFF00FF00)))
       (let ((x (+ 10 (round (canvas-mindmap-node-x a))))
             (y (+ 10 (floor (canvas-diagram-middle-y a)))))
         (should (= (canvas-cairo-pixel ctx x y) #xFFFF0000))
         (should (= (canvas-cairo-pixel ctx (+ x 4) y) #xFF00FF00)))
       (should (= (canvas-cairo-pixel ctx 299 199) #xFFFFFFFF))))))

(ert-deftest canvas-mindmap-every-edge-style-draws-an-edge ()
  ;; GIVEN a root with one child, level with it
  ;; WHEN the map is rendered in each edge style
  ;; THEN the middle of the gap between them carries the edge colour
  (canvas-mindmap-test--rendering
   (dolist (style '(curve straight elbow))
     (let ((canvas-mindmap-edge-style style)
           (canvas-diagram-colors '(:background "white" :node "blue" :text "black" :edge "red")))
       (canvas-mindmap-test--with-context ctx 300 100
         (let* ((diagram (canvas-mindmap-test--laid-out '("Root" ("A")) ctx))
                (root (canvas-diagram-model diagram)))
           (canvas-diagram--render diagram ctx '(300 . 100))
           (should (= (canvas-cairo-pixel ctx (+ 10 (round (canvas-mindmap-node-w root)) (/ (canvas-mindmap--gap-x) 2))
                                          (+ 10 (floor (canvas-diagram-middle-y root))))
                      #xFFFF0000))))))))

(ert-deftest canvas-mindmap-trail-is-drawn-in-the-selection-colour ()
  ;; GIVEN the keyboard on a child, with a selection colour configured
  ;; WHEN the map is drawn
  ;; THEN the edge from the root to the child is in that colour
  (canvas-mindmap-test--in-map-buffer '("Root" ("A")) '(300 . 200)
    (let* ((canvas-diagram-colors (append '(:selection "yellow") canvas-diagram-colors))
           (a (canvas-mindmap-test--labelled "A"))
           (root (canvas-diagram-current-model))
           (y (+ 10 (floor (canvas-diagram-middle-y a)))))
      (canvas-diagram--select a)
      (should (= (canvas-cairo-pixel canvas-diagram--context
                                     (+ 10 (round (/ (+ (+ (canvas-mindmap-node-x root) (canvas-mindmap-node-w root))
                                                        (canvas-mindmap-node-x a))
                                                     2.0)))
                                     y)
                 #xFFFFFF00)))))

(ert-deftest canvas-mindmap-card-tells-the-path-and-the-note ()
  ;; GIVEN a grandchild with a note, a leaf without, and a parent without
  ;; WHEN their cards are asked for
  ;; THEN the title is the label, the path the way down, the body the note
  ;;      or a count of children
  (let* ((root (canvas-mindmap--tree-of '("R" ("A" ("A1" :note "why")) ("B"))))
         (a (car (canvas-mindmap-node-children root)))
         (a1 (car (canvas-mindmap-node-children a)))
         (b (cadr (canvas-mindmap-node-children root))))
    (should (equal (canvas-mindmap--card-text root a1) '("A1" "R › A" "why")))
    (should (equal (canvas-mindmap--card-text root a) '("A" "R" "One child, no note.")))
    (should (equal (canvas-mindmap--card-text root b) '("B" "R" "A leaf, without a note.")))))

;;;; The buffer

(ert-deftest canvas-mindmap-mode-is-a-diagram-mode-showing-a-tree ()
  ;; GIVEN a map buffer
  ;; THEN the mode derives from the diagram mode, the model is the tree's
  ;;      root, the keyboard is on it, AND the header line reads the path
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1")) ("B")) '(300 . 200)
    (should (derived-mode-p 'canvas-diagram-mode))
    (should (canvas-mindmap-node-p (canvas-diagram-current-model)))
    (should (equal (canvas-mindmap-test--selected) "R"))
    (canvas-diagram--select (canvas-mindmap-test--labelled "A1"))
    (should (equal (canvas-diagram--header) "R › A › A1"))))

(ert-deftest canvas-mindmap-forward-goes-to-the-nearest-child-and-back-to-the-parent ()
  ;; GIVEN a root with three children
  ;; WHEN the keyboard moves forward, forward again, then back twice
  ;; THEN it lands on the middle child, stays there having no children,
  ;;      returns to the root, and stays there having no parent
  (canvas-mindmap-test--in-map-buffer '("Root" ("A") ("B") ("C")) '(600 . 400)
    (canvas-diagram-move-in)
    (should (equal (canvas-mindmap-test--selected) "B"))
    (canvas-diagram-move-in)
    (should (equal (canvas-mindmap-test--selected) "B"))
    (canvas-diagram-move-out)
    (should (equal (canvas-mindmap-test--selected) "Root"))
    (canvas-diagram-move-out)
    (should (equal (canvas-mindmap-test--selected) "Root"))))

(ert-deftest canvas-mindmap-next-and-previous-walk-a-depth-across-cousins ()
  ;; GIVEN two branches with two children each, the keyboard on the first grandchild
  ;; WHEN it moves down three times and up once along its depth
  ;; THEN it passes its sibling, crosses to the cousin, stops at the last,
  ;;      and comes back one
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1") ("A2")) ("B" ("B1") ("B2"))) '(600 . 400)
    (canvas-diagram--select (canvas-mindmap-test--labelled "A1"))
    (canvas-diagram-move-next-at-depth)
    (should (equal (canvas-mindmap-test--selected) "A2"))
    (canvas-diagram-move-next-at-depth)
    (should (equal (canvas-mindmap-test--selected) "B1"))
    (canvas-diagram-move-next-at-depth)
    (canvas-diagram-move-next-at-depth)
    (should (equal (canvas-mindmap-test--selected) "B2"))
    (canvas-diagram-move-previous-at-depth)
    (should (equal (canvas-mindmap-test--selected) "B1"))))

(ert-deftest canvas-mindmap-sexp-moves-walk-the-siblings-under-one-parent ()
  ;; GIVEN a branch with three children, the keyboard on the middle one
  ;; WHEN it moves forward twice and back once as over sexps
  ;; THEN it reaches the last sibling, holds there, and comes back one,
  ;;      AND on a branch the siblings are the other branches
  ;;      AND the root, alone at its level, does not move
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1") ("A2") ("A3")) ("B")) '(600 . 400)
    (canvas-diagram--select (canvas-mindmap-test--labelled "A2"))
    (canvas-diagram-move-next-sibling)
    (should (equal (canvas-mindmap-test--selected) "A3"))
    (canvas-diagram-move-next-sibling)
    (should (equal (canvas-mindmap-test--selected) "A3"))
    (canvas-diagram-move-previous-sibling)
    (should (equal (canvas-mindmap-test--selected) "A2"))
    (canvas-diagram--select (canvas-mindmap-test--labelled "A"))
    (canvas-diagram-move-next-sibling)
    (should (equal (canvas-mindmap-test--selected) "B"))
    (canvas-diagram-move-first)
    (canvas-diagram-move-next-sibling)
    (should (equal (canvas-mindmap-test--selected) "R"))))

(ert-deftest canvas-mindmap-defun-moves-go-between-top-level-branches ()
  ;; GIVEN two branches with children, the keyboard deep in the second
  ;; WHEN it moves to the branch, again, then to the next branch twice
  ;; THEN it reaches its branch, then the previous branch, then the next,
  ;;      and holds at the last
  ;;      AND from the root the first branch is next
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1") ("A2")) ("B" ("B1"))) '(600 . 400)
    (canvas-diagram--select (canvas-mindmap-test--labelled "B1"))
    (canvas-diagram-move-branch)
    (should (equal (canvas-mindmap-test--selected) "B"))
    (canvas-diagram-move-branch)
    (should (equal (canvas-mindmap-test--selected) "A"))
    (canvas-diagram-move-next-branch)
    (should (equal (canvas-mindmap-test--selected) "B"))
    (canvas-diagram-move-next-branch)
    (should (equal (canvas-mindmap-test--selected) "B"))
    (canvas-diagram-move-first)
    (canvas-diagram-move-next-branch)
    (should (equal (canvas-mindmap-test--selected) "A"))))

(ert-deftest canvas-mindmap-next-walks-the-tree-in-reading-order ()
  ;; GIVEN a small tree with the keyboard on the root
  ;; WHEN n goes forward past the end and p comes back from the root
  ;; THEN children come before siblings, the last node holds, and the root
  ;;      is reached again from the start
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1")) ("B")) '(600 . 400)
    (let (seen)
      (dotimes (_ 4)
        (canvas-diagram-move-next)
        (push (canvas-mindmap-test--selected) seen))
      (should (equal (nreverse seen) '("A" "A1" "B" "B"))))
    (canvas-diagram-move-first)
    (canvas-diagram-move-previous)
    (should (equal (canvas-mindmap-test--selected) "R"))
    (canvas-diagram-move-last)
    (should (equal (canvas-mindmap-test--selected) "B"))))

(ert-deftest canvas-mindmap-changing-the-direction-relays-out-around-the-selection ()
  ;; GIVEN a map buffer with the keyboard on a child, branches to the right
  ;; WHEN the direction is cycled to the left
  ;; THEN the child is still selected and now lies left of the root
  (canvas-mindmap-test--in-map-buffer '("Root" ("A") ("B")) '(600 . 300)
    (let ((a (canvas-mindmap-test--labelled "A")))
      (canvas-diagram--select a)
      (canvas-mindmap-cycle-direction)
      (should (eq canvas-mindmap-direction 'left))
      (should (eq (canvas-diagram-selected) a))
      (should (< (canvas-mindmap-node-x a) (canvas-mindmap-node-x (canvas-diagram-current-model)))))))

(ert-deftest canvas-mindmap-map-follows-its-org-buffer ()
  ;; GIVEN a map of an org buffer that follows it
  ;; WHEN a heading is added and the map reads the buffer again
  ;; THEN the map has the new node, the keyboard still on the root
  (canvas-mindmap-test--rendering
   (with-temp-buffer
     (insert "* Root\n** A\n")
     (let ((org (current-buffer)) map)
       (with-temp-buffer
         (setq map (current-buffer))
         (canvas-mindmap-mode)
         (canvas-diagram-adopt (canvas-mindmap--diagram) (canvas-mindmap--read-source org))
         (plist-put (cdr canvas-diagram--canvas) :data-width 300)
         (plist-put (cdr canvas-diagram--canvas) :data-height 200)
         (canvas-diagram--follow org)
         (unwind-protect
             (progn
               (with-current-buffer org
                 (goto-char (point-max))
                 (insert "** B\n"))
               (canvas-diagram--refresh-from-source map)
               (should (= (length (canvas-diagram-nodes-shown)) 3))
               (should (equal (canvas-mindmap-test--selected) "Root"))
               ;; AND going to the new node puts point on its heading
               (canvas-diagram-jump "B")
               (should (= (with-current-buffer org (point)) 13)))
           (canvas-diagram--release)))))))

;;;; Copying a node

(defconst canvas-mindmap-test--outline
  (concat "* One\n\nBody line\n- item a\n  - item a1\n    more of a1\n- item b\n"
          "** Two\nTwo's body\n#+begin_src sh\n* not a heading\n#+end_src\n* Three\n")
  "An org outline to copy from: bodies, nested items, a code block holding a star line.")

(defun canvas-mindmap-test--line-at (text)
  "Where the first line holding TEXT starts in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (line-beginning-position)))

(ert-deftest canvas-mindmap-source-text-of-an-org-heading ()
  ;; GIVEN an org outline with bodies, nested items and a code block
  ;; WHEN each part of its headings is asked for
  ;; THEN the header is the heading's line, the body the lines up to the
  ;;      next heading of any level, trimmed, a star line in a code block
  ;;      being no heading, AND a heading without a body has none
  (with-temp-buffer
    (insert canvas-mindmap-test--outline)
    (let ((one (canvas-mindmap-test--line-at "* One"))
          (two (canvas-mindmap-test--line-at "** Two"))
          (three (canvas-mindmap-test--line-at "* Three")))
      (should (equal (canvas-mindmap--outline-text 'org one 'header) "* One"))
      (should (equal (canvas-mindmap--outline-text 'org one 'body)
                     "Body line\n- item a\n  - item a1\n    more of a1\n- item b"))
      (should (equal (canvas-mindmap--outline-text 'org one 'all)
                     "* One\n\nBody line\n- item a\n  - item a1\n    more of a1\n- item b"))
      (should (equal (canvas-mindmap--outline-text 'org two 'body)
                     "Two's body\n#+begin_src sh\n* not a heading\n#+end_src"))
      (should-not (canvas-mindmap--outline-text 'org three 'body))
      (should (equal (canvas-mindmap--outline-text 'org three 'all) "* Three")))))

(ert-deftest canvas-mindmap-source-text-of-a-list-item ()
  ;; GIVEN the same outline
  ;; WHEN each part of its list items is asked for
  ;; THEN the header is the item's line, the body the lines indented deeper
  ;;      below it, AND an item with nothing below has no body
  (with-temp-buffer
    (insert canvas-mindmap-test--outline)
    (let ((a (canvas-mindmap-test--line-at "- item a"))
          (b (canvas-mindmap-test--line-at "- item b")))
      (should (equal (canvas-mindmap--outline-text 'org a 'header) "- item a"))
      (should (equal (canvas-mindmap--outline-text 'org a 'body) "  - item a1\n    more of a1"))
      (should (equal (canvas-mindmap--outline-text 'org a 'all) "- item a\n  - item a1\n    more of a1"))
      (should-not (canvas-mindmap--outline-text 'org b 'body)))))

(ert-deftest canvas-mindmap-source-text-of-a-markdown-heading ()
  ;; GIVEN a markdown outline whose first body has a fence holding a # line
  ;; WHEN the first heading's body is asked for
  ;; THEN it runs past the fence to the next real heading
  (with-temp-buffer
    (insert "# Root\n\nprose\n```\n# not a heading\n```\n## A\n")
    (should (equal (canvas-mindmap--outline-text 'markdown 1 'body) "prose\n```\n# not a heading\n```"))))

(ert-deftest canvas-mindmap-copies-a-node-and-its-org-source ()
  ;; GIVEN a map that follows an org buffer
  ;; WHEN a node with a body is copied, then from the source, then a leaf
  ;;      without a note has its body copied
  ;; THEN the node is its label and note, the source its heading and body,
  ;;      AND the leaf's body is a user error, not the card's placeholder
  (canvas-mindmap-test--rendering
   (let ((kill-ring nil) (kill-ring-yank-pointer nil) (interprogram-cut-function nil))
     (with-temp-buffer
       (insert "* Root\n** A\nA's body\n** B\n")
       (let ((org (current-buffer)))
         (with-temp-buffer
           (canvas-mindmap-mode)
           (canvas-diagram-adopt (canvas-mindmap--diagram) (canvas-mindmap--read-source org))
           (canvas-diagram--follow org)
           (unwind-protect
               (progn
                 (canvas-diagram-set-selected (canvas-mindmap-test--labelled "A"))
                 (canvas-diagram-copy-node)
                 (should (equal (current-kill 0) "A\n\nA's body"))
                 (canvas-diagram-copy-source)
                 (should (equal (current-kill 0) "** A\nA's body"))
                 (canvas-diagram-set-selected (canvas-mindmap-test--labelled "B"))
                 (should-error (canvas-diagram-copy-body) :type 'user-error))
             (canvas-diagram--release))))))))

(ert-deftest canvas-mindmap-source-text-only-from-its-own-outlines ()
  ;; GIVEN a code buffer, read by another source, and a map drawn by a
  ;;       package that follows a buffer with a reader of its own
  ;; WHEN source text is asked for
  ;; THEN the code buffer gives none, AND the package's map offers none,
  ;;      its reader in place of the outline's
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f () 1)\n")
    (should-not (canvas-mindmap--source-text nil (canvas-mindmap-node-create :label "f" :pos 1)
                                             'header (current-buffer))))
  (let ((diagram (canvas-mindmap--follow-diagram #'ignore)))
    (should (eq (plist-get (canvas-diagram-callbacks diagram) :read-source) #'ignore))
    (should-not (plist-get (canvas-diagram-callbacks diagram) :source-text))
    (should (plist-get canvas-mindmap--callbacks :source-text))))

;;;; Folding

;; The fold commands, their keys and the fold rule are canvas-diagram's,
;; tested there.  These tests cover how a mind map takes part.

(ert-deftest canvas-mindmap-depth-sets-the-levels-a-map-opens-with ()
  ;; GIVEN a spec three levels deep
  ;; WHEN it is shown with a depth of one, and then with no depth
  ;; THEN the root and its branches show, a branch says how many children it
  ;;      hides, one level more shows all, AND with no depth all shows at once
  (let ((spec '("R" ("A" ("A1") ("A2")) ("B"))))
    (canvas-mindmap-test--in-map-buffer-at-depth 1 spec '(600 . 300)
      (let ((a (canvas-mindmap-test--labelled "A")))
        (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "B")))
        (should (= (canvas-mindmap--hidden a) 2))
        (should (equal (canvas-mindmap--node-text a) "A +2"))
        (should (equal (canvas-mindmap--node-text (canvas-diagram-current-model)) "R"))
        (canvas-diagram-deeper)
        (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "A1" "A2" "B")))
        (should (equal (canvas-mindmap--node-text a) "A"))))
    (canvas-mindmap-test--in-map-buffer-at-depth nil spec '(600 . 300)
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "A1" "A2" "B"))))))

(ert-deftest canvas-mindmap-tab-folds-the-node-at-the-keyboard ()
  ;; GIVEN a map buffer with the keyboard on a branch that has two children
  ;; WHEN TAB is pressed, and then c
  ;; THEN both keys run the fold command of canvas-diagram, TAB takes the
  ;;      children out of the layout, the hot spots and the walk, the box
  ;;      says how many it hides, AND c brings them back
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1") ("A2")) ("B")) '(600 . 300)
    (let ((a (canvas-mindmap-test--labelled "A"))
          (wide (car (canvas-mindmap--bounds (canvas-diagram-current-model)))))
      (should (eq (key-binding (kbd "TAB")) #'canvas-diagram-toggle-fold))
      (should (eq (key-binding (kbd "c")) #'canvas-diagram-toggle-fold))
      (canvas-diagram--select a)
      (call-interactively (key-binding (kbd "TAB")))
      (should (canvas-mindmap-node-collapsed a))
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "B")))
      (should (< (car (canvas-mindmap--bounds (canvas-diagram-current-model))) wide))
      (should (equal (canvas-mindmap--node-text a) "A +2"))
      (should (= (length (canvas-diagram--hot-spots (canvas-diagram-nodes-shown) '(0 . 0))) 3))
      (canvas-diagram-move-next)
      (should (equal (canvas-mindmap-test--selected) "B"))
      (canvas-diagram--select a)
      (call-interactively (key-binding (kbd "c")))
      (should-not (canvas-mindmap-node-collapsed a))
      (should (= (length (canvas-diagram-nodes-shown)) 5))
      (should (= (car (canvas-mindmap--bounds (canvas-diagram-current-model))) wide)))))

(ert-deftest canvas-mindmap-going-into-a-folded-node-unfolds-it ()
  ;; GIVEN a map opened at a depth of one, the keyboard on a folded branch
  ;; WHEN the keyboard moves forward into a child
  ;; THEN the fold of the branch is open, over the levels, the branch shows
  ;;      its children, AND the keyboard lands on one of them
  (canvas-mindmap-test--in-map-buffer-at-depth 1 '("R" ("A" ("A1") ("A2")) ("B")) '(600 . 300)
    (let ((a (canvas-mindmap-test--labelled "A")))
      (canvas-diagram--select a)
      (canvas-diagram-move-in)
      (should-not (canvas-diagram-folded-p '("R" "A") 1))
      (should-not (canvas-mindmap-node-collapsed a))
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "A1" "A2" "B")))
      (should (member (canvas-mindmap-test--selected) '("A1" "A2"))))))

(ert-deftest canvas-mindmap-a-filter-keeps-the-folds ()
  ;; GIVEN a map with the branch "A" folded
  ;; WHEN a kind is hidden, and then every kind shows again
  ;; THEN "A" stays folded through both rebuilds
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1")) ("B" ("B1" :kind "risk")) ("C")) '(600 . 300)
    (canvas-diagram--select (canvas-mindmap-test--labelled "A"))
    (canvas-diagram-toggle-fold)
    (canvas-mindmap-toggle-kind "risk")
    (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "B" "C")))
    (canvas-mindmap-show-all-kinds)
    (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "B" "B1" "C")))))

(ert-deftest canvas-mindmap-rebuild-keeps-the-keyboard-on-its-path ()
  ;; GIVEN the keyboard on a grandchild
  ;; WHEN the tree is rebuilt with that grandchild gone, then with its
  ;;      parent folded
  ;; THEN the keyboard lands on the nearest node found on the way, then on
  ;;      the folded parent
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1")) ("B")) '(600 . 300)
    (canvas-diagram--select (canvas-mindmap-test--labelled "A1"))
    (setf (canvas-diagram-spec canvas-diagram--diagram) '("R" ("A" ("A2")) ("B")))
    (canvas-diagram-rebuild)
    (should (equal (canvas-mindmap-test--selected) "A"))
    (canvas-diagram--select (canvas-mindmap-test--labelled "A2"))
    (canvas-diagram-set-fold '("R" "A") 'folded)
    (canvas-diagram-rebuild)
    (should (equal (canvas-mindmap-test--selected) "A"))))

(ert-deftest canvas-mindmap-siblings-of-one-label-fold-on-their-own ()
  ;; GIVEN a map whose branch "A" has two children labelled "x"
  ;; WHEN the map opens, TAB folds the second "x", the map is rebuilt, and
  ;;      the keyboard moves into the second "x"
  ;; THEN the map shows both, TAB hides the child of the second alone, the
  ;;      rebuild keeps the keyboard on the second, AND the move unfolds the
  ;;      second alone
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("x" ("x1")) ("x" ("x2"))) ("B")) '(600 . 300)
    (cl-flet ((second-x () (cadr (canvas-mindmap-node-children (canvas-mindmap-test--labelled "A")))))
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "x" "x1" "x" "x2" "B")))
      (canvas-diagram--select (second-x))
      (call-interactively (key-binding (kbd "TAB")))
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "x" "x1" "x" "B")))
      (canvas-diagram-rebuild)
      (should (eq (canvas-diagram-selected) (second-x)))
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "x" "x1" "x" "B")))
      (canvas-diagram-move-in)
      (should (equal (canvas-mindmap-test--selected) "x2"))
      (should (equal (canvas-mindmap-test--shown-labels) '("R" "A" "x" "x1" "x" "x2" "B"))))))

(ert-deftest canvas-mindmap-double-click-toggles-the-fold-of-the-box ()
  ;; GIVEN a map buffer with the keyboard on the root
  ;; WHEN a double click lands on a branch through the diagram, and then again
  ;; THEN the branch folds with the keyboard on it, AND the second click
  ;;      unfolds it
  (canvas-mindmap-test--in-map-buffer '("R" ("A" ("A1")) ("B")) '(600 . 300)
    (let ((a (canvas-mindmap-test--labelled "A")))
      (cl-letf (((symbol-function 'posn-object-x-y) (lambda (_) (canvas-mindmap-test--inside a))))
        (canvas-diagram-double-click '(double-mouse-1 (nil)))
        (should (canvas-mindmap-node-collapsed a))
        (should (equal (canvas-mindmap-test--selected) "A"))
        (canvas-diagram-double-click '(double-mouse-1 (nil)))
        (should-not (canvas-mindmap-node-collapsed a))))))

;;;; Filtering by kind

(ert-deftest canvas-mindmap-hiding-a-kind-drops-its-nodes-but-keeps-the-way-to-shown-ones ()
  ;; GIVEN a code-like tree: a class with a method and a variable, a lone
  ;;       variable, and a plain node
  ;; WHEN it is built with variables hidden, then with classes hidden
  ;; THEN the variables are gone and the class still shows its method;
  ;;      the class stays as the way to its method though hidden itself,
  ;;      AND with nothing hidden all is there
  (let ((spec '("R" ("C" :kind "class" ("m" :kind "method") ("v" :kind "variable")) ("g" :kind "variable") ("p"))))
    (let* ((canvas-mindmap--hidden-kinds '("variable"))
           (root (canvas-mindmap--build nil spec)))
      (should (equal (mapcar #'canvas-mindmap-node-label (canvas-mindmap--preorder root)) '("R" "C" "m" "p"))))
    (let* ((canvas-mindmap--hidden-kinds '("class"))
           (root (canvas-mindmap--build nil spec)))
      (should (equal (mapcar #'canvas-mindmap-node-label (canvas-mindmap--preorder root)) '("R" "C" "m" "v" "g" "p"))))
    (let* ((canvas-mindmap--hidden-kinds '("class" "method" "variable"))
           (root (canvas-mindmap--build nil spec)))
      (should (equal (mapcar #'canvas-mindmap-node-label (canvas-mindmap--preorder root)) '("R" "p"))))
    (let* ((canvas-mindmap--hidden-kinds nil)
           (root (canvas-mindmap--build nil spec)))
      (should (= (length (canvas-mindmap--preorder root)) 6)))))

(ert-deftest canvas-mindmap-kinds-in-a-spec-are-counted-and-keyed ()
  ;; GIVEN a spec with kinds, some repeated
  ;; WHEN its kinds are counted and given keys
  ;; THEN each kind comes once with its count, in order of first appearance,
  ;;      AND each gets a key under K from a letter of its own, the first
  ;;      free one of its name, o being the focus key's and never free
  (let ((counts (canvas-mindmap--counts-in-spec
                 '("R" ("a" :kind "function" ("b" :kind "variable")) ("c" :kind "file") ("d" :kind "function") ("e")
                   ("f" :kind "object"))
                 :kind)))
    (should (equal counts '(("function" . 2) ("variable" . 1) ("file" . 1) ("object" . 1))))
    (should (equal (canvas-mindmap--kind-keys (mapcar #'car counts))
                   '(("function" . "K f") ("variable" . "K v") ("file" . "K i") ("object" . "K b"))))))

(ert-deftest canvas-mindmap-the-menu-lists-the-kinds-live-and-toggles-them ()
  ;; GIVEN a map buffer of a tree with two kinds
  ;; WHEN the kind suffixes are made, one is run, and then every kind is shown again
  ;; THEN there is a suffix per kind saying it is shown and how many there
  ;;      are, running one hides that kind and the map loses those nodes
  ;;      while the suffix says so, AND showing all brings them back
  (canvas-mindmap-test--in-map-buffer '("R" ("a" :kind "todo") ("b" :kind "done") ("c" :kind "todo") ("d")) '(600 . 300)
    (let* ((suffixes (canvas-mindmap--kind-suffixes))
           (todo (cl-find "K t" suffixes :key #'car :test #'equal))
           (say (lambda (suffix) (funcall (plist-get (cddr suffix) :description)))))
      (should (= (length suffixes) 4))
      (should (equal (funcall say todo) "■ todo (2)"))
      (should (equal (funcall say (cl-find "K d" suffixes :key #'car :test #'equal)) "■ done (1)"))
      (should (equal (car (car (last suffixes))) "K K"))
      (should (commandp (cadr todo)))
      (call-interactively (cadr todo))
      (should (equal canvas-mindmap--hidden-kinds '("todo")))
      (should (equal (mapcar #'canvas-mindmap-node-label (canvas-diagram-nodes-shown)) '("R" "b" "d")))
      (should (equal (funcall say todo) "□ todo (2)"))
      (canvas-mindmap-show-all-kinds)
      (should-not canvas-mindmap--hidden-kinds)
      (should (= (length (canvas-diagram-nodes-shown)) 5))
      ;; AND the menu's group is made of these, parsed for transient
      (should (cl-every (lambda (o) (eq (car o) 'transient-suffix)) (canvas-mindmap--kind-children nil))))))

;;;; Statuses

(ert-deftest canvas-mindmap-status-badge-takes-the-table-else-the-class-colour ()
  ;; GIVEN a table colouring TODO, and the colours of open and done keywords
  ;; WHEN the badges of a TODO node, an unlisted open one, an unlisted done
  ;;      one and a node without a status are asked for
  ;; THEN they say the keyword in the table's colour, the open colour, the
  ;;      done colour, AND the last has none
  (let ((canvas-mindmap-statuses '(("TODO" . "red")))
        (canvas-mindmap-status-open-color "blue")
        (canvas-mindmap-status-done-color "green"))
    (should (equal (canvas-mindmap--badge (canvas-mindmap-node-create :label "x" :status "TODO"))
                   (list "TODO" (canvas-diagram-rgb "red"))))
    (should (equal (canvas-mindmap--badge (canvas-mindmap-node-create :label "x" :status "WAIT"))
                   (list "WAIT" (canvas-diagram-rgb "blue"))))
    (should (equal (canvas-mindmap--badge (canvas-mindmap-node-create :label "x" :status "GAVEUP" :done t))
                   (list "GAVEUP" (canvas-diagram-rgb "green"))))
    (should-not (canvas-mindmap--badge (canvas-mindmap-node-create :label "x")))
    (let ((canvas-mindmap-statuses '(("TODO" . "no such colour"))))
      (should-error (canvas-mindmap--badge (canvas-mindmap-node-create :label "x" :status "TODO"))))))

(ert-deftest canvas-mindmap-status-badges-widen-boxes-and-join-the-legend ()
  ;; GIVEN a map with a TODO node and a plain one of the same label length
  ;; WHEN it is shown
  ;; THEN the TODO box is wider by its badge and the gap, AND the legend
  ;;      lists the badge before the kinds
  (canvas-mindmap-test--in-map-buffer '("R" ("a" :status "TODO") ("b" :kind "risk")) '(600 . 300)
    (should (> (canvas-mindmap-node-w (canvas-mindmap-test--labelled "a"))
               (canvas-mindmap-node-w (canvas-mindmap-test--labelled "b"))))
    (let ((rows (mapcar #'car (canvas-diagram--legend-entries canvas-diagram--diagram))))
      (should (member "TODO" rows))
      (should (< (cl-position "TODO" rows :test #'equal) (cl-position "risk" rows :test #'equal))))))

(ert-deftest canvas-mindmap-hiding-a-status-drops-its-nodes-but-keeps-the-way ()
  ;; GIVEN a tree with a DONE node over a TODO one, a lone DONE node, and a
  ;;       node of a kind
  ;; WHEN DONE is hidden, then the kind too, then TODO, then every status
  ;;      is shown again
  ;; THEN the lone DONE node goes but the one over TODO stays as its way,
  ;;      the kind goes as well, both DONE and TODO go together, AND showing
  ;;      the statuses brings all but the hidden kind back
  (canvas-mindmap-test--in-map-buffer '("R" ("a" :status "DONE" :done t ("b" :status "TODO"))
                                        ("c" :status "DONE" :done t) ("d" :kind "risk"))
      '(600 . 300)
    (let ((shown (lambda () (mapcar #'canvas-mindmap-node-label (canvas-diagram-nodes-shown)))))
      (canvas-mindmap-toggle-status "DONE")
      (should (equal (funcall shown) '("R" "a" "b" "d")))
      (canvas-mindmap-toggle-kind "risk")
      (should (equal (funcall shown) '("R" "a" "b")))
      (canvas-mindmap-toggle-status "TODO")
      (should (equal (funcall shown) '("R")))
      (canvas-mindmap-show-all-statuses)
      (should-not canvas-mindmap--hidden-statuses)
      (should (equal (funcall shown) '("R" "a" "b" "c"))))))

(ert-deftest canvas-mindmap-the-menu-lists-the-statuses-live-and-toggles-them ()
  ;; GIVEN a map buffer of a tree with two statuses
  ;; WHEN the status suffixes are made and one is run
  ;; THEN there is a suffix per status under S, saying it is shown and how
  ;;      many there are, then one showing them all; running one hides that
  ;;      status while the suffix says so, AND the group parses for transient
  (canvas-mindmap-test--in-map-buffer '("R" ("a" :status "TODO") ("b" :status "DONE" :done t)
                                        ("c" :status "TODO") ("d"))
      '(600 . 300)
    (let* ((suffixes (canvas-mindmap--status-suffixes))
           (todo (cl-find "S t" suffixes :key #'car :test #'equal))
           (say (lambda (suffix) (funcall (plist-get (cddr suffix) :description)))))
      (should (= (length suffixes) 3))
      (should (equal (funcall say todo) "■ TODO (2)"))
      (should (equal (funcall say (cl-find "S d" suffixes :key #'car :test #'equal)) "■ DONE (1)"))
      (should (equal (car (car (last suffixes))) "S S"))
      (call-interactively (cadr todo))
      (should (equal canvas-mindmap--hidden-statuses '("TODO")))
      (should (equal (mapcar #'canvas-mindmap-node-label (canvas-diagram-nodes-shown)) '("R" "b" "d")))
      (should (equal (funcall say todo) "□ TODO (2)"))
      (should (cl-every (lambda (o) (eq (car o) 'transient-suffix)) (canvas-mindmap--status-children nil))))))

;;;; Keys and the menu

(ert-deftest canvas-mindmap-keys-are-the-diagram-s-fold-keys-included ()
  ;; GIVEN the mode map
  ;; WHEN its keys are looked up
  ;; THEN the diagram's keys are inherited, AND no key of its own hides a
  ;;      fold key of the diagram
  (let ((map canvas-mindmap-mode-map)
        (own (copy-keymap canvas-mindmap-mode-map)))
    (set-keymap-parent own nil)
    (pcase-dolist (`(,key . ,_) canvas-diagram--fold-keys)
      (should-not (lookup-key own (kbd key))))
    (should (eq (lookup-key map (kbd "^")) #'canvas-diagram-move-out))
    (should (eq (lookup-key map (kbd "n")) #'canvas-diagram-move-next))
    (should (eq (lookup-key map (kbd "C-<return>")) #'canvas-diagram-visit-source))
    (should (eq (lookup-key map [double-mouse-1]) #'canvas-diagram-double-click))
    (should (eq (lookup-key map [canvas-diagram-node down-mouse-1]) #'canvas-diagram-mouse))))

(ert-deftest canvas-mindmap-menu-has-the-layout-group-and-the-shared-ones ()
  ;; GIVEN the menu and the mode map
  ;; WHEN their keys are looked up
  ;; THEN its own keys, direction and edges, are on it and in the buffer,
  ;;      the shared folds, zoom and looks follow, AND it is the diagram's menu
  (pcase-dolist (`(,key . ,command) canvas-mindmap--setting-keys)
    (let ((suffix (transient-get-suffix 'canvas-mindmap-menu key)))
      (should suffix)
      (should (eq (plist-get (cdr suffix) :command) command))
      (should (eq (lookup-key canvas-mindmap-mode-map (kbd key)) command))))
  (pcase-dolist (`(,key . ,command) '(("[" . canvas-diagram-shallower) ("]" . canvas-diagram-deeper)))
    (should (eq (plist-get (cdr (transient-get-suffix 'canvas-mindmap-menu key)) :command) command)))
  (should (transient-get-suffix 'canvas-mindmap-menu "+"))
  (should (transient-get-suffix 'canvas-mindmap-menu "B"))
  (should (transient-get-suffix 'canvas-mindmap-menu "i"))
  (should (eq (lookup-key canvas-mindmap-mode-map (kbd "SPC")) #'canvas-keys-menu))
  (should (eq (plist-get canvas-mindmap--callbacks :menu) 'canvas-mindmap-menu)))

;;;; Files

(ert-deftest canvas-mindmap-file-tree-reads-an-org-file ()
  ;; GIVEN an org file with two top-level headings
  ;; WHEN its tree is read
  ;; THEN they hang off a root named after the file
  (let ((file (make-temp-file "canvas-mindmap-test" nil ".org" "* One\n** One-a\n* Two\n")))
    (unwind-protect
        (should (equal (canvas-mindmap--file-tree file)
                       (list (file-name-nondirectory file) '("One" ("One-a")) '("Two"))))
      (delete-file file))))

(ert-deftest canvas-mindmap-file-without-headings-is-an-error ()
  ;; GIVEN a file with no headings
  ;; WHEN its tree is read
  ;; THEN that is an error naming the file
  (let ((file (make-temp-file "canvas-mindmap-test" nil ".org" "prose only\n")))
    (unwind-protect
        (should-error (canvas-mindmap--file-tree file))
      (delete-file file))))

(ert-deftest canvas-mindmap-export-writes-a-png-of-the-map ()
  ;; GIVEN a tree given as a nested list, and one given as an org file
  ;; WHEN each is exported
  ;; THEN a PNG of some size is written for each, with no buffer involved
  (let ((png (make-temp-file "canvas-mindmap-test" nil ".png"))
        (org (make-temp-file "canvas-mindmap-test" nil ".org" "* Root\n** Leaf\n"))
        (canvas-diagram-font "Sans 12px"))
    (unwind-protect
        (progn
          (should (equal (canvas-mindmap-export '("Root" ("A") ("B")) png) png))
          (should (canvas-mindmap-test--png-p png))
          (should (> (file-attribute-size (file-attributes png)) 500))
          (delete-file png)
          (canvas-mindmap-export org png)
          (should (canvas-mindmap-test--png-p png)))
      (delete-file png)
      (delete-file org))))

(ert-deftest canvas-mindmap-nest-hangs-each-node-off-the-nearest-lower-level ()
  ;; GIVEN nodes at levels as a reader finds them, indents say, one of them
  ;;       with a child of its own already, and a level stepping back
  ;; WHEN they are nested
  ;; THEN each hangs off the nearest earlier node of a lower level, after
  ;;      the children it had, AND two nodes at the lowest level make two trees
  (should (equal (canvas-mindmap-nest (list (list 2 "Root") (list 4 "A" :pos 7) (list 8 "A1")
                                            (list 4 "B" '("B0")) (list 6 "B1")))
                 '(("Root" ("A" :pos 7 ("A1")) ("B" ("B0") ("B1"))))))
  (should (equal (length (canvas-mindmap-nest (list (list 1 "One") (list 1 "Two")))) 2)))

;;;; Following what a reader finds

(ert-deftest canvas-mindmap-follow-shows-what-a-reader-finds-and-reads-again ()
  ;; GIVEN a reader that finds a tree in a buffer, and one that finds none
  ;; WHEN a mind map is shown following the buffer with each
  ;; THEN the first is shown in the mind map mode from the tree it read,
  ;;      the buffer followed with that reader to read it again and the rest
  ;;      of the map as ever, AND the second is a user error
  (let ((source (generate-new-buffer " canvas-mindmap-test-source"))
        (tree '("Root" ("A") ("B" ("B1"))))
        shown)
    (let ((read (lambda (_buffer) tree)))
      (unwind-protect
          (cl-letf (((symbol-function 'canvas-diagram-show)
                     (lambda (name mode diagram spec &optional followed)
                       (let ((callbacks (canvas-diagram-callbacks diagram)))
                         (setq shown (list name mode (plist-get callbacks :read-source)
                                           (plist-get callbacks :layout) spec followed))))))
            (canvas-mindmap-follow source read "*mine*")
            (should (equal shown (list "*mine*" #'canvas-mindmap-mode read #'canvas-mindmap--lay-out tree source)))
            (should-error (canvas-mindmap-follow source #'ignore) :type 'user-error))
        (kill-buffer source)))))

;;;; The package

(defun canvas-mindmap-test--library (name)
  "The source file of library NAME."
  (let ((file (locate-library (concat name ".el"))))
    (should file)
    file))

(defun canvas-mindmap-test--info (name)
  "Parse the package headers of library NAME."
  (with-temp-buffer
    (insert-file-contents (canvas-mindmap-test--library name))
    (package-buffer-info)))

(ert-deftest canvas-mindmap-package-headers-describe-the-package ()
  ;; GIVEN the library as it is published
  ;; WHEN package.el reads its headers
  ;; THEN it finds the name, a version, a one-line summary and a URL
  (let ((info (canvas-mindmap-test--info "canvas-mindmap")))
    (should (equal (package-desc-name info) 'canvas-mindmap))
    (should (version-list-<= '(0 1) (package-desc-version info)))
    (should (string-match-p "[a-z]" (package-desc-summary info)))
    (should-not (equal (package-desc-summary info) "No description available."))
    (should (string-prefix-p "https://" (cdr (assq :url (package-desc-extras info)))))))

(ert-deftest canvas-mindmap-package-requires-emacs-and-canvas-diagram ()
  ;; GIVEN the library, which cannot run without a canvas or the shared package
  ;; WHEN its requirements are read
  ;; THEN both are declared, AND the Emacs it asks for is one this very
  ;;      Emacs satisfies -- a requirement no built Emacs meets would keep
  ;;      package.el from ever installing it
  (let* ((reqs (package-desc-reqs (canvas-mindmap-test--info "canvas-mindmap")))
         (wanted (cadr (assq 'emacs reqs))))
    (should (version-list-<= '(32) wanted))
    (should (version-list-<= wanted (version-to-list emacs-version)))
    (should (assq 'canvas-diagram reqs))))

(ert-deftest canvas-mindmap-package-names-its-author-and-licence ()
  ;; GIVEN the library, published under the GPL alongside shipit and the
  ;;       other canvas packages
  ;; WHEN its headers and leading comments are read
  ;; THEN it names an author, holds the copyright the way they all do,
  ;;      AND carries a Commentary section and the licence notice
  (with-temp-buffer
    (insert-file-contents (canvas-mindmap-test--library "canvas-mindmap"))
    (should (string-match-p "[^ ]" (or (lm-header "author") "")))
    (should (save-excursion
              (goto-char (point-min))
              (re-search-forward "^;; Copyright (C) [0-9-]+ canvas-mindmap contributors$"
                                 nil t)))
    (should (lm-commentary-start))
    (should (save-excursion (re-search-forward "GNU General Public License" nil t)))))

(ert-deftest canvas-mindmap-licence-file-is-the-whole-gpl ()
  ;; GIVEN a package that says it is GPL-3.0-or-later
  ;; WHEN the LICENSE file beside it is read
  ;; THEN it is the licence itself, not a summary pointing elsewhere
  (let ((license (expand-file-name
                  "LICENSE" (file-name-directory
                             (canvas-mindmap-test--library "canvas-mindmap")))))
    (should (file-readable-p license))
    (with-temp-buffer
      (insert-file-contents license)
      (should (save-excursion (re-search-forward "TERMS AND CONDITIONS" nil t)))
      (should (save-excursion (re-search-forward "Version 3, 29 June 2007" nil t))))))

(ert-deftest canvas-mindmap-is-the-layout-of-its-kind ()
  ;; GIVEN canvas-mindmap loaded
  ;; THEN readers draw a mind map, a spec of kind mindmap, with it: it follows
  ;;      the source AND exports a picture
  (should (equal (alist-get 'mindmap canvas-diagram-layouts)
                 '(:follow canvas-mindmap-follow :export canvas-mindmap-export))))

;;; canvas-mindmap-tests.el ends here
