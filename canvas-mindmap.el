;;; canvas-mindmap.el --- Mind maps drawn on an Emacs canvas -*- lexical-binding: t -*-

;; Copyright (C) 2026 canvas-mindmap contributors

;; Author: Daskeladden
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0.50") (canvas-diagram "0.1.0"))
;; Keywords: outlines, tools, convenience
;; URL: https://github.com/Daskeladden/canvas-mindmap

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Draws an outline as a mind map on an Emacs 32 canvas, on top of
;; canvas-diagram, which holds everything a diagram of boxes shares:
;; the canvas, the boxes, the keys, the mouse, the minimap, the source.
;; This package is the tree: an org, markdown or imenu outline read into
;; nodes, laid out as a horizontal tree, folded and unfolded, walked as
;; a tree.  Each top-level branch has a colour that its subtree
;; inherits; a node's kind, an org todo keyword or tag or a :kind
;; option, outlines its box; a legend in the corner says which is which.
;;
;; This is a prototype.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'canvas-diagram)
(require 'treesit nil t)

(defgroup canvas-mindmap nil
  "Mind maps on a canvas."
  :group 'canvas-diagram)

(defcustom canvas-mindmap-branch-colors nil
  "Colour names for the top-level branches, in order, cycled.
nil takes them from the `canvas-diagram-palette' in use."
  :type '(repeat string))

(defcustom canvas-mindmap-gap-x 48
  "Pixels between a node and its children."
  :type 'integer)

(defcustom canvas-mindmap-gap-y 12
  "Pixels between sibling subtrees."
  :type 'integer)

(defcustom canvas-mindmap-depth nil
  "How many levels below the root to show; nil shows them all.
A node whose children are folded away says how many with a + count."
  :type '(choice (const nil) integer))

(defcustom canvas-mindmap-direction 'right
  "Where the branches go: to the `right' of the root, to the `left',
or half to each side, `both'."
  :type '(choice (const right) (const left) (const both)))

(defcustom canvas-mindmap-edge-style 'curve
  "The edges: a `curve', a `straight' line, or an `elbow'."
  :type '(choice (const curve) (const straight) (const elbow)))

;;;; The tree

(cl-defstruct (canvas-mindmap-node (:include canvas-diagram-node)
                                   (:constructor canvas-mindmap-node-create))
  children branch collapsed (extent 0))

(defun canvas-mindmap--options (rest)
  "Split REST, a spec after its label, into (PLIST . CHILDREN).
Options are keyword-value pairs before the children: :kind, :note,
:icon and :pos, where the node came from in its buffer."
  (let (plist)
    (while (keywordp (car rest))
      (unless (memq (car rest) '(:kind :note :icon :pos))
        (error "canvas-mindmap: unknown option %S" (car rest)))
      (push (pop rest) plist)
      (push (pop rest) plist))
    (cons (nreverse plist) rest)))

(defun canvas-mindmap--tree (spec)
  "Node tree from SPEC, a nested list (LABEL [OPTION VALUE]... CHILD...)."
  (unless (stringp (car spec))
    (error "canvas-mindmap: label %S is not a string" (car spec)))
  (pcase-let ((`(,plist . ,children) (canvas-mindmap--options (cdr spec))))
    (canvas-mindmap-node-create
     :label (car spec)
     :kind (plist-get plist :kind)
     :note (plist-get plist :note)
     :icon (plist-get plist :icon)
     :pos (plist-get plist :pos)
     :children (mapcar #'canvas-mindmap--tree children))))

(defun canvas-mindmap--shown-children (node)
  "NODE's children, unless NODE is collapsed, when there are none to show."
  (unless (canvas-mindmap-node-collapsed node)
    (canvas-mindmap-node-children node)))

(defun canvas-mindmap--hidden (node)
  "How many children NODE hides: all of them when collapsed, else none."
  (if (canvas-mindmap-node-collapsed node)
      (length (canvas-mindmap-node-children node))
    0))

(defun canvas-mindmap--walk (node fn)
  "Call FN on NODE and on every node shown below it."
  (funcall fn node)
  (dolist (child (canvas-mindmap--shown-children node))
    (canvas-mindmap--walk child fn)))

(defun canvas-mindmap--walk-all (node fn)
  "Call FN on NODE and on every node below it, collapsed or not."
  (funcall fn node)
  (dolist (child (canvas-mindmap-node-children node))
    (canvas-mindmap--walk-all child fn)))

(defun canvas-mindmap--number-branches (root)
  "Number ROOT's children from 0; each subtree inherits its number.
Return ROOT."
  (let ((branch 0))
    (dolist (child (canvas-mindmap-node-children root))
      (canvas-mindmap--walk-all child (lambda (n) (setf (canvas-mindmap-node-branch n) branch)))
      (setq branch (1+ branch))))
  root)

(defun canvas-mindmap--path (root node)
  "Labels from ROOT down to NODE, or nil when NODE is not below ROOT."
  (if (eq root node)
      (list (canvas-mindmap-node-label root))
    (cl-some (lambda (child)
               (when-let* ((path (canvas-mindmap--path child node)))
                 (cons (canvas-mindmap-node-label root) path)))
             (canvas-mindmap-node-children root))))

(defun canvas-mindmap--prune (node depth)
  "Collapse the nodes DEPTH levels below NODE; nil collapses none.
Return NODE."
  (let ((children (canvas-mindmap-node-children node)))
    (if (and depth (<= depth 0))
        (setf (canvas-mindmap-node-collapsed node) (and children t))
      (dolist (child children)
        (canvas-mindmap--prune child (and depth (1- depth))))))
  node)

(defun canvas-mindmap--node-text (node)
  "What NODE's box says: its label, and how many children it hides."
  (if (zerop (canvas-mindmap--hidden node))
      (canvas-mindmap-node-label node)
    (format "%s +%d" (canvas-mindmap-node-label node) (canvas-mindmap--hidden node))))

(defun canvas-mindmap--spec-depth (spec)
  "How many levels SPEC, a nested list, has below its root."
  (let ((children (cdr (canvas-mindmap--options (cdr spec)))))
    (if children
        (1+ (apply #'max (mapcar #'canvas-mindmap--spec-depth children)))
      0)))

(defun canvas-mindmap--node-by-path (root labels &optional exact)
  "The node LABELS lead to from ROOT, or the last one found on the way.
With EXACT, nil unless the whole path is found."
  (let ((node root) (found t))
    (dolist (label (cdr labels))
      (when found
        (let ((child (cl-find label (canvas-mindmap-node-children node)
                              :key #'canvas-mindmap-node-label :test #'equal)))
          (if child (setq node child) (setq found nil)))))
    (if (and exact (not found)) nil node)))

(defun canvas-mindmap--ancestry (root node)
  "The nodes from ROOT down to NODE, or nil when NODE is not below ROOT."
  (if (eq root node)
      (list root)
    (cl-some (lambda (child)
               (when-let* ((below (canvas-mindmap--ancestry child node)))
                 (cons root below)))
             (canvas-mindmap-node-children root))))

(defun canvas-mindmap--preorder (root)
  "ROOT and every node shown below it, parents before children, top to bottom."
  (let (nodes)
    (canvas-mindmap--walk root (lambda (n) (push n nodes)))
    (nreverse nodes)))

(defun canvas-mindmap--parent (root node)
  "The node NODE hangs off, or nil for ROOT."
  (cl-find-if (lambda (n) (memq node (canvas-mindmap-node-children n)))
              (canvas-mindmap--preorder root)))

(defun canvas-mindmap--depth (root node)
  "How many steps NODE is below ROOT."
  (1- (length (canvas-mindmap--path root node))))

(defun canvas-mindmap--at-depth (root depth)
  "The nodes DEPTH steps below ROOT, top to bottom."
  (sort (cl-remove-if-not (lambda (n) (= (canvas-mindmap--depth root n) depth))
                          (canvas-mindmap--preorder root))
        (lambda (a b) (< (canvas-diagram-middle-y a) (canvas-diagram-middle-y b)))))

(defun canvas-mindmap--siblings (root node)
  "NODE and the nodes that share its parent, top to bottom; ROOT alone for ROOT."
  (if-let* ((parent (canvas-mindmap--parent root node)))
      (canvas-mindmap-node-children parent)
    (list root)))

(defun canvas-mindmap--branch-of (root node)
  "The child of ROOT that NODE is or hangs under; nil for ROOT."
  (cl-find-if (lambda (branch) (canvas-mindmap--path branch node))
              (canvas-mindmap-node-children root)))

(defun canvas-mindmap--nearest (nodes y)
  "The node of NODES whose middle is nearest the drawing Y; the first on a tie."
  (cl-reduce (lambda (best n)
               (if (< (abs (- (canvas-diagram-middle-y n) y))
                      (abs (- (canvas-diagram-middle-y best) y)))
                   n
                 best))
             nodes))

;;;; Outline sources

(defvar-local canvas-mindmap-dialect nil
  "The outline source of this buffer by name, `org', `markdown',
`treesit', `imenu' or a package's; nil to take the best that applies.")

(cl-defstruct (canvas-mindmap-source (:constructor canvas-mindmap-source-create))
  "An outline source: its NAME, a symbol; WHEN, a function of no
arguments saying whether it applies to the current buffer; READ, a
function of the root's name giving the buffer's outline as a nested
spec, or nil when it finds nothing; its PRIORITY, the highest tried
first; and a line of DOC."
  name when read priority doc)

(defvar canvas-mindmap-sources nil
  "The outline sources, best first.  See `canvas-mindmap-define-source'.")

(defun canvas-mindmap--source-when (modes when)
  "The predicate of a source for MODES, WHEN, or both; an error without either."
  (cond ((and modes when) (lambda () (and (apply #'derived-mode-p modes) (funcall when))))
        (modes (lambda () (apply #'derived-mode-p modes)))
        (when when)
        (t (error "canvas-mindmap: a source needs :modes or :when"))))

(defun canvas-mindmap-define-source (name &rest props)
  "Define the outline source NAME, a symbol, from PROPS, or define it anew.
:read is the function of the root's name that gives the current
buffer's outline as a nested spec, (LABEL [:kind KIND] [:note NOTE]
[:pos POS] CHILD...), or nil when it finds nothing there, the next
source then serving; a spec built by tree-sitter comes from
`canvas-mindmap-treesit-tree'.  :modes lists the major modes the
source is for, :when is a function of no arguments saying whether it
applies to the current buffer, either or both.  :priority, 100 unless
given, ranks it above the built-in sources, markdown 50, org 40,
tree-sitter 20, imenu 10.  :doc says in a line what it reads."
  (let ((read (plist-get props :read)))
    (unless (functionp read) (error "canvas-mindmap: source %s needs a :read function" name))
    (setq canvas-mindmap-sources
          (sort (cons (canvas-mindmap-source-create
                       :name name :read read
                       :when (canvas-mindmap--source-when (plist-get props :modes) (plist-get props :when))
                       :priority (or (plist-get props :priority) 100)
                       :doc (plist-get props :doc))
                      (cl-remove name canvas-mindmap-sources :key #'canvas-mindmap-source-name))
                (lambda (a b) (> (canvas-mindmap-source-priority a) (canvas-mindmap-source-priority b)))))
    name))

(defun canvas-mindmap--sources ()
  "The sources for this buffer, best first: the one `canvas-mindmap-dialect'
names, else those that apply."
  (if canvas-mindmap-dialect
      (list (or (cl-find canvas-mindmap-dialect canvas-mindmap-sources :key #'canvas-mindmap-source-name)
                (error "canvas-mindmap: no outline source %S" canvas-mindmap-dialect)))
    (cl-remove-if-not (lambda (s) (funcall (canvas-mindmap-source-when s))) canvas-mindmap-sources)))

(defun canvas-mindmap--dialect ()
  "The name of this buffer's outline source, the best that applies."
  (if-let* ((source (car (canvas-mindmap--sources))))
      (canvas-mindmap-source-name source)
    (error "canvas-mindmap: no outline source for %s" (buffer-name))))

;;;; Outlines: org and markdown

(defconst canvas-mindmap--dialects
  '((org . (:heading "^\\(\\*+\\) +\\(.*?\\)[ \t]*$"
            :fence "^[ \t]*#\\+\\(?:begin\\|end\\)_"
            :keyword "^[ \t]*#\\+"))
    (markdown . (:heading "^\\(#+\\)[ \t]+\\(.*?\\)[ \t#]*$"
                 :fence "^[ \t]*\\(?:```\\|~~~\\)"
                 :keyword "^[ \t]*<!--")))
  "Per dialect: the heading, whose level is the first group's length,
the line that opens or closes a code block, and a line to leave out.")

(defun canvas-mindmap--dialect-of-name (name)
  "`markdown' for a NAME ending in .md or .markdown, else `org'."
  (if (and name (string-match-p "\\.\\(?:md\\|markdown\\)\\'" name)) 'markdown 'org))

(defun canvas-mindmap--dialect-re (dialect key)
  "The regexp KEY of DIALECT, `org' or `markdown'."
  (or (plist-get (alist-get dialect canvas-mindmap--dialects) key)
      (error "canvas-mindmap: no dialect %S" dialect)))

(defconst canvas-mindmap--tags-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)$")
(defconst canvas-mindmap--item-re
  "^\\([ \t]*\\)\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(.*?\\)[ \t]*$"
  "A plain list item: its indentation, then its text.")
(defconst canvas-mindmap--org-link-re
  "\\[\\[\\([^][]*\\)\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
  "An org link: the target, then the description if there is one.")
(defconst canvas-mindmap--md-link-re "\\[\\([^]]*\\)\\](\\([^)]*\\))"
  "A markdown link: the text, then the target.")

(defun canvas-mindmap--clean-label (text)
  "TEXT as a label: links reduced to their description, spaces squeezed."
  (string-trim
   (replace-regexp-in-string
    "[ \t]+" " "
    (replace-regexp-in-string
     canvas-mindmap--md-link-re "\\1"
     (replace-regexp-in-string canvas-mindmap--org-link-re
                               (lambda (link) (or (match-string 2 link) (match-string 1 link)))
                               text)))))

(defun canvas-mindmap--todo-keywords ()
  "The buffer's org todo keywords, or the two every org buffer has."
  (or (bound-and-true-p org-todo-keywords-1) '("TODO" "DONE")))

(defun canvas-mindmap--parse-title (title)
  "Split TITLE into (LABEL KIND).
A leading todo keyword is the kind, lower-cased; failing that, the
first tag."
  (let (kind)
    (when (string-match canvas-mindmap--tags-re title)
      ;; Read the match before `split-string' runs a search of its own.
      (let ((tags (match-string 1 title))
            (cut (match-beginning 0)))
        (setq kind (car (split-string tags ":" t))
              title (substring title 0 cut))))
    (when (and (string-match "^\\([[:upper:]]+\\)[ \t]+" title)
               (member (match-string 1 title) (canvas-mindmap--todo-keywords)))
      (setq kind (downcase (match-string 1 title))
            title (substring title (match-end 0))))
    (list (canvas-mindmap--clean-label title) kind)))

(defvar canvas-mindmap--with-positions nil
  "Whether specs read from a buffer carry :pos, where each node starts.
Bound while a map that follows a buffer reads it.")

(defun canvas-mindmap-pos-option (pos)
  "The :pos option for a node starting at POS, when positions are wanted:
\(:pos POS) then, else nil.  A source appends it to a node's spec so the
map can follow the node to its place in the buffer."
  (and canvas-mindmap--with-positions pos (list :pos pos)))

(defun canvas-mindmap--item-spec (text pos)
  "Spec of the list item TEXT, starting at POS: its label, a checkbox as its kind."
  (let ((kind (cond ((string-match "^\\[[Xx]\\][ \t]+" text) "done")
                    ((string-match "^\\[[ -]\\][ \t]+" text) "todo"))))
    (when kind
      (setq text (substring text (match-end 0))))
    (append (list (canvas-mindmap--clean-label text))
            (and kind (list :kind kind))
            (canvas-mindmap-pos-option pos))))

(defun canvas-mindmap--body-items (lines keyword-re)
  "Split LINES, a heading's body as (POS . TEXT) each, into (NOTE . ITEMS).
Plain list items become nested specs by their indentation; the other
lines, less blanks and those matching KEYWORD-RE, make the note."
  (let ((root (list "items")) stack note)
    (pcase-dolist (`(,pos . ,line) lines)
      (cond ((string-match canvas-mindmap--item-re line)
             (let* ((indent (length (match-string 1 line)))
                    (item (canvas-mindmap--item-spec (match-string 2 line) pos)))
               (while (and stack (>= (caar stack) indent))
                 (pop stack))
               (setcdr (last (if stack (cdar stack) root)) (list item))
               (push (cons indent item) stack)))
            ((string-match-p keyword-re line))
            ((string-blank-p line))
            (t (push (string-trim line) note))))
    (cons (and note (mapconcat #'identity (nreverse note) "\n"))
          (cdr root))))

(defun canvas-mindmap--lines (dialect)
  "The buffer's lines as (POS . TEXT), those inside DIALECT's code blocks left out."
  (let ((fence-re (canvas-mindmap--dialect-re dialect :fence))
        (in-fence nil)
        lines)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((text (buffer-substring-no-properties (point) (line-end-position))))
          (cond ((string-match-p fence-re text) (setq in-fence (not in-fence)))
                (in-fence)
                (t (push (cons (point) text) lines))))
        (forward-line 1)))
    (nreverse lines)))

(defun canvas-mindmap--headings (dialect)
  "List of (LEVEL LABEL KIND NOTE ITEMS POS) for the buffer's headings,
in order, org or markdown by DIALECT.  ITEMS are the specs of the plain
list items under the heading, POS is where it starts."
  (let ((heading-re (canvas-mindmap--dialect-re dialect :heading))
        (keyword-re (canvas-mindmap--dialect-re dialect :keyword))
        headings current body)
    (cl-flet ((finish ()
                (when current
                  (let ((parsed (canvas-mindmap--body-items (nreverse body) keyword-re)))
                    (push (append current (list (car parsed) (cdr parsed) (car (last current))))
                          headings)))))
      (pcase-dolist (`(,pos . ,text) (canvas-mindmap--lines dialect))
        (if (string-match heading-re text)
            (let ((level (length (match-string 1 text)))
                  (title (canvas-mindmap--parse-title (match-string 2 text))))
              (finish)
              (setq current (list level (car title) (cadr title) pos)
                    body nil))
          (when current
            (push (cons pos text) body))))
      (finish))
    (mapcar (lambda (h) (pcase-let ((`(,level ,label ,kind ,_pos ,note ,items ,pos) h))
                          (list level label kind note items pos)))
            (nreverse headings))))

(defun canvas-mindmap--heading-spec (heading)
  "Spec (LABEL [:kind KIND] [:note NOTE] [:pos POS] ITEM...) of HEADING,
a (LEVEL LABEL KIND NOTE ITEMS POS).  Sub-headings are appended later."
  (pcase-let ((`(,_ ,label ,kind ,note ,items ,pos) heading))
    (append (list label)
            (and kind (list :kind kind))
            (and note (list :note note))
            (canvas-mindmap-pos-option pos)
            items)))

(defun canvas-mindmap--forest (headings)
  "Nested specs from HEADINGS, each a (LEVEL LABEL KIND NOTE ITEMS POS).
A heading hangs off the nearest earlier heading of a shallower level."
  (let ((root (list "forest"))
        stack)                          ; (LEVEL . SPEC), innermost first
    (dolist (heading headings)
      (let ((node (canvas-mindmap--heading-spec heading))
            (level (car heading)))
        (while (and stack (>= (caar stack) level))
          (pop stack))
        (setcdr (last (if stack (cdar stack) root)) (list node))
        (push (cons level node) stack)))
    (cdr root)))

(defconst canvas-mindmap--category-kinds
  '(("functions" . "function") ("classes" . "class") ("variables" . "variable")
    ("types" . "type") ("methods" . "method") ("structs" . "struct")
    ("modules" . "module") ("macros" . "macro") ("interfaces" . "interface")
    ("enums" . "enum") ("constants" . "constant") ("sections" . "section"))
  "Imenu category names, lower-cased, and the kind they stand for.")

(defun canvas-mindmap--category-kind (name)
  "The kind an imenu category NAME stands for: its singular, lower-cased."
  (let ((lower (downcase name)))
    (or (cdr (assoc lower canvas-mindmap--category-kinds))
        (replace-regexp-in-string "s\\'" "" lower))))

(defconst canvas-mindmap--imenu-kinds
  '(("Module" . "module") ("Namespace" . "scope") ("Interface" . "port") ("Event" . "signal")
    ("Constant" . "constant") ("Enum" . "type") ("EnumMember" . "literal") ("Variable" . "variable")
    ("Function" . "function") ("Method" . "method") ("Class" . "class") ("Struct" . "struct")
    ("Field" . "field") ("Property" . "property") ("Package" . "package") ("TypeParameter" . "type"))
  "The kinds imenu puts on its entries as a property, and the kind each is here.")

(defun canvas-mindmap--imenu-name (name)
  "What an imenu entry NAME says: (LABEL KIND), KIND from the kind imenu
may put on the name as a property, nil when there is none."
  (unless (stringp name) (error "canvas-mindmap: odd imenu entry named %S" name))
  (let ((property (get-text-property 0 'imenu-kind name)))
    (list (substring-no-properties name)
          (and property (or (cdr (assoc property canvas-mindmap--imenu-kinds)) (downcase property))))))

(defun canvas-mindmap--imenu-position (entry)
  "Where imenu ENTRY starts: its position, or its region's start for one
holding entries; nil when neither is known."
  (let ((rest (cdr entry)))
    (cond ((integerp rest) rest)
          ((markerp rest) (marker-position rest))
          (t (car (get-text-property 0 'imenu-region (car entry)))))))

(defun canvas-mindmap--imenu-entry-p (entry)
  "Whether ENTRY is one the buffer put in the index, not one imenu keeps
for itself: the nil it marks an empty index with, or its `*Rescan*' item."
  (and entry (not (equal (car entry) "*Rescan*"))))

(defun canvas-mindmap--imenu-specs (index kind)
  "Specs of the imenu INDEX entries, each of KIND unless its name says
another or it is a category.  An entry is (NAME . POSITION) or (NAME .
ENTRIES); a category is the latter, named for a kind its entries take.
The items imenu keeps for itself are none of the map's."
  (let (specs)
    (dolist (entry index)
      (when (canvas-mindmap--imenu-entry-p entry)
        (let ((rest (cdr entry)))
          (pcase-let ((`(,label ,own) (canvas-mindmap--imenu-name (car entry))))
            (push (cond ((or (integerp rest) (markerp rest))
                         (append (list label)
                                 (when-let* ((k (or own kind))) (list :kind k))
                                 (canvas-mindmap-pos-option (canvas-mindmap--imenu-position entry))))
                        ((listp rest)
                         (let ((sub-kind (or kind (and (not own) (canvas-mindmap--category-kind label)))))
                           (append (list label)
                                   (list :kind (or own sub-kind))
                                   (canvas-mindmap-pos-option (canvas-mindmap--imenu-position entry))
                                   (canvas-mindmap--imenu-specs rest sub-kind))))
                        (t (error "canvas-mindmap: odd imenu entry %S" entry)))
                  specs)))))
    (nreverse specs)))

(defun canvas-mindmap--imenu-tree (root-name)
  "Nested spec of the buffer's imenu index under ROOT-NAME, or nil without one."
  (when-let* ((index (condition-case nil
                         (let ((imenu-auto-rescan t))
                           (imenu--make-index-alist t))
                       (imenu-unavailable nil)))
              (specs (canvas-mindmap--imenu-specs index nil)))
    (cons root-name specs)))

(canvas-mindmap-define-source
 'imenu :priority 10 :doc "Code, by its imenu index"
 :when (lambda () (derived-mode-p 'prog-mode)) :read #'canvas-mindmap--imenu-tree)

(canvas-mindmap-define-source
 'markdown :priority 50 :doc "Markdown headings and lists"
 :when (lambda () (or (derived-mode-p 'markdown-mode 'markdown-ts-mode)
                      (eq (canvas-mindmap--dialect-of-name buffer-file-name) 'markdown)))
 :read (lambda (root-name) (canvas-mindmap--outline-tree 'markdown root-name)))

(canvas-mindmap-define-source
 'org :priority 40 :doc "Org headings and lists; any text that is not code"
 :when (lambda () (or (derived-mode-p 'org-mode) (not (derived-mode-p 'prog-mode))))
 :read (lambda (root-name) (canvas-mindmap--outline-tree 'org root-name)))

;;;; Outlines: tree-sitter

(defcustom canvas-mindmap-treesit-node-re
  "_\\(?:declaration\\|definition\\|body\\|statement\\|specification\\|specifier\\)\\'"
  "Regexp for the tree-sitter node types that are nodes of the map, when
they carry a name; what precedes the match is the kind, `process' for
`process_statement'."
  :type 'regexp)

(defcustom canvas-mindmap-treesit-name-types
  '("identifier" "identifier_list" "designator" "field_identifier" "property_identifier")
  "Child node types that name a node whose grammar gives it no `name' or
`declarator' field, tried in this order; the node must have one such
child, not several."
  :type '(repeat string))

(defcustom canvas-mindmap-treesit-skip-kinds '("parameter")
  "Kinds from tree-sitter node types that are no nodes of the map."
  :type '(repeat string))

(defcustom canvas-mindmap-treesit-kind-names nil
  "Kinds from tree-sitter node types renamed: ((KIND . NAME)...), with
\(\"full type\" . \"type\") say."
  :type '(alist :key-type string :value-type string))

(defvar canvas-mindmap--treesit-rules nil
  "The rules of the tree-sitter reading under way, a plist; see
`canvas-mindmap-treesit-tree'.")

(defun canvas-mindmap--treesit-rule (key &optional default)
  "The rule KEY of the reading under way, else DEFAULT."
  (if (plist-member canvas-mindmap--treesit-rules key)
      (plist-get canvas-mindmap--treesit-rules key)
    default))

(defun canvas-mindmap--treesit-language ()
  "The tree-sitter language of this buffer, when its grammar is installed:
the reading's own, its parser's, or the one named like the mode, `vhdl'
for `vhdl-mode'."
  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
    (let ((language (cond ((canvas-mindmap--treesit-rule :language))
                          ((bound-and-true-p treesit-primary-parser)
                           (treesit-parser-language treesit-primary-parser))
                          (t (intern (replace-regexp-in-string "-\\(?:ts-\\)?mode\\'" "" (symbol-name major-mode)))))))
      (and (treesit-language-available-p language) language))))

(defun canvas-mindmap--treesit-outline-p ()
  "Whether tree-sitter is to outline this buffer: a grammar for its
language is installed and the mode has no parser of its own, whose
imenu would be tree-sitter's view already."
  (and (not (bound-and-true-p treesit-primary-parser)) (canvas-mindmap--treesit-language)))

(defun canvas-mindmap--treesit-only-child (node type)
  "NODE's child of TYPE when it has exactly one, else nil."
  (let ((children (cl-remove-if-not (lambda (c) (equal (treesit-node-type c) type))
                                    (treesit-node-children node t))))
    (and children (null (cdr children)) (car children))))

(defun canvas-mindmap--treesit-declarator (node)
  "The leaf NODE's `declarator' field comes down to, through declarators
with declarators of their own; nil without one."
  (when-let* ((declarator (treesit-node-child-by-field-name node "declarator")))
    (if (treesit-node-child-by-field-name declarator "declarator")
        (canvas-mindmap--treesit-declarator declarator)
      declarator)))

(defun canvas-mindmap--treesit-name-node (node)
  "The child naming NODE: for a statement its label, else what its `name'
field holds, what its `declarator' field comes down to, or its one child
of the first of the :name-types it has; nil when there is none."
  (if (string-suffix-p "_statement" (treesit-node-type node))
      (canvas-mindmap--treesit-only-child node "label")
    (or (treesit-node-child-by-field-name node "name")
        (canvas-mindmap--treesit-declarator node)
        (cl-loop for type in (canvas-mindmap--treesit-rule :name-types canvas-mindmap-treesit-name-types)
                 thereis (canvas-mindmap--treesit-only-child node type)))))

(defun canvas-mindmap--treesit-name (node)
  "The text naming NODE: what the :name rule says, else its naming child's,
its spaces squeezed and a trailing colon dropped; nil when nothing names it."
  (or (when-let* ((rule (canvas-mindmap--treesit-rule :name))) (funcall rule node))
      (when-let* ((name (canvas-mindmap--treesit-name-node node)))
        (string-trim (replace-regexp-in-string
                      "[ \t\n]*:\\'" "" (replace-regexp-in-string "[ \t\n]+" " " (treesit-node-text name t)))))))

(defun canvas-mindmap--treesit-kind (type)
  "The kind of a node of TYPE: what precedes the suffix, its underscores
spaces, or the name the :kind-names give it."
  (let ((kind (replace-regexp-in-string
               "_" " " (replace-regexp-in-string (canvas-mindmap--treesit-rule :node-re canvas-mindmap-treesit-node-re) "" type))))
    (or (cdr (assoc kind (canvas-mindmap--treesit-rule :kind-names canvas-mindmap-treesit-kind-names))) kind)))

(defun canvas-mindmap--treesit-passing-p (node)
  "Whether NODE only names something in passing: a specifier without a
body, `struct ctx' where a variable is declared, say."
  (and (string-suffix-p "_specifier" (treesit-node-type node))
       (not (treesit-node-child-by-field-name node "body"))))

(defun canvas-mindmap--treesit-extra (key node)
  "What the rule KEY, :note or :below, gives for NODE; nil without the rule."
  (when-let* ((rule (canvas-mindmap--treesit-rule key))) (funcall rule node)))

(defun canvas-mindmap--treesit-spec (node)
  "The spec of NODE when it is a node of the map: of a kept type, not in
passing, named, its kind not skipped; else nil.  It carries the note
the :note rule gives it, and below the grammar's own nodes those the
:below rule adds."
  (when-let* (((string-match-p (canvas-mindmap--treesit-rule :node-re canvas-mindmap-treesit-node-re) (treesit-node-type node)))
              ((not (canvas-mindmap--treesit-passing-p node)))
              (kind (canvas-mindmap--treesit-kind (treesit-node-type node)))
              ((not (member kind (canvas-mindmap--treesit-rule :skip-kinds canvas-mindmap-treesit-skip-kinds))))
              (name (canvas-mindmap--treesit-name node)))
    (append (list name :kind kind)
            (when-let* ((note (canvas-mindmap--treesit-extra :note node))) (list :note note))
            (canvas-mindmap-pos-option (treesit-node-start node))
            (canvas-mindmap--treesit-specs node)
            (canvas-mindmap--treesit-extra :below node))))

(defun canvas-mindmap--treesit-specs (node)
  "Specs of the map's nodes below NODE, itself aside, each with the ones
below it; a node that is none of the map's passes its own up."
  (cl-mapcan (lambda (child)
               (if-let* ((spec (canvas-mindmap--treesit-spec child)))
                   (list spec)
                 (canvas-mindmap--treesit-specs child)))
             (treesit-node-children node t)))

(defun canvas-mindmap--treesit-tree (root-name)
  "Nested spec of the buffer's outline by tree-sitter under ROOT-NAME, by
the rules under way, or nil when the grammar finds nothing to map.  The
text is parsed on its own, so the buffer gets no parser it did not have."
  (when-let* ((language (canvas-mindmap--treesit-language))
              (text (buffer-substring-no-properties (point-min) (point-max)))
              (specs (with-temp-buffer
                       (insert text)
                       (canvas-mindmap--treesit-specs (treesit-parser-root-node (treesit-parser-create language))))))
    (cons root-name specs)))

(defun canvas-mindmap-treesit-tree (root-name &rest rules)
  "Nested spec of the current buffer's outline by tree-sitter, under
ROOT-NAME, or nil when the grammar finds nothing.  A language package
passes RULES, a plist, for what the grammar alone does not say:
:language, the grammar to parse with, else the buffer's; :node-re,
:name-types, :skip-kinds and :kind-names in place of the
`canvas-mindmap-treesit-' settings so named; :name, a function of a
node giving its name, or nil to leave that to the grammar; :note, a
function of a node giving its note, or nil; :below, a function of a
node giving specs to hang under it after the grammar's own, or nil.
Nodes are tree-sitter nodes of a parse of the buffer's text, so their
positions hold in the buffer."
  (let ((canvas-mindmap--treesit-rules rules))
    (canvas-mindmap--treesit-tree root-name)))

(canvas-mindmap-define-source
 'treesit :priority 20 :doc "Code, by its language's tree-sitter grammar"
 :when (lambda () (and (derived-mode-p 'prog-mode) (canvas-mindmap--treesit-outline-p)))
 :read #'canvas-mindmap--treesit-tree)

(defun canvas-mindmap--outline-tree (dialect root-name)
  "Nested spec of the buffer's headings and lists by DIALECT, `org' or
`markdown'.  A single top-level heading is the root; several hang off
ROOT-NAME.  nil when there is none."
  (let ((forest (canvas-mindmap--forest (canvas-mindmap--headings dialect))))
    (cond ((null forest) nil)
          ((null (cdr forest)) (car forest))
          (t (cons root-name forest)))))

(defun canvas-mindmap--headings-tree (&optional root-name)
  "Nested spec of the buffer's outline, from the first of the sources for
the buffer, best first, that reads something; the root is named
ROOT-NAME, by default the buffer's name, when the source hangs several
top-level nodes off one.  nil when nothing reads."
  (cl-loop for source in (canvas-mindmap--sources)
           thereis (funcall (canvas-mindmap-source-read source) (or root-name (buffer-name)))))

(defun canvas-mindmap--file-tree (file)
  "Nested spec of the headings in FILE, org or markdown by its name.
Several top-level headings hang off a root named after the file."
  (or (with-temp-buffer
        (insert-file-contents file)
        (setq canvas-mindmap-dialect (canvas-mindmap--dialect-of-name file))
        (canvas-mindmap--headings-tree (file-name-nondirectory file)))
      (error "canvas-mindmap: no headings in %s" file)))

(defun canvas-mindmap--read-source (buffer)
  "The outline of BUFFER as a spec with positions, for following it."
  (with-current-buffer buffer
    (let ((canvas-mindmap--with-positions t))
      (canvas-mindmap--headings-tree (buffer-name)))))

(defun canvas-mindmap--tree-of (source)
  "Node tree of SOURCE, a nested spec or the name of an org file.
Its branches are numbered and it is folded at `canvas-mindmap-depth'."
  (canvas-mindmap--prune
   (canvas-mindmap--number-branches
    (canvas-mindmap--tree (if (stringp source)
                              (canvas-mindmap--file-tree source)
                            source)))
   canvas-mindmap-depth))

;;;; The layout

(defun canvas-mindmap--gap-x ()
  "Pixels between a node and its children, at the current spacing."
  (canvas-diagram-spaced canvas-mindmap-gap-x))

(defun canvas-mindmap--gap-y ()
  "Pixels between sibling subtrees, at the current spacing."
  (canvas-diagram-spaced canvas-mindmap-gap-y))

(defun canvas-mindmap--layout (node measure gap-x gap-y &optional direction)
  "Place NODE's subtree with its top-left corner at the origin.
Children go to the right of their parent, stacked GAP-Y apart, GAP-X
away, unless DIRECTION is `left', or `both', which puts the first half
of NODE's children right and the rest left.  MEASURE maps a text to
its box's (W . H)."
  (canvas-mindmap--size node measure)
  (pcase (or direction 'right)
    ('both (canvas-mindmap--place-both node gap-x gap-y))
    ('left (canvas-mindmap--place-side node (canvas-mindmap--shown-children node) gap-x gap-y)
           (canvas-mindmap--mirror node (car (canvas-mindmap--bounds node))))
    (_ (canvas-mindmap--place-side node (canvas-mindmap--shown-children node) gap-x gap-y))))

(defun canvas-mindmap--place-side (root children gap-x gap-y)
  "Lay ROOT out at the origin with only CHILDREN below it, to the right.
ROOT's own children are put back afterwards."
  (let ((all (canvas-mindmap-node-children root)))
    (setf (canvas-mindmap-node-children root) children)
    (unwind-protect
        (progn
          (canvas-mindmap--extent root gap-y)
          (canvas-mindmap--place root 0 0 gap-x gap-y))
      (setf (canvas-mindmap-node-children root) all))))

(defun canvas-mindmap--mirror (node width)
  "Flip NODE's subtree, so what sat at the left edge sits at WIDTH."
  (canvas-mindmap--walk node (lambda (n)
                               (setf (canvas-mindmap-node-x n)
                                     (- width (canvas-mindmap-node-x n) (canvas-mindmap-node-w n))))))

(defun canvas-mindmap--place-both (root gap-x gap-y)
  "Place ROOT's children on both sides: the first half right, the rest left."
  (let* ((children (canvas-mindmap--shown-children root))
         (half (ceiling (length children) 2))
         (right (cl-subseq children 0 half))
         (left (nthcdr half children)))
    (canvas-mindmap--place-side root right gap-x gap-y)
    (let ((root-y (canvas-mindmap-node-y root)))
      (canvas-mindmap--place-side root left gap-x gap-y)
      (let ((dy (- root-y (canvas-mindmap-node-y root)))
            (w (canvas-mindmap-node-w root)))
        (dolist (child left)
          (canvas-mindmap--walk child (lambda (n)
                                        (setf (canvas-mindmap-node-x n)
                                              (- w (canvas-mindmap-node-x n) (canvas-mindmap-node-w n))
                                              (canvas-mindmap-node-y n)
                                              (+ (canvas-mindmap-node-y n) dy))))))
      (setf (canvas-mindmap-node-y root) root-y))
    (canvas-mindmap--normalize root)))

(defun canvas-mindmap--normalize (root)
  "Shift ROOT's subtree so that its top-left corner is at the origin."
  (let ((min-x most-positive-fixnum) (min-y most-positive-fixnum))
    (canvas-mindmap--walk root (lambda (n)
                                 (setq min-x (min min-x (canvas-mindmap-node-x n))
                                       min-y (min min-y (canvas-mindmap-node-y n)))))
    (canvas-mindmap--walk root (lambda (n)
                                 (setf (canvas-mindmap-node-x n) (- (canvas-mindmap-node-x n) min-x)
                                       (canvas-mindmap-node-y n) (- (canvas-mindmap-node-y n) min-y))))))

(defun canvas-mindmap--size (node measure)
  "Give every node shown in NODE's subtree the box MEASURE says it needs."
  (let ((size (canvas-diagram-box-size node (canvas-mindmap--node-text node) measure)))
    (setf (canvas-mindmap-node-w node) (car size)
          (canvas-mindmap-node-h node) (cdr size)))
  (dolist (child (canvas-mindmap--shown-children node))
    (canvas-mindmap--size child measure)))

(defun canvas-mindmap--extent (node gap-y)
  "Store the height of NODE's subtree on every node of it and return it.
That is the node's own height or its children's stacked extents,
whichever is taller."
  (let ((children (canvas-mindmap--shown-children node)))
    (dolist (child children)
      (canvas-mindmap--extent child gap-y))
    (setf (canvas-mindmap-node-extent node)
          (max (canvas-mindmap-node-h node)
               (canvas-mindmap--stacked children gap-y)))))

(defun canvas-mindmap--stacked (nodes gap-y)
  "Height of NODES' extents stacked with GAP-Y between them."
  (if nodes
      (+ (apply #'+ (mapcar #'canvas-mindmap-node-extent nodes))
         (* gap-y (1- (length nodes))))
    0))

(defun canvas-mindmap--place (node x y gap-x gap-y)
  "Position NODE's subtree so its extent's top-left corner is at X Y.
The node is centred on its extent, and so is the stack of children."
  (let* ((extent (canvas-mindmap-node-extent node))
         (children (canvas-mindmap--shown-children node))
         (child-x (+ x (canvas-mindmap-node-w node) gap-x))
         (child-y (+ y (/ (- extent (canvas-mindmap--stacked children gap-y)) 2.0))))
    (setf (canvas-mindmap-node-x node) x
          (canvas-mindmap-node-y node) (+ y (/ (- extent (canvas-mindmap-node-h node)) 2.0)))
    (dolist (child children)
      (canvas-mindmap--place child child-x child-y gap-x gap-y)
      (cl-incf child-y (+ (canvas-mindmap-node-extent child) gap-y)))))

(defun canvas-mindmap--bounds (node)
  "(WIDTH . HEIGHT) of the laid-out subtree at NODE."
  (canvas-diagram--bounds (canvas-mindmap--preorder node)))

;;;; Colours, edges, trail

(defun canvas-mindmap--branch-rgb (branch)
  "(R G B) filling the boxes of BRANCH, a number; the node colour for nil."
  (if branch
      (canvas-diagram-palette-rgb branch canvas-mindmap-branch-colors)
    (canvas-diagram-color :node)))

(defun canvas-mindmap--edge-ends (from to)
  "(X0 Y0 X1 Y1) of an edge: from the side of FROM that faces TO, at
its middle, to the side of TO that faces FROM."
  (let ((rightward (> (canvas-diagram-middle-x to) (canvas-diagram-middle-x from))))
    (list (+ (canvas-mindmap-node-x from) (if rightward (canvas-mindmap-node-w from) 0))
          (canvas-diagram-middle-y from)
          (+ (canvas-mindmap-node-x to) (if rightward 0 (canvas-mindmap-node-w to)))
          (canvas-diagram-middle-y to))))

(defun canvas-mindmap--edge (ctx from to)
  "Stroke the edge from FROM to TO in the current `canvas-mindmap-edge-style'."
  (pcase-let* ((`(,x0 ,y0 ,x1 ,y1) (canvas-mindmap--edge-ends from to))
               (mid (/ (+ x0 x1) 2.0)))
    (canvas-cairo-move-to ctx x0 y0)
    (pcase canvas-mindmap-edge-style
      ('straight (canvas-cairo-line-to ctx x1 y1))
      ('elbow (canvas-cairo-line-to ctx mid y0)
              (canvas-cairo-line-to ctx mid y1)
              (canvas-cairo-line-to ctx x1 y1))
      (_ (canvas-cairo-curve-to ctx mid y0 mid y1 x1 y1)))
    (canvas-cairo-stroke ctx)))

(defun canvas-mindmap--draw-edges (ctx node)
  "Stroke the curves from NODE to each shown child, recursively."
  (dolist (child (canvas-mindmap--shown-children node))
    (canvas-mindmap--edge ctx node child)
    (canvas-mindmap--draw-edges ctx child)))

(defun canvas-mindmap--draw-trail (ctx root node width)
  "Draw the edges from ROOT down to NODE again in the selection colour,
WIDTH pixels wide, so the way to the keyboard's node stands out."
  (let ((ancestry (canvas-mindmap--ancestry root node)))
    (canvas-diagram-set-rgb ctx (canvas-diagram-color :selection))
    (canvas-cairo-set-line-width ctx width)
    (cl-loop for (from to) on ancestry while to
             do (canvas-mindmap--edge ctx from to))))

(defun canvas-mindmap--legend-entries (root)
  "The legend's rows for the branches, (LABEL RGB fill) each, when there
are several; the kinds are the diagram's business."
  (let ((children (canvas-mindmap-node-children root)))
    (when (cdr children)
      (mapcar (lambda (c) (list (canvas-mindmap-node-label c)
                                (canvas-mindmap--branch-rgb (canvas-mindmap-node-branch c))
                                'fill))
              children))))

(defun canvas-mindmap--card-text (root node)
  "The card's (TITLE PATH BODY) for NODE: its label, where it hangs, its note."
  (list (canvas-mindmap-node-label node)
        (mapconcat #'identity (butlast (canvas-mindmap--path root node)) " › ")
        (or (canvas-mindmap-node-note node)
            (pcase (length (canvas-mindmap-node-children node))
              (0 "A leaf, without a note.")
              (1 "One child, no note.")
              (n (format "%d children, no note." n))))))

;;;; Folding

(defvar-local canvas-mindmap--folds nil
  "Alist of label paths to `collapsed' or `expanded': the user's own
folds, applied again whenever the tree is built, over the level limit.")

(defun canvas-mindmap--shown-node (root node)
  "NODE if it is shown, else the folded ancestor that hides it."
  (or (cl-find-if #'canvas-mindmap-node-collapsed (butlast (canvas-mindmap--ancestry root node)))
      node))

(defun canvas-mindmap--apply-folds (root)
  "Collapse or expand the nodes of ROOT the user has folded by hand."
  (pcase-dolist (`(,path . ,fold) canvas-mindmap--folds)
    (when-let* ((node (canvas-mindmap--node-by-path root path t)))
      (setf (canvas-mindmap-node-collapsed node)
            (and (eq fold 'collapsed) (canvas-mindmap-node-children node) t))))
  root)

(defun canvas-mindmap--root ()
  "The root of the tree this buffer shows."
  (canvas-diagram-current-model))

(defun canvas-mindmap--set-fold (node how)
  "Collapse or expand NODE, HOW being `collapsed' or `expanded', and
remember that as the user's choice for its path."
  (when (canvas-mindmap-node-children node)
    (setf (canvas-mindmap-node-collapsed node) (eq how 'collapsed))
    (setf (alist-get (canvas-mindmap--path (canvas-mindmap--root) node) canvas-mindmap--folds
                     nil nil #'equal)
          how)))

(defun canvas-mindmap--fold (node how)
  "Fold NODE as `canvas-mindmap--set-fold' does, then lay out again with
the keyboard on the nearest node still shown."
  (canvas-mindmap--set-fold node how)
  (canvas-diagram-set-selected
   (canvas-mindmap--shown-node (canvas-mindmap--root) (canvas-diagram-selected)))
  (canvas-diagram-relayout))

(defun canvas-mindmap--fold-below (node how)
  "Fold every node below NODE, not NODE itself, as HOW says."
  (dolist (child (canvas-mindmap-node-children node))
    (canvas-mindmap--set-fold child how)
    (canvas-mindmap--fold-below child how)))

(defun canvas-mindmap--children-folded-p (node)
  "Whether every child of NODE that has children of its own is folded."
  (cl-every (lambda (child) (or (null (canvas-mindmap-node-children child))
                                (canvas-mindmap-node-collapsed child)))
            (canvas-mindmap-node-children node)))

(defun canvas-mindmap-toggle-collapse (&optional node)
  "Fold the keyboard's node, or NODE, hiding its children; or unfold it."
  (interactive)
  (let ((node (or node (canvas-diagram-selected))))
    (cond ((null (canvas-mindmap-node-children node))
           (message "canvas-mindmap: %s has nothing to fold" (canvas-mindmap-node-label node)))
          ((canvas-mindmap-node-collapsed node) (canvas-mindmap--fold node 'expanded))
          (t (canvas-mindmap--fold node 'collapsed)))))

(defun canvas-mindmap-cycle-node ()
  "Cycle the keyboard's node as magit cycles a section: folded, then
its children with their own children folded, then everything below."
  (interactive)
  (let ((node (canvas-diagram-selected)))
    (cond ((null (canvas-mindmap-node-children node))
           (message "canvas-mindmap: %s has nothing to fold" (canvas-mindmap-node-label node)))
          ((canvas-mindmap-node-collapsed node)
           (canvas-mindmap--fold-below node 'collapsed)
           (canvas-mindmap--fold node 'expanded))
          ((canvas-mindmap--children-folded-p node)
           (canvas-mindmap--fold-below node 'expanded)
           (canvas-mindmap--fold node 'expanded))
          (t (canvas-mindmap--fold node 'collapsed)))))

(defun canvas-mindmap--fold-to-level (node levels)
  "Fold NODE's subtree so that LEVELS levels show, NODE's own being the first."
  (cl-labels ((fold (n depth)
                (canvas-mindmap--set-fold n (if (>= depth (1- levels)) 'collapsed 'expanded))
                (dolist (child (canvas-mindmap-node-children n))
                  (fold child (1+ depth)))))
    (fold node 0)))

(defun canvas-mindmap-show-level (levels)
  "Show LEVELS levels of the keyboard's node, its own being the first,
as a digit does for a magit section."
  (interactive "p")
  (canvas-mindmap--fold-to-level (canvas-diagram-selected) levels)
  (canvas-diagram-set-selected
   (canvas-mindmap--shown-node (canvas-mindmap--root) (canvas-diagram-selected)))
  (canvas-diagram-relayout))

(defun canvas-mindmap-show-all-level (levels)
  "Show LEVELS levels of the whole map, the root's being the first,
as a meta digit does for a magit buffer.  Folds made by hand are let go."
  (interactive "p")
  (setq canvas-mindmap--folds nil
        canvas-mindmap-depth (1- levels))
  (canvas-diagram-rebuild))

(defmacro canvas-mindmap--define-level-commands ()
  "Define the commands digits 1 to 4 and their meta forms run."
  `(progn
     ,@(cl-loop for n from 1 to 4
                collect `(defun ,(intern (format "canvas-mindmap-show-level-%d" n)) ()
                           ,(format "Show %d level%s of the keyboard's node." n (if (= n 1) "" "s"))
                           (interactive)
                           (canvas-mindmap-show-level ,n))
                collect `(defun ,(intern (format "canvas-mindmap-show-all-level-%d" n)) ()
                           ,(format "Show %d level%s of the whole map." n (if (= n 1) "" "s"))
                           (interactive)
                           (canvas-mindmap-show-all-level ,n)))))

(canvas-mindmap--define-level-commands)

(defun canvas-mindmap--levels ()
  "How many levels the map has in all."
  (canvas-mindmap--spec-depth (canvas-diagram-spec canvas-diagram--diagram)))

(defun canvas-mindmap-cycle-global ()
  "Cycle the whole map through its levels, as magit cycles all sections:
one level, two, and so on, then all of them, then one again."
  (interactive)
  (let ((levels (canvas-mindmap--levels)))
    (setq canvas-mindmap-depth
          (cond ((null canvas-mindmap-depth) 1)
                ((>= (1+ canvas-mindmap-depth) levels) nil)
                (t (1+ canvas-mindmap-depth))))
    (canvas-diagram-rebuild)))

(defun canvas-mindmap-shallower ()
  "Show one level fewer, down to the branches."
  (interactive)
  (setq canvas-mindmap-depth (max 1 (1- (or canvas-mindmap-depth (canvas-mindmap--levels)))))
  (canvas-diagram-rebuild))

(defun canvas-mindmap-deeper ()
  "Show one level more, up to all of them."
  (interactive)
  (when canvas-mindmap-depth
    (let ((depth (1+ canvas-mindmap-depth)))
      (setq canvas-mindmap-depth (and (< depth (canvas-mindmap--levels)) depth)))
    (canvas-diagram-rebuild)))

;;;; Filtering by kind

(defvar-local canvas-mindmap--hidden-kinds nil
  "The kinds of node this map leaves out, as strings.")

(defun canvas-mindmap--keep-p (node hidden)
  "Whether NODE stays in a map without the kinds HIDDEN: its own kind is
shown, or something below it stays.  Its subtree is filtered first."
  (canvas-mindmap--filter-kinds node hidden)
  (or (not (member (canvas-mindmap-node-kind node) hidden))
      (and (canvas-mindmap-node-children node) t)))

(defun canvas-mindmap--filter-kinds (node hidden)
  "Take the nodes of the kinds HIDDEN out of NODE's tree, save those on the
way to nodes kept.  Return NODE."
  (setf (canvas-mindmap-node-children node)
        (cl-remove-if-not (lambda (child) (canvas-mindmap--keep-p child hidden))
                          (canvas-mindmap-node-children node)))
  node)

(defun canvas-mindmap--kinds-in-spec (spec)
  "The kinds in SPEC with how many nodes have each: ((KIND . COUNT)...), in
order of first appearance."
  (let (counts)
    (cl-labels ((walk (node)
                  (pcase-let ((`(,plist . ,children) (canvas-mindmap--options (cdr node))))
                    (when-let* ((kind (plist-get plist :kind)))
                      (if-let* ((entry (assoc kind counts)))
                          (cl-incf (cdr entry))
                        (push (cons kind 1) counts)))
                    (mapc #'walk children))))
      (walk spec))
    (nreverse counts)))

(defun canvas-mindmap--kind-keys (kinds)
  "A key under K for each of KINDS, ((KIND . KEY)...): the first letter of
its name not taken by an earlier kind, o being the focus key's, else a
digit."
  (let ((taken (list ?o)) keys)
    (dolist (kind kinds)
      (let ((letter (or (cl-find-if (lambda (c) (and (string-match-p "[a-z]" (string c)) (not (memq c taken))))
                                    (downcase kind))
                        (cl-find-if (lambda (c) (not (memq c taken))) "123456789"))))
        (push letter taken)
        (push (cons kind (format "K %c" letter)) keys)))
    (nreverse keys)))

(defun canvas-mindmap-toggle-kind (kind)
  "Leave the nodes of KIND out of the map, or take them back in."
  (interactive (list (completing-read "Kind: " (mapcar #'car (canvas-mindmap--kinds-in-spec
                                                             (canvas-diagram-spec canvas-diagram--diagram))))))
  (setq canvas-mindmap--hidden-kinds
        (if (member kind canvas-mindmap--hidden-kinds)
            (remove kind canvas-mindmap--hidden-kinds)
          (cons kind canvas-mindmap--hidden-kinds)))
  (canvas-diagram-rebuild))

(defun canvas-mindmap-show-all-kinds ()
  "Take every kind back into the map."
  (interactive)
  (setq canvas-mindmap--hidden-kinds nil)
  (canvas-diagram-rebuild))

(defun canvas-mindmap--kinds ()
  "The kinds of this map, in order of first appearance."
  (mapcar #'car (canvas-mindmap--kinds-in-spec (canvas-diagram-spec canvas-diagram--diagram))))

(defun canvas-mindmap--focused-kind ()
  "The one kind the map shows, when every other is hidden; else nil."
  (let ((shown (cl-set-difference (canvas-mindmap--kinds) canvas-mindmap--hidden-kinds :test #'equal)))
    (and shown (null (cdr shown)) (cdr (canvas-mindmap--kinds)) (car shown))))

(defun canvas-mindmap-focus-next-kind ()
  "Show one kind alone: the first, or the one after the kind shown alone
now, round and round."
  (interactive)
  (let* ((kinds (canvas-mindmap--kinds))
         (focused (canvas-mindmap--focused-kind))
         (next (or (and focused (cadr (member focused kinds))) (car kinds))))
    (unless next
      (user-error "canvas-mindmap: this map has no kinds"))
    (setq canvas-mindmap--hidden-kinds (remove next kinds))
    (canvas-diagram-rebuild)))

(defun canvas-mindmap--kind-command (kind)
  "A command toggling KIND, named for it, so the menu has one to bind."
  (let ((symbol (intern (format "canvas-mindmap-toggle-kind/%s" kind))))
    (unless (fboundp symbol)
      (defalias symbol (lambda () (interactive) (canvas-mindmap-toggle-kind kind))
        (format "Leave the nodes of kind %s out of the map, or take them back in." kind)))
    symbol))

(defun canvas-mindmap--kind-suffixes ()
  "The menu's suffixes for this map's kinds: one toggling each, saying
whether it shows and how many nodes it has, and one showing them all."
  (let* ((counts (canvas-mindmap--kinds-in-spec (canvas-diagram-spec canvas-diagram--diagram)))
         (keys (canvas-mindmap--kind-keys (mapcar #'car counts))))
    (append (mapcar (lambda (entry)
                      (let ((kind (car entry)))
                        (list (cdr (assoc kind keys)) (canvas-mindmap--kind-command kind)
                              :transient t
                              :description (lambda ()
                                             (format "%s %s (%d)"
                                                     (if (member kind canvas-mindmap--hidden-kinds) "□" "■")
                                                     kind (cdr entry))))))
                    counts)
            (list (list "K o" #'canvas-mindmap-focus-next-kind :transient t
                        :description (lambda ()
                                       (if-let* ((kind (canvas-mindmap--focused-kind)))
                                           (format "only %s; next" kind)
                                         "only the next kind")))
                  (list "K K" #'canvas-mindmap-show-all-kinds :transient t :description "every kind again")))))

(defun canvas-mindmap--kind-children (_children)
  "The menu's Kinds group, made when the menu opens, of this map's kinds."
  (transient-parse-suffixes 'canvas-mindmap-menu (canvas-mindmap--kind-suffixes)))

;;;; The diagram: how the tree plugs into canvas-diagram

(defun canvas-mindmap--build (_diagram spec)
  "The tree SPEC describes, less the kinds hidden, folded at the level
limit and by hand."
  (canvas-mindmap--apply-folds
   (canvas-mindmap--filter-kinds (canvas-mindmap--tree-of spec) canvas-mindmap--hidden-kinds)))

(defun canvas-mindmap--lay-out (diagram ctx)
  "Lay DIAGRAM's tree out on CTX; the nodes shown, in reading order."
  (let ((root (canvas-diagram-model diagram)))
    (canvas-mindmap--layout root (canvas-diagram-measure ctx)
                            (canvas-mindmap--gap-x) (canvas-mindmap--gap-y)
                            canvas-mindmap-direction)
    (canvas-mindmap--preorder root)))

(defun canvas-mindmap--move (diagram node direction)
  "The node DIRECTION leads to from NODE in DIAGRAM's tree, or nil.
Going into a folded node unfolds it first."
  (let ((root (canvas-diagram-model diagram)))
    (pcase direction
      ('in (when-let* ((children (canvas-mindmap-node-children node)))
             (when (canvas-mindmap-node-collapsed node)
               (canvas-mindmap--fold node 'expanded))
             (canvas-mindmap--nearest children (canvas-diagram-middle-y node))))
      ('out (canvas-mindmap--parent root node))
      ('next (canvas-diagram-neighbour (canvas-mindmap--preorder root) node 1))
      ('previous (canvas-diagram-neighbour (canvas-mindmap--preorder root) node -1))
      ('next-at-depth (canvas-diagram-neighbour
                       (canvas-mindmap--at-depth root (canvas-mindmap--depth root node)) node 1))
      ('previous-at-depth (canvas-diagram-neighbour
                           (canvas-mindmap--at-depth root (canvas-mindmap--depth root node)) node -1))
      ('next-sibling (canvas-diagram-neighbour (canvas-mindmap--siblings root node) node 1))
      ('previous-sibling (canvas-diagram-neighbour (canvas-mindmap--siblings root node) node -1))
      ('branch (canvas-mindmap--branch-move root node -1))
      ('next-branch (canvas-mindmap--branch-move root node 1))
      ('first root)
      ('last (car (last (canvas-mindmap--preorder root)))))))

(defun canvas-mindmap--branch-move (root node step)
  "The top-level branch NODE is under; from a branch, or the root, STEP
branches on.  As `beginning-of-defun' and `end-of-defun' move."
  (let ((branches (canvas-mindmap-node-children root))
        (branch (canvas-mindmap--branch-of root node)))
    (cond ((null branches) nil)
          ((null branch) (car branches))
          ((or (> step 0) (eq branch node)) (canvas-diagram-neighbour branches branch step))
          (t branch))))

(defun canvas-mindmap--restore (diagram key)
  "The node at KEY, a label path, in DIAGRAM's rebuilt tree, or the
nearest one shown."
  (let ((root (canvas-diagram-model diagram)))
    (canvas-mindmap--shown-node root (canvas-mindmap--node-by-path root key))))

(defconst canvas-mindmap--callbacks
  (list :build #'canvas-mindmap--build
        :layout #'canvas-mindmap--lay-out
        :draw-edges (lambda (diagram ctx)
                      (canvas-mindmap--draw-edges ctx (canvas-diagram-model diagram)))
        :draw-trail (lambda (diagram ctx node width)
                      (canvas-mindmap--draw-trail ctx (canvas-diagram-model diagram) node width))
        :node-rgb (lambda (_diagram node)
                    (canvas-mindmap--branch-rgb (canvas-mindmap-node-branch node)))
        :node-text (lambda (_diagram node) (canvas-mindmap--node-text node))
        :header (lambda (diagram node)
                  (mapconcat #'identity (canvas-mindmap--path (canvas-diagram-model diagram) node)
                             " › "))
        :card (lambda (diagram node)
                (canvas-mindmap--card-text (canvas-diagram-model diagram) node))
        :legend (lambda (diagram) (canvas-mindmap--legend-entries (canvas-diagram-model diagram)))
        :move #'canvas-mindmap--move
        :node-key (lambda (diagram node) (canvas-mindmap--path (canvas-diagram-model diagram) node))
        :restore #'canvas-mindmap--restore
        :double-click (lambda (_diagram node) (canvas-mindmap-toggle-collapse node))
        :read-source #'canvas-mindmap--read-source
        :menu 'canvas-mindmap-menu)
  "How the tree plugs into canvas-diagram.")

(defun canvas-mindmap--diagram ()
  "A fresh diagram of a mind map."
  (canvas-diagram-create :callbacks canvas-mindmap--callbacks))

;;;; The mode and its keys

(canvas-diagram-define-setting canvas-mindmap-cycle-direction canvas-mindmap-direction
  '(right left both) "Send the branches right, left, or to both sides.")
(canvas-diagram-define-setting canvas-mindmap-cycle-edge-style canvas-mindmap-edge-style
  '(curve straight elbow) "Curve, straighten or bend the edges.")

(defconst canvas-mindmap--setting-keys
  '(("d" . canvas-mindmap-cycle-direction) ("e" . canvas-mindmap-cycle-edge-style)
    ("[" . canvas-mindmap-shallower) ("]" . canvas-mindmap-deeper))
  "The mind map's own menu keys, bound in the map buffer too.")

(defun canvas-mindmap--make-mode-map ()
  "The keys a mind map has over any diagram: folding as in magit, the
levels, and its own looks.  The diagram's keys are its parent from the
start, not from the first buffer in the mode."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map canvas-diagram-mode-map)
    (define-key map (kbd "TAB") #'canvas-mindmap-toggle-collapse)
    (define-key map (kbd "C-<tab>") #'canvas-mindmap-cycle-node)
    (define-key map (kbd "<backtab>") #'canvas-mindmap-cycle-global)
    (define-key map (kbd "c") #'canvas-mindmap-toggle-collapse)
    ;; Digits show levels of the node, meta digits of the map, as in magit.
    (dotimes (i 4)
      (define-key map (kbd (format "%d" (1+ i)))
                  (intern (format "canvas-mindmap-show-level-%d" (1+ i))))
      (define-key map (kbd (format "M-%d" (1+ i)))
                  (intern (format "canvas-mindmap-show-all-level-%d" (1+ i)))))
    (pcase-dolist (`(,key . ,command) canvas-mindmap--setting-keys)
      (define-key map (kbd key) command))
    map))

(defvar canvas-mindmap-mode-map (canvas-mindmap--make-mode-map)
  "Keys of a mind map over those of every diagram.")

(define-derived-mode canvas-mindmap-mode canvas-diagram-mode "Mindmap"
  "Major mode of a buffer showing a mind map on a canvas.

The keyboard is on one node, ringed.  The keys that move point move
it: a child, the parent, the next and previous node in reading order;
the list commands walk the siblings and the hierarchy; the defun
commands go from branch to branch; \\[canvas-diagram-jump] jumps to a
node by name.  \\[canvas-diagram-toggle-card] opens the node's card,
\\[canvas-diagram-visit-source] goes to its line in the source.
\\[canvas-mindmap-toggle-collapse] folds and unfolds as in magit,
\\[canvas-mindmap-cycle-node] cycles a node and
\\[canvas-mindmap-cycle-global] the whole map; the digits show levels
of the node and the meta digits of the map.  \\[canvas-diagram-menu]
opens the menu of layouts, shapes and colours, whose keys work here
directly as well.

\\{canvas-mindmap-mode-map}")

(canvas-diagram-define-menu canvas-mindmap-menu
  "Change how the map is laid out and drawn.
Each key cycles its setting and redraws; the menu stays up, and so do
the map's own keys and the mouse, so the effect can be tried at once."
  ["Layout"
   ("d" canvas-mindmap-cycle-direction :transient t
    :description (lambda () (canvas-diagram-setting "direction" 'canvas-mindmap-direction)))
   ("e" canvas-mindmap-cycle-edge-style :transient t
    :description (lambda () (canvas-diagram-setting "edges" 'canvas-mindmap-edge-style)))
   ("[" canvas-mindmap-shallower :transient t
    :description (lambda () (canvas-diagram-setting "levels" 'canvas-mindmap-depth "all")))
   ("]" canvas-mindmap-deeper :transient t :description "one level more")]
  [:description "Kinds" :class transient-column :setup-children canvas-mindmap--kind-children])

;;;; Showing and exporting

;;;###autoload
(defun canvas-mindmap-show (&optional spec)
  "Show the current buffer's outline as a mind map.
With SPEC, a nested list (LABEL [:kind KIND] [:note NOTE] CHILD...) or
the name of an org or markdown file, show that tree instead.  A map of
a buffer follows it: the map is drawn again once typing there pauses."
  (interactive)
  (let* ((source (and (null spec) (current-buffer)))
         (tree (cond ((stringp spec) (canvas-mindmap--file-tree spec))
                     (spec spec)
                     ((canvas-mindmap--read-source (current-buffer)))
                     (t (user-error "canvas-mindmap: no headings in %s" (buffer-name))))))
    (canvas-diagram-show "*canvas-mindmap*" #'canvas-mindmap-mode
                         (canvas-mindmap--diagram) tree source)))

;;;###autoload
(defun canvas-mindmap-show-file (file)
  "Show the headings of FILE as a mind map."
  (interactive "fOrg or markdown file: ")
  (canvas-mindmap-show (canvas-mindmap--file-tree file)))

;;;###autoload
(defun canvas-mindmap-export (source file)
  "Draw SOURCE as a mind map into FILE, a PNG, an SVG or a PDF by its
name, and return FILE.  SOURCE is a nested spec (LABEL [:kind KIND]
[:note NOTE] CHILD...) or the name of an org or markdown file.  No
buffer or frame is needed, so this works in batch; in a map buffer
\\[canvas-diagram-write] writes the map shown."
  (canvas-diagram-export (canvas-mindmap--diagram) source file))

;;;; Demo

;;;###autoload
(defun canvas-mindmap-demo ()
  "Show a small map that exercises the module: branches, kinds, notes."
  (interactive)
  (canvas-mindmap-show
   '("Emacs 32 canvas" :icon "material:brain"
     ("draws" :note "A canvas is a writable ARGB32 pixel buffer.  The canvas-cairo module lays a cairo surface over it and sets text with pango."
      ("pixels, kept between refreshes") ("text set by pango" :kind "done")
      ("curves and fills by cairo" :kind "done"))
     ("refreshes" ("in place, no new image object") ("from a timer or a command"))
     ("interacts" :kind "todo" :note "Hover a box for its note, click it for this card, drag or use the wheel and arrows to pan.  The minimap shows the whole map with the view outlined."
      ("hover shows the note") ("click opens a card") ("drag pans the map" :kind "idea")
      ("the minimap shows it all" :kind "question"))
     ("replaces a glyph" :kind "risk" :note "A canvas cannot composite over buffer text, so the map lives in its own window."
      ("so it lives in its own window")))))

(provide 'canvas-mindmap)
;;; canvas-mindmap.el ends here
