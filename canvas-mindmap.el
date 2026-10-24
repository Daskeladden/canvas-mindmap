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
;; inherits; a node's kind, a tag, a checkbox or a :kind option,
;; outlines its box; its status, an org todo keyword, is a badge before
;; its label; a legend in the corner says which is which.
;;
;; This is a prototype.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'canvas-diagram)
(require 'canvas-diagram-reader)
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
  "How many levels below the root a map shows when it opens.
nil shows them all.  The fold keys change the levels after that.  A
node whose children are folded away says how many with a + count."
  :type '(choice (const nil) integer))

(defcustom canvas-mindmap-direction 'right
  "Where the branches go: to the `right' of the root, to the `left',
or half to each side, `both'."
  :type '(choice (const right) (const left) (const both)))

(defcustom canvas-mindmap-edge-style 'curve
  "The edges: a `curve', a `straight' line, or an `elbow'."
  :type '(choice (const curve) (const straight) (const elbow)))

(defcustom canvas-mindmap-statuses
  '(("TODO" . "dark orange") ("NEXT" . "steel blue") ("WAITING" . "gold")
    ("DONE" . "sea green") ("CANCELLED" . "gray"))
  "Alist of org todo keywords and the colour of their badge.
A keyword not listed takes `canvas-mindmap-status-open-color', or
`canvas-mindmap-status-done-color' when org counts it done."
  :type '(alist :key-type string :value-type string))

(defcustom canvas-mindmap-status-open-color "dark orange"
  "The badge colour of an open todo keyword that `canvas-mindmap-statuses' lacks."
  :type 'string)

(defcustom canvas-mindmap-status-done-color "sea green"
  "The badge colour of a done todo keyword that `canvas-mindmap-statuses' lacks."
  :type 'string)

;;;; The tree

(cl-defstruct (canvas-mindmap-node (:include canvas-diagram-node)
                                   (:constructor canvas-mindmap-node-create))
  children branch collapsed (extent 0) status done tags)

(defun canvas-mindmap--options (rest)
  "Split REST, a spec after its label, into (PLIST . CHILDREN).
Options are keyword-value pairs before the children: :kind, :tags, the
node's tags as a list of strings, :status, the node's todo keyword,
:done, whether org counts that keyword done, :note, :icon and :pos,
where the node came from in its buffer."
  (let (plist)
    (while (keywordp (car rest))
      (unless (memq (car rest) '(:kind :tags :status :done :note :icon :pos))
        (error "canvas-mindmap: unknown option %S" (car rest)))
      (push (pop rest) plist)
      (push (pop rest) plist))
    (cons (nreverse plist) rest)))

(defun canvas-mindmap--status-option (plist)
  "The :status of PLIST, a string or nil; any other value is an error."
  (let ((status (plist-get plist :status)))
    (unless (or (null status) (stringp status))
      (error "canvas-mindmap: status %S is not a string" status))
    status))

(defun canvas-mindmap--tags-option (plist)
  "The :tags of PLIST, a list of strings; any other value is an error."
  (let ((tags (plist-get plist :tags)))
    (unless (and (listp tags) (cl-every #'stringp tags))
      (error "canvas-mindmap: tags %S are not a list of strings" tags))
    tags))

(defun canvas-mindmap--tree (spec)
  "Node tree from SPEC, a nested list (LABEL [OPTION VALUE]... CHILD...)."
  (unless (stringp (car spec))
    (error "canvas-mindmap: label %S is not a string" (car spec)))
  (pcase-let ((`(,plist . ,children) (canvas-mindmap--options (cdr spec))))
    (canvas-mindmap-node-create
     :label (car spec)
     :kind (plist-get plist :kind)
     :tags (canvas-mindmap--tags-option plist)
     :status (canvas-mindmap--status-option plist)
     :done (and (plist-get plist :done) t)
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

(defun canvas-mindmap--status-rgb (node)
  "The colour of NODE's status badge: its keyword's in
`canvas-mindmap-statuses', else the open or the done colour.  A name
that is no colour is an error."
  (let ((name (or (cdr (assoc (canvas-mindmap-node-status node) canvas-mindmap-statuses))
                  (if (canvas-mindmap-node-done node)
                      canvas-mindmap-status-done-color
                    canvas-mindmap-status-open-color))))
    (or (canvas-diagram-rgb name)
        (error "canvas-mindmap: %S is no colour, for the status %s" name (canvas-mindmap-node-status node)))))

(defun canvas-mindmap--badge (node)
  "NODE's badge, (KEYWORD RGB) for its status, or nil without one."
  (when-let* ((status (canvas-mindmap-node-status node)))
    (list status (canvas-mindmap--status-rgb node))))

(defun canvas-mindmap--node-text (node)
  "What NODE's box says: its label, and how many children it hides."
  (if (zerop (canvas-mindmap--hidden node))
      (canvas-mindmap-node-label node)
    (format "%s +%d" (canvas-mindmap-node-label node) (canvas-mindmap--hidden node))))

(defun canvas-mindmap--child-segments (node)
  "The key segments of NODE's children, in their order.
A segment is the child's label, or (LABEL . N) when the child is the
Nth child of NODE with that label, N being 2 or more."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (child)
              (let* ((label (canvas-mindmap-node-label child))
                     (n (puthash label (1+ (gethash label seen 0)) seen)))
                (if (= n 1) label (cons label n))))
            (canvas-mindmap-node-children node))))

(defun canvas-mindmap--segment (parent child)
  "The key segment of CHILD, a child of PARENT."
  (nth (or (cl-position child (canvas-mindmap-node-children parent))
           (error "canvas-mindmap: %s is not a child of %s"
                  (canvas-mindmap-node-label child) (canvas-mindmap-node-label parent)))
       (canvas-mindmap--child-segments parent)))

(defun canvas-mindmap--key (root node)
  "The key of NODE in the tree of ROOT, which names NODE across rebuilds.
The key is the list of key segments from ROOT down to NODE, ROOT's
being its label.  Without siblings of one label, it is the label path.
See `canvas-mindmap--child-segments'.  A NODE outside the tree is an error."
  (let ((ancestry (or (canvas-mindmap--ancestry root node)
                      (error "canvas-mindmap: %s is not in this map" (canvas-mindmap-node-label node)))))
    (cons (canvas-mindmap-node-label root)
          (cl-mapcar #'canvas-mindmap--segment ancestry (cdr ancestry)))))

(defun canvas-mindmap--child-keys (node key)
  "The children of NODE, whose key is KEY, each with its own key.
Return ((CHILD . CHILD-KEY)...), in the order of the children."
  (cl-mapcar (lambda (child segment) (cons child (append key (list segment))))
             (canvas-mindmap-node-children node)
             (canvas-mindmap--child-segments node)))

(defun canvas-mindmap--node-by-key (root key)
  "The node KEY leads to from ROOT, or the last one found on the way."
  (let ((node root) (found t))
    (dolist (segment (cdr key))
      (when found
        (let ((i (cl-position segment (canvas-mindmap--child-segments node) :test #'equal)))
          (if i (setq node (nth i (canvas-mindmap-node-children node))) (setq found nil)))))
    node))

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

(defvar org-todo-keywords)
(defvar org-todo-keywords-1)
(defvar org-done-keywords)

(defconst canvas-mindmap--todo-line-re "^[ \t]*#\\+\\(?:SEQ_\\|TYP_\\)?TODO:[ \t]*\\(.*\\)$"
  "A line giving the file's own todo keywords, as org reads them.")

(defun canvas-mindmap--todo-words (words)
  "The keywords of the org todo sequence WORDS as ((KEYWORD . DONE)...):
those after the bar are done, or the last one when there is none.  The
fast-access keys and logging, (t) or (d!) say, are left off."
  (let* ((words (mapcar (lambda (w) (replace-regexp-in-string "(.*)\\'" "" w)) words))
         (bar (cl-position "|" words :test #'equal))
         (open (if bar (cl-subseq words 0 bar) (butlast words)))
         (done (if bar (nthcdr (1+ bar) words) (last words))))
    (append (mapcar #'list open)
            (mapcar (lambda (word) (cons word t)) done))))

(defun canvas-mindmap--todo-lines ()
  "The todo sequences the buffer's #+TODO lines give, each a list of words."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t) sequences)
        (while (re-search-forward canvas-mindmap--todo-line-re nil t)
          (push (split-string (match-string 1) "[ \t]+" t) sequences))
        (nreverse sequences)))))

(defun canvas-mindmap--org-sequences ()
  "The todo sequences of `org-todo-keywords', each a list of words, when
org is loaded; its older form, a flat list of keywords, is one."
  (let ((keywords (bound-and-true-p org-todo-keywords)))
    (if (stringp (car keywords))
        (list keywords)
      (mapcar #'cdr keywords))))

(defun canvas-mindmap--todo-keywords ()
  "The buffer's org todo keywords as ((KEYWORD . DONE)...), DONE when org
counts the keyword done: org's own in an org buffer, else those of the
buffer's #+TODO lines, else `org-todo-keywords', else TODO and DONE."
  (if (bound-and-true-p org-todo-keywords-1)
      (mapcar (lambda (keyword) (cons keyword (and (member keyword org-done-keywords) t)))
              org-todo-keywords-1)
    (cl-mapcan #'canvas-mindmap--todo-words
               (or (canvas-mindmap--todo-lines) (canvas-mindmap--org-sequences) '(("TODO" "DONE"))))))

(defun canvas-mindmap--parse-title (title keywords)
  "Split TITLE into (LABEL . OPTIONS): a leading keyword of KEYWORDS,
\((KEYWORD . DONE)...), is its :status, with :done when it is done; the
first tag its :kind, and each tag in its :tags."
  (let (tags status)
    (when (string-match canvas-mindmap--tags-re title)
      ;; Read the match before `split-string' runs a search of its own.
      (let ((text (match-string 1 title))
            (cut (match-beginning 0)))
        (setq tags (split-string text ":" t)
              title (substring title 0 cut))))
    (when (and (string-match "^\\([[:upper:]][[:upper:][:digit:]_-]*\\)[ \t]+" title)
               (assoc (match-string 1 title) keywords))
      (setq status (assoc (match-string 1 title) keywords)
            title (substring title (match-end 0))))
    (cons (canvas-mindmap--clean-label title)
          (append (and tags (list :kind (car tags) :tags tags))
                  (and status (list :status (car status)))
                  (and (cdr status) (list :done t))))))

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
  "List of (LEVEL LABEL OPTIONS NOTE ITEMS POS) for the buffer's headings,
in order, org or markdown by DIALECT.  OPTIONS are the :kind, :status
and :done its title gives, ITEMS the specs of the plain list items under
the heading, POS where it starts."
  (let ((heading-re (canvas-mindmap--dialect-re dialect :heading))
        (keyword-re (canvas-mindmap--dialect-re dialect :keyword))
        (keywords (canvas-mindmap--todo-keywords))
        headings current body)
    (cl-flet ((finish ()
                (when current
                  (pcase-let ((`(,level ,label ,options ,pos) current)
                              (`(,note . ,items) (canvas-mindmap--body-items (nreverse body) keyword-re)))
                    (push (list level label options note items pos) headings)))))
      (pcase-dolist (`(,pos . ,text) (canvas-mindmap--lines dialect))
        (if (string-match heading-re text)
            (let ((level (length (match-string 1 text)))
                  (title (canvas-mindmap--parse-title (match-string 2 text) keywords)))
              (finish)
              (setq current (list level (car title) (cdr title) pos)
                    body nil))
          (when current
            (push (cons pos text) body))))
      (finish))
    (nreverse headings)))

(defun canvas-mindmap--heading-spec (heading)
  "Spec (LABEL [:kind KIND] [:status STATUS [:done t]] [:note NOTE] [:pos
POS] ITEM...) of HEADING, a (LEVEL LABEL OPTIONS NOTE ITEMS POS).
Sub-headings are appended later."
  (pcase-let ((`(,_ ,label ,options ,note ,items ,pos) heading))
    (append (list label)
            options
            (and note (list :note note))
            (canvas-mindmap-pos-option pos)
            items)))

(defun canvas-mindmap-nest (items)
  "Nested specs from ITEMS, each (LEVEL . SPEC), SPEC a node's spec as a
source reads it, its children to come: a node hangs off the nearest
earlier node of a lower level, after the children it has.  Each node
at the lowest level is a tree of its own.  The specs are changed."
  (let ((root (list "forest"))
        stack)                          ; (LEVEL . SPEC), innermost first
    (pcase-dolist (`(,level . ,node) items)
      (while (and stack (>= (caar stack) level))
        (pop stack))
      (setcdr (last (if stack (cdar stack) root)) (list node))
      (push (cons level node) stack))
    (cdr root)))

(defun canvas-mindmap--forest (headings)
  "Nested specs from HEADINGS, each a (LEVEL LABEL OPTIONS NOTE ITEMS POS).
A heading hangs off the nearest earlier heading of a shallower level."
  (canvas-mindmap-nest (mapcar (lambda (heading) (cons (car heading) (canvas-mindmap--heading-spec heading)))
                               headings)))

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

;;;; The source text of a node

(defun canvas-mindmap--next-heading (dialect pos)
  "Where the first heading of DIALECT after the line at POS starts, the
lines of code blocks skipped; the buffer's end when there is none."
  (let ((heading-re (canvas-mindmap--dialect-re dialect :heading)))
    (or (car (cl-find-if (lambda (line) (and (> (car line) pos) (string-match-p heading-re (cdr line))))
                         (canvas-mindmap--lines dialect)))
        (point-max))))

(defun canvas-mindmap--item-end (pos)
  "Where the list item whose line starts at POS ends: at its last line
indented deeper than it, before the first line that is not."
  (save-excursion
    (goto-char pos)
    (let ((indent (current-indentation))
          (end (line-end-position)))
      (forward-line 1)
      (while (and (not (eobp))
                  (or (looking-at-p "[ \t]*$") (> (current-indentation) indent)))
        (unless (looking-at-p "[ \t]*$")
          (setq end (line-end-position)))
        (forward-line 1))
      end)))

(defun canvas-mindmap--outline-text (dialect pos part)
  "PART, `header', `body' or `all', of the node of the current buffer's
DIALECT outline whose line starts at POS, a heading or a list item.  The
header is its line, the body the lines up to the next heading, or for an
item those indented deeper below it; nil for an empty part."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (let* ((start (line-beginning-position))
             (header-end (line-end-position))
             (end (if (looking-at-p (canvas-mindmap--dialect-re dialect :heading))
                      (canvas-mindmap--next-heading dialect start)
                    (canvas-mindmap--item-end start)))
             (body (string-trim (buffer-substring-no-properties header-end (max header-end end))
                                "[ \t\n]*\n" "[ \t\n]+")))
        (pcase part
          ('header (buffer-substring-no-properties start header-end))
          ('body (and (not (string-empty-p body)) body))
          ('all (string-trim-right (buffer-substring-no-properties start (max header-end end))))
          (_ (error "canvas-mindmap: a node has no part %S" part)))))))

(defun canvas-mindmap--source-text (_diagram node part buffer)
  "PART of the text NODE came from in BUFFER, when the map is of an org or
markdown outline of it; nil when another source read it."
  (with-current-buffer buffer
    (let ((dialect (canvas-mindmap--dialect)))
      (and (memq dialect '(org markdown))
           (canvas-mindmap--outline-text dialect (canvas-diagram-node-pos node) part)))))

(defun canvas-mindmap--tree-of (source)
  "Node tree of SOURCE, a nested spec or the name of an org file.
Its branches are numbered.  The layout folds it."
  (canvas-mindmap--number-branches
   (canvas-mindmap--tree (if (stringp source)
                             (canvas-mindmap--file-tree source)
                           source))))

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
  "Give every node shown in NODE's subtree the box MEASURE says it needs,
its badge's room too."
  (let ((size (canvas-diagram-box-size node (canvas-mindmap--node-text node) measure
                                       (car (canvas-mindmap--badge node)))))
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

;; The folds, the rule that reads them, the fold commands and their keys
;; are canvas-diagram's.  A map gives its tree as fold trees under the
;; keys of its nodes, and the layout folds each node as the rule says.

(defun canvas-mindmap--shown-node (root node)
  "NODE if it is shown, else the folded ancestor that hides it."
  (or (cl-find-if #'canvas-mindmap-node-collapsed (butlast (canvas-mindmap--ancestry root node)))
      node))

(defun canvas-mindmap--fold-tree (node key)
  "The fold tree of NODE, whose key is KEY: (KEY . CHILD-TREES).
It holds the folded nodes too."
  (cons key (canvas-mindmap--child-fold-trees node key)))

(defun canvas-mindmap--child-fold-trees (node key)
  "The fold trees of the children of NODE, whose key is KEY."
  (mapcar (lambda (child) (canvas-mindmap--fold-tree (car child) (cdr child)))
          (canvas-mindmap--child-keys node key)))

(defun canvas-mindmap--fold-trees (diagram node)
  "The fold trees under NODE in DIAGRAM's tree, or the root's own for nil.
Each tree is under the key `canvas-mindmap--key' gives its node."
  (let ((root (canvas-diagram-model diagram)))
    (if (null node)
        (list (canvas-mindmap--fold-tree root (canvas-mindmap--key root root)))
      (canvas-mindmap--child-fold-trees node (canvas-mindmap--key root node)))))

(defun canvas-mindmap--fold-start (_diagram)
  "The levels a map opens with: those `canvas-mindmap-depth' gives."
  (list :levels (and canvas-mindmap-depth (1+ canvas-mindmap-depth))))

(defun canvas-mindmap--fold-by-rule (node key)
  "Fold NODE, whose key is KEY, and every node below it.
The fold rule of canvas-diagram says which nodes hide their children."
  (setf (canvas-mindmap-node-collapsed node)
        (and (canvas-mindmap-node-children node)
             (canvas-diagram-folded-p key (1- (length key)))
             t))
  (pcase-dolist (`(,child . ,child-key) (canvas-mindmap--child-keys node key))
    (canvas-mindmap--fold-by-rule child child-key)))

(defun canvas-mindmap--unfold (diagram node)
  "Open the fold of NODE in DIAGRAM's tree, and lay the map out again."
  (canvas-diagram-set-fold (canvas-mindmap--key (canvas-diagram-model diagram) node) 'open)
  (canvas-diagram-relayout))

(defun canvas-mindmap--double-click (_diagram node)
  "Put the keyboard on NODE, the box a double click hit, and fold or unfold it."
  (canvas-diagram-set-selected node)
  (canvas-diagram-toggle-fold))

;;;; Filtering by kind, by status and by tag

(defvar-local canvas-mindmap--hidden-kinds nil
  "The kinds of node this map leaves out, as strings.")

(defvar-local canvas-mindmap--hidden-statuses nil
  "The statuses, todo keywords, of node this map leaves out, as strings.")

(defvar-local canvas-mindmap--shown-tag nil
  "The tag that each node of this map has, save the nodes on the way to
such a node, or nil when the map has the nodes of each tag.")

(defun canvas-mindmap--hidden-p (node kinds statuses tag)
  "Whether NODE itself is left out: its kind is one of KINDS, its status
one of STATUSES, or it does not have TAG when TAG is not nil."
  (let ((kind (canvas-mindmap-node-kind node))
        (status (canvas-mindmap-node-status node)))
    (or (and kind (member kind kinds) t)
        (and status (member status statuses) t)
        (and tag (not (member tag (canvas-mindmap-node-tags node)))))))

(defun canvas-mindmap--keep-p (node kinds statuses tag)
  "Whether NODE stays in a map without the KINDS and the STATUSES, and
with the nodes of TAG only: it is not left out itself, or something
below it stays.  Its subtree is filtered first."
  (canvas-mindmap--filter node kinds statuses tag)
  (or (not (canvas-mindmap--hidden-p node kinds statuses tag))
      (and (canvas-mindmap-node-children node) t)))

(defun canvas-mindmap--filter (node kinds statuses tag)
  "Take the nodes of the KINDS and the STATUSES, and the nodes without
TAG when it is not nil, out of NODE's tree, save those on the way to
nodes kept.  Return NODE."
  (setf (canvas-mindmap-node-children node)
        (cl-remove-if-not (lambda (child) (canvas-mindmap--keep-p child kinds statuses tag))
                          (canvas-mindmap-node-children node)))
  node)

(defun canvas-mindmap--counts-in-spec (spec option)
  "The values of OPTION, :kind or :status, in SPEC with how many nodes
have each: ((VALUE . COUNT)...), in order of first appearance."
  (let (counts)
    (cl-labels ((walk (node)
                  (pcase-let ((`(,plist . ,children) (canvas-mindmap--options (cdr node))))
                    (when-let* ((value (plist-get plist option)))
                      (if-let* ((entry (assoc value counts)))
                          (cl-incf (cdr entry))
                        (push (cons value 1) counts)))
                    (mapc #'walk children))))
      (walk spec))
    (nreverse counts)))

(defun canvas-mindmap--spec-values (option)
  "The values of OPTION in this map's spec, in order of first appearance."
  (mapcar #'car (canvas-mindmap--counts-in-spec (canvas-diagram-spec canvas-diagram--diagram) option)))

(defun canvas-mindmap--toggle-keys (values prefix taken)
  "A key under PREFIX for each of VALUES, ((VALUE . KEY)...): the first
letter of its name, lower-cased, that neither TAKEN, a list of
characters, nor an earlier value has, else a digit."
  (let (keys)
    (dolist (value values)
      (let ((letter (or (cl-find-if (lambda (c) (and (string-match-p "[a-z]" (string c)) (not (memq c taken))))
                                    (downcase value))
                        (cl-find-if (lambda (c) (not (memq c taken))) "123456789"))))
        (push letter taken)
        (push (cons value (format "%s %c" prefix letter)) keys)))
    (nreverse keys)))

(defun canvas-mindmap--kind-keys (kinds)
  "A key under K for each of KINDS, ((KIND . KEY)...), o being the focus
key's."
  (canvas-mindmap--toggle-keys kinds "K" (list ?o)))

(defun canvas-mindmap--toggled (value hidden)
  "HIDDEN, a list of strings, with VALUE taken out when it is in, else put in."
  (if (member value hidden)
      (remove value hidden)
    (cons value hidden)))

(defun canvas-mindmap-toggle-kind (kind)
  "Leave the nodes of KIND out of the map, or take them back in."
  (interactive (list (completing-read "Kind: " (canvas-mindmap--spec-values :kind))))
  (setq canvas-mindmap--hidden-kinds (canvas-mindmap--toggled kind canvas-mindmap--hidden-kinds))
  (canvas-diagram-rebuild))

(defun canvas-mindmap-toggle-status (status)
  "Leave the nodes of STATUS, a todo keyword, out of the map, or take
them back in."
  (interactive (list (completing-read "Status: " (canvas-mindmap--spec-values :status))))
  (setq canvas-mindmap--hidden-statuses (canvas-mindmap--toggled status canvas-mindmap--hidden-statuses))
  (canvas-diagram-rebuild))

(defun canvas-mindmap-show-all-kinds ()
  "Take every kind back into the map."
  (interactive)
  (setq canvas-mindmap--hidden-kinds nil)
  (canvas-diagram-rebuild))

(defun canvas-mindmap-hide-statuses (statuses)
  "Leave the nodes of STATUSES, a list of todo keywords, out of the map,
and take every other status back in.  The map is built again only when
STATUSES are new to it."
  (unless (equal statuses canvas-mindmap--hidden-statuses)
    (setq canvas-mindmap--hidden-statuses (copy-sequence statuses))
    (canvas-diagram-rebuild)))

(defun canvas-mindmap-show-tag (tag)
  "Leave out of the map each node that does not have TAG, save the nodes
on the way to a node that has it.  With nil for TAG, take each node back
in.  The map is built again only when TAG is new to it."
  (cl-check-type tag (or null string))
  (unless (equal tag canvas-mindmap--shown-tag)
    (setq canvas-mindmap--shown-tag tag)
    (canvas-diagram-rebuild)))

(defun canvas-mindmap-show-all-statuses ()
  "Take every status back into the map."
  (interactive)
  (canvas-mindmap-hide-statuses nil))

(defun canvas-mindmap--kinds ()
  "The kinds of this map, in order of first appearance."
  (canvas-mindmap--spec-values :kind))

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

(defun canvas-mindmap--toggle-command (what value toggle)
  "A command calling TOGGLE with VALUE, named for WHAT, kind or status,
and VALUE, so the menu has one to bind."
  (let ((symbol (intern (format "canvas-mindmap-toggle-%s/%s" what value))))
    (unless (fboundp symbol)
      (defalias symbol (lambda () (interactive) (funcall toggle value))
        (format "Leave the nodes of %s %s out of the map, or take them back in." what value)))
    symbol))

(defun canvas-mindmap--toggle-suffixes (counts keys command hidden)
  "Menu suffixes toggling each value of COUNTS, ((VALUE . COUNT)...), under
its key in KEYS, by the command COMMAND gives for it.  Each says whether
the value is in HIDDEN, a variable, and how many nodes have it."
  (mapcar (lambda (entry)
            (let ((value (car entry)))
              (list (cdr (assoc value keys)) (funcall command value)
                    :transient t
                    :description (lambda ()
                                   (format "%s %s (%d)" (if (member value (symbol-value hidden)) "□" "■")
                                           value (cdr entry))))))
          counts))

(defun canvas-mindmap--kind-suffixes ()
  "The menu's suffixes for this map's kinds: one toggling each, saying
whether it shows and how many nodes it has, and one showing them all."
  (let ((counts (canvas-mindmap--counts-in-spec (canvas-diagram-spec canvas-diagram--diagram) :kind)))
    (append (canvas-mindmap--toggle-suffixes
             counts (canvas-mindmap--kind-keys (mapcar #'car counts))
             (lambda (kind) (canvas-mindmap--toggle-command "kind" kind #'canvas-mindmap-toggle-kind))
             'canvas-mindmap--hidden-kinds)
            (list (list "K o" #'canvas-mindmap-focus-next-kind :transient t
                        :description (lambda ()
                                       (if-let* ((kind (canvas-mindmap--focused-kind)))
                                           (format "only %s; next" kind)
                                         "only the next kind")))
                  (list "K K" #'canvas-mindmap-show-all-kinds :transient t :description "every kind again")))))

(defun canvas-mindmap--status-suffixes ()
  "The menu's suffixes for this map's statuses, under S: one toggling
each, saying whether it shows and how many nodes have it, and one
showing them all.  None for a map without statuses."
  (when-let* ((counts (canvas-mindmap--counts-in-spec (canvas-diagram-spec canvas-diagram--diagram) :status)))
    (append (canvas-mindmap--toggle-suffixes
             counts (canvas-mindmap--toggle-keys (mapcar #'car counts) "S" nil)
             (lambda (status) (canvas-mindmap--toggle-command "status" status #'canvas-mindmap-toggle-status))
             'canvas-mindmap--hidden-statuses)
            (list (list "S S" #'canvas-mindmap-show-all-statuses :transient t :description "every status again")))))

(defun canvas-mindmap--kind-children (_children)
  "The menu's Kinds group, made when the menu opens, of this map's kinds."
  (transient-parse-suffixes 'canvas-mindmap-menu (canvas-mindmap--kind-suffixes)))

(defun canvas-mindmap--status-children (_children)
  "The menu's Statuses group, made when the menu opens, of this map's statuses."
  (transient-parse-suffixes 'canvas-mindmap-menu (canvas-mindmap--status-suffixes)))

;;;; The diagram: how the tree plugs into canvas-diagram

(defun canvas-mindmap--build (_diagram spec)
  "The tree SPEC describes, less the kinds and the statuses hidden, and
less the nodes without the tag shown.  The layout folds it."
  (canvas-mindmap--filter (canvas-mindmap--tree-of spec)
                          canvas-mindmap--hidden-kinds canvas-mindmap--hidden-statuses
                          canvas-mindmap--shown-tag))

(defun canvas-mindmap--lay-out (diagram ctx)
  "Fold DIAGRAM's tree as the fold rule says, and lay it out on CTX.
Return the nodes shown, in reading order."
  (let ((root (canvas-diagram-model diagram)))
    (canvas-mindmap--fold-by-rule root (canvas-mindmap--key root root))
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
               (canvas-mindmap--unfold diagram node))
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
  "The node at KEY, as `canvas-mindmap--key' gives it, in DIAGRAM's
rebuilt tree, or the nearest one shown."
  (let ((root (canvas-diagram-model diagram)))
    (canvas-mindmap--shown-node root (canvas-mindmap--node-by-key root key))))

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
        :badge (lambda (_diagram node) (canvas-mindmap--badge node))
        :header (lambda (diagram node)
                  (mapconcat #'identity (canvas-mindmap--path (canvas-diagram-model diagram) node)
                             " › "))
        :card (lambda (diagram node)
                (canvas-mindmap--card-text (canvas-diagram-model diagram) node))
        :legend (lambda (diagram) (canvas-mindmap--legend-entries (canvas-diagram-model diagram)))
        :move #'canvas-mindmap--move
        :node-key (lambda (diagram node) (canvas-mindmap--key (canvas-diagram-model diagram) node))
        :restore #'canvas-mindmap--restore
        :fold-trees #'canvas-mindmap--fold-trees
        :fold-start #'canvas-mindmap--fold-start
        :double-click #'canvas-mindmap--double-click
        :read-source #'canvas-mindmap--read-source
        :source-text #'canvas-mindmap--source-text
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
  '(("d" . canvas-mindmap-cycle-direction) ("e" . canvas-mindmap-cycle-edge-style))
  "The mind map's own menu keys, bound in the map buffer too.")

(defun canvas-mindmap--make-mode-map ()
  "The keys a mind map has over any diagram: c, which folds as TAB does,
and its own looks.  The fold keys are the diagram's.  The diagram's keys
are its parent from the start, not from the first buffer in the mode."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map canvas-diagram-mode-map)
    (define-key map (kbd "c") #'canvas-diagram-toggle-fold)
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
\\[canvas-diagram-visit-source] goes to its line in the source,
\\[canvas-diagram-copy-node] copies its label and note, and with a
prefix argument its heading and body in the source.
The fold keys come from the diagram and work as in magit.  TAB folds a node or
unfolds it, and c and a double click do the same.  C-TAB cycles the
fold of a node, and S-TAB cycles the levels of the whole map.  The
digits show levels of the node, and the meta digits show levels of the
map.  Moving into a folded node unfolds it.  \\[canvas-diagram-menu]
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
    :description (lambda () (canvas-diagram-setting "edges" 'canvas-mindmap-edge-style)))]
  [:description "Kinds" :class transient-column :setup-children canvas-mindmap--kind-children]
  [:description "Statuses" :class transient-column :setup-children canvas-mindmap--status-children])

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

(defun canvas-mindmap--follow-diagram (read)
  "A fresh diagram of a mind map that READ, a package's reader, finds in
the buffer it follows.  The outline's source text is not the package's,
so it has none."
  (canvas-diagram-create :callbacks (append (list :read-source read :source-text nil)
                                            canvas-mindmap--callbacks)))

(defun canvas-mindmap-follow (buffer read &optional name)
  "Show the tree READ finds in BUFFER as a mind map, in the buffer NAME or
*canvas-mindmap*, and follow BUFFER: once typing there pauses, READ
reads it again and a changed tree is drawn.  READ is a function of a
buffer giving a nested spec, or nil when it finds none, the last map
then staying.  Return the buffer."
  (canvas-diagram-show (or name "*canvas-mindmap*") #'canvas-mindmap-mode
                       (canvas-mindmap--follow-diagram read)
                       (or (funcall read buffer)
                           (user-error "canvas-mindmap: no tree to draw in %s" (buffer-name buffer)))
                       buffer))

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

(canvas-diagram-define-layout 'mindmap :follow #'canvas-mindmap-follow :export #'canvas-mindmap-export)

(provide 'canvas-mindmap)
;;; canvas-mindmap.el ends here
