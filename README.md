# canvas-mindmap

A mind map drawn on an Emacs 32 canvas: an outline as a horizontal
tree of rounded boxes joined by curves. Built on
[canvas-diagram](https://github.com/Daskeladden/canvas-diagram), which
brings the `canvas-cairo` module and everything a diagram of boxes
shares; this package is only the tree.

This is a prototype.

## What it draws

Each top-level branch has a colour its subtree inherits; a node's
kind, from an org todo keyword or tag, a checkbox or a `:kind` option,
outlines its box; a legend in the corner says which is which. The
canvas is the size of its window and scrolls over the map with the
arrows, the wheel or a drag. Hovering a box shows its note, clicking
one opens a card with it, and with canvas-minimap running the minimap
shows the whole map scaled down with the part in view outlined.

## Requirements

- Emacs 32.0.50, built from source. Emacs 32 is not released; this
  needs a build from the master branch, configured with canvas images
  and with modules: `(image-type-available-p 'canvas)` and
  `module-file-suffix` should both answer.
- [canvas-diagram](https://github.com/Daskeladden/canvas-diagram),
  and its `canvas-cairo` module, which is compiled from C against
  cairo and pango. Its README says what headers that needs and what
  to do without them.
- transient, for the menus; it comes with Emacs.
- tree-sitter grammars, for code maps in languages whose mode brings
  no parser of its own. Without them such a buffer falls back to its
  imenu index.
- [canvas-minimap](https://github.com/Daskeladden/canvas-minimap) is
  optional: with it running, the map gets a thumbnail.

## Install

No archive carries this: it needs Emacs 32.0.50 built from source, and
the drawing goes through a module compiled on your machine. With that
Emacs in hand, clone the two repositories beside each other and build
the module once.

```sh
git clone https://github.com/Daskeladden/canvas-diagram.git
git clone https://github.com/Daskeladden/canvas-mindmap.git
make -C canvas-diagram        # builds canvas-cairo.so
```

Then put both on the load path:

```elisp
(use-package canvas-mindmap
  :load-path ("/path/to/canvas-diagram" "/path/to/canvas-mindmap")
  :commands (canvas-mindmap-show canvas-mindmap-show-file
             canvas-mindmap-export canvas-mindmap-demo))
```

## Use

`M-x canvas-mindmap-demo` draws a small built-in tree, and is the
quickest way to see whether the module built and the canvas is there.
`M-x canvas-mindmap-show` draws the current buffer, and
`M-x canvas-mindmap-show-file` another file. An org or markdown
outline gives its headings, with plain list items under a heading as
nodes too, nested by their indentation, and sub-headings after them. A
todo keyword or the first tag of a heading is its kind, and so is a
list item's checkbox; the other text under a heading is its note;
links show their description; fenced code is skipped. Code is
outlined by tree-sitter when a grammar for its language is installed
and the mode brought no parser of its own: the grammar's declarations,
definitions, bodies and specifiers with a name, and its statements with
a label, nest as the tree does, and each takes the grammar's own word
for it as its kind, `process` for a `process_statement`, `signal
interface` for a port; a name comes from the node's `name` field, what
its `declarator` comes down to, or its one identifier, and parameters
are left out. Nothing here knows any one language;
`canvas-mindmap-treesit-node-re`, `-name-types`, `-skip-kinds` and
`-kind-names` tune the rules and rename kinds. Any other prog-mode
buffer, and one whose mode has its own parser, gives its imenu index,
which tree-sitter modes and eglot fill with classes and their methods,
the categories and imenu's kind property serving as kinds. A language
package can know more than either; see [Plugging in a
language](#plugging-in-a-language).

A map of a buffer follows it: once typing there pauses, the map is
read again and redrawn, the keyboard staying on its node. Going to a
node from the keyboard, or clicking its box, puts point in the source
where that node came from and pulses the line with the pulse you have
on. `C-x C-w` in the map buffer, or `W` in its menu, writes the map as
a PNG, an SVG or a PDF by the file's name; `canvas-mindmap-export`
does the same from a spec or an org file, and needs no frame:

```sh
emacs -Q --batch -L /path/to/canvas-diagram -L /path/to/canvas-mindmap \
  -l canvas-mindmap \
  --eval '(canvas-mindmap-export (quote ("Root" ("A" :kind "todo") ("B" :note "why"))) "map.png")'
```

A spec is a nested list: `(LABEL [:kind KIND] [:note NOTE] [:icon ICON] CHILD...)`.

## Keys

The shared keys are canvas-diagram's: the keyboard is on a node,
ringed, and your own movement keys move it, `forward-char` to the
nearest child, `backward-char` to the parent, `next-line` and
`previous-line` through the map in reading order, `M-n` and `M-p`
along a depth across cousins, the list commands along the siblings and
up and down the hierarchy, the defun commands between the top-level
branches, the buffer ends to the root and the last node, `goto-line`
to a node by name; `RET` opens the card, `C-RET` visits the source,
`SPC` opens the menu, `?` lists every key.

Folding works as in magit: `TAB` folds the selected node, hiding its
children behind a count on its box, or unfolds it; `C-TAB` cycles it
through folded, its children with their own folded, and everything
below; `S-TAB` cycles the whole map through its levels; `1` to `4`
show that many levels of the selected node and `M-1` to `M-4` of the
whole map. `c` and a double click on a box fold too, and going into a
folded node with `f` unfolds it. Your folds survive re-reads of the
source and changes of the level limit.

The menu's own groups are the layout and the kinds. `d` sends the
branches right, left or to both sides of the root, `e` draws the edges
as curves, straight lines or elbows, `[` and `]` show fewer or more
levels, folding everything below. The Kinds group is made when the
menu opens: one entry per kind in the map, `K f` for functions say,
each saying whether it shows and how many nodes it has; pressing one
leaves that kind out of the map, or takes it back, while the menu stays
up, so a code map can be pared down to its classes and methods, a
VHDL map to its processes, or an org map to what is still to do; `K o`
keeps only the next kind in turn and `K K` shows every kind again. A
hidden node stays as the way to a shown one below it. The shared groups
follow, zoom, boxes, spacing, font, palette, kinds, legend, icons and
paper; the single keys work in the map directly as well.

## Settings

`canvas-mindmap-gap-x` and `-gap-y` space the tree, scaled by the
shared spacing; `canvas-mindmap-depth` limits the levels;
`canvas-mindmap-direction` and `-edge-style` are what the menu cycles;
`canvas-mindmap-branch-colors` names the branch colours, or nil takes
them from the palette in use. Font, colours, padding, box shape, kinds
and icons are canvas-diagram's settings.

## Plugging in a language

The outline comes from sources, each a name, a test of whether it
applies to the buffer, a reader, and a priority; the first that applies
and reads something wins, and a buffer's `canvas-mindmap-dialect` names
one outright. The built-in sources are `markdown` (50), `org` (40, and
any text that is not code), `treesit` (20) and `imenu` (10). A language
package defines its own above them:

```elisp
(canvas-mindmap-define-source 'vhdl
  :modes '(vhdl-mode vhdl-ts-mode)          ; or :when, a predicate; or both
  :read #'canvas-vhdl-mindmap-tree           ; (ROOT-NAME) -> spec or nil
  :doc "VHDL: design units, declarations, processes and their state machines")
```

The reader runs in the buffer and returns a nested spec, `(LABEL
[:kind KIND] [:note NOTE] [:pos POS] CHILD...)`, or nil to let the next
source serve; `canvas-mindmap-pos-option` gives the `:pos` option only
when the map asks for positions, which it does to follow a node to its
place. A reader needs no parser of its own: `canvas-mindmap-treesit-tree`
reads the buffer with the generic tree-sitter outline and takes rules
for what the grammar alone does not say, `:language`, the grammar;
`:node-re`, `:name-types`, `:skip-kinds` and `:kind-names` in place of
the settings so named; `:name`, a function of a node giving its name,
or nil to leave that to the grammar; `:note`, a function of a node
giving its note; `:below`, a function of a node giving specs to hang
under it after the grammar's own. The first such package is
[canvas-vhdl-mindmap](https://github.com/Daskeladden/canvas-vhdl-fsm),
which names unlabelled
processes by their sensitivity lists, notes ports with their mode and
type, and hangs each state machine under the process that switches it.
Colours for new kinds go in `canvas-diagram-extra-kinds`, icons in
`canvas-diagram-extra-kind-icons`.

## Tests

```sh
make test
```

builds the module in the canvas-diagram checkout beside this one --
`make test DIAGRAM=/path/to/canvas-diagram` if it sits elsewhere --
and runs the ERT suite in batch: the tree and its readers, the sources
and their order, tree-sitter outlines and rules when the vhdl and c
grammars are installed, the layouts, colours and the legend, folding,
the kind filter, the keyboard, following an org buffer, export, and the
package headers themselves.

## Licence

GPL-3.0-or-later; see [LICENSE](LICENSE).
