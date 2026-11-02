;;; canvas-mindmap-acceptance.el --- the map's nodes in imenu -*- lexical-binding: t -*-

;; These tests use the real commands `imenu' and `consult-imenu' in a
;; real window, so they need a display.  `make acceptance' runs them on
;; a private Xvfb display.  It runs the plain tests with no consult on
;; the load path, so that a dependency on consult makes them fail.

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'canvas-mindmap)

;;;; Helpers

(defconst canvas-mindmap-acceptance--map-name "*canvas-mindmap*"
  "The name of the buffer that `canvas-mindmap-show' opens.")

(defconst canvas-mindmap-acceptance--plan
  (concat "* Plans\n** Build\n*** Tests\n*** Docs\n** Ship\n* Notes\n** Idea\n")
  "An outline with two top headings, so that the root is the buffer's name.")

(defun canvas-mindmap-acceptance--open-map (org)
  "Show the map of the buffer ORG and make its window the selected one."
  (switch-to-buffer org)
  (delete-other-windows)
  (canvas-mindmap-show)
  (select-window (or (get-buffer-window canvas-mindmap-acceptance--map-name)
                     (error "The map has no window")))
  (redisplay t))

(defmacro canvas-mindmap-acceptance--with-map (text &rest body)
  "Run BODY in the window of the map of an org buffer that holds TEXT.
In BODY, `org' is the org buffer."
  (declare (indent 1))
  `(let ((org (generate-new-buffer "plan.org"))
         (canvas-mindmap-depth nil))
     (unwind-protect
         (progn
           (with-current-buffer org
             (insert ,text)
             (org-mode)
             (goto-char (point-min)))
           (canvas-mindmap-acceptance--open-map org)
           ,@body)
       (when (get-buffer canvas-mindmap-acceptance--map-name)
         (kill-buffer canvas-mindmap-acceptance--map-name))
       (with-current-buffer org (set-buffer-modified-p nil))
       (kill-buffer org))))

(defun canvas-mindmap-acceptance--run-from-keyboard (command)
  "Run COMMAND as a key does.  With no key pressed yet, imenu takes the
call for one from the mouse and opens a popup menu."
  (let ((last-nonmenu-event ?\r))
    (call-interactively command)))

(defun canvas-mindmap-acceptance--offered (command)
  "What the minibuffer of COMMAND offers, in order.  The list is not chosen from."
  (with-timeout (10 (error "%s did not open the minibuffer" command))
    (catch 'offered
      (minibuffer-with-setup-hook
          (lambda ()
            (throw 'offered
                   (mapcar #'substring-no-properties
                           (all-completions "" minibuffer-completion-table
                                            minibuffer-completion-predicate))))
        (canvas-mindmap-acceptance--run-from-keyboard command)))))

(defun canvas-mindmap-acceptance--choose (command choice)
  "Run COMMAND, type CHOICE into its minibuffer and press RET."
  (with-timeout (10 (error "%s did not accept %s" command choice))
    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
      (minibuffer-with-setup-hook (lambda () (insert choice))
        (canvas-mindmap-acceptance--run-from-keyboard command))))
  (redisplay t))

(defun canvas-mindmap-acceptance--ring ()
  "The label of the node with the ring."
  (canvas-diagram-node-label (canvas-diagram-selected)))

(defun canvas-mindmap-acceptance--shown ()
  "The labels of the nodes that the map shows."
  (mapcar #'canvas-diagram-node-label (canvas-diagram-nodes-shown)))

(defun canvas-mindmap-acceptance--fold (label)
  "Go to the node called LABEL and fold it with the key TAB."
  (canvas-diagram-jump label)
  (execute-kbd-macro (kbd "TAB"))
  (redisplay t))

(defun canvas-mindmap-acceptance--add-heading (org heading)
  "Type HEADING at the end of the buffer ORG, and wait until the map shows it."
  (with-current-buffer org
    (goto-char (point-max))
    (insert "* " heading "\n"))
  (let ((deadline (+ (float-time) 5)))
    (while (and (< (float-time) deadline)
                (not (member heading (canvas-mindmap-acceptance--shown))))
      ;; An idle timer runs only while Emacs waits for a key with no limit
      ;; of time, as at top level, so `sit-for' is no pause for the map.
      (with-timeout (1) (read-event))))
  (should (member heading (canvas-mindmap-acceptance--shown))))

(defun canvas-mindmap-acceptance--point-on-heading-p (org heading)
  "Whether point in the buffer ORG is at the start of the line HEADING."
  (with-current-buffer org
    (and (bolp) (looking-at-p (concat (regexp-quote heading) "$")))))

(defun canvas-mindmap-acceptance--require-consult ()
  "Load consult, which a consult test cannot run without."
  (require 'consult-imenu))

(defun canvas-mindmap-acceptance--expect-folded-node-reached (command)
  "The steps of the scenario that COMMAND goes to a folded node."
  (canvas-mindmap-acceptance--with-map canvas-mindmap-acceptance--plan
    (canvas-mindmap-acceptance--fold "Build")
    (canvas-mindmap-acceptance--fold "Notes")
    (should-not (member "Tests" (canvas-mindmap-acceptance--shown)))
    (should-not (member "Idea" (canvas-mindmap-acceptance--shown)))
    (canvas-mindmap-acceptance--choose command "Plans › Build › Tests")
    (should (equal (canvas-mindmap-acceptance--ring) "Tests"))
    (should (member "Tests" (canvas-mindmap-acceptance--shown)))
    (should (canvas-mindmap-acceptance--point-on-heading-p org "*** Tests"))
    (should-not (member "Idea" (canvas-mindmap-acceptance--shown)))))

(defun canvas-mindmap-acceptance--expect-current-list (command)
  "The steps of the scenario that COMMAND offers what the map has now."
  (canvas-mindmap-acceptance--with-map "* Plans\n* DONE Old\n"
    (should (member "Old" (canvas-mindmap-acceptance--offered command)))
    (canvas-mindmap-acceptance--add-heading org "Later")
    (should (member "Later" (canvas-mindmap-acceptance--offered command)))
    (canvas-mindmap-toggle-status "DONE")
    (should-not (member "Old" (canvas-mindmap-acceptance--offered command)))))

;;;; The scenarios

(ert-deftest canvas-mindmap-acceptance-plain-list-is-flat-with-paths ()
  ;; GIVEN a map of an outline with folded and open headings
  ;; WHEN the user runs imenu
  ;; THEN the list has each node once in reading order, the root by its
  ;; own name and each other node with its path below the root
  (canvas-mindmap-acceptance--with-map canvas-mindmap-acceptance--plan
    (canvas-mindmap-acceptance--fold "Notes")
    (should (equal (canvas-mindmap-acceptance--offered #'imenu)
                   '("plan.org" "Plans" "Plans › Build" "Plans › Build › Tests"
                     "Plans › Build › Docs" "Plans › Ship" "Notes" "Notes › Idea")))))

(ert-deftest canvas-mindmap-acceptance-plain-choice-opens-the-folds-above ()
  ;; GIVEN a map where Build and Notes are folded
  ;; WHEN the user chooses Plans › Build › Tests in imenu
  ;; THEN Build opens and the ring is on Tests
  ;; AND point in the source is on that heading
  ;; AND Notes stays folded
  (canvas-mindmap-acceptance--expect-folded-node-reached #'imenu))

(ert-deftest canvas-mindmap-acceptance-consult-choice-opens-the-folds-above ()
  ;; GIVEN a map where Build and Notes are folded
  ;; WHEN the user chooses Plans › Build › Tests in consult-imenu
  ;; THEN Build opens and the ring is on Tests
  ;; AND point in the source is on that heading
  ;; AND Notes stays folded
  (canvas-mindmap-acceptance--require-consult)
  (canvas-mindmap-acceptance--expect-folded-node-reached #'consult-imenu))

(ert-deftest canvas-mindmap-acceptance-plain-filter-leaves-nodes-out ()
  ;; GIVEN a map whose status filter hides DONE
  ;; WHEN the user runs imenu
  ;; THEN the hidden nodes are not in the list
  ;; AND a hidden node that leads to a node that stays is in the list,
  ;; as it is on the map
  (canvas-mindmap-acceptance--with-map
      "* Plans\n** DONE Old\n** TODO New\n* DONE Archive\n** TODO Box\n* DONE Gone\n"
    (canvas-mindmap-toggle-status "DONE")
    (should (equal (canvas-mindmap-acceptance--offered #'imenu)
                   '("plan.org" "Plans" "Plans › New" "Archive" "Archive › Box")))))

(ert-deftest canvas-mindmap-acceptance-plain-same-path-gets-a-number ()
  ;; GIVEN two headings with the same name under the same parent
  ;; WHEN the user runs imenu
  ;; THEN the second entry has the number 2 after its path
  ;; WHEN the user chooses that entry
  ;; THEN point in the source is on the second heading
  (canvas-mindmap-acceptance--with-map "* Plans\n** Build\ntext\n** Build\n* Notes\n"
    (should (equal (canvas-mindmap-acceptance--offered #'imenu)
                   '("plan.org" "Plans" "Plans › Build" "Plans › Build (2)" "Notes")))
    (canvas-mindmap-acceptance--choose #'imenu "Plans › Build (2)")
    (should (equal (canvas-mindmap-acceptance--ring) "Build"))
    (should (= (with-current-buffer org (line-number-at-pos)) 4))))

(ert-deftest canvas-mindmap-acceptance-plain-list-follows-the-map ()
  ;; GIVEN a map whose list the user opened once in imenu
  ;; WHEN the user adds a heading to the source and the map draws it
  ;; THEN the list has the new heading
  ;; WHEN the user hides DONE with the status filter
  ;; THEN the list no longer has the DONE heading
  (canvas-mindmap-acceptance--expect-current-list #'imenu))

(ert-deftest canvas-mindmap-acceptance-consult-list-follows-the-map ()
  ;; GIVEN a map whose list the user opened once in consult-imenu
  ;; WHEN the user adds a heading to the source and the map draws it
  ;; THEN the list has the new heading
  ;; WHEN the user hides DONE with the status filter
  ;; THEN the list no longer has the DONE heading
  (canvas-mindmap-acceptance--require-consult)
  (canvas-mindmap-acceptance--expect-current-list #'consult-imenu))

;;;; Running them

(defun canvas-mindmap-acceptance-run (selector)
  "Run the tests that SELECTOR, a regexp of their names, picks, show the
result on the standard error stream and leave Emacs."
  (let ((stats (ert-run-tests-batch selector)))
    (princ (with-current-buffer "*Messages*" (buffer-string)) #'external-debugging-output)
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'canvas-mindmap-acceptance)
;;; canvas-mindmap-acceptance.el ends here
