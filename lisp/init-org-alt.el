(require 'org-indent)
(setq org-startup-indented t)

(set-default 'truncate-lines nil)
(set-default 'word-wrap t)

(setq org-auto-align-tags nil)

;; Melpa org-bulletsi
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
;;(org-bullets :fetcher github :repo "integral-dw/org-bullets")

;; Configuring org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list (quote ("◉" "◆" "✚" "☀" "○")))

(setq org-startup-folded t)
(setq org-checkbox-hierarchical-statistics nil)
(setq org-todo-keywords
      (quote
       ((sequence "TODO" "PROG" "PAUS" "|" "DONE" "CANC"))))
(setq org-todo-keyword-faces
      '(("PROG" . "orange") ("PAUS" . "magenta") ("CANC" . "red") ("DONE" . "green")))
(setq org-default-priority 72)
(setq org-highest-priority 65)
(setq org-lowest-priority 90)

;; Capture

(global-set-key (kbd "<f6>") 'org-capture)
(setq org-capture-templates
      (quote (
              ("w"         ; hotkey
               "Work Todo" ; name
               entry       ; type
               (file+headline "~/Dropbox/org/work.org" "Tasks") ;target
               "* TODO [#A] %^{Task}" ; template
               )
              ("t"
               "Task Diary"
               entry
               (file+datetree "~/Dropbox/org/tasks.org")
               "* TODO [#A] %^{Task}")
              ("j"
               "Journal"
               item
               (file+datetree "~/Dropbox/org/journal.org")
               "- %U - %^{Activity}")
              ("b"
               "Add a book to read"
               entry
               (file+headline "~/Dropbox/org/notes.org" "Books to read")
               "* TODO %^{Book name}\n%^{Why to read this book?}"
               )
              ("s"
               "Schedule an event or a task"
               entry
               (file+datetree "~/Dropbox/org/tasks.org")
               "* %^{Event or Task}\nSCHEDULED: %^t"
               )
              )))

;; Agenda

(setq org-agenda-compact-blocks t)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-default-appointment-duration 30)
(add-hook 'org-agenda-mode-hook
          (lambda () (local-set-key [tab] 'org-agenda-tree-to-indirect-buffer)))
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-span 45)
(setq org-deadline-warning-days 90)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-sorting-strategy
      (quote
       ((agenda priority-down alpha-up)
        (todo priority-down alpha-up)
        (tags priority-down alpha-up))))
(setq org-agenda-prefix-format
      (quote
       ((agenda . "%s %?-12t %e ")
        (timeline . "  %s")
        (todo . " %i %e ")
        (tags . " %i %e ")
        (search . " %i %e "))))
(setq org-columns-default-format
      "%75ITEM %TODO %PRIORITY %SCHEDULED %DEADLINE %CLOSED %ALLTAGS")
(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))
(setq org-agenda-window-setup 'reorganize-frame)
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)

;; Custom agenda views

(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property. If a is before b, return -1. If a is after b, return 1. If they are equal return t."
  (lexical-let ((prop prop))
               #'(lambda (a b)

                   (let* ((a-pos (get-text-property 0 'org-marker a))
                          (b-pos (get-text-property 0 'org-marker b))
                          (a-date (or (org-entry-get a-pos prop)
                                      (format "<%s>" (org-read-date t nil "now"))))
                          (b-date (or (org-entry-get b-pos prop)
                                      (format "<%s>" (org-read-date t nil "now"))))
                          (cmp (compare-strings a-date nil nil b-date nil nil))
                          )
                     (if (eq cmp t) nil (signum cmp))
                     ))))
(defun remove-priority (str)
  (replace-regexp-in-string "\\[#[^\\[]*\\] " "" str))
(defun extract-link-text (str)
  (replace-regexp-in-string "\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]" "\\3" str))
(defun org-cmp-alpha-2 (a b)
  "Compare the headlines, alphabetically. (after extract link texts if any links present)"
  (let* ((pla (text-property-any 0 (length a) 'org-heading t a))
         (plb (text-property-any 0 (length b) 'org-heading t b))
         (ta (and pla (substring a pla)))
         (tb (and plb (substring b plb)))
         (case-fold-search nil))
    (when pla
      (when (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp a) "")
                                  "\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *") ta)
        (setq ta (substring ta (match-end 0))))
      (setq ta (downcase ta)))
    (when plb
      (when (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp b) "")
                                  "\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *") tb)
        (setq tb (substring tb (match-end 0))))
      (setq tb (downcase tb)))
    (setq ta (extract-link-text ta))
    (setq tb (extract-link-text tb))
    (cond ((not (or ta tb)) nil)
          ((not ta) +1)
          ((not tb) -1)
          ((string-lessp ta tb) -1)
          ((string-lessp tb ta) +1))))
(setq org-agenda-custom-commands
      (quote
       (
        ("Q" "Closed Tasks"
         ((tags "CLOSED>=\"<-4w>\"" (
                                     (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
                                     (org-agenda-sorting-strategy '(user-defined-down))
                                     (org-agenda-overriding-header (format "Tasks done in the last week (%s)" (org-agenda-count "CLOSED")))
                                     )))
         nil)
        ("H" "Z Tasks"
         ((tags-todo "+PRIORITY=\"Z\""
                     ((org-agenda-overriding-header (format "Z Tasks (%s)" (org-agenda-count ""))))))
         nil)
        ("W" "Work ToDos"
         ((tags-todo "+work"
                     ((org-agenda-overriding-header (format "Work Tasks (%s)" (org-agenda-count "")))
                      (org-agenda-hide-tags-regexp "work")
                      )))
         nil)
        ("E" "Non-Work ToDos"
         ((tags-todo "-work" (
                              (org-agenda-overriding-header (format "Non-Work Tasks (%s)" (org-agenda-count "")))
                              (org-agenda-cmp-user-defined 'org-cmp-alpha-2)
                              (org-agenda-sorting-strategy '(user-defined-up))
                              )))
         nil)
        )))

;; Export

(setq org-html-htmlize-output-type 'css)
(setq org-html-html5-fancy t
      org-html-doctype "html5")

(setq org-export-backends (quote (html icalendar md)))

;;Refile

(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path (quote file))       ; Show full paths for refiling

;; Clocking

(setq org-log-into-drawer "LOGBOOK")
(setq org-remember-clock-out-on-exit t)
(setq org-clock-clocked-in-display (quote both))

;; Miscelanous

(setq org-modules (quote (org-crypt org-habit org-mouse)))
(defun org-show-current-heading-tidily ()
  (interactive)
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

;;Dashboard

(defun org-dashboard ()
  "Dashboard-like setting in org"
  (interactive)
  (setq org-agenda-sticky t)
  (setq org-agenda-window-setup 'current-window)
  (setq-default mode-line-format nil)
  (split-window-right)
  ;; (split-window-below)
  ;; (org-agenda nil "W")
  ;; (other-window 1)
  (org-agenda nil "E")
  (other-window 1)
  (split-window-below)
  (org-agenda nil "a")
  (other-window 1)
  (org-agenda nil "Q")
  ;; (other-window 1)
  ;; (shrink-window-if-larger-than-buffer)
  ;; (other-window 2)
  ;; (shrink-window-horizontally 10)
  ;; (other-window 1)
  ;; (shrink-window 15)
  ;; (other-window 1)
  (run-with-timer 0 (* 5 60) 'refresh-dashboard)
  )
(global-set-key (kbd "<f7>") 'org-dashboard)

(defun refresh-dashboard ()
  "Run some commands in sequence."
  (interactive)
  ;; (message "%s" "i started")
  ;; (message nil)
  (cl-loop repeat 3 do (execute-kbd-macro (kbd "r")) (other-window 1))
  ;; (message "%s" "i ran")
  ;; (message nil)
  )

(require 'cl)
(defun bk-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
        (kill-matching-buffers regexp)))
(defun close-dashboard ()
  "Dashboard-like setting in org"
  (interactive)
  (cancel-function-timers 'refresh-dashboard)
  (bk-kill-buffers ".*Org.*Agenda.*")
  (delete-other-windows)
  )


(provide 'init-org-alt)
