;;; vc-timemachine.el --- Walk through revisions of a file  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Credits:
;;
;;  Peter Stiernström - wrote the original, git-only version
;;  John Yates        - refactored Peter's code for the VC environment

;;; Commentary:
;;
;; A timemachine buffer (tmbuf) is a sliding read-only window over the
;; distinct revisions of a single file on a VCS branch.  To create a tmbuf:
;;
;;    (vc-prefix-map ",")   vc-tm-revision-head
;;
;; Timemachine implements prefix-free minor mode vc-tm-mode:
;;
;;    "," . vc-tm-revision-head
;;    "~" . vc-tm-revision-select
;;    "g" . vc-tm-revision-nth
;;    "n" . vc-tm-revision-newer
;;    "p" . vc-tm-revision-older
;;    "q" . vc-tm-quit
;;    "s" . vc-tm-revision-complete-subject
;;    "w" . vc-tm-abbreviated-revision-to-kill-ring
;;    "W" . vc-tm-revision-to-kill-ring
;;
;; To support timemachine functionality a VCS backend needs to implement:
;;
;;   - tm-revisions (file)
;;
;; And ideally:
;;
;;   - tm-map-line-no (file from-revision from-line to-revision from-is-older)
;;
;; For more details see the large comment at the front of vc.el.

;;; Todo:
;;
;; * implement missing blame
;; * implement missing show-commit
;; * vc-tm-create: when called from a revision buffer, should jump to that revision
;; * vc-tm--timemachine: confirm revision is present in tmbuf--branch-revisions

;;; Code:

(require 'vc)

(defgroup vc-timemachine nil
  "Timemachine functionality for VC backends."
  :group 'vc
  :version "30.1")

(defcustom vc-tm-date-format
  "%a %I:%M %p %Y-%m-%d"
  "Revision creation date format (emphasis on easy date comparison)."
  :type 'string
  :version "30.1")

(defcustom vc-tm-echo-area t
  "When non-nil, while navigating revisions, show details in the echo-area."
  :type 'boolean
  :version "30.1")

(defcustom vc-tm-echo-area-detail 'subject
  "What to display when `vc-tm-echo-area` is t.
Available values are:
`ID`      : The revision's ID (commit hash)
`subject` : The revision's message subject line"
  :type '(radio (const :tag "Revision ID (commit hash)" commit)
                (const :tag "Revision message subject line" subject))
  :version "30.1")

(defface vc-tm-echo-area-detail-face
  '((((class color) (background dark))
     :foreground "yellow")
    (((class color) (background light))
     :foreground "yellow4"))
  "Face to use in echo area details display."
  :version "30.1")

(defcustom vc-tm-echo-area-author t
  "Prepend author to echo-area details."
  :type 'boolean
  :version "30.1")

(defface vc-tm-echo-area-author-face
  '((((class color) (background dark))
     :foreground "orange")
    (((class color) (background light))
     :foreground "DarkOrange4"))
  "Face to use in echo area details display for author."
  :version "30.1")

(defcustom vc-tm-abbreviation-length 12
  "Number of chars from full revision id to use for abbreviation."
  :type 'integer
  :version "30.1")

(defcustom vc-tm-quit-to-invoking-buffer t
  "Switch to invoking buffer on ‘vc-tm-quit’."
  :type 'boolean
  :version "30.1")

(defvar vc-tm-switch-to-revision-hook nil
  "Normal hook run after a timemachine buffer switches to another revision.")

(defvar-local vc--tmbuf nil
  "Bind a non-timemachine buffer to its tmbuf.")
(put 'vc--tmbuf 'permanent-local t)

(defvar-local vc--tmbuf-file nil
  "Version controlled file being traversed by this tmbuf.")
(put 'vc--tmbuf-file 'permanent-local t)
(defvar-local vc--tmbuf-backend nil
  "The VC backend being used by this tmbuf")
(put 'vc--tmbuf-backend 'permanent-local t)
(defvar-local vc--tmbuf-branch-index nil
  "Zero-base index into vc--tmbuf-branch-revisions.")
(put 'vc--tmbuf-branch-index 'permanent-local t)
(defvar-local vc--tmbuf-branch-revisions nil
  "When non-nil, a vector of revision-info lists.")
(put 'vc--tmbuf-branch-revisions 'permanent-local t)

(defun vc-tm--timemachine ()
  "Return a valid tmbuf for the current buffer.

A tmbuf (timemachine buffer) has a properly filled-out set of vc--tmbuf*
local variables.  If the current buffer is already a valid tmbuf then
just return that buffer.  Otherwise create a revbuf for the current buffer's
file (or, if the current buffer is an indirect buffer, then for the base
buffer's file) and set its vc--tmbuf* data.  Most importantly, set
vc--tmbuf-branch-revisions  to an ordered vector of revision information
lists for the revisions on the work file's branch."
  (if vc--tmbuf-backend
      (current-buffer)
    (let ((revision vc--revbuf-revision)) ;; caller could be a revision buffer
      (set-buffer (or (buffer-base-buffer) (current-buffer)))
      (vc-ensure-vc-buffer)

      (let* ((parent vc-parent-buffer)
             (file (buffer-file-name))
             (backend  (if vc-force-bos 'bos (vc-backend file)))
             (work-rev (vc-working-revision file))
             (tmbuf vc--tmbuf))

        ;; ensure that there is a revision with which to work.
        (unless revision
          (setq revision work-rev))

        ;; Validate any current tmbuf.
        (when tmbuf
          (with-current-buffer tmbuf
            (unless (and (equal file vc--tmbuf-file)
                         (equal backend  vc--tmbuf-backend)
                         ;; TODO: confirm that revision is in vc--tmbuf-branch-revisions.
                         )
              ;; Discard an unvalid TM buffer.
              (setq tmbuf nil))))

        ;; Create a fresh TM buffer if needed.
        (unless tmbuf
          (with-current-buffer (setq tmbuf (get-buffer-create "*nascent TM*"))
            (setq vc-parent-buffer parent)
            (setq vc--tmbuf tmbuf)
            (setq vc--revbuf-revision revision)
            (setq vc--tmbuf-file file)
            (setq vc--tmbuf-backend backend)
            (setq vc--tmbuf-branch-index 0)
            (setq vc--tmbuf-branch-revisions
                  (with-temp-buffer
                    (prog2
                        (message "Enumerating revisions...")
                        (let* ((vec (cl-coerce (if (eq backend 'bos)
                                         (vc-bos-tm-revisions file)
                                       (vc-call-backend backend 'tm-revisions file))
                                     'vector))
                               (branch (nreverse vec)))
                          branch)
                      (message "Enumerating revisions...done"))))))

        (set-buffer tmbuf)
        tmbuf))))

;;;###autoload
;; Doc string duplicated in vc-hooks.el.
(defun vc-tm-revision-head (&optional bos)
  "Show work file's HEAD revision at index 0 on checked-out branch.
With a prefix argument, disregard registration under any other
VCS and show vc-bos backup-on-save revisions."
  (interactive "P")
  (let ((vc-force-bos bos))
    (with-current-buffer (vc-tm--timemachine)
      (vc-tm--switch-to-revision 0))))

(defun vc-tm-revision-newer (&optional arg)
  "Show work file's ARG'th newer revision on checked-out branch."
  (interactive "p")
  (or arg (setq arg 1))
  (with-current-buffer (vc-tm--timemachine)
    (vc-tm--switch-to-revision (- vc--tmbuf-branch-index arg))))

(defun vc-tm-revision-older (&optional arg)
  "Show work file's ARG'th older revision on checked-out branch."
  (interactive "p")
  (or arg (setq arg 1))
  (with-current-buffer (vc-tm--timemachine)
    (vc-tm--switch-to-revision (+ vc--tmbuf-branch-index arg))))

(defun vc-tm-revision-nth (number)
  "Show work file's N'th revision on checked-out branch (0 is HEAD)."
  (interactive "nEnter revision position: ")
  (with-current-buffer (vc-tm--timemachine)
    (vc-tm--switch-to-revision number)))

(defun vc-tm-revision-complete-subject ()
  "Choose work file's revision on checked-out branch by completing its subject."
  (interactive)
  (let* ((s (completing-read
             "Subject: "
             (mapcar (apply-partially #'nth 2) vc--tmbuf-branch-revisions))))
    (vc-tm--switch-to-revision
     (cl-loop for revision-number from 1
              for info across vc--tmbuf-branch-revisions
              if (equal s (nth 2 info)) return revision-number
              finally (error "Subject not found")))))

(defun vc-tm--show-echo-area-details (revision-info date)
  "Show details for REVISION-INFO in echo-area."
  (if (eq vc--tmbuf-backend 'bos)
      (message "%s" date)
    (let* ((author (if vc-tm-echo-area-author (concat " | " (nth 3 revision-info)) ""))
           (sha-or-subject (if (eq vc-tm-echo-area-detail 'commit)
                               (car revision-info)
                             (nth 2 revision-info))))
      (message "%s%s: %s"
               date
               (propertize author 'face 'vc-tm-echo-area-author-face)
               (propertize sha-or-subject 'face 'vc-tm-echo-area-detail-face)))))

(defun vc-tm--switch-to-revision (to-index)
  "Show work file's revision at position TO-INDEX on checked-out branch."
  (let ((branch-length (length vc--tmbuf-branch-revisions))
        (calling-window-buffer (window-buffer))
        (cursor-win-pos))
    (with-current-buffer calling-window-buffer
      (setq cursor-win-pos (vc-tm--get-cursor-win-position)))
    (cond
     ((< to-index 0)
      (error "Revisions indices are never negative (%d); newest revision has index 0"
             to-index))
     ((> to-index branch-length)
      (error "Revision index %d is too large; oldest available revision has index %d)"
             to-index branch-length))
     (t
      (let* ((new-revision-info (vc-tm--tmbuf-revision-info to-index))
             (new-revision (car new-revision-info))
             (abbrev-rev (vc-tm--abbreviate new-revision))
             (date (format-time-string
                    vc-tm-date-format (nth 1 new-revision-info)))
             (n-of-m (format " [%d/%d %s]" to-index (- branch-length 1) date))
             ;; Use the file-name from new-revision-info to reveal renames.
             (file-name (file-name-nondirectory (nth 4 new-revision-info)))
             (tmbuf (current-buffer))
             (from-line (line-number-at-pos))
             (to-line from-line))
        (when vc--revbuf-revision
          (unless (= vc--tmbuf-branch-index to-index)
            (setq to-line (vc-tm--map-line-no from-line to-index))))
        (vc-find-revision vc--tmbuf-file new-revision vc--tmbuf-backend tmbuf)
        ;; Reuse timemachine windows, otherwise create them in some other-window.
        (if (eq calling-window-buffer tmbuf)
            (switch-to-buffer tmbuf)
          (switch-to-buffer-other-window tmbuf))
        (vc-tm-mode +1)
        (forward-line (- to-line (line-number-at-pos)))
        (vc-tm--set-cursor-win-position cursor-win-pos)
        (setq vc--tmbuf-branch-index to-index)

        (rename-buffer (concat file-name " " abbrev-rev) t)
        (setq mode-line-buffer-identification
              (list (propertized-buffer-identification "%12b") n-of-m))

        (when vc-tm-echo-area
          (vc-tm--show-echo-area-details new-revision-info date))))
      (run-hooks vc-tm-switch-to-revision-hook))))

(defun vc-tm--map-line-no (from-line to-index)
  "Return a suggested new current line number after a revision jump."
  ;; Newer and older are first guesses; subsequently they may get swapped.
  (let* ((to-info (vc-tm--tmbuf-revision-info to-index))
         (to-revision (car to-info))
         (from-revision vc--revbuf-revision)
         (from-is-older (< vc--tmbuf-branch-index to-index))
         (backend vc--tmbuf-backend)
         (file vc--tmbuf-file))
    (with-temp-buffer
      (vc-call-backend backend 'tm-map-line-no file
                       from-revision from-line to-revision from-is-older))))

(defun vc-default-tm-map-line-no (_backend _file _from-revision from-line
                                           _to-revision _from-is-older)
  "Default `map-line-no' implementation.
It merely returns FROM-LINE."
  from-line)

(defun vc-tm--get-cursor-win-position ()
  "Return the cursor visual line number w.r.t. the current window first line."
  (let* ((win-point-min (save-excursion (move-to-window-line 0) (point)))
         (cur-pos (count-screen-lines win-point-min (point))))
    cur-pos))

(defun vc-tm--set-cursor-win-position (POS)
  "Set the cursor position to the POS visual line w.r.t. the window first line."
  (recenter POS))

(defun vc-tm--abbreviate (revision)
  "Return REVISION abbreviated to `vc-tm-abbreviation-length' chars."
  (if (length< revision vc-tm-abbreviation-length)
      revision
    (substring revision 0 vc-tm-abbreviation-length)))

(defun vc-tm-revision-to-kill-ring ()
  "Kill the current revision's full commit hash."
  (interactive)
  (let ((revision (vc-tm--tmbuf-revision)))
    (message revision)
    (kill-new revision)))

(defun vc-tm-abbreviated-revision-to-kill-ring ()
  "Kill the current revision's abbreviated commit hash."
  (interactive)
  (let ((revision (vc-tm--abbreviate (vc-tm--tmbuf-revision))))
    (message revision)
    (kill-new revision)))

;; (defun vc-tm-show-commit ()
;;   "Show commit for current revision."
;;   (interactive)
;;   (let ((rev (vc-tm--tmbuf-revision)))
;;     (if (fboundp 'magit-show-commit)
;;         (magit-show-commit rev)
;;       (message "You need to install magit to show commit"))))

;; (defun vc-tm-blame ()
;;   "Call ‘magit-blame’ on current revision."
;;   (interactive)
;;   (if (fboundp 'magit-blame)
;;       (let ((magit-buffer-revision (car tm--revision-info)))
;;      (magit-blame))
;;     (message "You need to install magit for blame capabilities")))

(defun vc-tm--tmbuf-revision (&optional index)
  "Return the unique revision id for this tmbuf's current revision."
  (car (vc-tm--tmbuf-revision-info index)))

(defun vc-tm--tmbuf-revision-info (&optional index)
  "Return the revision-info list for this tmbuf's current revision."
  (aref vc--tmbuf-branch-revisions (or index vc--tmbuf-branch-index)))

(defun vc-tm-quit ()
  "Exit the timemachine."
  (interactive)
  (let ((parent-buffer-name buffer-file-name))
    (kill-buffer)
    (let ((parent-buffer (find-buffer-visiting parent-buffer-name)))
      (when (and parent-buffer vc-tm-quit-to-invoking-buffer)
        (switch-to-buffer parent-buffer nil t)))))

(transient-define-prefix vc-tm-help ()
  "Show online help."
  ["Navigate"
   [("," "switch to HEAD revision" vc-tm-revision-head)
    ("~" "switch to selected revision id" vc-tm-revision-select)
    ("n" "switch to next revision" vc-tm-revision-newer)
    ("p" "switch to previous revision" vc-tm-revision-older)
    ("j" "switch to n'th revision" vc-tm-revision-nth)
    ("s" "switch to subject revision" vc-tm-revision-complete-subject)]]
  ["Revision ID to kill ring"
   [("w" "abbreviated revision id" vc-tm-abbreviated-revision-to-kill-ring)
    ("W" "full revision id" vc-tm-revision-to-kill-ring)]]
  ["Misc"
   [ ;; ("B" "blame current revision" vc-tm-blame)
;;  ("C" "view commit" vc-tm-show-commit)
    ("?" "show help" vc-tm-help)
    ("q" "quit" vc-tm-quit)]])

(define-minor-mode vc-tm-mode
  "VC Timemachine, feel the wings of history."
  :init-value nil
  :lighter " TM"
  :keymap
  '(("," . vc-tm-revision-head)
    ("~" . vc-tm-revision-select)
;;  ("B" . vc-tm-blame)
;;  ("C" . vc-tm-show-commit)
    ("j" . vc-tm-revision-nth)
    ("n" . vc-tm-revision-newer)
    ("p" . vc-tm-revision-older)
    ("q" . vc-tm-quit)
    ("s" . vc-tm-revision-complete-subject)
    ("S" . vc-tm-revision-complete-subject) ; in vc-mode "s" is vc-create-tag
    ("w" . vc-tm-abbreviated-revision-to-kill-ring)
    ("W" . vc-tm-revision-to-kill-ring)
    ("?" . vc-tm-help))
  :group 'vc-timemachine)

(provide 'vc-timemachine)

;;; vc-timemachine.el ends here
