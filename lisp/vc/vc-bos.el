;;; vc-bos.el --- VC Backup On Save (to RCS)

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;; Maintainer: Conor Nash <conor@nashcobusinessservicesllc.com>
;; Maintainer: John S. Yates, Jr. <john@yates-sheets.org>
;; Version: 0.8

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Derived from and then heavily modified:
;;   https://www.emacswiki.org/emacs/backup-each-save.el
;;
;; Modern version control systems, such as git, are wonderful.  But they
;; have drawbacks when dealing with lightweight save operations:
;;
;; * Too invasive: new revisions are created only by explicitly action;
;;   this includes supplying a commit message (even if empty)
;; * Too coarse: a revision captures an entire "project"
;; * Too smart: even files listed in .gitignore (or equivalent) remain
;;   eligible for edting and hence deserve to get backed-up
;; * Requires setup: what about files that have no project?
;;
;; Enter vc-bos...
;;
;; vc-bos provides easy access to past saved states of edited files by
;; integrating with VC's timemachine functionality.  To do this it
;; requires that VC's vc-mirror-root option be set and that it have
;; '/RCS' as one of its directory components (typically the last).
;;
;; Given such a configuration, vc-bos maintains a mirror tree of RCS
;; 'control' files below vc-mirror-root.  A control file appears at
;; the same position and has exactly the same name as the file that
;; it tracks (meaning no ',v' suffix).  This works because RCS treats
;; *any* file *anywhere* beneath an RCS directory as a control file.
;;
;; On a FIRST change and EVERY subsequent save, vc-bos...
;;
;; * Qualifies the buffer's path
;; * Ensures existence of a mirror directory beneath vc-mirror-root
;; * Records the newly saved file as the latest RCS revision with
;;   a timestamp and an empty commit message
;;
;; vc-bos's tracking is independent of whether a file is track by
;; any other VCS.
;;
;; vc-bos requires that the rcs executable be available (typically
;; installed at /usr/bin/rcs).
;;
;; To activate globally, place this file in your `load-path', ensure
;; that vc-mirror-root is set, then add the following to your init.el:
;;
;;     (vc-bos-mode t)
;;
;; To filter which files vc-bos backs up, setup a custom function for
;; `vc-bos-filter-function'.  For example, to filter out the saving of
;; gnus .newsrc.eld files, do:
;;
;;     (defun vc-bos-no-newsrc-eld (filename)
;;       (cond
;;        ((string= (file-name-nondirectory filename) ".newsrc.eld") nil)
;;        (t t)))
;;     (setq vc-bos-filter-function 'bos-no-newsrc-eld)

;;; Todo:
;;
;; * garbage collection: it would be nice to have a cron script to purge
;;   ancient revisions

;;; Notes:

;;; Code:

(require 'vc-rcs)


(defgroup vc-bos nil
  "Backup On Save (to an RCS file)."
  :group 'vc
  :group 'vc-timemachine
  :group 'backup
  :version "30.1")

(defcustom vc-bos-remote-files nil
  "Whether to backup remote files at each save (off by default)."
  :type 'boolean
  :group 'vc-bos
  :version "30.1")

(defcustom vc-bos-filter-function #'identity
  "Function which should return non-nil if the file should be backed up."
  :type 'function
  :group 'vc-bos
  :version "30.1")

(defcustom vc-bos-size-limit 50000
  "Maximum size (in byte) beyond which a file will not get backed-up.
Setting this variable to nil disables the size check."
  :type 'natnum
  :group 'vc-bos
  :version "30.1")

(defcustom vc-bos-rcs "/usr/bin/rcs"
  "Path to the rcs executable (required for vc-bos functionality)."
  :type '(file :must-match t)
  :group 'vc-bos
  :version "30.1")

(defconst vc-bos-witnesses-regex
  "/\\(SCCS\\|RCS\\|CVS\\|MCVS\\|[.]src\\|[.]svn\\|[.]git\\|[.]hg\\|[.]bzr\\|_MTN\\|_darcs\\|[{]arch[}]\\)/"
  "Writes to any point below one of these witnesses should be ignored.

FIXME: This is a regex-ified copy of vc-hooks's vc-directory-exclusion-list.")


;; This implementation does not handle RCS branches.
;;;###autoload
(defun vc-bos-tm-revisions (file)
  "Return data about backup-on-save revisions of FILE."
  (let ((master-file (concat vc-mirror-root file)))
    (vc-do-command t 0 vc-bos-rcs master-file "log"))
  (vc-rcs-tm-revisions-parse-log file))

;;;###autoload
(defun vc-bos-find-revision (file rev buffer)
  "Return in BUFFER FILE's backup-on-save revision REV."
  (let ((master-file (concat vc-mirror-root file)))
    (vc-do-command (or buffer "*vc*") 0
                   vc-bos-rcs master-file "co" "-q" (concat "-p" rev))))

(defun vc-bos-add-revision ()
  "Record a new RCS backup-on-save revision of buffer's file."
  (setq vc-consult-headers nil)
  (let ((bfn buffer-file-name))
    (when (and bfn
               (not (string-match-p vc-bos-witnesses-regex bfn))
               (or vc-bos-remote-files
		   (not (file-remote-p bfn)))
	       (or (not vc-bos-size-limit)
		   (<= (buffer-size) vc-bos-size-limit))
               (funcall vc-bos-filter-function bfn))
      (let* ((mirror-file (vc-bos--mirror-file bfn)))
        (call-process vc-bos-rcs
                      nil (get-buffer-create "*vc-bos-log*") nil
                      "ci" "-l" "-m''" "-t-''" bfn mirror-file)))))

(defun vc-bos--mirror-file (file)
  "Return path to FILE's RCS control file within vc-mirror-root."
  (let* ((dir (file-name-directory file))
	 (file (file-name-nondirectory file))
	 (mirror-dir (concat (expand-file-name vc-mirror-root) dir))
         (mirror-file (concat mirror-dir file)))
    (unless (file-exists-p mirror-dir)
      (make-directory mirror-dir t))
    mirror-file))

(define-minor-mode vc-bos-mode
  "Silently backup saved files as new RCS revisions beneath vc-mirror-root.

Visit saved revisions using vc-tm-revision-head: (C-u C-x v ,)."
  :global t
  :group 'backup
  :group 'vc-bos
  :version "30.1"
  :lighter " BoS"
  (when vc-bos-mode
    (unless (and (stringp vc-mirror-root)
                 (string-match-p "/RCS$" vc-mirror-root))
      (setq vc-bos-mode nil)
      (error
       "vc-bos-mode requires vc-mirror-root (%s) to contains a '/RCS' component"
       vc-mirror-root))
    (add-hook 'first-change-hook #'vc-bos-add-revision)
    (add-hook 'after-save-hook   #'vc-bos-add-revision))
  (unless vc-bos-mode
    (remove-hook 'first-change-hook #'vc-bos-add-revision)
    (remove-hook 'after-save-hook   #'vc-bos-add-revision)))

(provide 'vc-bos)

;;; vc-bos.el ends here
