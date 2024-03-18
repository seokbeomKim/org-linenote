;;; org-linenote.el --- A package to add notes based on source code tree -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Jason Kim <sukbeom.kim@gmail.com>
;; Created: February 18, 2024
;; Modified: February 18, 2024
;; Version: 0.1.2
;; Keywords: tools, note, org
;; Homepage: https://github.com/seokbeomKim/org-linenote
;; Package-Requires: ((emacs "29.1") (projectile "2.8.0") (vertico "1.7"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a Emacs package to add notes motivated by VSCode's Line Note.

;; Setup:

;; (require 'org-linenote)

;;; Code:

(require 'projectile)
(require 'vertico)
(require 'subr-x)

(defvar org-linenote--default-extension ".org"
  "Configure the default note extension.
If you set this to `.md', then it supports compability with
vscode's linenote.")

(defvar org-linenote--highlight-style '(:background "medium turquoise" :underline nil)
  "Highlight style for the note.")

(defvar org-linenote--in-browse nil
  "A flag of browse function.")

(defvar org-linenote--prev-window -1
  "Temporary value to store previously focused window.")

(defvar-local org-linenote--overlays nil
  "Overlays in a local buffer.")

(defvar-local org-linenote-mode nil
  "Org-linenote mode flag.")

(defun org-linenote--lines-to-highlight (filename)
  "Get beginning/end line number to highlight from `FILENAME'."
  (let* ((basename filename)
         (matched (string-match "\\`.*#L\\([0-9]+\\)\\(-L\\)?\\([0-9]+\\)?.*\\'" basename)))
    (if matched
        (let ((line-beginning (string-to-number (match-string 1 basename)))
              (line-end (string-to-number (or (match-string 3 basename) (match-string 1 basename)))))
          `(,line-beginning ,(+ 1 line-end)))
      (let ((line (string-to-number (match-string 1 basename))))
        `(,line ,(+ 1 line))))))

(defun org-linenote--highlight (filename &optional undo)
  "Highlight the line specified in FILENAME.
if `UNDO' is t, then unhighlight regions related to `FILENAME'."
  (let* ((lines (org-linenote--lines-to-highlight filename))
         (min-line (- (car lines) 1))
         (max-line (- (car (cdr lines)) 1))
         (diff-line (- max-line min-line)))
    (goto-char (point-min))
    (forward-line min-line)
    (beginning-of-line)
    (set-mark (line-beginning-position))
    (forward-line diff-line)
    (org-linenote--remove-overlays-at (region-beginning))
    (if (null undo)
        (let ((ov (make-overlay (region-beginning) (- (region-end) 1))))
          (overlay-put ov 'face org-linenote--highlight-style)
          (if (overlay-buffer ov)
              (push ov org-linenote--overlays))))
    (forward-line -1)
    (deactivate-mark)
    (goto-char (point-min))
    (forward-line min-line)))

(defun org-linenote-mark-notes ()
  "Highlight lines with annotated notes."
  (let* ((current-line (line-number-at-pos))
         (note-relpath (org-linenote--get-relpath))
         (note-rootdir (org-linenote--get-note-rootdir))
         (list-notes (directory-files (expand-file-name (or (file-name-directory note-relpath) "")
                                                        note-rootdir)
                                      nil (file-name-base note-relpath))))
    (mapc #'org-linenote--highlight list-notes)
    (goto-char (point-min))
    (forward-line (1- current-line))))

(defun org-linenote--get-relpath ()
  "Get the relative path of the current file."
  (if (projectile-project-root)
      (string-remove-prefix (projectile-project-root) (buffer-file-name))
    (file-name-nondirectory (buffer-file-name))))

(defun org-linenote--validate ()
  "Validate the current working directory."

  (unless org-linenote-mode
    (error "Please enable org-linenote mode"))

  (if-let ((project-root (projectile-project-root)))
      (let* ((note-dir (org-linenote--get-note-rootdir))
             (note-path (expand-file-name
                         (or (file-name-directory (org-linenote--get-relpath)) "")
                         note-dir)))
        (make-directory note-dir t)
        (make-directory note-path t)
        t)
    (error "The working directory is not a git repo")))

(defun org-linenote--get-note-rootdir()
  "Get the root directory of the note based on projectile.
If not available, then return empty string."
  (if-let ((project-root (projectile-project-root)))
      (let ((note-dir (expand-file-name ".linenote" project-root)))
        (unless (file-exists-p note-dir)
          (make-directory note-dir t))
        note-dir)
    ""))

(defalias 'org-linenote-edit-annotate #'org-linenote-add-annotate
  "This is an alias to `org-linenote-add-annotate'.")

(defun org-linenote-get-linenum-string ()
  "Get the linenum string for filename."
  (if (use-region-p)
      (format "#L%S-L%S"
              (line-number-at-pos (use-region-beginning))
              (line-number-at-pos (use-region-end)))
    (format "#L%S" (line-number-at-pos))))

(defun org-linenote--get-line-range-by-fname (filename)
  "Extracts line range from filename using regex.
`FILENAME' must be passed by argument."
  (with-temp-buffer
    (insert filename)
    (goto-char (point-min))
    (when (re-search-forward ".*#L\\([0-9]+\\)\\(-L\\([0-9]+\\)\\)?\\(.*\\)?" nil t)
      (let ((min (string-to-number (match-string 1)))
            (max (if (match-beginning 3)
                     (string-to-number (match-string 3))
                   nil)))
        (cons min max)))))

(defun org-linenote--check-line-range (line)
  "Check if there is a note within the `LINE'."
  (let ((res nil))
    (dolist (file (org-linenote--directory-files))
      (let* ((range (org-linenote--get-line-range-by-fname file))
             (min (car range))
             (max (cdr range)))
        (if (and max
                 (and (<= min line)
                      (<= line max)))
            (setq res file)
          (if (and (null max)
                   (= min line))
              (setq res file)))))
    res))

(defun org-linenote--check-already-exist ()
  "Check whether the note for current line exists."
  (or (org-linenote--check-line-range (line-number-at-pos))
      (expand-file-name (concat (org-linenote--get-relpath)
                                (org-linenote-get-linenum-string)
                                org-linenote--default-extension)
                        (org-linenote--get-note-rootdir))))

(defun org-linenote-add-annotate ()
  "Annotate on the line."
  (interactive)
  (org-linenote--validate)
  (let ((note-path (org-linenote--check-already-exist))
        (working-buf (selected-window))
        (current-line (line-number-at-pos)))
    (pop-to-buffer (find-file-noselect note-path) 'reusable-frames)
    (save-buffer)
    (select-window working-buf)
    (goto-char (point-min))
    (forward-line current-line)
    (org-linenote-mark-notes)
    (pop-to-buffer (find-file-noselect note-path) 'reusable-frames)))

(defun org-linenote-remove-annotate ()
  "Remove the annotation on the line."
  (interactive)
  (org-linenote--validate)
  (let ((note-path (org-linenote--check-already-exist)))
    (if (not (file-exists-p note-path))
        (error "No notes to remove from here")
      (condition-case _
          (progn
            (pop-to-buffer (find-file-noselect note-path) 'reusable-frames)
            (let ((do-remove (yes-or-no-p (format "Remove %S?" note-path))))
              (delete-window)
              (when do-remove
                (delete-file note-path)
                (org-linenote--highlight (file-name-base note-path) t))))
        (quit (delete-window))))))

(defun org-linenote--directory-files ()
  "Do `directory-files' to find notes (except for files with names ending with ~)."
  (directory-files (expand-file-name (or (file-name-directory (org-linenote--get-relpath)) "")
                                     (org-linenote--get-note-rootdir))
                   'full (concat (file-name-base (org-linenote--get-relpath)) ".[^.].*[^~]$")))

(defun org-linenote--get-note-list ()
  "Get the list of note in the current buffer."
  (setq org-linenote--in-browse t)
  (setq org-linenote--prev-window (selected-window))
  (org-linenote--directory-files))

(defun org-linenote--post-command-hook ()
  "Post-command-hook implementation."
  (when org-linenote--in-browse
    (let ((focused-item (nth (symbol-value 'vertico--index) (symbol-value 'vertico--candidates))))
      (when (length> focused-item 0)
        (select-window org-linenote--prev-window)
        (org-linenote--highlight focused-item)
        (if (active-minibuffer-window)
            (select-window (active-minibuffer-window)))))))

(defun org-linenote--overlayed-by (ov)
  "Check `OV' instance is actually overlayed by this package."
  (member ov org-linenote--overlays))

(defun org-linenote--remove-overlays-at (pos)
  "Remove overlays at `POS' by checking the `org-linenote--overlays'."
  (mapc (lambda (ov)
          (if (org-linenote--overlayed-by ov)
              (progn
                (delete-overlay ov)
                (delete ov org-linenote--overlays)))) (overlays-at pos)))

(defun org-linenote--minibuf-setup-hook ()
  "A function added to minibuf-setup-hook used for org-linenote."
  (add-hook 'post-command-hook #'org-linenote--post-command-hook))

(defun org-linenote--minibuf-exit-hook ()
  "A function added to minibuf-exit-hook used for org-linenote."
  (setq org-linenote--in-browse nil)
  (setq org-linenote--prev-window -1)
  (org-linenote--remove-overlays-at (line-beginning-position))
  (remove-hook 'post-command-hook #'org-linenote--post-command-hook))

(define-minor-mode org-linenote-mode
  "Toggle `org-linenote-mode'."
  :init-value nil
  :global nil
  :lighter " Org-Linenote"

  (if org-linenote-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'org-linenote--minibuf-setup-hook)
        (add-hook 'minibuffer-exit-hook #'org-linenote--minibuf-exit-hook)
        (org-linenote-mark-notes))
    (progn
      (mapc (lambda (ov)
              (delete-overlay ov)) org-linenote--overlays)
      (setq org-linenote--overlays nil))))

(defun org-linenote-browse ()
  "Browse notes for this buffer."
  (interactive)
  (org-linenote--validate)
  (condition-case err
      (funcall #'org-linenote--browse)
    (quit
     (org-linenote--minibuf-exit-hook)
     (org-linenote-mark-notes)
     (message "Note browsing aborted: %s" err))))

(defun org-linenote--browse ()
  "Browse notes in the current buffer.
Argument CHOICE user's selection."
  (let ((choice (completing-read "Choose the note: "
                                 (org-linenote--get-note-list) nil t)))
    (org-linenote-mark-notes)
    (pop-to-buffer (find-file-noselect choice 'reusable-frames))))

(provide 'org-linenote)
;;; org-linenote.el ends here
