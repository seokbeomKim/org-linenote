;;; org-linenote.el --- A package inspired by VSCode Linenote -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Jason Kim <sukbeom.kim@gmail.com>
;; Created: February 18, 2024
;; Modified: April 10, 2024
;; Version: 0.2.3
;; Keywords: tools, note, org
;; Homepage: https://github.com/seokbeomKim/org-linenote
;; Package-Requires: ((emacs "29.1") (projectile "2.8.0") (vertico "1.7") (eldoc "1.11") (lsp-mode "9.0.0"))

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

;;; Setup:

;; (require 'org-linenote)

;;; Commentary:

;; This file provides a source for linenote that manages notes based on the line
;; number in a buffer.  The package provides some interactive functions:

;; - org-linenote-move-forward
;; - org-linenote-move-backward
;; - org-linenote-add-annotate
;; - org-linenote-edit-annotate (alias to org-linenote-add-annotate)
;; - org-linenote-remove-annotate
;; - org-linenote-browse
;; - org-linenote-find-root-dir
;; - org-linenote-find-note-dir
;; - org-linenote-auto-open

;; All notes are stored at $PROJECT_ROOT/.linenote directory.

;;; Code:

(require 'projectile)
(require 'vertico)
(require 'subr-x)
(require 'filenotify)
(require 'lsp-mode)
(require 'eldoc)

(defcustom org-linenote-default-extension ".org"
  "Configure the default note extension.
If you set this to `.md', then it supports compability with
vscode's linenote."
  :type 'string
  :group 'org-linenote)

(defface org-linenote--highlight-style '((t :background "medium turquoise" :underline nil))
  "Highlight style for the note.")

(defvar org-linenote--in-browse nil
  "A flag of browse function.")

(defvar org-linenote--prev-window -1
  "Temporary value to store previously focused window.")

(defvar org-linenote--buffers nil
  "The target buffer to ensure line tracking.")

(defvar-local org-linenote--overlays nil
  "Overlays in a local buffer.")

(defvar-local org-linenote--fwatch-id nil
  "File watcher id for org-linenote.")

(defvar-local org-linenote--follow-cursor nil
  "A flag indicating whether the org-linenote feature should follow \
the cursor.")

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
          (overlay-put ov 'face 'org-linenote--highlight-style)
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

(defun org-linenote--get-note-rootdir ()
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

(defun org-linenote--get-linenum-string ()
  "Get the linenum string for filename."
  (if (use-region-p)
      (format "#L%S-L%S"
              (line-number-at-pos (use-region-beginning))
              (line-number-at-pos (- (use-region-end) 1)))
    (format "#L%S" (line-number-at-pos))))

(defun org-linenote--get-line-range-by-fname (filename)
  "Extracts line range from filename using regex.
`FILENAME' must be passed by argument."
  (with-temp-buffer
    (insert filename)
    (goto-char (point-min))
    (if (re-search-forward ".*#L\\([0-9]+\\)\\(-L\\([0-9]+\\)\\)?\\(.*\\)?" nil t)
        (let ((min (string-to-number (match-string 1)))
              (max (if (match-beginning 3)
                       (string-to-number (match-string 3))
                     nil)))
          (cons min max)))))

(defun org-linenote--get-note-linum-by-direction (line is-forward)
  "Check if there is a note within the `LINE'.

If `IS-FORWARD' is t, then find the next note.  Otherwise, find
the previous note."
  (let ((res
         (cond (is-forward (line-number-at-pos (point-max)))
               (t 0)))
        (found nil))
    (dolist (file (org-linenote--directory-files))
      (let* ((range (org-linenote--get-line-range-by-fname file))
             (min (car range))
             (f (if is-forward #'< #'>)))
        (if (and (funcall f line min)
                 (funcall f min res))
            (progn
              (setq found t)
              (setq res min)))))
    (if found res)))

(defun org-linenote--move-forward (is-forward)
  "Move to the next note.

If `IS-FORWARD' is nil, then move to the previous note."
  (let* ((current-line (line-number-at-pos))
         (next-line (org-linenote--get-note-linum-by-direction
                     current-line
                     is-forward))
         (f (if is-forward #'> #'<)))
    (if (and next-line
             (funcall f next-line current-line))
        (forward-line (- next-line current-line))
      (message "No more notes"))))

(defun org-linenote-move-forward ()
  "Move to the next note."
  (interactive)
  (org-linenote--validate)
  (org-linenote--move-forward t))

(defun org-linenote-move-backward ()
  "Move to the previous note."
  (interactive)
  (org-linenote--validate)
  (org-linenote--move-forward nil))

(defun org-linenote--check-line-range (line)
  "Check if there is a note within the `LINE'."
  (let ((res nil))
    (dolist (file (org-linenote--directory-files))
      (let* ((range (org-linenote--get-line-range-by-fname file))
             (min (car range))
             (max (cdr range)))
        (if (and max
                 (<= min line)
                 (<= line max))
            (setq res file)
          (if (and (null max)
                   (= min line))
              (setq res file)))))
    res))

(defun org-linenote--check-note-exist ()
  "Check whether the note for current line exists.
If the note exists, return the absolute path, otherwise return nil."
  (org-linenote--check-line-range (line-number-at-pos)))

(defun org-linenote--get-candidate-note-path ()
  "Get the note's absolute path for corresponding line."
  (or (org-linenote--check-note-exist)
      (expand-file-name (concat (org-linenote--get-relpath)
                                (org-linenote--get-linenum-string)
                                org-linenote-default-extension)
                        (org-linenote--get-note-rootdir))))

(defun org-linenote-add-annotate (&optional keep-focus)
  "Annotate on the line.

`KEEP-FOCUS': by default, the cursor will be into newly opened
buffer.  If you set this argument to t, the function will not
change the focus after the line highlight."
  (interactive)
  (org-linenote--validate)
  (let ((note-path (org-linenote--get-candidate-note-path))
        (working-buf (selected-window))
        (current-line (line-number-at-pos)))
    (pop-to-buffer (find-file-noselect note-path) 'reusable-frames)
    (save-buffer)
    (select-window working-buf)
    (goto-char (point-min))
    (forward-line current-line)
    (org-linenote-mark-notes)
    (forward-line -1)
    (if (not keep-focus)
        (pop-to-buffer (find-file-noselect note-path) 'reusable-frames))))

(defun org-linenote-remove-annotate ()
  "Remove the annotation on the line."
  (interactive)
  (org-linenote--validate)
  (let ((note-path (org-linenote--get-candidate-note-path)))
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

(defun org-linenote--file-changed (event)
  "A function to handle file watch `EVENT'."
  (let* ((fs-id (nth 0 event))
         (etype (nth 1 event))
         (fpath (nth 2 event))
         (buffer-of-event (cdr (assoc fs-id org-linenote--buffers))))

    (when (string-match-p
           (regexp-quote (file-name-nondirectory
                          (buffer-file-name buffer-of-event)))
           (file-name-base fpath))
      (with-current-buffer buffer-of-event
        (cond
         ((string= etype "deleted")
          (org-linenote--highlight fpath t))
         ((string= etype "created")
          (org-linenote--highlight fpath)))))))

(defun org-linenote--dealloc-fswatch ()
  "Remove out the file watchers and corresponding list."
  (file-notify-rm-watch org-linenote--fwatch-id)
  (setq-local org-linenote--overlays nil)
  (setq org-linenote--buffers
        (delete (assoc org-linenote--fwatch-id org-linenote--buffers) org-linenote--buffers)))

(defun org-linenote--buffer-killed ()
  "A hook function for `kill-buffer-hook'."
  (org-linenote--dealloc-fswatch))

(define-minor-mode org-linenote-mode
  "Toggle `org-linenote-mode'."
  :init-value nil
  :global nil
  :lighter " Org-Linenote"

  (unless (projectile-project-root)
    (error "The working directory is not a git repo"))

  (if org-linenote-mode
      (progn
        (org-linenote--validate)

        (add-hook 'minibuffer-setup-hook #'org-linenote--minibuf-setup-hook)
        (add-hook 'minibuffer-exit-hook #'org-linenote--minibuf-exit-hook)
        (add-hook 'kill-buffer-hook #'org-linenote--buffer-killed :local)

        (let* ((watch-directory (expand-file-name (or (file-name-directory (org-linenote--get-relpath)) "")
                                                  (org-linenote--get-note-rootdir)))
               (buffer-id (current-buffer))
               (watch-id (file-notify-add-watch watch-directory
                                                '(change)
                                                #'org-linenote--file-changed)))
          (setq-local org-linenote--fwatch-id watch-id)
          (setq-local org-linenote--follow-cursor nil)
          (push `(,watch-id . ,buffer-id) org-linenote--buffers))

        (org-linenote-mark-notes)
        (setq-local eldoc-documentation-functions
                    (cons 'org-linenote--eldoc-show-buffer eldoc-documentation-functions)))

    (progn
      (setq-local eldoc-documentation-functions
                  (delete 'org-linenote--eldoc-show-buffer eldoc-documentation-functions))
      (mapc #'delete-overlay org-linenote--overlays)
      (org-linenote--auto-open-at-cursor 'false)
      (org-linenote--dealloc-fswatch))))

(defun org-linenote-browse ()
  "Browse notes for this buffer."
  (interactive)
  (org-linenote--validate)
  (condition-case _
      (funcall #'org-linenote--browse)
    (quit
     (org-linenote--minibuf-exit-hook)
     (org-linenote-mark-notes))))

(defun org-linenote-find-root-dir ()
  "Open the linenote root directory for the current project."
  (interactive)
  (org-linenote--validate)
  (let ((note-dir (org-linenote--get-note-rootdir)))
    (if (file-exists-p note-dir)
        (find-file note-dir)
      (error "No notes found"))))

(defun org-linenote-find-note-dir ()
  "Open the note directory for the current file."
  (interactive)
  (org-linenote--validate)
  (let ((note-dir (expand-file-name (or (file-name-directory (org-linenote--get-relpath)) "")
                                    (org-linenote--get-note-rootdir))))
    (if (file-exists-p note-dir)
        (find-file note-dir)
      (error "No notes found"))))

(defun org-linenote--follow-func ()
  "A hook function for `note-follow' feature."
  (if (org-linenote--check-note-exist)
      (org-linenote-edit-annotate t)))

(defun org-linenote--auto-open-at-cursor (&optional toggle)
  "Toggle org-linenote follow mode.

This let you open the note automatically.  if `TOGGLE' is \=false,
disable note-follow.  if `TOGGLE' is \=true, enable note-follow."
  (let ((set-to (cond ((eq toggle 'true) t)
                      ((eq toggle 'false) nil)
                      ((null toggle) (not org-linenote--follow-cursor)))))
    (setq-local org-linenote--follow-cursor set-to)
    (if org-linenote--follow-cursor
        (add-hook 'post-command-hook #'org-linenote--follow-func nil t)
      (remove-hook 'post-command-hook #'org-linenote--follow-func t))))

(defun org-linenote-auto-open ()
  "Toggle org-linenote follow mode."
  (interactive)
  (org-linenote--auto-open-at-cursor)
  (message "org-linenote note-follow %s"
           (if org-linenote--follow-cursor "enabled" "disabled")))

(defun org-linenote--browse ()
  "Browse notes in the current buffer.
Argument CHOICE user's selection."
  (let ((choice (completing-read "Choose the note: "
                                 (org-linenote--get-note-list) nil t)))
    (org-linenote-mark-notes)
    (pop-to-buffer (find-file-noselect choice 'reusable-frames))))

(defun org-linenote--eldoc-show-buffer (&optional args)
  "Show the first line of a candidate note in the mini-buffer.
Optional argument `ARGS' Return the string for eldoc.  Since we need
only note buffer, there is no usage of `ARGS' at all."

  (let ((note-path (org-linenote--get-candidate-note-path)))
    (when (and note-path (file-exists-p note-path))
      (with-temp-buffer
        (insert-file-contents note-path)
        (let* ((file-buffer (buffer-string))
               (file-ext (file-name-extension note-path))
               (language '(("org" . "org")
                           ("md" . "markdown"))))
          (condition-case e
              (lsp--render-string file-buffer (cdr (assoc file-ext language)))
            (error (message "handle error: %s" e))))))))

(provide 'org-linenote)
;;; org-linenote.el ends here
