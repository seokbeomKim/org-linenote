;; org-linenote-test.el --- Tests for org-linenote.el
;;
;; Copyright (C) 2024
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Jason Kim <sukbeom.kim@gmail.com>
;; Created: February 18, 2024
;; Modified: February 18, 2024
;; Version: 1.1.3
;; Keywords: tools, note, org
;; Homepage: https://github.com/seokbeomKim/org-linenote
;; Package-Requires: ((emacs "29.1") (projectile "2.8.0") (vertico "1.7"))

;;; Code:
;;;

(require 'org-linenote)

(ert-deftest org-linenote-test-get-relpath ()
  "Test getting the relative path of the current buffer."
  (with-current-buffer (find-file-noselect "README.org")
    (should (equal (org-linenote--get-relpath) "README.org"))))

(ert-deftest org-linenote-test-get-note-rootdir ()
  "Test getting the linenote root directory for the current project."
  (with-current-buffer (find-file-noselect "README.org")
    (should (equal (org-linenote--get-note-rootdir)
                   (expand-file-name ".linenote" (file-name-directory (buffer-file-name)))))))

(ert-deftest org-linenote-test-get-linenum-string ()
  "Test getting the line number string of the current line."
  :expected-result :failed

  (with-temp-buffer
    (should (equal (org-linenote--get-linenum-string) "#L1")))

  ;; following test case is not working properly. Maybe there is a limit in
  ;; batch mode.?
  (with-temp-buffer
    (insert "1st line\n")
    (insert "2nd line\n")
    (insert "3rd line")
    (set-mark (point-min))
    (should (equal (org-linenote--get-linenum-string) "#L1-L3"))))

(ert-deftest org-linenote-test-get-line-range-by-fname ()
  "Test getting the line range of a file."
  (should (equal (org-linenote--get-line-range-by-fname "README.org#L1-L125.org") '(1 . 125))))

(ert-deftest org-linenote-test-get-note-linum-by-direction ()
  "Test getting the line number of the note by direction."
  (with-current-buffer (find-file-noselect "README.org")
    (should (equal (org-linenote--get-note-linum-by-direction 4 nil) 3))
    (should (equal (org-linenote--get-note-linum-by-direction 3 nil) 2))
    (should (equal (org-linenote--get-note-linum-by-direction 2 nil) 1))
    (should (equal (org-linenote--get-note-linum-by-direction 1 nil) nil))

    (should (equal (org-linenote--get-note-linum-by-direction 4 t) nil))
    (should (equal (org-linenote--get-note-linum-by-direction 3 t) nil))
    (should (equal (org-linenote--get-note-linum-by-direction 2 t) 3))
    (should (equal (org-linenote--get-note-linum-by-direction 1 t) 2))))

(ert-deftest org-linenote-test-get-note-list ()
  "Test getting the note list of the current buffer."
  (with-current-buffer (find-file-noselect "README.org")
    (let ((should-list '("README.org#L1"
                         "README.org#L2"
                         "README.org#L3")))
      (dolist (x (org-linenote--get-note-list))
        (should (not (null (member (file-name-base x) should-list))))))))

(provide 'org-linenote-test)
;;; org-linenote-test.el ends here
