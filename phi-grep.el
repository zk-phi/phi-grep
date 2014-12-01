;;; phi-grep.el --- Interactively-editable recursive grep implementation in elisp

;; Copyright (C) 2014 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 0.1.0

;;; Commentary:

;; This script is based on "traverselisp.el", originally authored by
;; Thierry Volpiatto.

;; Put this file in your load-path and load from init script,
;;
;;   (require 'phi-grep)
;;
;; then following commands are available:
;;
;;   - phi-grep-in-file
;;   - phi-grep-in-directory
;;
;; phi-grep can also invoked from dired buffers.
;;
;;   - phi-grep-dired-in-dir-at-point
;;   - phi-grep-dired-in-file-at-point
;;   - phi-grep-dired-in-marked-files
;;   - phi-grep-dired-in-all-files
;;   - phi-grep-dired-dwim
;;
;; You can optionally bind some keys.
;;
;;   (global-set-key (kbd "<f6>") 'phi-grep-in-directory)

;;; Change Log:

;; 0.1.0 test release

;;; Code:

;; *NEED REFACTORING*

(require 'files)
(require 'dired)
(require 'cl-lib)

(defconst phi-grep-version "0.1.0")

;; + user options

(defgroup phi-grep nil
  "Recursive editable grep implemented in elisp."
  :group 'text)

(defcustom phi-grep-ignored-files
  '("\.elc$" "\.pyc$" "\.orig$" "\.bz2$" "\.gz$" "\.zip$"
    "\.vdi$" "\.doc$" "\.jpg$" "\.avi$" "\.jpeg$" "\.png$"
    "\.xpm$" "\.jar$" "\.pbm$" "\.gif$" "\.xls$" "\.ppt$"
    "\.mdb$" "\.adp$" "\\<\\(TAGS\\)\\>" "\.tiff$" "\.img$"
    "\.pdf$" "\.dvi$" "\.xbm$" "\.gpg$" "\.svg$" "\.rej$"
    "\.exe$" "\.gch$" "\.o$" "\.so$" "\.class$"
    "\.ppt$" "\.pptx$" "\.xls$" "\.xlsx$" "\.doc$" "\.docx$"
    "~$" "#[^/]+#$")
  "List of regexps that defines what files to ignore."
  :group 'phi-grep)

(defcustom phi-grep-ignored-dirs
  '("." ".." ".hg" ".svn" "RCS" ".bzr" ".git"
    ".VirtualBox" ".arch-ids" "CVS" "{arch}" "knits")
  "List of directory names we don't want to search in."
  :group 'phi-grep)

(defcustom phi-grep-mode-hook nil
  "Hook run when initializing phi-grep buffers."
  :group 'phi-grep)

(defcustom phi-grep-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") 'phi-grep-exit)
    (define-key kmap (kbd "C-g") 'phi-grep-abort)
    (define-key kmap [remap kill-buffer] 'phi-grep-abort)
    kmap)
  "Keymap for phi-grep buffers."
  :group 'phi-grep)

(defcustom phi-grep-make-backup t
  "when non-nil, backup-files are created on comitting changes."
  :group 'phi-grep)

(defcustom phi-grep-window-height 20
  "height of phi-grep window"
  :group 'phi-grep)

;; + faces

(defface phi-grep-heading-face '((t (:inverse-video t)))
  "Face used to highlight the header line."
  :group 'phi-grep)

(defface phi-grep-match-face '((t (:background "yellow1" :foreground "black")))
  "Face used to highlight matches."
  :group 'phi-grep)

(defface phi-grep-overlay-face '((t (:background "Indianred4" :underline t)))
  "Face used to highlight the corresponding line in a preview buffer."
  :group 'phi-grep)

(defface phi-grep-line-number-face '((t (:bold t)))
  "Face used for line-numbers in phi-grep buffer."
  :group 'phi-grep)

(defface phi-grep-modified-face '((((background light)) (:background "#fdc6b3"))
                                  (t (:background "#502b36")))
  "Face used for modified lines in phi-grep buffer")

;; + utility functions
;; ++ general

(defun phi-grep--goto-line (line)
  "like goto-line but not for interactive use"
  (goto-char (point-min))
  (forward-line (1- line)))

(defun phi-grep--filter (pred lst)
  "Return a list of elements in LIST that satisfies PRED."
  (delq nil (mapcar (lambda (elem) (and (funcall pred elem) elem)) lst)))

;; ++ regexp

(defun phi-grep--string-match-any (str regexps)
  "Return non-nil iff some regexps in REGEXPS match STR."
  (and regexps
       (or (string-match (car regexps) str)
           (phi-grep--string-match-any str (cdr regexps)))))

(defun phi-grep--search-all-regexp (regexp)
  "Return a list of all (LINE-NUM LINE-STR (BEG . END) (BEG
 . END) ...) in the buffer that matches REGEXP."
  (save-excursion
    (let ((cnt 1) res)
      (dolist (line (split-string
                     (buffer-substring-no-properties (point-min) (point-max))
                     "\n"))
        (when (string-match regexp line)
          (let (matches)
            (while (progn
                     (push (cons (match-beginning 0) (match-end 0)) matches)
                     (string-match regexp line (1+ (caar matches)))))
            (push (cons cnt (cons line matches)) res)))
        (cl-incf cnt))
      (nreverse res))))

;; ++ files

(defun phi-grep--directory-files (dirname &optional recursive only-list)
  "List of files (not directory nor symlink) in DIRNAME. Files
  listed in \"phi-grep-ignored-files\" are excluded. When
  optional arg ONLY-LIST is specified, files not listed in the
  list are also excluded. Directories listed in
  \"phi-grep-ignored-dirs\" are not searched in."
  (let ((lst (phi-grep--filter
              (lambda (file) (not (file-symlink-p file)))
              (directory-files dirname t))))
    (message "scanning directory ... %s" (file-relative-name dirname))
    (apply 'nconc
           (phi-grep--filter
            (lambda (file)
              (and (file-regular-p file)
                   (not (phi-grep--string-match-any file phi-grep-ignored-files))
                   (or (null only-list)
                       (phi-grep--string-match-any file only-list))))
            lst)
           (and recursive
                (mapcar (lambda (dir)
                          (when (and (file-directory-p dir)
                                     (not (member (file-name-nondirectory dir)
                                                  phi-grep-ignored-dirs)))
                            (phi-grep--directory-files dir t only-list)))
                        lst)))))

;; ++ overlays

(defun phi-grep--make-overlay (beg end advance &rest properties)
  "Make an overlay from BEG to END, and put PROPERTIES to the
  overlay. PROPERTIES must be a plist."
  (declare (indent 3))
  (let ((ov (make-overlay beg end nil (not advance) advance)))
    (while properties
      (overlay-put ov (car properties) (cadr properties))
      (setq properties (cddr properties)))
    ov))

(defun phi-grep--overlay-at (point category)
  "Return an overlay at POINT whose category is CATEGORY, if
there is one."
  (cl-some
   (lambda (ov)
     (and (eq (overlay-get ov 'category) category) ov))
   (overlays-at point)))

(defun phi-grep--overlay-string (ov)
  "String which is overlayed by OV. text-properties are removed."
  (with-current-buffer (overlay-buffer ov)
    (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))

(defun phi-grep--replace-overlay-string (ov replacement)
  "Replace string overlayed by OV with REPLACEMENT."
  (with-current-buffer (overlay-buffer ov)
    (delete-region (overlay-start ov) (overlay-end ov))
    (goto-char (overlay-start ov))
    (move-overlay ov (point) (progn (insert replacement) (point)))))

;; + main function

(defvar phi-grep--original-buf nil)
(defvar phi-grep--original-pos nil)
(defvar phi-grep--original-win nil)
(defvar phi-grep--occurence-overlay nil)
(defvar phi-grep--preview-file nil "file showm in *phi-grep preview* buffer")
(defvar phi-grep--items nil "list of (FILE . (OV OV OV ...))")

(defun phi-grep-mode ()
  "Major mode to recurse in a tree and perform diverses actions on files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map phi-grep-mode-map)
  (setq mode-name  "phi-grep"
        major-mode 'phi-grep-mode)
  (toggle-truncate-lines 1)
  (add-hook 'post-command-hook 'phi-grep--update-preview-window nil t)
  (run-hooks 'phi-grep-mode-hook))

(defun phi-grep (target-files regexp)
  "Do phi-grep on TARGET-FILES with REGEXP."
  (let ((win (split-window (selected-window) (- phi-grep-window-height) 'below))
        (buf (get-buffer-create "*phi-grep*")))
    (setq phi-grep--original-buf (current-buffer)
          phi-grep--original-pos (point)
          phi-grep--original-win (selected-window)
          phi-grep--preview-file nil
          phi-grep--items        nil)
    (select-window win)
    (switch-to-buffer buf)
    (phi-grep-mode)
    (unwind-protect
        (let ((num-targets (length target-files))
              (num-done 0))
          (with-silent-modifications
            (dolist (file target-files)
              (phi-grep--insert-items file regexp)
              (cl-incf num-done)
              (message "searching [%3d/%3d] ... %4d match(es) found"
                       num-done num-targets (1- (line-number-at-pos))))))
      (message "Found %s match(es) for `%s'" (1- (line-number-at-pos)) regexp)
      (goto-char (point-min)))))

(defun phi-grep--make-item-overlay (beg end file linum &optional with-heading)
  (phi-grep--make-overlay beg end t
    'category 'phi-grep
    'filename file
    'linum    linum
    'insert-in-front-hooks '(phi-grep--sync-modifications)
    'insert-behind-hooks   '(phi-grep--sync-modifications)
    'modification-hooks    '(phi-grep--sync-modifications)
    'before-string
    (concat (when with-heading
              (propertize (concat (file-relative-name file) "\n")
                          'face 'phi-grep-heading-face))
            (propertize (format "%4d: " linum)
                        'face 'phi-grep-line-number-face))))

(defun phi-grep--make-partner-overlay (item &optional buf)
  "make a partner overlay for ITEM in buffer BUF."
  (save-current-buffer
    (when buf (set-buffer buf))
    (phi-grep--goto-line (overlay-get item 'linum))
    (let ((ov (phi-grep--make-overlay (point-at-bol) (point-at-eol) t
                'category 'phi-grep-partner
                'partner  item
                'insert-in-front-hooks '(phi-grep--sync-modifications)
                'insert-behind-hooks   '(phi-grep--sync-modifications)
                'modification-hooks    '(phi-grep--sync-modifications))))
      (overlay-put item 'partner ov)
      ov)))

(defun phi-grep--insert-items (file regexp)
  "Insert items in FILE that matches REGEXP."
  (with-silent-modifications
    (let* ((buf (get-file-buffer file))
           (matched-lines (if buf
                              (with-current-buffer buf
                                (phi-grep--search-all-regexp regexp))
                            (with-temp-buffer
                              (insert-file-contents file)
                              (phi-grep--search-all-regexp regexp))))
           (first-item-flag t)
           items)
      (dolist (line matched-lines)
        (cl-destructuring-bind (linum string . matches) line
          (let ((beg (point)))
            (insert string "\n")
            (push (phi-grep--make-item-overlay
                   beg (1- (point)) file linum first-item-flag) items)
            (setq first-item-flag nil)
            (when buf (phi-grep--make-partner-overlay (car items) buf))
            ;; make highlight overlays
            (dolist (match matches)
              (phi-grep--make-overlay (+ (car match) beg) (+ (cdr match) beg) nil
                'face               'phi-grep-match-face
                'modification-hooks '(phi-grep--delete-overlay-on-modifications))))))
      (push (cons file items) phi-grep--items))))

(defun phi-grep--sync-modifications (ov after &rest _)
  (let ((partner (overlay-get ov 'partner)))
    (if after
        (let* ((buf (and partner (overlay-buffer partner)))
               (previewbuf (get-buffer "*phi-grep preview*"))
               (str (phi-grep--overlay-string ov))
               (inhibit-modification-hooks t))
          (cond ((buffer-live-p buf)
                 (with-current-buffer buf
                   (phi-grep--replace-overlay-string partner str)))
                ((and (buffer-live-p previewbuf)
                      (string= (overlay-get ov 'filename) phi-grep--preview-file))
                 (with-current-buffer previewbuf
                   (phi-grep--goto-line (overlay-get ov 'linum))
                   (insert str)
                   (delete-region (point) (point-at-eol)))))
          (overlay-put ov 'face 'phi-grep-modified-face))
      (let ((item-ov (if (eq (overlay-get ov 'category) 'phi-grep)
                         ov
                       partner)))
        (when (null (overlay-get item-ov 'original-str))
          (overlay-put item-ov 'original-str (phi-grep--overlay-string item-ov)))))))

(defun phi-grep--delete-overlay-on-modifications (ov after &rest _)
  (when after (delete-overlay ov)))

(defun phi-grep--update-preview-window ()
  "Show the selected line at pos in the preview window."
  (let ((ov (phi-grep--overlay-at (point) 'phi-grep)))
    (when ov
      (let ((filename (overlay-get ov 'filename))
            (linum (overlay-get ov 'linum))
            deactivate-mark)
        (when (window-live-p phi-grep--original-win)
          (with-selected-window phi-grep--original-win
            (let ((buf (get-file-buffer filename)))
              (cond (buf
                     (let ((pbuf (get-buffer "*phi-grep preview*")))
                       (when (and pbuf (buffer-live-p pbuf))
                         (kill-buffer pbuf)))
                     (switch-to-buffer buf))
                    (t
                     (unless (string= phi-grep--preview-file filename)
                       (let ((buf (get-buffer "*phi-grep preview*")))
                         (when (and buf (buffer-live-p buf))
                           (kill-buffer buf)))
                       (with-current-buffer (get-buffer-create "*phi-grep preview*")
                         (with-silent-modifications
                           (insert-file-contents filename))
                         (let ((buffer-file-name filename))
                           (ignore-errors (set-auto-mode)))
                         (setq phi-grep--preview-file filename)
                         ;; sync changes
                         (dolist (item (cdr (assoc filename phi-grep--items)))
                           (when (eq (overlay-get item 'face) 'phi-grep-modified-face)
                             (phi-grep--goto-line (overlay-get item 'linum))
                             (save-excursion
                               (delete-region (point-at-bol) (point-at-eol)))
                             (insert (phi-grep--overlay-string item))))
                         (add-hook 'before-change-functions
                                   'phi-grep--find-preview-file nil t)))
                     (switch-to-buffer "*phi-grep preview*"))))
            ;; jump to the line
            (phi-grep--goto-line linum)
            (recenter)
            ;; make overlay
            (if phi-grep--occurence-overlay
                (move-overlay
                 phi-grep--occurence-overlay
                 (point-at-bol) (1+ (point-at-eol)) (current-buffer))
              (setq phi-grep--occurence-overlay
                    (phi-grep--make-overlay (point-at-bol) (1+ (point-at-eol)) nil
                      'face 'phi-grep-overlay-face)))))))))

(defun phi-grep--find-preview-file (&rest _)
  "make it a file buffer."
  (let ((buf (get-buffer "*phi-grep preview*")))
    (when buf
      (with-current-buffer buf
        (let ((modified (buffer-modified-p)))
          (set-visited-file-name phi-grep--preview-file)
          (restore-buffer-modified-p modified)
          ;; make partner overlays
          (save-excursion
            (dolist (ov (cdr (assoc phi-grep--preview-file phi-grep--items)))
              (let ((partner (phi-grep--make-partner-overlay ov)))
                ;; apply modifications
                (when (eq (overlay-get ov 'face) 'phi-grep-modified-face)
                  (phi-grep--replace-overlay-string partner (phi-grep--overlay-string ov))))))
          (setq phi-grep--preview-file nil))))))

(defadvice find-file-noselect (before phi-grep--find-file-ad activate)
  "If the preview buffer is showing the file going to be found,
reuse the buffer instead of finding file to keep modifications."
  (when (and phi-grep--preview-file
             (get-buffer "*phi-grep preview*")
             (string= (file-truename phi-grep--preview-file)
                      (file-truename (ad-get-arg 0)))
             (null (ad-get-arg 1)))
    (phi-grep--find-preview-file)))

(defun phi-grep--quit ()
  "Clean up phi-grep buffer and preview buffer."
  ;; commit or revert changes
  (let ((file-changes nil)
        (buf-changes nil))
    (dolist (items phi-grep--items)
      (when (cl-some (lambda (ov)
                       (eq (overlay-get ov 'face) 'phi-grep-modified-face))
                     (cdr items))
        (if (get-file-buffer (car items))
            (push items buf-changes)
          (push items file-changes))))
    (when (or file-changes buf-changes)
      (cond ((y-or-n-p (format "commit changes to %d files, %d buffers ?"
                               (length file-changes)
                               (length buf-changes)))
             (dolist (changes file-changes)
               (let ((file (car changes)))
                 (with-temp-buffer
                   (insert-file-contents file)
                   (when phi-grep-make-backup
                     (copy-file file (concat file "~") t))
                   (dolist (change (cdr changes))
                     (let ((line (overlay-get change 'linum)))
                       (phi-grep--goto-line line)
                       (save-excursion
                         (delete-region (point-at-bol) (point-at-eol)))
                       (insert (phi-grep--overlay-string change))))
                   (write-region (point-min) (point-max) file)))))
            (t
             (dolist (changes buf-changes)
               (with-current-buffer (get-file-buffer (car changes))
                 (dolist (ov (cdr changes))
                   (let* ((partner (overlay-get ov 'partner))
                          (str (overlay-get ov 'original-str)))
                     (when (and partner str)
                       (phi-grep--replace-overlay-string partner str))))))))))
  ;; remove partner overlays
  (dolist (items phi-grep--items)
    (when (get-file-buffer (car items))
      (dolist (item (cdr items))
        (let ((partner (overlay-get item 'partner)))
          (when partner (delete-overlay partner))))))
  ;; clean-up preview buffers and phi-grep windows
  (when phi-grep--occurence-overlay
    (delete-overlay phi-grep--occurence-overlay))
  (let ((buf (get-buffer "*phi-grep preview*")))
    (when buf (kill-buffer buf)))
  (delete-window))

;; + interactive commands

(defun phi-grep--dired-get-marked-files ()
  "Return list of all marked files in a dired buffer."
  (unless (eq major-mode 'dired-mode)
    (error "not in dired buffer"))
  (let ((all-marked (dired-get-marked-files nil nil nil t)))
    (when (symbolp (car all-marked))
      (setq all-marked (cdr all-marked)))
    (phi-grep--filter 'file-regular-p all-marked)))

;;;###autoload
(defun phi-grep-in-file (fname regexp)
  "phi-grep in a single file."
  (interactive (list (read-file-name "Grep File: " nil (buffer-file-name))
                     (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
  (phi-grep (list fname) regexp))

;;;###autoload
(defun phi-grep-in-directory (tree regexp &optional only)
  "phi-grep in all files recursively in a directory."
  (interactive
   (list (read-directory-name "Tree: ")
         (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
         (read-string "CheckOnly: ")))
  (phi-grep (phi-grep--directory-files tree t (split-string only " ")) regexp))

;;;###autoload
(defun phi-grep-dired-in-dir-at-point (regex &optional only)
  "phi-grep in all files recursively in the selected directory."
  (interactive (list (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
                     (read-string "CheckOnly: ")))
  (unless (eq major-mode 'dired-mode)
    (error "not in dired buffer"))
  (let ((tree (dired-get-filename)))
    (unless (file-directory-p tree)
      (error "%s is not a directory" tree))
    (phi-grep-in-directory tree regex only)))

;;;###autoload
(defun phi-grep-dired-in-file-at-point (regex)
  "phi-grep in the selected file."
  (interactive (list (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
  (unless (eq major-mode 'dired-mode)
    (error "not in dired buffer"))
  (let ((fname (dired-get-filename)))
    (unless (file-regular-p fname)
      (error "%s is not a regular file" fname))
    (phi-grep-in-file fname regex)))

;;;###autoload
(defun phi-grep-dired-in-marked-files (regexp)
  "phi-grep in all marked files."
  (interactive (list (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
  (unless (eq major-mode 'dired-mode)
    (error "not in dired buffer"))
  (phi-grep (phi-grep--dired-get-marked-files) regexp))

;;;###autoload
(defun phi-grep-dired-in-all-files (regexp only)
  "phi-grep in all files in the current directory."
  (interactive (list (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
                     (read-string "CheckOnly: ")))
  (unless (eq major-mode 'dired-mode)
    (error "not in dired buffer"))
  (phi-grep
   (phi-grep--directory-files (dired-current-directory) nil (split-string only)) regexp))

;;;###autoload
(defun phi-grep-dired-dwim (regexp &optional only)
  "Call the phi-grep-dired command you want (Do What I Mean).

- file at point
- marked files
- directory at point (recursion)"
  (interactive
   (let ((f-or-d-name (dired-get-filename)))
     (cond ((phi-grep--dired-get-marked-files)
            (list (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))))
           ((file-directory-p f-or-d-name)
            (list (read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
                  (read-string "CheckOnly: "))))))
  (let ((fname (dired-get-filename)))
    (cond ((phi-grep--dired-get-marked-files)
           (phi-grep-dired-in-marked-files regexp))
          ((file-directory-p fname)
           (phi-grep-dired-in-dir-at-point regexp only)))))

;;;###autoload
(defun phi-grep-find-file-flat (tree)
  (interactive (list (read-directory-name "Tree : ")))
  (find-file
   (expand-file-name
    (completing-read "Find File: "
                     (mapcar (lambda (f) (file-relative-name f tree))
                             (phi-grep--directory-files tree t))
                     nil t)
    tree)))

(defun phi-grep-abort ()
  "Quit phi-grep window and back to the original position."
  (interactive)
  (phi-grep--quit)
  (when (buffer-live-p phi-grep--original-buf)
    (switch-to-buffer phi-grep--original-buf)
    (goto-char phi-grep--original-pos)))

(defun phi-grep-exit ()
  "Quit phi-grep window with the preview window kept."
  (interactive)
  (phi-grep--update-preview-window)
  (phi-grep--find-preview-file)
  (phi-grep--quit))

;; + provide

(provide 'phi-grep)

;;; phi-grep.el ends here
