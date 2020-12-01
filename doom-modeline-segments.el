;;; doom-modeline-segments.el --- The segments for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Vincent Zhang

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; The segments for doom-modeline.
;; Use `doom-modeline-def-segment' to create a new segment.
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'doom-modeline-core)


;;
;; Externals
;;

(defvar Info-current-file)
(defvar Info-current-node)
(defvar Info-mode-line-node-keymap)
(defvar aw-keys)
(defvar edebug-execution-mode)
(defvar flycheck-current-errors)
(defvar flycheck-mode-menu-map)
(defvar iedit-occurrences-overlays)
(defvar text-scale-mode-amount)
(defvar tracking-buffers)
(defvar winum-auto-setup-mode-line)

(declare-function async-inject-variables 'async)
(declare-function async-start 'async)
(declare-function avy-traverse 'avy)
(declare-function avy-tree 'avy)
(declare-function aw-update 'ace-window)
(declare-function aw-window-list 'ace-window)
(declare-function dap--cur-session 'dap-mode)
(declare-function dap--debug-session-name 'dap-mode)
(declare-function dap--debug-session-state 'dap-mode)
(declare-function dap--session-running 'dap-mode)
(declare-function dap-debug-recent 'dap-mode)
(declare-function dap-disconnect 'dap-mode)
(declare-function dap-hydra 'dap-hydra)
(declare-function edebug-help 'edebug)
(declare-function edebug-next-mode 'edebug)
(declare-function edebug-stop 'edebug)
(declare-function face-remap-remove-relative 'face-remap)
(declare-function flycheck-buffer 'flycheck)
(declare-function flycheck-count-errors 'flycheck)
(declare-function flycheck-error-level-compilation-level 'flycheck)
(declare-function flycheck-list-errors 'flycheck)
(declare-function flycheck-next-error 'flycheck)
(declare-function flycheck-previous-error 'flycheck)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function jsonrpc--request-continuations 'jsonrpc)
(declare-function jsonrpc-last-error 'jsonrpc)
(declare-function lsp--workspace-root 'lsp-mode)
(declare-function lsp-describe-session 'lsp-mode)
(declare-function lsp-workspace-folders-open 'lsp-mode)
(declare-function lsp-workspace-restart 'lsp-mode)
(declare-function lsp-workspaces 'lsp-mode)
(declare-function lsp-workspace-shutdown 'lsp-mode)
(declare-function lsp-workspaces 'lsp-mode)
(declare-function mc/num-cursors 'multiple-cursors-core)
(declare-function org-edit-src-save 'org-src)
(declare-function pdf-cache-number-of-pages 'pdf-cache)
(declare-function popup-create 'popup)
(declare-function popup-delete 'popup)
(declare-function tracking-next-buffer 'tracking)
(declare-function tracking-previous-buffer 'tracking)
(declare-function tracking-shorten 'tracking)
(declare-function warning-numeric-level 'warnings)
(declare-function winum--clear-mode-line 'winum)
(declare-function winum--install-mode-line 'winum)
(declare-function winum-get-number-string 'winum)


;;
;; Buffer information
;;

(defvar-local doom-modeline--remote-host 'unset)

(defun doom-modeline-update-buffer-file-state (&rest _)
  "Update the buffer or file state in mode-line."
  (concat
   (cond
    (buffer-read-only
     (propertize "%1*" 'face '(:inherit doom-modeline-warning :weight bold)))

    ((and buffer-file-name
          (buffer-modified-p))
     (propertize "%1*" 'face '(:inherit doom-modeline-buffer-modified :weight bold)))

    ((and buffer-file-name ;; donot remove call file-exists-p when file is on remote host
          (or (not doom-modeline--remote-host)
              (eq doom-modeline--remote-host 'unset))
          (not (file-exists-p buffer-file-name)))
     (propertize "!" 'face 'doom-modeline-urgent))

    (t ""))
   (when (or (buffer-narrowed-p)
             (bound-and-true-p dired-narrow-mode))
     (propertize "><" 'face 'doom-modeline-warning))))

(defvar-local doom-modeline--buffer-file-name nil)
(defun doom-modeline-update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq doom-modeline--buffer-file-name
        (ignore-errors
          (save-match-data
            (if buffer-file-name
                (doom-modeline-buffer-file-name)
              (propertize "%b" 'face 'doom-modeline-buffer-file))))))

(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'after-save-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-name)
(advice-add #'not-modified :after #'doom-modeline-update-buffer-file-name)
(advice-add #'rename-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'doom-modeline-update-buffer-file-name)
(advice-add #'pop-to-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo :after #'doom-modeline-update-buffer-file-name)
(advice-add #'fill-paragraph :after #'doom-modeline-update-buffer-file-name)
(advice-add #'popup-create :after #'doom-modeline-update-buffer-file-name)
(advice-add #'popup-delete :after #'doom-modeline-update-buffer-file-name)
(advice-add #'org-edit-src-save :after #'doom-modeline-update-buffer-file-name)

(defsubst doom-modeline--buffer-state ()
  "current buffer state."
  (let ((state (doom-modeline-update-buffer-file-state)))
    (concat
     (if (doom-modeline--active)
         state
       (propertize state 'face 'mode-line-inactive))
     (doom-modeline-spc))))

(defsubst doom-modeline--buffer-name ()
  "The current buffer name."
  (let ((modified (and buffer-file-name (buffer-modified-p)))
        (active (doom-modeline--active)))
    (if doom-modeline--limited-width-p
        (propertize "%b" 'face (cond (modified 'doom-modeline-buffer-modified)
                                     (active 'doom-modeline-buffer-file)
                                     (t 'mode-line-inactive)))
      (when-let ((name (or doom-modeline--buffer-file-name
                           (doom-modeline-update-buffer-file-name))))
        (if active
            ;; Check if the buffer is modified
            (if modified
                (propertize name 'face 'doom-modeline-buffer-modified)
              name)
          (propertize name 'face 'mode-line-inactive))))))

(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (let ((name (concat
               "%["
               (doom-modeline--buffer-state)
               (doom-modeline--buffer-name)
               "%]")))
    (if (and (listp mode-line-buffer-identification)
             (equal (car mode-line-buffer-identification) "%b"))
        (cons name (cdr mode-line-buffer-identification))
      name)))

(doom-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-state)
   (propertize "%b" 'face (cond ((and buffer-file-name (buffer-modified-p))
                                 'doom-modeline-buffer-modified)
                                ((doom-modeline--active) 'doom-modeline-buffer-file)
                                (t 'mode-line-inactive)))))

(doom-modeline-def-segment buffer-default-directory
  "Displays `default-directory' . This is for special buffers
like the scratch buffer where knowing the current project directory is important."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-state)
   (propertize (abbreviate-file-name default-directory)
               'face (cond ((buffer-modified-p) 'doom-modeline-buffer-modified)
                           ((doom-modeline--active) 'doom-modeline-buffer-path)
                           (t 'mode-line-inactive)))))

(doom-modeline-def-segment buffer-default-directory-simple
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (concat (doom-modeline-spc)
          (propertize (abbreviate-file-name default-directory)
                      'face (if (doom-modeline--active)
                                'doom-modeline-buffer-path
                              'mode-line-inactive))))


;;
;; Encoding
;;

(doom-modeline-def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer the same way Atom does."
  (let ((face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)))
    (concat
     (doom-modeline-spc)

     ;; eol type
     (propertize
      (pcase (coding-system-eol-type buffer-file-coding-system)
        (0 "LF ")
        (1 "CRLF ")
        (2 "CR ")
        (_ ""))
      'face face)

     ;; coding system
     (propertize
      (let ((sys (coding-system-plist buffer-file-coding-system)))
        (cond ((memq (plist-get sys :category)
                     '(coding-category-undecided coding-category-utf-8))
               "UTF-8")
              (t (upcase (symbol-name (plist-get sys :name))))))
      'face face)

     (doom-modeline-spc))))

;;
;; Remote host
;;

(doom-modeline-def-segment remote-host
  "Hostname for remote buffers."
  (or (when (not (eq doom-modeline--remote-host 'unset))
        doom-modeline--remote-host)
      (setq doom-modeline--remote-host
            (when-let ((host (and default-directory
                                  (file-remote-p default-directory 'host))))
              (propertize
               (concat "@" host)
               'face (if (doom-modeline--active) 'doom-modeline-host 'mode-line-inactive))))))


;;
;; Major mode
;;

(doom-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (doom-modeline-spc)
    (format-mode-line mode-name)
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (doom-modeline-spc))
   'face (if (doom-modeline--active)
             'doom-modeline-buffer-major-mode
           'mode-line-inactive)))


;;
;; Process
;;

(doom-modeline-def-segment process
  "The process info."
  (if (doom-modeline--active)
      mode-line-process
    (propertize (format-mode-line mode-line-process)
                'face 'mode-line-inactive)))


;;
;; VCS
;;

(defvar-local doom-modeline--vcs-state nil)
(defun doom-modeline-update-vcs-state (&rest _)
  "Update vcs state in mode-line."
  (setq doom-modeline--vcs-state
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added))
                   (propertize "*" 'face 'doom-modeline-info))
                  ((eq state 'needs-merge)
                   (propertize "M" 'face 'doom-modeline-info))
                  ((eq state 'needs-update)
                   (propertize "U" 'face 'doom-modeline-warning))
                  ((memq state '(removed conflict unregistered))
                   (propertize "!" 'face 'doom-modeline-urgent))
                  (t
                   (propertize "@" 'face 'doom-modeline-info)))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs-state)
(add-hook 'after-save-hook #'doom-modeline-update-vcs-state)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs-state)

(defvar-local doom-modeline--vcs-text nil)
(defun doom-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (setq doom-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
            (propertize
             (if (> (length str) doom-modeline-vcs-max-length)
                 (concat
                  (substring str 0 (- doom-modeline-vcs-max-length 3))
                  "...")
               str)
             'face (cond ((eq state 'needs-update) 'doom-modeline-warning)
                         ((memq state '(removed conflict unregistered)) 'doom-modeline-urgent)
                         (t 'doom-modeline-info)))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs-text)
(add-hook 'after-save-hook #'doom-modeline-update-vcs-text)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs-text)

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (let ((active (doom-modeline--active)))
    (when-let ((state doom-modeline--vcs-state)
               (text doom-modeline--vcs-text))
      (concat
       (doom-modeline-spc)
       (propertize
        (concat
         (if active
             state
           (propertize state 'face 'mode-line-inactive))
         (doom-modeline-spc)))
       (if active
           text
         (propertize text 'face 'mode-line-inactive))
       (doom-modeline-spc)))))


;;
;; Checker
;;

;; Flycheck

(defun doom-modeline--flycheck-count-errors ()
  "Count the number of ERRORS, grouped by level.

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let ((info 0) (warning 0) (error 0))
    (mapc
     (lambda (item)
       (let ((count (cdr item)))
         (pcase (flycheck-error-level-compilation-level (car item))
           (0 (cl-incf info count))
           (1 (cl-incf warning count))
           (2 (cl-incf error count)))))
     (flycheck-count-errors flycheck-current-errors))
    `((info . ,info) (warning . ,warning) (error . ,error))))

(defvar-local doom-modeline--flycheck-state nil)
(defun doom-modeline-update-flycheck-state (&optional status)
  "Update flycheck state via STATUS."
  (setq doom-modeline--flycheck-state
        (pcase status
          ('finished
           (if flycheck-current-errors
               (let-alist (doom-modeline--flycheck-count-errors)
                 (concat
                  (propertize "!" 'face (cond ((> \.error 0) 'doom-modeline-urgent)
                                              ((> \.warning 0) 'doom-modeline-warning)
                                              (t 'doom-modeline-info)))
                  (propertize (number-to-string \.error) 'face 'doom-modeline-urgent)
                  "/"
                  (propertize (number-to-string \.warning) 'face 'doom-modeline-warning)
                  "/"
                  (propertize (number-to-string \.info) 'face 'doom-modeline-info)))
             (propertize "--" 'face 'doom-modeline-info)))
          ('running (propertize "running" 'face 'doom-modeline-debug))
          ('no-checker (propertize "no-checker" 'face 'doom-modeline-debug))
          ('errored (propertize "Error" 'face 'doom-modeline-urgent))
          ('interrupted (propertize "Interrupted" 'face 'doom-modeline-debug))
          ('suspicious (propertize "Suspicious" 'face 'doom-modeline-urgent))
          (_ nil))))
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-state)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-state)

(doom-modeline-def-segment checker
  "Displays color-coded error status in the current buffer with pretty icons."
  (when doom-modeline--flycheck-state
    (concat
     (doom-modeline-spc)
     (if (doom-modeline--active)
         doom-modeline--flycheck-state
       (propertize doom-modeline--flycheck-state 'face 'mode-line-inactive))
     (doom-modeline-spc))))


;;
;; Matches (macro,, iedit and multi-cursors)
;;

(defsubst doom-modeline--macro-recording ()
  "Display current Emacs macro being recorded."
  (when (and (doom-modeline--active)
             (or defining-kbd-macro executing-kbd-macro))
    (propertize " Macro > " 'face 'doom-modeline-panel)))

(defun doom-modeline--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defsubst doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc
            (or (let ((inhibit-message t))
                  (iedit-find-current-occurrence-overlay))
                (save-excursion (iedit-prev-occurrence)
                                (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format
        " %s/%d "
        (if this-oc
            (- length
               (length (memq this-oc
                             (sort (append iedit-occurrences-overlays nil)
                                   #'doom-modeline--overlay-sort)))
               -1)
          "-")
        length))
     'face (if (doom-modeline--active)
               'doom-modeline-panel
             'mode-line-inactive))))

(defsubst doom-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (let ((count (mc/num-cursors)))
      (propertize (format " I %d " count)
                  'face (if (doom-modeline--active)
                            'doom-modeline-panel
                          'mode-line-inactive)))))

(defsubst doom-modeline--buffer-size ()
  "Show buffer size."
  (when size-indication-mode
    (concat
     (doom-modeline-spc)
     (propertize "%I " 'face (if (doom-modeline--active)
                                 'mode-line
                               'mode-line-inactive))
     (doom-modeline-spc))))

(doom-modeline-def-segment matches
  "Displays:
1. the currently recording macro
2. The number of active `iedit' regions,
3. The number of active `multiple-cursors'."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--iedit)
                      (doom-modeline--multiple-cursors))))
    (if (string-empty-p meta)
        (doom-modeline--buffer-size)
      meta)))

(doom-modeline-def-segment buffer-size
  "Display buffer size"
  (doom-modeline--buffer-size))

;;
;; Media
;;

(doom-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (when (eq major-mode 'image-mode)
    (cl-destructuring-bind (width . height)
        (when (fboundp 'image-size)
          (image-size (image-get-display-property) :pixels))
      (propertize
       (format "  %dx%d  " width height)
       'face (if (doom-modeline--active)
                 'mode-line
               'mode-line-inactive)))))


;;
;; Bars
;;

(defvar doom-modeline--bar-active nil)
(defvar doom-modeline--bar-inactive nil)
(doom-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI."
  (if (doom-modeline--active)
      doom-modeline--bar-active
    doom-modeline--bar-inactive))

(defun doom-modeline-refresh-bars (&optional width height)
  "Refresh mode-line bars with `WIDTH' and `HEIGHT'."
  (let ((width (or width doom-modeline-bar-width))
        (height (max (or height doom-modeline-height)
                     (doom-modeline--font-height))))
    (when (and (numberp width) (numberp height))
      (setq doom-modeline--bar-active
            (doom-modeline--make-xpm 'doom-modeline-bar width height)
            doom-modeline--bar-inactive
            (doom-modeline--make-xpm 'doom-modeline-bar-inactive width height)))))

(doom-modeline-add-variable-watcher
 'doom-modeline-height
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-modeline-refresh-bars doom-modeline-bar-width val))))

(doom-modeline-add-variable-watcher
 'doom-modeline-bar-width
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-modeline-refresh-bars val doom-modeline-height))))

(add-hook 'after-setting-font-hook #'doom-modeline-refresh-bars)
(add-hook 'window-configuration-change-hook #'doom-modeline-refresh-bars)


;;
;; Window number
;;

(advice-add #'winum--install-mode-line :override #'ignore)
(advice-add #'winum--clear-mode-line :override #'ignore)

(doom-modeline-def-segment window-number
  (when-let (num (ignore-errors (winum-get-number-string)))
    (propertize
     (format " %s " num)
     'face (if (doom-modeline--active)
               'doom-modeline-buffer-major-mode
             'mode-line-inactive))))

;;
;; Misc info
;;

(doom-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  `((t mode-line-misc-info)
    ,(propertize
      (or doom-modeline--project-parent-path default-directory)
      'face font-lock-doc-face)))


;;
;; Position
;;

;; Be compatible with Emacs 25.
(defvar doom-modeline-column-zero-based
  (if (boundp 'column-number-indicator-zero-based)
      column-number-indicator-zero-based
    t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar doom-modeline-percent-position
  (if (boundp 'mode-line-percent-position)
      mode-line-percent-position
    '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(doom-modeline-add-variable-watcher
 'column-number-indicator-zero-based
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-column-zero-based val))))

(doom-modeline-add-variable-watcher
 'mode-line-percent-position
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-percent-position val))))

(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let* ((active (doom-modeline--active))
         (lc '(line-number-mode
               (column-number-mode
                (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                "%l")
               (column-number-mode (doom-modeline-column-zero-based ":%c" ":%C"))))
         (face (if active 'mode-line 'mode-line-inactive)))
    (concat
     (doom-modeline-spc)

     (propertize (format-mode-line lc) 'face face)

     (when doom-modeline-percent-position
       (concat
        (doom-modeline-spc)
        (propertize (format-mode-line '("" doom-modeline-percent-position "%%"))
                    'face face)))
     (when (or line-number-mode column-number-mode doom-modeline-percent-position)
       (doom-modeline-spc)))))

;;
;; Info
;;

(doom-modeline-def-segment info-nodes
  "The topic and nodes in the Info buffer."
  (let ((active (doom-modeline--active)))
    (concat
     (propertize " (" 'face (if active 'mode-line 'mode-line-inactive))
     ;; topic
     (propertize (if (stringp Info-current-file)
                     (replace-regexp-in-string
                      "%" "%%"
                      (file-name-sans-extension
                       (file-name-nondirectory Info-current-file)))
                   (format "*%S*" Info-current-file))
                 'face (if active 'doom-modeline-info 'mode-line-inactive))
     (propertize ") " 'face (if active 'mode-line 'mode-line-inactive))
     ;; node
     (when Info-current-node
       (propertize (replace-regexp-in-string
                    "%" "%%" Info-current-node)
                   'face (if active 'doom-modeline-buffer-path 'mode-line-inactive))))))

;;
;; LSP
;;

(doom-modeline-def-segment lsp
  "The LSP server state."
  (when (and doom-modeline-lsp
             (bound-and-true-p lsp-managed-mode)
             (not doom-modeline--limited-width-p))
    (concat
     (doom-modeline-spc)
     (propertize "LSP" 'face (if (doom-modeline--active)
                                 (if (lsp-workspaces)
                                     'doom-modeline-lsp-success
                                   'doom-modeline-lsp-warning)
                               'mode-line-inactive))
     (doom-modeline-spc))))

;;
;; PDF pages
;;

(defvar-local doom-modeline--pdf-pages nil)
(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "  P%d/%d "
                (eval `(pdf-view-current-page))
                (pdf-cache-number-of-pages))))
(add-hook 'pdf-view-change-page-hook #'doom-modeline-update-pdf-pages)

(doom-modeline-def-segment pdf-pages
  "Display PDF pages."
  (propertize
   doom-modeline--pdf-pages
   'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)))

;;
;; Package information
;;

(doom-modeline-def-segment package
  "Show package information via `paradox'."
  (let ((active (doom-modeline--active)))
    (concat
     (let ((front (format-mode-line 'mode-line-front-space)))
       (if active
           front
         (propertize front 'face 'mode-line-inactive)))

     (let ((info (format-mode-line 'mode-line-buffer-identification)))
       (if active
           info
         (propertize info 'face 'mode-line-inactive))))))

;;
;; Git timemachine
;;

(doom-modeline-def-segment git-timemachine
  (let ((active (doom-modeline--active)))
    (concat
     (doom-modeline-spc)
     ;; Snapshot icon
     (propertize "%1*" 'face (if active
                                 '(:inherit doom-modeline-warning :weight normal)
                               'mode-line-inactive))
     ;; Buffer name
     (propertize "*%b*" 'face (if active
                                  'doom-modeline-buffer-timemachine
                                'mode-line-inactive)))))

(provide 'doom-modeline-segments)

;;; doom-modeline-segments.el ends here
