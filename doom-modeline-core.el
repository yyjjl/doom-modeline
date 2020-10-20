;;; doom-modeline-core.el --- The core libraries for doom-modeline -*- lexical-binding: t; -*-

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
;; The core libraries for doom-modeline.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)

;;
;; Compatibilities
;;

(eval-and-compile
  (when (< emacs-major-version 26)
    ;; Define `if-let*' and `when-let*' variants for 25 users.
    (unless (fboundp 'if-let*) (defalias 'if-let* #'if-let))
    (unless (fboundp 'when-let*) (defalias 'when-let* #'when-let))))

;; Don’t compact font caches during GC.
(when (eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t))

;;`file-local-name' is introduced in 25.2.2.
(unless (fboundp 'file-local-name)
  (defsubst file-local-name (file)
    "Return the local name component of FILE.
It returns a file name which can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
    (or (file-remote-p file 'localname) file)))

;;
;; Customizations
;;

(defgroup doom-modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defcustom doom-modeline-height 25
  "How tall the mode-line should be. It's only respected in GUI.
If the actual char height is larger, it respects the actual char height."
  :type 'integer
  :set (lambda (sym val)
         (set sym (if (> val 0) val 1)))
  :group 'doom-modeline)

(defcustom doom-modeline-bar-width (if (eq system-type 'darwin) 3 6)
  "How wide the mode-line bar should be. It's only respected in GUI."
  :type 'integer
  :set (lambda (sym val)
         (set sym (if (> val 0) val 1)))
  :group 'doom-modeline)

(defcustom doom-modeline-window-width-limit fill-column
  "The limit of the window width.

If `window-width' is smaller than the limit, some information won't be displayed."
  :type '(choice integer
                 (const :tag "Disable" nil))
  :group 'doom-modeline)

(defcustom doom-modeline-project-detection
  (cond ((fboundp 'projectile-project-root) 'projectile)
        ((fboundp 'project-current) 'project)
        (t nil))
  "How to detect the project root.

The default priority is `projectile' > `project'.
nil means to use `default-directory'.

The project management packages have some issues on detecting project root.
e.g. `projectile' doesn't handle symlink folders well, while `project' is
unable to hanle sub-projects.
Specify another one if you encounter the issue."
  :type '(choice (const :tag "Projectile" projectile)
                 (const :tag "Built-in Project" project)
                 (const :tag "Disable" nil))
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-encoding t
  "Whether display the buffer encoding."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-number-limit 99
  "The maximum number displayed for notifications."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-repl t
  "Whether display the `repl' state.

Non-nil to display in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-lsp t
  "Whether display the `lsp' state.

Non-nil to display in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-env-version t
  "Whether display the environment version."
  :type 'boolean
  :group 'doom-modeline)

;;
;; Faces
;;

(defgroup doom-modeline-faces nil
  "The faces of `doom-modeline'."
  :group 'doom-modeline
  :group 'faces
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defface doom-modeline-spc-face
  '((t (:inherit mode-line)))
  "Face used for the white space."
  :group 'doom-modeline-faces)

(defface doom-modeline-vspc-face
  '((t (:inherit variable-pitch)))
  "Face used for the variable white space."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-minor-mode
  '((t (:inherit font-lock-doc-face :slant normal)))
  "Face used for the minor-modes segment in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-dir
  '((t (:inherit (font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `anzu', `evil-substitute' and`iedit', etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-host
  '((t (:inherit italic)))
  "Face for remote hosts in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-input-method
  '((t (:inherit (mode-line-emphasis bold))))
  "Face for input method in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-input-method-alt
  '((t (:inherit (font-lock-doc-face bold) :slant normal)))
  "Alternative face for input method in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-debug
  '((t (:inherit (font-lock-doc-face bold) :slant normal)))
  "Face for debug-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-info
  '((t (:inherit (success bold))))
  "Face for info-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-warning
  '((t (:inherit (warning bold))))
  "Face for warnings in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-urgent
  '((t (:inherit (error bold))))
  "Face for errors in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-unread-number
  '((t (:slant italic :weight normal)))
  "Face for unread number in the mode-line. Used by GitHub, mu4e, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-bar
  '((t (:inherit highlight)))
  "The face used for the left-most bar in the mode-line of an active window."
  :group 'doom-modeline-faces)

(defface doom-modeline-bar-inactive
  `((t (:background ,(face-foreground 'mode-line-inactive))))
  "The face used for the left-most bar in the mode-line of an inactive window."
  :group 'doom-modeline-faces)

(defface doom-modeline-debug-visual
  '((((class color) (background light))
     (:background "#D4843E"))
    (((class color) (background dark))
     (:background "#915B2D")))
  "Face to use for the mode-line while debugging."
  :group 'doom-modeline)

(defface doom-modeline-repl-success
  '((t (:inherit success :weight normal)))
  "Face for REPL success state."
  :group 'doom-modeline-faces)

(defface doom-modeline-repl-warning
  '((t (:inherit warning :weight normal)))
  "Face for REPL warning state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-success
  '((t (:inherit success :weight normal)))
  "Face for LSP success state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-warning
  '((t (:inherit warning :weight normal)))
  "Face for LSP warning state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-error
  '((t (:inherit error :weight normal)))
  "Face for LSP error state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-running
  '((t (:inherit compilation-mode-line-run :weight normal :slant normal)))
  "Face for LSP running state."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-timemachine
  '((t (:inherit (doom-modeline-buffer-file italic underline))))
  "Face for timemachine status."
  :group 'doom-modeline-faces)


;;
;; Externals
;;

(declare-function face-remap-remove-relative 'face-remap)
(declare-function project-roots 'project)
(declare-function projectile-project-root 'projectile)


;;
;; Core helpers
;;

;; FIXME #183: Force to caculate mode-line height
;; @see https://github.com/seagle0128/doom-modeline/issues/183
(defvar-local doom-modeline--size-hacked-p nil)
(defsubst doom-modeline-redisplay (&rest _)
  "Call `redisplay' to trigger mode-line height calculations.

Certain functions, including e.g. `fit-window-to-buffer', base
their size calculations on values which are incorrect if the
mode-line has a height different from that of the `default' face
and certain other calculations have not yet taken place for the
window in question.

These calculations can be triggered by calling `redisplay'
explicitly at the appropriate time and this functions purpose
is to make it easier to do so.

This function is like `redisplay' with non-nil FORCE argument.
It accepts an arbitrary number of arguments making it suitable
as a `:before' advice for any function.  If the current buffer
has no mode-line or this function has already been calle in it,
then this function does nothing."
  (when (and (bound-and-true-p doom-modeline-mode)
             mode-line-format
             (not doom-modeline--size-hacked-p))
    (setq doom-modeline--size-hacked-p t)
    (redisplay t)))
(advice-add #'fit-window-to-buffer :before #'doom-modeline-redisplay)
(advice-add #'resize-temp-buffer-window :before #'doom-modeline-redisplay)

;; Keep `doom-modeline-current-window' up-to-date
(defsubst doom-modeline--get-current-window (&optional frame)
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar doom-modeline-current-window (doom-modeline--get-current-window))

(defsubst doom-modeline--active ()
  "Whether is an active window."
  (and doom-modeline-current-window
       (eq (doom-modeline--get-current-window) doom-modeline-current-window)))

(defsubst doom-modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (when-let ((win (doom-modeline--get-current-window)))
    (unless (or (minibuffer-window-active-p win)
                (and (bound-and-true-p lv-wnd) (eq lv-wnd win)))
      (setq doom-modeline-current-window win))))

(defsubst doom-modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq doom-modeline-current-window nil))

(add-hook 'window-configuration-change-hook #'doom-modeline-set-selected-window)
(add-hook 'buffer-list-update-hook #'doom-modeline-set-selected-window)
(add-hook 'after-make-frame-functions #'doom-modeline-set-selected-window)
(add-hook 'delete-frame-functions #'doom-modeline-set-selected-window)
(add-hook 'exwm-workspace-switch-hook #'doom-modeline-set-selected-window)
(advice-add #'handle-switch-frame :after #'doom-modeline-set-selected-window)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defsubst doom-modeline-refresh-frame ()
          (setq doom-modeline-current-window nil)
          (cl-loop for frame in (frame-list)
                   if (eq (frame-focus-state frame) t)
                   return (setq doom-modeline-current-window
                                (doom-modeline--get-current-window frame)))
          (force-mode-line-update))
        (add-function :after after-focus-change-function #'doom-modeline-refresh-frame))
    (progn
      (add-hook 'focus-in-hook #'doom-modeline-set-selected-window)
      (add-hook 'focus-out-hook #'doom-modeline-unset-selected-window))))

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar doom-modeline-remap-face-cookie nil)
(defsubst doom-modeline-focus ()
  "Focus mode-line."
  (when doom-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative doom-modeline-remap-face-cookie)))
(defsubst doom-modeline-unfocus ()
  "Unfocus mode-line."
  (setq doom-modeline-remap-face-cookie
        (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defsubst doom-modeline-focus-change (&rest _)
          (if (frame-focus-state)
              (doom-modeline-focus)
            (doom-modeline-unfocus)))
        (advice-add #'handle-switch-frame :after #'doom-modeline-focus-change)
        (add-function :after after-focus-change-function #'doom-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'doom-modeline-focus)
      (add-hook 'focus-out-hook #'doom-modeline-unfocus))))


;;
;; Core
;;

(defvar doom-modeline-fn-alist ())
(defvar doom-modeline-var-alist ())

(defmacro doom-modeline-def-segment (name &rest body)
  "Defines a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           (add-to-list 'doom-modeline-var-alist (cons name (car body)))
           `(add-to-list 'doom-modeline-var-alist (cons ',name ',(car body))))
          (t
           (add-to-list 'doom-modeline-fn-alist (cons name sym))
           `(progn
              (defsubst ,sym () ,docstring ,@body)
              (add-to-list 'doom-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (byte-compile #',sym))))))))

(defun doom-modeline--prepare-segments (segments)
  "Prepare mode-line `SEGMENTS'."
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (cdr (assq seg doom-modeline-fn-alist)))
                    (push (list it) forms))
                   ((setq it (cdr (assq seg doom-modeline-var-alist)))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defmacro doom-modeline-def-modeline (name segments)
  "Defines a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `doom-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`doom-modeline-def-segment'."
  (declare (indent defun))
  (let ((sym (intern (format "doom-modeline-format--%s" name))))
    `(defun ,sym ()
       ,(concat "Modeline: " (prin1-to-string segments))
       ,(cl-list* 'list "" (doom-modeline--prepare-segments segments)))))

(defsubst doom-modeline (key)
  "Return a mode-line configuration associated with KEY (a symbol).
Throws an error if it doesn't exist."
  (let ((fn (intern-soft (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defsubst doom-modeline-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))


;;
;; Helpers
;;

(defsubst doom-modeline-spc ()
  "Text style with whitespace."
  (propertize " " 'face (if (doom-modeline--active)
                            'doom-modeline-spc-face
                          '(:inherit mode-line-inactive))))

(defsubst doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (let ((height (face-attribute 'mode-line :height)))
    ;; WORKAROUND: Fix tall issue of 27 on Linux
    ;; @see https://github.com/seagle0128/doom-modeline/issues/271
    (round
     (* (if (and (>= emacs-major-version 27)
                 (not (eq system-type 'darwin)))
            1.0
          1.25)
        (cond ((integerp height) (/ height 10))
              ((floatp height) (* height (frame-char-height)))
              (t (frame-char-height)))))))

(defsubst doom-modeline-add-variable-watcher (symbol watch-function)
  "Cause WATCH-FUNCTION to be called when SYMBOL is set if possible.

See docs of `add-variable-watcher'."
  (when (fboundp 'add-variable-watcher)
    (add-variable-watcher symbol watch-function)))

(defsubst doom-modeline--make-xpm (face width height)
  "Create an XPM bitmap via FACE, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (when (and (display-graphic-p)
             (image-type-available-p 'xpm))
    (propertize
     " " 'display
     (let ((data (make-list height (make-list width 1)))
           (color (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat
           (format
            "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
            (length (car data)) (length data) color color)
           (apply #'concat
                  (cl-loop with idx = 0
                           with len = (length data)
                           for dl in data
                           do (cl-incf idx)
                           collect
                           (concat
                            "\""
                            (cl-loop for d in dl
                                     if (= d 0) collect (string-to-char " ")
                                     else collect (string-to-char "."))
                            (if (eq idx len) "\"};" "\",\n")))))
  'xpm t :ascent 'center))))))

;; Check whether `window-width' is smaller than the limit
(defvar-local doom-modeline--limited-width-p nil)
(defsubst doom-modeline-window-size-change-function (&rest _)
  "Function for `window-size-change-functions'."
  (setq doom-modeline--limited-width-p
        (and (numberp doom-modeline-window-width-limit)
             (<= (+ (window-width)
                    (or scroll-bar-width 0)
                    (or left-fringe-width 0)
                    (or right-fringe-width 0)
                    (or left-margin-width 0)
                    (or right-margin-width 0))
                 doom-modeline-window-width-limit))))

(add-hook 'window-size-change-functions #'doom-modeline-window-size-change-function)
(add-hook 'buffer-list-update-hook #'doom-modeline-window-size-change-function)

(defvar-local doom-modeline--project-detected-p nil)
(defvar-local doom-modeline--project-root nil)
(defvar-local doom-modeline--project-parent-path nil)
(defsubst doom-modeline--project-root ()
  "Get the path to the root of your project.
Return nil if no project was found."
  (unless doom-modeline--project-detected-p
    (setq doom-modeline--project-root
          (pcase doom-modeline-project-detection
            ('projectile
             (when (fboundp 'projectile-project-root)
               (projectile-project-root)))
            ('project
             (when (fboundp 'project-current)
               (when-let ((project (project-current)))
                 (car (project-roots project))))))
          doom-modeline--project-detected-p t)
    (setq doom-modeline--project-parent-path
          (abbreviate-file-name
           (file-name-directory
            (directory-file-name doom-modeline--project-root)))))
  doom-modeline--project-root)

(defsubst doom-modeline-project-p ()
  "Check if the file is in a project."
  (doom-modeline--project-root))

(defsubst doom-modeline-project-root ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (or (doom-modeline--project-root) default-directory))

(defsubst doom-modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name'."
  (let* ((buffer-file-name
          (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
         (file-name
          (doom-modeline--buffer-file-name buffer-file-name)))

    (propertize (if (string-empty-p file-name)
                    (propertize "%b" 'face 'doom-modeline-buffer-file)
                  file-name)
                'mouse-face 'mode-line-highlight
                'help-echo (concat buffer-file-truename
                                   (unless (string= (file-name-nondirectory buffer-file-truename)
                                                    (buffer-name))
                                     (concat "\n" (buffer-name)))
                                   "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
                'local-map mode-line-buffer-identification-keymap)))

(defsubst doom-modeline--buffer-file-name (file-path)
  "Propertized variable `buffer-file-name' given by FILE-PATH.

Example:
  ~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el
"
  (let* ((project-root (file-local-name (doom-modeline-project-root)))
         (relative-path (file-relative-name
                         (or (file-name-directory file-path) "./")
                         project-root)))
    (concat
     ;; Project directory
     (propertize
      (concat (file-name-nondirectory (directory-file-name project-root)) "/")
      'face 'doom-modeline-project-dir)
     ;; relative path
     (propertize
      (when relative-path
        (if (string= relative-path "./")
            ""
          relative-path))
      'face 'doom-modeline-buffer-path)
     ;; File name
     (propertize
      (file-name-nondirectory file-path)
      'face 'doom-modeline-buffer-file))))

(provide 'doom-modeline-core)

;;; doom-modeline-core.el ends here
