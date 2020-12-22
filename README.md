# doom-modeline

CHANGE: Remove fancy features & Improve performance.

[![Build Status](https://github.com/seagle0128/doom-modeline/workflows/CI/badge.svg?branch=master)](https://github.com/seagle0128/doom-modeline/actions)
[![MELPA](https://melpa.org/packages/doom-modeline-badge.svg)](https://melpa.org/#/doom-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/doom-modeline-badge.svg)](https://stable.melpa.org/#/doom-modeline)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [doom-modeline](#doom-modeline)
    - [Feature](#feature)
    - [Screenshots](#screenshots)
    - [Install](#install)
        - [Manual](#manual)
        - [Use-package](#use-package)
    - [Customize](#customize)
    - [FAQ](#faq)

<!-- markdown-toc end -->

A fancy and fast mode-line inspired by minimalism design.

It's integrated into [Centaur Emacs](https://github.com/seagle0128/.emacs.d), 
[Doom Emacs](https://github.com/hlissner/doom-emacs) and
[Spacemacs](https://github.com/syl20bnr/spacemacs).

## Feature

The `doom-modeline` was designed for minimalism, and offers:

- A match count panel (for `iedit`, `multiple-cursors`)
- An indicator for recording a macro
- A customizable mode-line height (see `doom-modeline-height`)
- An error/warning count segment for `flycheck`
- A window number segment for `winum`
- An indicator for debug state
- An indicator for remote host
- An indicator for LSP state with `lsp-mode`
- An indicator for PDF page number with `pdf-tools`
- Truncated file name, buffer state and project name in buffer
  information segment, which is compatible with `project`, `projectile`
- New mode-line for `Info-mode` buffers
- New mode-line for `git-timemachine` buffers

## Screenshots

![modeline](https://user-images.githubusercontent.com/140797/49694177-10dcd280-fbc0-11e8-8d21-971ede6afdb5.png
 "Mode-line")

![search_replace](https://user-images.githubusercontent.com/140797/49694189-6913d480-fbc0-11e8-93ae-9578455dcd2c.png
"Search and Replace")

![macro](https://user-images.githubusercontent.com/140797/49694199-cc056b80-fbc0-11e8-9bb1-533b1e64da66.png
"Macro")

![no_icons](https://user-images.githubusercontent.com/140797/51301117-0805d900-1a69-11e9-957d-b4c7a70a1cf8.png
"No Icons")

![lsp_version](https://user-images.githubusercontent.com/140797/53592864-c751c180-3bc9-11e9-9914-493007c013d5.png
"Perspective, LSP, Version, VCS and Flycheck")

![battery](https://user-images.githubusercontent.com/140797/53593622-ba35d200-3bcb-11e9-85b3-38d64d05c127.png
"Battery")

![package](https://user-images.githubusercontent.com/140797/57503916-e769d380-7324-11e9-906d-44c79f7408a3.png
"Package")

![info](https://user-images.githubusercontent.com/140797/57506248-d96c8080-732d-11e9-8167-644c8fc4e0db.png
"Info")

## Install

### Manual

From melpa, `M-x package-install RET doom-modeline RET`.

In `init.el`,

``` emacs-lisp
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Or use this
;; Use `window-setup-hook' if the right segment is displayed incorrectly
(add-hook 'after-init-hook #'doom-modeline-mode)
```

### Use-package

``` emacs-lisp
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Or use this
;; Use `window-setup-hook' if the right segment is displayed incorrectly
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
```

This package requires the fonts included with `all-the-icons` to be installed.
Run `M-x all-the-icons-install-fonts` to do so. Please refer to the
[installation guide](https://github.com/domtronn/all-the-icons.el#installation).

Strongly recommend to use
[doom-themes](https://github.com/hlissner/emacs-doom-themes) at the same time.

## Customize

Run `M-x customize-group RET doom-modeline RET` or set the variables.

``` emacs-lisp
;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be displayed.
(setq doom-modeline-window-width-limit fill-column)

;; How to detect the project root.
;; The default priority of detection is `ffip' > `projectile' > `project'.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'project)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)
```

## FAQ

1. I am experiencing the laggy issue, how to resolve it?

   Add this configuration into your init file:

   ``` emacs-lisp
   ;; Don’t compact font caches during GC.
   (setq inhibit-compacting-font-caches t)
   ```

1. A ridiculous path is displayed on the mode-line while visiting a symbolink.

    It's the default behaviors of Vanilla Emacs. If you want to display the real
    names, please put this into your init file.

    ``` emacs-lisp
    (setq find-file-visit-truename t)
    ```

    If the file is controlled by vc, refer to the documentation of
    `vc-follow-symlinks`.

1. Why doesn't change of branch reflect in modeline?

   Actually it's related to `magit` and `vc-mode`.
   - Workaround:
     - Revert the buffers manually.
     - `(setq auto-revert-check-vc-info t)` brings the performance issue.
   - Refer to:
     - [The mode-line information isn’t always
       up-to-date](https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html)
     - [Maybe provide an alternative to VC's mode-line
       information](https://github.com/magit/magit/issues/2687)

1. Can I add my mode-line segments myself? How to do that?
   How can I define my own mode-line?

   There are two methods.
   - If the information is simple, just add to `mode-line-misc-info` or `global-mode-string`.

   - Use `doom-modeline-def-modeline` to define your own mode-line and set it as
     default.

     For example:

       ```emacs-lisp
       ;; Define your custom doom-modeline
       (doom-modeline-def-modeline 'my-simple-line
         '(bar matches buffer-info remote-host buffer-position parrot selection-info)
         '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

       ;; Add to `doom-modeline-mode-hook` or other hooks
       (defun setup-custom-doom-modeline ()
          (doom-modeline-set-modeline 'my-simple-line 'default))
       (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
       ```

1. How to specify font family in modeline?

    For example:

    ``` emacs-lisp
    (setq doom-modeline-height 1)
    (set-face-attribute 'mode-line nil :family "Noto Sans" :height 100)
    (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)
    ```

    or

    ```emacs-lisp
    (custom-set-faces
      '(mode-line ((t (:family "Noto Sans" :height 0.9))))
      '(mode-line-inactive ((t (:family "Noto Sans" :height 0.9)))))
    ```

    Please refer to
    [#189](https://github.com/seagle0128/doom-modeline/issues/189) and
    [#301](https://github.com/seagle0128/doom-modeline/issues/301).

1. How to disable symbolic links expanding in mode-line?

   If you encounter the issue like this

   ![Screenshot](https://user-images.githubusercontent.com/9449246/62822565-c3f93380-bb74-11e9-95f6-f9c24a6cbd14.png)

   please try this setting

   ```elisp
   ;; built-in `project' on 26+
   (setq doom-modeline-project-detection 'project)
   ;; or `find-in-project' if it's installed
   (setq doom-modeline-project-detection 'ffip)
   ```

   For more details, refer to
   [#209](https://github.com/seagle0128/doom-modeline/issues/209) and
   [#224](https://github.com/seagle0128/doom-modeline/issues/224).

## Donate

If you think it's helpful for you, please consider paying a cup of coffee for
me. Thank you! :smile:

<img
src="https://user-images.githubusercontent.com/140797/65818854-44204900-e248-11e9-9cc5-3e6339587cd8.png"
alt="Alipay" width="120"/>
&nbsp;&nbsp;&nbsp;&nbsp;
<img
src="https://user-images.githubusercontent.com/140797/65818844-366ac380-e248-11e9-931c-4bd872d0566b.png"
alt="Wechat Pay" width="120"/>

<a href="https://paypal.me/seagle0128" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.buymeacoffee.com/s9giES1" target="_blank">
<img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee"
width="160"/>
</a>
