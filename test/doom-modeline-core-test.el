;;; doom-modeline-core-test.el --- Unit tests for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

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
;;  Unit tests for doom-modeline.
;;

;;; Code:

(require 'cl-lib)
(require 'doom-modeline-core)

(ert-deftest doom-modeline-project-root/projectile ()
  (let ((default-directory "/home/user/projectile/")
        (doom-modeline-project-detection 'projectile)
        (doom-modeline--project-detected-p t)
        (doom-modeline--project-root nil))
    (cl-flet ((projectile-project-root () default-directory))
      (should (string= (doom-modeline-project-root) "/home/user/projectile/")))))

(ert-deftest doom-modeline-project-root/project ()
  (let ((default-directory "/home/user/project-current/")
        (doom-modeline-project-detection 'project)
        (doom-modeline--project-detected-p t)
        (doom-modeline--project-root nil))
    (cl-flet ((project-current (&optional _maybe-prompt _dir)
                               `(vc . ,default-directory)))
      (should (string= (doom-modeline-project-root) "/home/user/project-current/")))))

(ert-deftest doom-modeline-project-root/default ()
  (let ((default-directory "/home/user/project/")
        (doom-modeline-project-detection nil)
        (doom-modeline--project-detected-p t))
    (should (string= (doom-modeline-project-root) "/home/user/project/"))))

;;; doom-modeline-core-test.el ends here
