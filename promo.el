;;; promo.el --- Project Mode  -*- lexical-binding t -*-

;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.

;; Author: Eeli Reilin <eeli@fea.st>
;; Keywords: project workspace
;; Version: 0.1.0
;; Package-Requires: ((proto "0.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Project mode provides a global minor mode and useful functions for
;; navigating and working with projects and workspaces.

;;; Code:


(defconst promo-version "0.1.0")

(defvar promo-map (make-sparse-keymap)
  "Keymap for promo.")

;;; Customizable variables:

(defgroup promo nil
  "promo; Project mode."
  :prefix "promo-"
  :link '(emacs-commentary-link "promo")
  :link '(url-link "https://github.com/gaudecker/proto"))

(defcustom promo-ignored-projects '()
  "List of projects to ignore.
Project names can be written as whole or as regular expressions
to match multiple projects."
  :type '(repeat string)
  :group 'promo)

(defcustom promo-ignored-files '()
  "List of files to ignore.
Filenames can be written as whole or as regular expressions
to match multiple files."
  :type '(repeat string)
  :group 'promo)

(defcustom promo-workspaces '()
  "List of paths to workspaces.
A workspace is a directory which can contain multiple projects as
subdirectories.  If workspaces contain duplicate projects, then
the functionality of promo becomes undefined."
  :type '(repeat directory)
  :group 'promo)

(defcustom promo-custom-commands '()
  "List of project specific custom commands."
  :type '(repeat sexp)
  :group 'promo)

(defcustom promo-show-on-mode-line t
  "Show currently open project on the mode line.
The project name is appended to the global-mode-string, and
should be the rightmost visible string on the mode line.
Project is represented according to promo-mode-line-format."
  :type 'boolean
  :group 'promo)

(defcustom promo-mode-line-format " [%s]"
  "Template for displaying open project on mode line.
A string is printed verbatim in the mode line except for %-constructs:
  %s -- print project name.")

(defcustom promo-completion-function #'completing-read
  "Function to be called when requesting input from the user."
  :type 'function
  :group 'promo)

(defcustom promo-open-project-after-find-file nil
  "Open project if a found file belongs to it.
This can be a little distracting if you're working on multiple
projects at once.  Use with caution."
  :type 'boolean
  :group 'promo)

(defcustom promo-open-project-hook nil
  "List of functions to be called after a project is opened."
  :type 'hook
  :group 'promo
  :options '(magit-status))

(defcustom promo-close-project-hook nil
  "List of functions to be called after a project is closed."
  :type 'hook
  :group 'promo)

(defcustom promo-refresh-project-hook nil
  "List of functions to be called after a project is refreshed."
  :type 'hook
  :group 'promo)

;;; Functions

(setq promo-workspaces '("/Users/eeli/Projects"))
(setq promo-ignored-projects '())

(defun promo--strip-project-path (project)
  (file-name-nondirectory project))

(defun promo-list-projects ()
  (seq-map (lambda (project)
             `(,(intern (promo--strip-project-path project)) . ,project))
           (proto-list-projects promo-workspaces
                                promo-ignored-projects)))

;; TODO: Check if there's a better function to get list of keys in alist
(defun promo-ido-completing-read (prompt choices)
  "Completing-read command that invokes `ido-completing-read'.
This function is wrapped so we can use it with alists."
  (let ((keys (seq-map (lambda (pair)
                         (if (listp pair)
                             (symbol-name (car pair))
                           pair))
                        choices)))
    (ido-completing-read prompt keys)))

(defun promo-nested-completing-read (prompt choices)
  "Completing-read command that invokes itself.
If CHOICES is an alist and a value of list is selected, call
itself on the sub-list."
  (let ((choice (funcall promo-completion-function prompt choices)))
    (if (consp (car choices))
        (let ((pair (assoc (intern choice)
                           choices)))
          (if (and (listp (cdr pair)) (cdr pair))
              (promo-nested-completing-read prompt (cdr pair))
            (cdr pair)))
      choice)))

(defun promo-open-project ()
  (interactive)
  (let* ((projects (promo-list-projects))
         (project (cdr (assoc (intern (funcall promo-completion-function "Open project: " projects))
                              projects))))
    (cd project)
    (message "Project opened:" project)))

(defun promo-find-file ()
  (interactive)
  (let ((file (promo-nested-completing-read "Find file: "
                                            (proto-project-list-files promo-project promo-ignored-files))))
    file))

;;;###autoload
(define-minor-mode promo
  "Toggle project mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode."
  nil
  " promo"
  promo-map)

;;;###autoload
(define-globalized-minor-mode global-promo promo (lambda () (promo t)))

(provide 'promo)

;;; promo.el ends here
