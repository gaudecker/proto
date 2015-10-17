;;; proto.el --- Project Tools
;; -*- lexical-binding t; -*-

;; Copyright (C) 2015 Free Software Foundation

;; Author: Eeli Reilin <eeli@fea.st>
;; Created: 27 Apr 2015
;; Keywords: project workspace
;; Version: 0.1.0
;; Package-Requires: ((seq "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Project tools provides a library of functions for examining and exploring
;; workspaces and projects.

;;; Code:

(require 'seq)

(defun proto--prefix-path (prefix paths)
  "Return a list of pairs of PREFIX and path."
  (seq-map (lambda (path)
             (list prefix path))
           paths))

(defun proto--should-ignore-project (project &optional ignored)
  "Return non-nil if PROJECT is not a directory or it matches any of
the rules in IGNORED."
  (or (not (file-directory-p project))
      (and (listp ignored)
           (let ((rules (append '("\\.\\{1,2\\}$") ignored)))
             (seq-some (lambda (rule)
                         (string-match-p rule project))
                       rules)))))

(defun proto-list-projects (dirs &optional ignored)
  "Return a list of projects in DIRS.

If IGNORED is a list, projects that match the rules are ignored."
  (if (and (listp dirs) (car dirs))
      (append (proto-list-projects-in-dir (car dirs) ignored)
              (proto-list-projects (cdr dirs) ignored))))

(defun proto-list-projects-in-dir(dir &optional ignored)
  "Return a list of projects in DIR.

If IGNORED is a list, projects that match the rules are ignored."
  (seq-remove (lambda (project)
                (proto--should-ignore-project project ignored))
              (directory-files dir t)))

(proto-list-projects '("~/Projects"));"\\.\\{1,2\\}.*$"))
(proto--prefix-path "work" (proto-list-projects-in-dir "~/Projects" '("-")))

;;; proto.el ends here
