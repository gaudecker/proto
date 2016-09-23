;;; proto.el --- Project Tools  -*- lexical-binding t -*-

;; Copyright (C) 2015-2016 Free Software Foundation, Inc.

;; Author: Eeli Reilin <eeli@fea.st>
;; Keywords: project workspace
;; Version: 0.1.0
;; Package-Requires: ((seq "1.11"))

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

(defun classify-nodejs (project)
  (when (proto-project-root-contains-p project "package.json")
    '(node)))

(defun classify-gulp (project)
  (when (proto-project-root-contains-p project "gulpfile.js")
    '(gulp)))

(defun classify-git (project)
  (when (proto-project-root-contains-p project ".git")
    '(git)))

(defun classify-rust (project)
  (when (proto-project-root-contains-p project "Cargo.toml")
    '(cargo rust)))

(defvar proto--type-classifiers
  '(classify-nodejs classify-gulp classify-git classify-rust))
(defvar proto-type-classifiers proto--type-classifiers
  "List of classifier functions.

Each function maps the single argument PROJECT into a list of
symbols that represent tags.")

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

(defun proto--should-ignore-file (file &optional ignored)
  (when (listp ignored)
    (seq-some (lambda (rule)
                (string-match-p rule file))
              ignored)))

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

(defun proto-project-root-contains-p (project path)
  "Return t if PROJECT directory contains PATH, nil otherwise."
  (seq-contains (directory-files project nil) path))

(defun proto-project-list-files (project &optional ignored no-prefix)
  "Return a list of files in PROJECT.

Filter out files that match rules in IGNORED."
  (let ((files (directory-files project nil)))
    (seq-map (lambda (file)
               (if (file-directory-p (concat project "/" file))
                   (proto-project-list-files (concat project "/" file) ignored no-prefix)
                 (substring (concat project "/" file)
                            (if no-prefix (+ (length project) 1) 0))))
             (seq-filter (lambda (file)
                           (not (proto--should-ignore-file file ignored)))
                         files))))



(defun proto-project-classify (project &optional classifiers)
  "Return a list of project classification tags."
  (seq-mapcat (lambda (fn) (funcall fn project))
              (or classifiers
                  proto--type-classifiers)))

;;; proto.el ends here
