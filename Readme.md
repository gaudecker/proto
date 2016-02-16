# Proto

Project tools for GNU Emacs

## Introduction

Proto is a collection of functions for extracting metadata from, browsing, and
manipulating projects and workspaces.

A **project** is a directory on a filesystem, like
`/home/richard/projects/emacs`.  Projects usually contain source files and other
miscellaneous files.

A **workspace** is a directory that contains projects. Following the previous
example, `/home/richard/projects` could be a workspace.  Proto considers every
directory inside a workspace a project, this means that we will never have to
save Proto-specific project files on the filesystem.

## Usage

### `(proto-list-projects WORKSPACES &optional IGNORED)`

Returns a list of paths to projects from all specified workspaces.

```elisp
(let ((workspaces '("/home/richard/projects"))
      (ignored '("\\."))) ;; Ignore hidden directories
  (proto-list-projects workspaces))
```

### `(proto-project-root-contains-p PROJECT PATH)`

Returns `t` if file with `path` is found within the `project`.

```elisp
(let ((project "/home/richard/projects/emacs")
      (path "README"))
  (proto-project-root-contains-p project path))
```

### `(proto-project-list-files PROJECT &optional IGNORED NO-PREFIX)`

Returns a list of files within the `project`.  Leaves out any files that match
the rules in `ignored`.  If `no-prefix` is non-nil, each file is returned
without its path.

```elisp
(let ((project "/home/richard/projects/emacs")
      (ignored '("^\\."))) ;; Ignore hidden files
  (proto-project-list-files project ignored))
```

### `(proto-project-classify PROJECT &optional CLASSIFIERS)`

Returns a list of tags to identify project.  If `classifiers` is nil, use
`proto-project-classifiers` instead.

```elisp
(let ((project "/home/richard/projects/emacs"))
  (proto-project-classify project)) ;; (git autotools ...)
```
