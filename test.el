(setq promo-completion-function 'promo-ido-completing-read)
(promo-list-projects)

(setq promo-ignored-projects '())
(setq promo-ignored-files '(".git"))
(setq promo-workspaces '("/Users/eeli/Projects"))
(setq promo-completion-function 'promo-ido-completing-read)
(setq promo-project "/Users/eeli/Projects/proto")

(let ((project "/Users/eeli/Projects/essence-lang")
      (ignored '(".git")))
  (proto-project-list-files project ignored))
