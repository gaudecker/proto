(setq promo-completion-function 'promo-ido-completing-read)
(promo-list-projects)

(promo-ido-completing-read "foo " (promo-list-projects))
(completing-read "foo " (promo-list-projects))
