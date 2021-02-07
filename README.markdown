# README

## Name

duplicate-thing

## Description

*duplicate-thing.el* is Emacs lisp. Easy duplicate line or region, with comment out.

1. Duplicate current line.
2. Duplicate a selection when selection is active.
3. Only C-u, replicate, comment out the range.
4. Numerical prefix is specified as 'C-u 5': do multiple times repeatedly.

## Authors

* ongaeshi <ongaeshi0621@gmail.com>

## License

GPLv3

## Install

```emacs-lisp
;; auto-install
(auto-install-from-url "https://raw.github.com/ongaeshi/duplicate-thing/master/duplicate-thing.el")
```

### Install with `straight.el` and `use-package`

```emacs-lisp
(straight-use-package '(duplicate-thing :type git :host github :repo "ongaeshi/duplicate-thing"))

(use-package duplicate-thing
  :bind
  ("M-c" . duplicate-thing))
```


## .emacs.d/init.el

```emacs-lisp
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)
```

## Thanks

* @a_ars: Idea, Can determine the difference between 'C-U' and 'C-U 4'
* @k_somemo: Bug report
