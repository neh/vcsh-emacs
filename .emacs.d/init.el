;;; init.el --- Initialize emacs
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq delete-old-versions t)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq-default indent-tabs-mode nil)
;; (setq-default show-trailing-whitespace t)
(setq make-backup-files nil)
(setq sentence-end-double-space nil)
(setq ring-bell-function 'ignore)
(setq enable-recursive-minibuffers t)
(save-place-mode 1)
(global-hl-line-mode t)
;; TODO emacs loses its window title along with focus. why?
(setq frame-title-format "%b")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error)

;; from http://www.jethrokuan.com/init.html
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(global-subword-mode 1)

(require 'package)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; (use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package gruvbox-theme :ensure t)
(load-theme 'gruvbox t)

(use-package undo-tree
  :diminish undo-tree-mode)

(diminish 'subword-mode)

(use-package paren
  :init
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  :config
  (show-paren-mode 1))

;; A fun to call from programming mode hooks to deal with auto-fill if
;; the below config doesn't work well.
(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;; (auto-fill-mode 1)
(setq-default fill-column 80)
(setq comment-auto-fill-only-comments t)
(setq-default auto-fill-function 'do-auto-fill)

;; uses the fill-column setting for visual-line-mode
(use-package visual-fill-column)

(use-package whitespace
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing tabs lines-tail))
  :config
  (whitespace-mode))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

;; nlinum is supposed to be faster, but elpa is maybe broken? so can't install
;; (use-package nlinum-relative
;;   :config
;;   (nlinum-relative-setup-evil)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;;   (setq nlinum-relative-current-symbol ""))

;; (use-package linum-relative
;;   :diminish linum-relative-mode
;;   :init
;;   (setq linum-relative-current-symbol "")
;;   :config
;;   (linum-relative-global-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0.25))

(use-package general
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "<SPC>" '(save-buffer :which-key "save")

    "b" '(:ignore t :which-key "buffer")
    "bd" '(evil-delete-buffer :which-key "delete buffer")

    "cc" '(comment-or-uncomment-region-or-line :which-key "toggle comment")

    "f" '(:ignore t :which-key "formatting")
    "fa" '(auto-fill-mode :which-key "auto fill")
    "fi" '(indent-region :which-key "indent region")
    "fp" '(fill-paragraph :which-key "paragraph")
    "fr" '(fill-region :which-key "fill region")
    "ft" '(toggle-truncate-lines :which-key "wrap lines")
    "fw" '(whitespace-mode :which-key "show whitespace")

    "g" '(:ignore t :which-key "git")
    "gd" '(magit-diff-popup :which-key "diff")
    "gl" '(magit-log-popup :which-key "log")
    "gp" '(magit-dispatch-popup :which-key "menu")
    "gs" '(magit-status :which-key "status")

    "h" '(help-command :which-key "help")
    "ha" 'helm-apropos
    "hf" '(counsel-describe-function :which-key "describe function")
    "hv" '(counsel-describe-variable :which-key "describe variable")

    "i" '(:ignore t :which-key "insert")
    "ip" '(clipboard-yank :which-key "paste from clipboard")

    "nh" 'neotree-find
    "nt" 'neotree-toggle

    "o" '(:ignore t :which-key "open")
    "oa" '(counsel-linux-app :which-key "app")
    "oe" '(mode-line-other-buffer :which-key "previous buffer")
    "of" '(counsel-find-file :which-key "open file")
    "og" '(counsel-git :which-key "open git file")
    "ol" '(org-open-at-point :which-key "follow link")
    "oo" '(ivy-switch-buffer :which-key "switch buffer")

    "pr" '(package-refresh-contents :which-key "refresh package info")

    "s" '(:ignore t :which-key "search")
    "sa" '(swiper-all :which-key "search all buffers")
    "sf" '(counsel-ag :which-key "search files")
    "sh" '(counsel-grep-or-swiper :which-key "search buffer")

    "v" '(:ignore t :which-key "view")

    "x" '(:ignore t :which-key "execute")
    "xa" '(ivy-resume :which-key "ivy resume")
    "xb" '(eval-buffer :which-key "eval buffer")
    "xe" '(eval-last-sexp :which-key "eval sexp")
    "xr" '(eval-region :which-key "eval region")
    "xx" '(counsel-M-x :which-key "M-x")
    )

  (general-define-key
   :states '(normal visual)
    "h" 'evil-backward-char
    "t" 'evil-next-visual-line
    "n" 'evil-previous-visual-line
    "s" 'evil-forward-char

    "l" 'evil-search-next
    "L" 'evil-search-previous
    "S" 'evil-window-bottom
    )
  )

(use-package evil
  :init
  (setq evil-move-cursor-back t)
  :config
  (evil-mode 1)

  (evil-set-initial-state 'ivy-occur-mode 'emacs)

  (unbind-key "s" evil-normal-state-map)
  (unbind-key "C-t" evil-normal-state-map)
  (unbind-key "C-n" evil-normal-state-map)
  (unbind-key "C-e" evil-motion-state-map)

  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (use-package evil-surround
    :config
    (global-evil-surround-mode)))


(use-package flx)
;; like easymotion. we'll see.
(use-package avy
  :general
  (general-define-key
   :states '(normal visual)
   :prefix "j"
   "l" '(avy-goto-line :which-key "line")
   "c" '(avy-goto-char-2 :which-key "char")
   )
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  )
(use-package smex)
(use-package ivy
  :diminish ivy-mode
  :init
  (defun reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))
  (defun given-file (cmd prompt) ; needs lexical-binding
    (lambda (source)
      (let ((target
             (let ((enable-recursive-minibuffers t))
               (read-file-name
                (format "%s %s to:" prompt source)))))
        (funcall cmd source target 1))))
  (defun confirm-delete-file (x)
    (dired-delete-file x 'confirm-each-subdirectory))

  :general
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "<escape>" 'keyboard-escape-quit
   "C-t" 'ivy-next-line
   "C-n" 'ivy-previous-line
   "C-M-t" 'ivy-next-line-and-call
   "C-M-n" 'ivy-previous-line-and-call
   "C-b" 'ivy-scroll-down-command
   "C-f" 'ivy-scroll-up-command
   "C-d" 'ivy-call
   )

  (general-define-key
   :keymaps 'counsel-find-file-map
   "TAB" 'ivy-alt-done
   )

  (general-define-key
   :keymaps 'ivy-occur-mode-map
   "t" 'ivy-occur-next-line
   "n" 'ivy-occur-previous-line
   "RET" 'ivy-occur-press
   "a" 'ivy-occur-read-action
   "c" 'ivy-occur-toggle-calling
   "C-f" 'evil-scroll-page-down
   "C-b" 'evil-scroll-page-up
   )

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-extra-directories nil)
  (setq ivy-height 10)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)

  (use-package counsel
    :diminish counsel-mode
    :config
    (counsel-mode 1)
    ;; These don't work on a fresh load, but seem to start working at some
    ;; point. Strange.
    (ivy-add-actions
     'counsel-find-file
     `(("c" ,(given-file #'copy-file "Copy") "copy")
       ("d" ,(reloading #'confirm-delete-file) "delete")
       ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
    (ivy-add-actions
     'counsel-projectile-find-file
     `(("c" ,(given-file #'copy-file "Copy") "copy")
       ("d" ,(reloading #'confirm-delete-file) "delete")
       ("m" ,(reloading (given-file #'rename-file "Move")) "move")
       ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))
    )
  )
(use-package hydra)
(use-package ivy-hydra)

(use-package ace-window
  :general
  (general-define-key
   :states '(normal motion emacs)
   :prefix "SPC"
   "w" 'ace-window
   )
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?d aw-delete-window " Ace - Delete Window")
          (?m aw-swap-window " Ace - Swap Window")
          (?M aw-move-window " Ace - Move Window")
          (?w aw-flip-window)
          (?c aw-split-window-fair " Ace - Split Fair Window")
          (?v aw-split-window-vert " Ace - Split Vert Window")
          (?b aw-split-window-horz " Ace - Split Horz Window")
          (?x delete-other-windows " Ace - Maximize Window"))
        ))


(use-package validate
  :demand t)

;; (use-package helm
;;   :init
;;   (setq helm-M-x-fuzzy-match t)
;;   :config
;;   (helm-mode 1)
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;   (define-key helm-map (kbd "<backtab>") 'helm-select-action)

;;   (use-package helm-ag)
;;   (use-package helm-projectile))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  )

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))


(fringe-mode 16)
;; (use-package diff-hl
;;   :config
;;   (diff-hl-mode)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package git-gutter-fringe+
  :config
  ;; (global-git-gutter+-mode)
  ;; (setq git-gutter+-window-width 2)
  (setq git-gutter+-added-sign ">")
  (setq git-gutter+-deleted-sign "<")
  (setq git-gutter+-modified-sign "*")
  ;; (setq git-gutter+-added-sign "▶")
  ;; (setq git-gutter+-deleted-sign "◀")
  ;; (setq git-gutter+-modified-sign "◆")
  )

(use-package magit
  :general
  (general-define-key
   :keymaps 'magit-mode-map
   "C-b" 'evil-scroll-page-up
   "C-f" 'evil-scroll-page-down
   "C-t" 'magit-section-forward
   "C-n" 'magit-section-backward
   "M-t" 'magit-section-forward-sibling
   "M-n" 'magit-section-backward-sibling
   "r" 'magit-refresh
   "R" 'magit-rebase-popup
   "g" 'magit-tag-popup
   "t" 'evil-next-visual-line
   "n" 'evil-previous-visual-line
   )
  (general-define-key
   :keymaps 'magit-diff-mode-map
   "/" 'evil-search-forward
   "l" 'evil-search-next
   "L" 'evil-search-previous
   )
  :config
  ;; There doesn't seem to be a "nice" way to adjust magit popups, so I stole
  ;; this method from evil-magit
  ;; refresh
  (magit-change-popup-key 'magit-dispatch-popup
                          :action (string-to-char "g") (string-to-char "r"))
  ;; rebase popup
  (magit-change-popup-key 'magit-dispatch-popup
                          :action (string-to-char "r") (string-to-char "R"))
  ;; tag popup
  (magit-change-popup-key 'magit-dispatch-popup
                          :action (string-to-char "t") (string-to-char "g"))

  (setq magit-completing-read-function 'ivy-completing-read)
  )


;; Installing org-plus-contrib as a lazy workaround for the built-in older
;; org-mode
;; https://github.com/jwiegley/use-package/issues/319
(use-package org
  :ensure org-plus-contrib
  ;; :general
  ;; (:keymaps 'org-mode-map
  ;;  "H" 'org-shiftleft
  ;;  "T" 'org-shiftdown
  ;;  "N" 'org-shiftup
  ;;  "S" 'org-shiftright
  ;;  "C-h" 'org-shiftleft
  ;;  "C-t" 'org-shiftdown
  ;;  "C-n" 'org-shiftup
  ;;  "C-s" 'org-shiftright
  ;;  )
  :init
  (setq org-ellipsis " …")
  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords
        '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'visual-fill-column-mode)
  )
(use-package org-evil
  ;; :general
  ;; (:states 'normal
  ;;  "M-h" 'org-promote
  ;;  )
  )

(use-package focus)
(use-package darkroom
  :init
  (setq darkroom-text-scale-increase 0)
  :general
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "vd" '(darkroom-tentative-mode :which-key "darkroom")
   )
  )

(use-package adaptive-wrap)

(use-package yaml-mode)
(use-package ansible-doc
  :general
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "hA" '(:ignore t :which-key "ansible")
   "hAA" '(ansible-doc :which-key "ansible docs")
   "hAR" '((lambda ()
             (interactive)
             (setq ansible-doc--modules nil)
             (ansible-doc))
           :which-key "ansible docs refresh")
  ))


(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
                face my-pl-segment1-active)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

;; (setq-default mode-line-format
;;       '("%e"
;;         mode-line-front-space
;;         ;; mode-line-mule-info -- I'm always on utf-8
;;         mode-line-client
;;         mode-line-modified
;;         ;; mode-line-remote -- no need to indicate this specially
;;         ;; mode-line-frame-identification -- this is for text-mode emacs only
;;         " "
;;         mode-line-directory
;;         mode-line-buffer-identification
;;         " "
;;         mode-line-position
;;         ;; (vc-mode vc-mode)  -- I use magit, not vc-mode
;;         (flycheck-mode flycheck-mode-line)
;;         " "
;;         mode-line-modes
;;         mode-line-misc-info
;;         mode-line-end-spaces))

(defface my-pl-segment1-active
  '((t (:foreground "#282828" :background "#d5c4a1")))
  "Powerline first segment active face.")
(defface my-pl-segment1-inactive
  '((t (:foreground "#ebdbb2" :background "#3c3836")))
  "Powerline first segment inactive face.")
(defface my-pl-segment15-active
  '((t (:foreground "#282828" :background "#bdae93")))
  "Powerline first segment active face.")
(defface my-pl-segment15-inactive
  '((t (:foreground "#ebdbb2" :background "#3c3836")))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-active
  '((t (:foreground "#ebdbb2" :background "#504945")))
  "Powerline second segment active face.")
(defface my-pl-segment2-inactive
  '((t (:foreground "#ebdbb2" :background "#3c3836")))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-active
  '((t (:foreground "#ebdbb2" :background "#32302f")))
  "Powerline third segment active face.")
(defface my-pl-segment3-inactive
  '((t (:foreground "#ebdbb2" :background "#32302f")))
  "Powerline third segment inactive face.")
(defface my-pl-hud
  '((t (:foreground "#fe8019" :background "#fe8019")))
  "Powerline hud face.")

(defun neh-powerline-theme ()
  "Powerline's Vim-like mode-line with evil state at the beginning in color."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (seg1 (if active
                                    'my-pl-segment1-active
                                  'my-pl-segment1-inactive))
                          (seg15 (if active
                                     'my-pl-segment15-active
                                   'my-pl-segment15-inactive))
                          (seg2 (if active
                                    'my-pl-segment2-active
                                  'my-pl-segment2-inactive))
                          (seg3 (if active
                                    'my-pl-segment3-active
                                  'my-pl-segment3-inactive))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)))
                                     (if evil-mode
                                         (funcall separator-left (powerline-evil-face) seg1))
                                     ;; (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
                                     ;; (powerline-raw mode-line-directory seg1)
                                     (powerline-buffer-id seg1)
                                     (powerline-raw " " seg1)
                                     (when (buffer-modified-p)
                                       (powerline-raw " " seg1))
                                     (when buffer-read-only
                                       (powerline-raw " " seg1))
                                     (when (and vc-mode buffer-file-name)
                                       (let ((backend (vc-backend buffer-file-name)))
                                         (pcase backend
                                           ('Git (powerline-raw (format "  %s " (car (vc-git-branches))) seg1))
                                           )))
                                         ;; Below is the way I _want_ this to
                                         ;; work; branch in a different segment
                                         ;; with separators, but I can't seem to
                                         ;; make the funcalls work inside the
                                         ;; concat for some reason
                                         ;; (concat (funcall separator-left seg1 seg15)
                                         ;;         (pcase backend
                                         ;;           ('Git (powerline-raw (format "   %s " (car (vc-git-branches))) seg1)))
                                         ;;         (funcall separator-left seg15 seg2))))
                                     (funcall separator-left seg1 seg2)
                                     (powerline-raw " " seg2 'l)
                                     (powerline-major-mode seg2)
                                     (powerline-process seg2)
                                     (powerline-raw " " seg2)
                                     ;; (powerline-raw "[%Z]" seg2)
                                     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") seg2)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object seg2 'l))
                                     (powerline-raw "[" seg2 'l)
                                     (powerline-minor-modes seg2)
                                     (powerline-raw "%n" seg2)
                                     (powerline-raw "]" seg2)
                                     (powerline-raw " " seg2)
                                     (funcall separator-left seg2 seg3)))
                          (rhs (list (funcall separator-right seg3 seg2)
                                     (powerline-raw "%l:" seg2 'l)
                                     (powerline-raw "%c" seg2 'r)
                                     (powerline-raw "%p " seg2 'p)
                                     (powerline-hud seg1 seg3))))
                     (concat (powerline-render lhs)
                             (powerline-fill seg3 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package powerline
  :init
  (setq powerline-default-separator 'slant)
  :config
  (column-number-mode 1)

  (use-package powerline-evil
    :config
    (neh-powerline-theme)
    (custom-theme-set-faces
      'user
      '(powerline-evil-base-face ((t (:inherit mode-line :foreground "#282828" :weight extra-bold))))
      '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "#98971a"))))
      '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "#458588"))))
      '(powerline-evil-visual-face ((t (:inherit powerline-evil-base-face :background "#fe8019"))))
      '(powerline-evil-operator-face ((t (:inherit powerline-evil-base-face :background "#83a598"))))
      '(powerline-evil-replace-face ((t (:inherit powerline-evil-base-face :background "#fb4934"))))
      '(powerline-evil-motion-face ((t (:inherit powerline-evil-base-face :background "#b16286"))))
      '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "#8ec07c")))))))


(use-package neotree
  :init
  (setq neo-theme 'arrow)
  :general
  (general-define-key
   :keymaps 'neotree-mode-map
   :states 'normal
   "RET" 'neotree-enter
   "o" 'neotree-enter
   "q" 'neotree-hide
   "C" 'neotree-change-root
   "U" 'neotree-select-up-node
   "R" 'neotree-refresh
   "I" 'neotree-hidden-file-toggle
   ;; "-" 'neotree-select-up-node
   "M-n" 'neotree-create-node
   "M-c" 'neotree-copy-node
   "M-d" 'neotree-delete-node
   "M-m" 'neotree-rename-node
   )
  )

(use-package expand-region
  :general
  ;; (general-define-key
  ;;  :states '(normal visual insert emacs)
  ;;  :prefix "SPC"
  ;;  :non-normal-prefix "M-SPC"
  ;;  "v" '(er/expand-region :which-key "expand region")
  ;;  )
  (general-define-key
   :states 'visual
   "v" 'er/expand-region))

(use-package markdown-mode)

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

;; (use-package js-mode
;;   :mode ("\\.json$" . js-mode)
;;   :init
;;   (progn
;;     (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

;; (use-package smart-mode-line-powerline-theme
  ;; :ensure t)

;; (use-package smart-mode-line
  ;; :ensure t
  ;; :config
;;   (require 'powerline)
;;   (setq powerline-default-separator 'wave)
  ;; (setq sml/theme 'powerline)
  ;; (sml/setup))
;;   ;; These colors are more pleasing (for gruvbox)
;;   (custom-theme-set-faces
;;    'user
;;    '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse3"))))
;;    '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "grey50" :weight normal))) t)
;;    '(sml/git ((t (:background "grey22" :foreground "chartreuse3"))) t)))


(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat "…/" output)))
       output))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


;; from https://www.reddit.com/r/emacs/comments/5rnpsm/nice_hydra_to_set_frame_transparency/
(defun my--set-transparency (inc)
  "Increase or decrease the selected frame transparency"
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

(defhydra hydra-transparency (:columns 2)
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("n" (lambda () (interactive) (my--set-transparency +1)) "+ more")
  ("t" (lambda () (interactive) (my--set-transparency -1)) "- less")
  ("N" (lambda () (interactive) (my--set-transparency +10)) "++ more")
  ("T" (lambda () (interactive) (my--set-transparency -10)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

;; (defun switch-to-previous-buffer ()
;;   "Switch to previously open buffer.
;; Repeated invocations toggle between the two most recently open buffers."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'init)
;;; init.el ends here
