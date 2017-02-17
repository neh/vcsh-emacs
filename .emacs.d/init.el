;;; init.el --- Initialize emacs
;;; Commentary:
;;; Code:

(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq make-backup-files nil)
(save-place-mode 1)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error)

(setq show-paren-delay 0)
(show-paren-mode 1)

(global-subword-mode 1)

;; not sure this is helping anything
(setq sentence-end-double-space nil)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package gruvbox-theme :ensure t)
(load-theme 'gruvbox t)

(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

(use-package linum
  :ensure t
  :config
  (linum-mode)

  (use-package linum-relative
    :ensure t
    :config
    (setq linum-relative-current-symbol "")
    (linum-relative-global-mode)))


(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)

  ;; Doesn't work, not sure how to use evil in customize
  ;; (evil-set-initial-state 'custom 'normal)

  ;; (evil-set-initial-state 'NeoTree 'emacs)
  (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-change-root)
  (evil-define-key 'normal neotree-mode-map (kbd "U") 'neotree-select-up-node)
  (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "I") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "-") 'neotree-select-up-node)
  (define-key evil-motion-state-map (kbd "-") 'neotree-find)
  (evil-define-key 'normal neotree-mode-map (kbd "M-n") 'neotree-create-node)
  (evil-define-key 'normal neotree-mode-map (kbd "M-c") 'neotree-copy-node)
  (evil-define-key 'normal neotree-mode-map (kbd "M-d") 'neotree-delete-node)
  (evil-define-key 'normal neotree-mode-map (kbd "M-m") 'neotree-rename-node)

  (define-key evil-normal-state-map (kbd "s") nil)
  (define-key evil-normal-state-map (kbd "\C-t") nil)
  (define-key evil-normal-state-map (kbd "\C-n") nil)
  (define-key evil-motion-state-map (kbd "\C-e") nil)

  (define-key evil-motion-state-map (kbd "h") 'evil-backward-char)
  (define-key evil-motion-state-map (kbd "t") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "n") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "s") 'evil-forward-char)

  (define-key evil-motion-state-map (kbd "l") 'evil-search-next)
  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (global-set-key (kbd "C-e") 'mode-line-other-buffer)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-t") 'evil-window-down)
  (global-set-key (kbd "C-n") 'evil-window-up)
  (global-set-key (kbd "C-s") 'evil-window-right)

  (use-package evil-leader
    :ensure t
    :config
    (setq evil-leader/in-all-states t)
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'save-buffer
      "x" 'helm-M-x
      "o" 'helm-find-files
      "e" 'helm-mini
      "cc" 'comment-or-uncomment-region-or-line
      "u" 'mode-line-other-buffer
      "h" 'help-command

      "bd" 'evil-delete-buffer

      "gs" 'magit-status
      "gp" 'magit-dispatch-popup

      "nt" 'neotree-toggle

      "wo" 'delete-other-windows
      "wc" 'evil-window-delete))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))


(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<backtab>") 'helm-select-action))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t)


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))


(use-package magit
  :ensure t)


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
                          (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
                          (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
                          (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
                          ;; (mode-line (if active 'mode-line 'mode-line-inactive))
                          ;; (face1 (if active 'powerline-active1 'powerline-inactive1))
                          ;; (face2 (if active 'powerline-active2 'powerline-inactive2))
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
                                         (when backend
                                           (concat (powerline-raw "[" seg3 'l)
                                                   (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
                                                   (powerline-raw "]" seg3)))))
                                     (funcall separator-left seg1 seg2)
                                     (powerline-raw "[" seg2 'l)
                                     (powerline-major-mode seg2)
                                     (powerline-process seg2)
                                     (powerline-raw "]" seg2)
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
  :ensure t
  :config
  (setq powerline-default-separator 'slant)

  (column-number-mode 1)

  (use-package powerline-evil
    :ensure t
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
  :ensure t
  :config
  (setq neo-theme 'arrow))


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

;; (defun switch-to-previous-buffer ()
;;   "Switch to previously open buffer.
;; Repeated invocations toggle between the two most recently open buffers."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'init)
;;; init.el ends here
