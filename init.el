(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
          Your version of Emacs does not support SSL connections,
          which is unsafe because it allows man-in-the-middle attacks.
          There are two things you can do about this warning:
          1. Install an Emacs version that does support SSL and be safe.
          2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(eval-when-compile
  (require 'use-package))


(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-.") nil))

(use-package powerline
  :ensure t
  :init
  :config
  (powerline-default-theme))

;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and nil
(setq powerline-default-separator nil)

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1))

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))

(advice-add 'magit-checkout
            :after #'run-projectile-invalidate-cache)
(advice-add 'magit-branch-and-checkout ; This is `b c'.
            :after #'run-projectile-invalidate-cache)
(use-package rg
  :init
  :config
(rg-enable-default-bindings))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x d") 'helm-find-files)
  (global-set-key (kbd "C-x f") 'helm-find-files)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package helm-projectile)
(helm-projectile-on)

(use-package elixir-mode)
(use-package alchemist)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  ;; (setq dashboard-banner-logo-title ";; Happy Hacking")
  (setq dashboard-startup-banner 1)
  (setq dashboard-items '((projects . 10)
                          (bookmarks . 10)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))


(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package js2-mode)
(use-package xref-js2)
(use-package rjsx-mode
             :ensure t
             :config
    (define-key js2-mode-map (kbd "M-.") nil))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(setq ring-bell-function 'ignore)
(global-visual-line-mode t)
(global-evil-matchit-mode 1)
(global-company-mode 1)
(global-linum-mode t)
(menu-bar-mode -1)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(set-frame-parameter nil 'fullscreen 'fullboth)

;; (setq c-basic-offset 4)
;; (setq tab-width 4)

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-css-paths '("/Users/fiedler/.emacs.d/pandoc.css"))
  (setq markdown-command "/usr/local/bin/pandoc"))

;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(use-package general)

(define-key evil-normal-state-map (kbd "gf")
  (lambda () (interactive) (find-tag (find-tag-default))))

(define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

(define-key evil-normal-state-map (kbd "gn")
  (lambda () (interactive) (find-tag last-tag t)))

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load-theme 'tango-plus t)

