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
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  :config ;; tweak evil after loading it
  (evil-mode))

(use-package company
  :commands (company-mode company-indent-or-complete-common company-tng)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  :init)

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

(use-package helm-projectile)
(helm-projectile-on)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x d") 'helm-find-files)
  (global-set-key (kbd "C-x f") 'helm-find-files)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package elixir-mode)
(use-package alchemist)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  ;; (setq dashboard-banner-logo-title ";; Happy Hacking")
  (setq dashboard-startup-banner 1)
  (setq dashboard-items '((recents . 10)
                          (projects . 10)
                          (agenda . 5)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook))

(setq ring-bell-function 'ignore)
(setq-default fill-column 80)
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
  :init (setq markdown-command "/usr/local/bin/pandoc"))

;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(use-package general)

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

(defun open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app"))
    (shell-command (concat "open -a " iterm-app-path " ."))))

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "`"
 "t" '(iterm-focus :which-key "focus iterm")
 "d" '(open-dir-in-iterm :which-key "open dir in iterm")
 )

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
