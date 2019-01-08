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

;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and ni
(setq powerline-default-separator nil)

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

(use-package powerline
  :ensure t
  :init
  :config
  (powerline-default-theme))

;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and ni
(setq powerline-default-separator nil)

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

(use-package powerline
  :ensure t
  :init
  :config
  (powerline-default-theme))

;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and ni
(setq powerline-default-separator nil)

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

(use-package powerline
  :ensure t
  :init
  :config
  (powerline-default-theme))

;; chamfer, contour, curve, rounded, roundstub, slant, wave, zigzag, and ni
(setq powerline-default-separator nil)

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

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))

(advice-add 'magit-checkout
            :after #'run-projectile-invalidate-cache)
(advice-add 'magit-branch-and-checkout ; This is `b c'.
            :after #'run-projectile-invalidate-cache)

(use-package helm-projectile)
(helm-projectile-on)

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

(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package elixir-mode)
(use-package alchemist)

(use-package js2-mode)
(use-package xref-js2)
(use-package rjsx-mode)

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(define-key js2-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

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
  :init (setq markdown-css-paths '("/Users/fiedler/.emacs.d/pandoc.css"))
  (setq markdown-command "/usr/local/bin/pandoc"))

;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(use-package general)

(defun helm-ff-directory-files (directory)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
  (setq directory (file-name-as-directory
                   (expand-file-name directory)))
  (let* (file-error
         (ls   (condition-case err
                   (helm-list-directory directory)
                 ;; Handle file-error from here for Windows
                 ;; because predicates like `file-readable-p' and friends
                 ;; seem broken on emacs for Windows systems (always returns t).
                 ;; This should never be called on GNU/Linux/Unix
                 ;; as the error is properly intercepted in
                 ;; `helm-find-files-get-candidates' by `file-readable-p'.
                 (file-error
                  (prog1
                      (list (format "%s:%s"
                                    (car err)
                                    (mapconcat 'identity (cdr err) " ")))
                    (setq file-error t)))))
         (dot  (concat directory "."))
         (dot2 (concat directory "..")))
    (puthash directory (+ (length ls) 2) helm-ff--directory-files-hash)
    ;; (append (and (not file-error) (list dot dot2)) ls)
    ;; return the files only, excluding the "." and ".."
    ls
    ))

(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (projectile-project-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (projectile-project-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

;;(defun create-tags (dir-name)
;;    "Create tags file."
;;    (interactive "Directory: ")
;;    (shell-command
;;     (format "%s -f TAGS -e -R %s" "/usr/local/bin/ctags" (directory-file-name dir-name)))
;;    )

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat (projectile-project-root) "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))


(define-key evil-normal-state-map (kbd "gf")
  (lambda () (interactive) (find-tag (find-tag-default))))

(define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

(define-key evil-normal-state-map (kbd "gn")
  (lambda () (interactive) (find-tag last-tag t)))

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

;;(general-define-key
;; :states '(normal visual insert emacs)
;; :prefix "`"
;; "t" '(iterm-focus :which-key "focus iterm")
;; "d" '(open-dir-in-iterm :which-key "open dir in iterm")
;; )

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
