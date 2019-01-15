(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (espresso)))
 '(custom-safe-themes
   (quote
    ("1a1cdd9b407ceb299b73e4afd1b63d01bbf2e056ec47a9d95901f4198a0d2428" default)))
 '(evil-search-module (quote evil-search))
 '(mode-line-format
   (quote
    ("%e"
     (:eval
      (let*
          ((active
            (powerline-selected-window-active))
           (mode-line-buffer-id
            (if active
                (quote mode-line-buffer-id)
              (quote mode-line-buffer-id-inactive)))
           (mode-line
            (if active
                (quote mode-line)
              (quote mode-line-inactive)))
           (face0
            (if active
                (quote powerline-active0)
              (quote powerline-inactive0)))
           (face1
            (if active
                (quote powerline-active1)
              (quote powerline-inactive1)))
           (face2
            (if active
                (quote powerline-active2)
              (quote powerline-inactive2)))
           (separator-left
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (car powerline-default-separator-dir))))
           (separator-right
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (cdr powerline-default-separator-dir))))
           (lhs
            (list
             (powerline-raw "%*" face0
                            (quote l))
             (when powerline-display-buffer-size
               (powerline-buffer-size face0
                                      (quote l)))
             (when powerline-display-mule-info
               (powerline-raw mode-line-mule-info face0
                              (quote l)))
             (powerline-buffer-id
              (\`
               (mode-line-buffer-id
                (\, face0)))
              (quote l))
             (when
                 (and
                  (boundp
                   (quote which-func-mode))
                  which-func-mode)
               (powerline-raw which-func-format face0
                              (quote l)))
             (powerline-raw " " face0)
             (funcall separator-left face0 face1)
             (when
                 (and
                  (boundp
                   (quote erc-track-minor-mode))
                  erc-track-minor-mode)
               (powerline-raw erc-modified-channels-object face1
                              (quote l)))
             (powerline-major-mode face1
                                   (quote l))
             (powerline-process face1)
             (powerline-minor-modes face1
                                    (quote l))
             (powerline-narrow face1
                               (quote l))
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (when
                 (bound-and-true-p nyan-mode)
               (powerline-raw
                (list
                 (nyan-create))
                face2
                (quote l)))))
           (rhs
            (list
             (powerline-raw global-mode-string face2
                            (quote r))
             (funcall separator-right face2 face1)
             (unless window-system
               (powerline-raw
                (char-to-string 57505)
                face1
                (quote l)))
             (powerline-raw "%4l" face1
                            (quote l))
             (powerline-raw ":" face1
                            (quote l))
             (powerline-raw "%3c" face1
                            (quote r))
             (funcall separator-right face1 face0)
             (powerline-raw " " face0)
             (powerline-raw "%6p" face0
                            (quote r))
             (when powerline-display-hud
               (powerline-hud face0 face2))
             (powerline-fill face0 0))))
        (concat
         (powerline-render lhs)
         (powerline-fill face2
                         (powerline-width rhs))
         (powerline-render rhs)))))))
 '(org-agenda-files (quote ("~/documentation.org")))
 '(package-selected-packages
   (quote
    (doom-themes typescript-mode helm-rg deadgrep ## rg rjsx-mode xref-js2 js2-mode evil-magit powerline dockerfile-mode alchemist format-all dashboard auto-org-md espresso-theme evil-matchit leuven-theme github-theme exec-path-from-shell helm-projectile use-package projectile helm evil-visual-mark-mode)))
 '(split-height-threshold 200))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fira Code Light"))))
 '(whitespace-line ((t (:foreground "violet")))))

