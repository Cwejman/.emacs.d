(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode evil-magit which-key web-mode use-package typescript-mode tree-sitter-langs rainbow-delimiters projectile prettier-js magit lsp-ui lsp-treemacs lsp-ivy json-mode ivy-rich general evil-nerd-commenter evil-collection doom-themes doom-modeline dired-single dired-open dired-hide-dotfiles counsel company-box all-the-icons-dired))
 '(warning-suppress-types '((use-package))))

;;
;;


(setq make-backup-files nil)
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar
(recentf-mode 1)

(set-fringe-mode 0)         ; Give some breathing room
(column-number-mode)
(global-display-line-numbers-mode t)

(setq mac-right-command-modifier 'super
      mac-command-modifier 'meta
      mac-option-modifier nil)

(set-face-attribute 'default nil :font "Menlo" :height 124)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		treemacs-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;
;; Initialize package sources


(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;
;;

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package general
  :config
  (general-create-definer jc/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (jc/leader-keys
    "s"  '(counsel-switch-buffer :which-key "switch buffer")
    "r"  '(counsel-recentf :which-key "recent files")
    "f" '(toggle-frame-fullscreen :which-key "fullscreen")
    "t" '(counsel-load-theme :which-key "choose theme")))
    ;; "t"  '(:ignore t :which-key "toggles")

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;;
;;


(defun my/project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))

(use-package project
  ;; Cannot use :hook because 'project-find-functions does not end in -hook
  ;; Cannot use :init (must use :config) because otherwise
  ;; project-find-functions is not yet initialized.
  :config
  (add-hook 'project-find-functions #'my/project-override))


;;
;;


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;
;;


(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (setq dired-dwim-target t)
  (when (string= system-type "darwin")
    (setq insert-directory-program "/usr/local/bin/gls"))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))


;;
;;


(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;;
;;


(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq-default indent-tabs-mode nil)

(use-package web-mode
  :ensure t
  :hook (web-mode . lsp)
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Increase garbage collector threshold
(setq gc-cons-threshold 100000000) ;; 100 MB
;; Increase amount of data read from a process
(setq read-process-output-max (* 1024 1024)) ;; 1 MB

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  :hook (lsp-mode . efs/lsp-mode-setup)
  :config
  (lsp-enable-which-key-integration t)
  ;; (lsp-log-io nil) ;; Don't log everything = speed
  ;; (lsp-restart 'auto-restart))
  )


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(setq lsp-ui-sideline-show-diagnostics t)

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; (use-package typescript-mode
;;   :mode "\\.ts\\'" "\\.tsx\\'"
;;   :ensure t
;;   :hook (typescript-mode . lsp)
;;   :init
;;   (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
;;   :config
;;   (setq typescript-indent-level 2)
;;   ;; (add-hook 'typescript-mode #'subword-mode)
;;   ;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
;;   )

;; (use-package tree-sitter
;;   :ensure t
;;   :hook ((typescript-mode . tree-sitter-hl-mode)
;; 	 (typescript-tsx-mode . tree-sitter-hl-mode)))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter
;;   :config
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package company
  :after lsp-mode
  :hook
  (lsp-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  :bind (:map company-active-map
    ("<tab>" . company-complete-selection))
    (:map lsp-mode-map
    ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package prettier-js
  :ensure t
  :hook (web-mode . prettier-js-mode))


;;
;;

(use-package json-mode
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window))
  :config
  (treemacs-resize-icons 16))

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;;


(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))
