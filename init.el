(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" default))
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter web-mode evil-nerd-commenter lsp-treemacs company-box company all-the-icons typescript-mode lsp-ivy lsp-ui lsp-mode doom-themes doom-modeline ivy-rich which-key rainbow-delimiters evil-magit magit counsel-projectile projectile evil-collection evil general use-package)))


;;
;;


(setq inhibit-startup-message t)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar
(recentf-mode 1)

(set-fringe-mode 0)         ; Give some breathing room
(column-number-mode)
(global-display-line-numbers-mode t)

(set-face-attribute 'default nil :font "Menlo" :height 144)

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
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

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

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;;
;;


(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Increase garbage collector threshold
(setq gc-cons-threshold 100000000) ;; 100 MB
;; Increase amount of data read from a process
(setq read-process-output-max (* 1024 1024)) ;; 1 MB

(use-package lsp-mode
  :commands (lsp lsp-deferred)
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

(use-package typescript-mode
  ;; :mode "\\.ts\\'" "\\.tsx\\'"
  :ensure t
  :hook (typescript-mode . lsp)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
	 (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
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
  :hook (typescript-mode . prettier-js-mode))

;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (use-package web-mode
;;   :ensure t
;;   :mode (("\\.js\\'" . web-mode)
;; 	 ("\\.jsx\\'" .  web-mode)
;; 	 ("\\.ts\\'" . web-mode)
;; 	 ("\\.tsx\\'" . web-mode)
;; 	 ("\\.html\\'" . web-mode)))


;;
;;

(use-package json-mode
  :ensure t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
