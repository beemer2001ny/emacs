;; =============================================================================
;; Function Key assignments/definitions   ;;
;; =============================================================================
;; =============================================================================
;; Add Main Repositories for Emacs packages
;; to refresh packages - M-x package-refresh-contents
;; to view packages    - M-x package-list-packages
;; to install a packge - M-x package-install <RET> <package_name> <RET>
;;
;; possilbe packages to use
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;                                 ("melpa-stable" . "https://stable.melpa.org/packages/")
;; (add-to-list 'package-archives '("melpa.org" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;;
;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; =============================================================================
;; Load custom file setups
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;;==============================================================================
;; bunch of global settings at startup
(global-font-lock-mode t)                   ; font lock
(defalias 'yes-or-no-p 'y-or-n-p)           ; answer emacs
(setq inhibit-startup-message t)            ; ignore splash screen
(scroll-bar-mode -1)                        ; disable scroll bar
(tool-bar-mode -1)                          ; remove toolbar
(tooltip-mode -1)                           ; disable tooltips
(set-fringe-mode 10)                        ; sets side bar limits
(menu-bar-mode -1)                          ; disable menu bar
(setq column-number-mode t)                 ; display column number
(setq size-indication-mode t)               ; display location percentage of buffer
(display-time)                              ; to display time in the status bar
(setq minibuffer-max-depth nil)             ; set minibuffer max depth to ignore
(setq bell-volumne 0)                       ; turn off bell
(setq visual-bell t)                        ; set visual bell
(setq makefile-electric-keys t)             ; install electric key bindings for makefile mode
(global-display-line-numbers-mode t)        ; display line number

;; =============================================================================
;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; =============================================================================
;; set fonts
;; (set-face-attribute 'default nil :font "<name>" :height xxx))

;;==============================================================================
;;;Key Bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts
(global-set-key (kbd "<f1>")    'help-command)          ; help-command
(global-set-key (kbd "<f2>")    'undo)                  ; undo
(global-set-key (kbd "<f3>")    'find-file)             ; find-file
(global-set-key (kbd "<f4>")    'set-mark-command)      ; set-mark-command
(global-set-key (kbd "<f5>")    'query-replace)         ; search and replace
(global-set-key (kbd "<f7>")    'save-buffer)           ; save buffer
(global-set-key (kbd "C-<f5>")  'linum-mode)            ; show line numbers
(global-set-key (kbd "<C-tab>") 'bury-buffer)           ; toggle between buffers
(global-set-key (kbd "C-c C-c") 'comment-region)

;; =============================================================================
;; open buffer to the side to see the keybindings
(use-package command-log-mode)

;; =============================================================================
;; completion frame
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
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; =============================================================================
;; helpful information
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; =============================================================================
;; general key binding
(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

;; =============================================================================
;; evil VIM emulator
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; =============================================================================
;; faster keybinding
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; =============================================================================
;; projectile helps with projects (git, .json, etc)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/APL")
    (setq projectile-project-search-path '("~/Projects/APL")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;(use-package evil-magit
;;  :after magit)

;; =============================================================================
;; Match delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; =============================================================================
;; which-keys displays keybindings available
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; =============================================================================
;; note: first time loading configuration on a new machine
;; must run interactively
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; =============================================================================
;; spacemacs theme
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))
