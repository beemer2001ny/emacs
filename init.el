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
;; Initialize package sources

;;(defvar efs/default-font-size 180)
;;(defvar efs/default-variable-font-size 180)

;; Make frame transparency overridable
;;(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; =============================================================================
;; load custom file setups
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; =============================================================================
;; bunch of global settings at startup
;(global-font-lock-mode t)                  ; font lock
(defalias 'yes-or-no-p 'y-or-n-p)           ; answer emacs
(setq inhibit-startup-message t)            ; ignore splash screen
(scroll-bar-mode -1)                        ; disable scroll bar
(tool-bar-mode -1)                          ; remove toolbar
(tooltip-mode -1)                           ; disable tooltips
(set-fringe-mode 10)                        ; sets side bar limits
(menu-bar-mode 1)                           ; disable menu bar
(column-number-mode)                        ; display column number
(setq size-indication-mode t)               ; display location percentage of buffer
(display-time)                              ; to display time in the status bar
(setq minibuffer-max-depth nil)             ; set minibuffer max depth to ignore
(setq bell-volumne 0)                       ; turn off bell
(setq visual-bell t)                        ; set visual bell
(setq makefile-electric-keys t)             ; install electric key bindings for makefile mode
(global-display-line-numbers-mode t)        ; display line number

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;==============================================================================
;; Key Bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)  ; Make ESC quit prompts
(global-set-key (kbd "<f1>")     'help-command)          ; help-command
(global-set-key (kbd "<f2>")     'undo)                  ; undo
(global-set-key (kbd "<f3>")     'find-file)             ; find-file
(global-set-key (kbd "<f4>")     'set-mark-command)      ; set-mark-command
(global-set-key (kbd "<f5>")     'query-replace)         ; search and replace
(global-set-key (kbd "<f7>")     'save-buffer)           ; save buffer
(global-set-key (kbd "C-<f5>")   'linum-mode)            ; show line numbers
(global-set-key (kbd "<C-tab>")  'bury-buffer)           ; toggle between buffers
(global-set-key (kbd "C-c C-c")  'comment-region)        ; comment region
(global-set-key (kbd "C-=")      'text-scale-increase)   ; increase text
(global-set-key (kbd "C--")      'text-scale-decrease)   ; decrease text

;;==============================================================================
;; cua-mode
;; Highlight and overwrite selected regions
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(setq cua-enable-modeline-indications t)
(setq cua-remap-control-v nil)
(setq cua-remap-control-z nil)

;;==============================================================================
;; marking text
;; make the current selection visible
(transient-mark-mode t)
;; delete the selection area with a keypress
(delete-selection-mode t)
;; allow emacs to play nice with others
(setq x-select-enable-clipboard t)
;; Make the delete key delete forward instead of doing the same thing as the
;; backspace key
(normal-erase-is-backspace-mode 1)
;;==============================================================================
;; put spaces instead of tabs (for readability by other editors)
;;(setq-default indent-tabs-mode nil)
;; Space out to under next indent point in previous nonblank line.
;;(setq indent-relative t)

;;==============================================================================
;; make the copy/paste function work nicely with others
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key (kbd "<mouse-2>") 'mouse-yank-at-click)
;;;highlight when searching and replacing
(setq search-highlight t
  query-replace-highlight t)
;; This forces emacs to open the target of the link, rather than the link itself
(setq find-file-visit-truename t)

;;==============================================================================
;; mode-line visual que of what text editing mode (overwrite, read_only, normal)
;; Change cursor color according to mode;
;; inspired by: http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "gray")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(setq djcb-read-only-color       "blue")
(setq djcb-read-only-cursor-type 'box)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "green")
(setq djcb-normal-cursor-type    'box)
;;
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color djcb-read-only-color)
    (setq cursor-type djcb-read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color djcb-overwrite-color)
    (setq cursor-type djcb-overwrite-cursor-type))
   (t
    (set-cursor-color djcb-normal-color)
    (setq cursor-type djcb-normal-cursor-type))))
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)


;;==============================================================================
;; install the the fonts package
;; https://github.com/domtronn/all-the-icons.el#installing-fonts
;; install in the following locations:
;; win10 - c:\windows\font
;; linux - 
(use-package all-the-icons
  :ensure t)

;;==============================================================================
;; global-command-log-mode
(use-package command-log-mode
  :commands command-log-mode)

;;==============================================================================
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;;==============================================================================
(use-package dashboard
  :ensure t  ;; install if not installed
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-set-footer nil)       ;; disable random messages at startup
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

;;==============================================================================
;(use-package general
;  :ensure t
;  :config
;  (general-create-definer leader-keys
;    ;:keymaps '(normal insert visual emacs)
;    ;:prefix "SPC"
;    :global-prefix "C-c");
;
;  (leader-keys
;    "t"  '(:ignore t :which-key "toggles")
;    "tt" '(counsel-load-theme :which-key "choose theme")
;    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

;;==============================================================================
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;==============================================================================
;; fast keybindings
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
;(leader-keys
;  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;==============================================================================
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
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;;==============================================================================
;; magit used to integrate git
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;==============================================================================
;; org mode


;;==============================================================================
;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;;==============================================================================
;; themes
;;(use-package doom-themes
;;  :init (load-theme 'doom-palenight t))
;;(use-package doom-themes
;;  :init (load-theme 'doom-one t))
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (setq spacemacs-theme-comment-bg nil
	spacemacs-theme-common-italic nil)
  (load-theme 'spacemacs-dark t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;==============================================================================
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;==============================================================================
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;==============================================================================
;; setup my vhdl mode configuration
(use-package vhdl-mode
  :init
  :bind (:map vhdl-mode-map
	      ("S-<f1>" . vhdl-speedbar))
  :config
  (setq vhdl-speedbar-update-on-saving t
	vhdl-clock-name "i_clk"
        vhdl-clock-rising-edge t
        vhdl-clock-edge-condition 'function
        ;; RESET
        vhdl-reset-kind 'sync
        vhdl-reset-name "i_rst"
	vhdl-reset-active-high t
        ;; COMMENTS
	vhdl-self-insert-comments nil
	vhdl-include-port-comments nil
	vhdl-include-direction-comments nil
	vhdl-include-type-comments nil
	vhdl-include-group-comments 'always
	vhdl-end-comment-column 80
	vhdl-inline-comment-column 40
	vhdl-stutter-mode t
	vhdl-comment-inline-offset 2
	vhdl-comment-empty-lines t
	;; GENERAL
	vhdl-standard '(93 nil)
	vhdl-indent-tabs-mode nil
	vhdl-basic-offset 2
	vhdl-electric-mode t
	vhdl-index-menu t
	vhdl-source-file-menu t
	vhdl-insert-empty-lines nil
	vhdl-upper-case-keywords nil
	vhdl-upper-case-types nil
	vhdl-upper-case-attributes nil
	vhdl-upper-case-enum-values nil
	vhdl-highlight-case-sensitive nil
	vhdl-highlight-translate-off nil
        vhdl-word-completion-case-sensitive nil
        vhdl-underscore-is-part-of-word t
        vhdl-align-groups nil
        vhdl-fixup-whitespace-region t
        vhdl-conditions-in-parenthesis t
        vhdl-optional-labels 'process
        ;; PORT MAPS
        vhdl-actual-port-name '(".*" . "\\&")
        ;; INSTANCE
        vhdl-instance-name '(".*" . "u_\\& ")
        vhdl-component-instance t
        ;; VHDL HEADER
	vhdl-file-header "-------------------------------------------------------------------------------
-- Author      : <name>
-- Filename    : <filename>
-- Date        : <date>
-- Description :
--             :
-- Revision    :
--
-------------------------------------------------------------------------------
-- PROPRIETARY INFORMATION:
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

<cursor>")
 )
