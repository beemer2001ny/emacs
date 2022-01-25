;;==============================================================================
;;                    Function Key assignments/definitions                    ;;
;;==============================================================================

;; =========================================
;; Add Main Repositories for Emacs packages
;; to refresh packages - M-x package-refresh-contents
;; to view packages    - M-x package-list-packages
;; to install a packge - M-x package-install <RET> <package_name> <RET>
;;
;; possilbe packages to use
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;(add-to-list 'package-archives '("melpa.org" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
                         ;;("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; install 'use-package if not installed
(unless (or (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
;;(setq use-package-verbose t)

;; On non-Guix systems, "ensure" packages by default
;;(setq use-package-always-ensure (not dw/is-guix-system))

;; Open dired in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")

;; Copy and move files netween dired buffers
(setq dired-dwim-target t)

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; Define external image viewer/editor
;;(setq image-dired-external-viewer "/usr/bin/gimp")

;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
    (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external))

;;==============================================================================
;; keep backup files in there own location.  keeps working directory clean
(defconst my-backup-dir
  (expand-file-name (concat user-emacs-directory "backups")))

(setq make-backup-files t                              ; make backup first time
                                                       ; a file is saved
      version-control t                                ; keep versions of backups
      backup-by-copying t                              ; and copy them to...
      backup-directory-alist `(("." . ,my-backup-dir)) ; ...here
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      vc-make-backup-files t                           ; even backup files under
                                                       ; version control (git/svn/etc.)
      make-backup-files nil                            ; No annoying "~file.txt"
      auto-save-default nil)                           ; no auto saves to #file#

;; if no backup directory exists, then create it:
(if (not (file-exists-p my-backup-dir))
    (mkdir my-backup-dir t))

;;==============================================================================
;; highlight current line
;(global-hl-line-mode t)

;;==============================================================================
;; Use 'command' as 'meta' in macOS
(setq mac-command-modifier 'meta)

;;==============================================================================
;; bunch of global settings at startup
(global-font-lock-mode t)          ; font lock
(defalias 'yes-or-no-p 'y-or-n-p)  ; answer emacs
(setq inhibit-startup-message t)   ; ignore splash screen
(tool-bar-mode -1)                 ; remove toolbar
(setq line-number-mode t)          ; display line number
(setq column-number-mode t)        ; display column number
(setq size-indication-mode t)      ; display location percentage of buffer
(display-time)                     ; to display time in the status bar
(setq minibuffer-max-depth nil)    ; set minibuffer max depth to ignore
(setq bell-volumne 0)              ; turn off bell
(setq makefile-electric-keys t)    ; install electric key bindings for makefile mode
(global-linum-mode t)              ; turn on line numbers


;;==============================================================================
;; auto complete the braces (), [], {}
;;(add-to-list 'load-path "~/.emacs.d/custom/autopair-master/")
;;(require 'autopair)
;;(autopair-global-mode)
;;;; show-paren-mode
;;;; show matching parenthesis
;;;; no delay when showing matching parenthesis
;;;; highlight the parenthesis
;;;; 'parenthesis 'expression
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;;==============================================================================
;; Highlight and overwrite selected regions (CUA mode) new in emacs24
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(setq cua-enable-modeline-indications t)
(setq cua-remap-control-v nil)
(setq cua-remap-control-z nil)

;;==============================================================================
;; Enable editing in columns: C-x C-n (enable) and C-u C-x C-n (disable)
(put 'set-goal-column 'disabled nil)
;; Enable narrowing of pages: C-x n p (narrow) and C-x n w (widen)
(setq minibuffer-max-depth nil)    ; set minibuffer max depth to ignore
(setq bell-volumne 0)              ; turn off bell
(setq makefile-electric-keys t)    ; install electric key bindings for makefile mode
(global-linum-mode t)              ; turn on line numbers

;;==============================================================================
;; auto complete the braces (), [], {}
;;(add-to-list 'load-path "~/.emacs.d/custom/autopair-master/")
;;(require 'autopair)
;;(autopair-global-mode)
;;;; show-paren-mode
;;;; show matching parenthesis
;;;; no delay when showing matching parenthesis
;;;; highlight the parenthesis
;;;; 'parenthesis 'expression
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;;==============================================================================
;; Highlight and overwrite selected regions (CUA mode) new in emacs24
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(setq cua-enable-modeline-indications t)
(setq cua-remap-control-v nil)
(setq cua-remap-control-z nil)
;;==============================================================================
(put 'narrow-to-page 'disabled nil)

;;==============================================================================
;; When working with rectangles, highlight them
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

;;==============================================================================
;; opacity / font size
;; http://www.djcbsoftware.nl/dot-emacs.html
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC i t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 5) (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0")
  '(lambda()(interactive) (modify-frame-parameters nil `((alpha . 100)))))
;; font size
;; ctrl--       decreases size
;; ctrl-shift-+ increases size
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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
(setq-default indent-tabs-mode nil)
;; Space out to under next indent point in previous nonblank line.
(setq indent-relative t)

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
;;;Key Bindings
(global-set-key (kbd "<f1>")    'help-command)       ; help-command
(global-set-key (kbd "<f2>")    'undo)               ; undo
(global-set-key (kbd "<f3>")    'find-file)          ; find-file
(global-set-key (kbd "<f4>")    'set-mark-command)   ; set-mark-command
(global-set-key (kbd "<f5>")    'query-replace)      ; search and replace
(global-set-key (kbd "<f7>")    'save-buffer)        ; save buffer
(global-set-key (kbd "C-<f5>")  'linum-mode)         ; show line numbers
(global-set-key (kbd "<C-tab>") 'bury-buffer)        ; toggle between buffers
(global-set-key (kbd "C-c C-c") 'comment-region)

;; org mode
;;(global-set-key (kbd "C-c l") 'org-store-link)
;;(global-set-key (kbd "C-c a") 'org-agenda)c
;;(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;==============================================================================
;; Load custom file setups
;; use emacs user direcotory works on all systems
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

(require 'use-package)
;; =========================================
;; from this point on add additional packages and configurations
;; use-package setup
;;
;; (use-package <package-name> <-- make sure the package name is represented by provide
;; : ensure t (note this will download any missing packages)
;; :init
;; <code to be executed before loading the package>
;; : config
;; <code to be executed after loading the package>
;; :bind
;; <key binding for this package>
;;
;;==============================================================================
;; spacemacs theme
(use-package spacemacs-common
;  ;:disabled nil
;  :ensure t
  :config
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italic nil)
  (load-theme 'spacemacs-dark t))
;;:delight)

;;==============================================================================
;; Helm configuration
(use-package helm
  :ensure t
  :demand
  :bind (("M-x"     . helm-M-x) ;; Evaluate functions
         ("C-x C-f" . helm-find-files) ;; Open or create files
         ("C-x b"   . helm-mini) ;; Select buffers
         ("C-x C-r" . helm-recentf) ;; Select recently saved files
         ("C-c i"   . helm-imenu) ;; Select document heading
         ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
         :map helm-map
         ("C-z" . helm-select-action)
         ("<tab>" . helm-execute-persistent-action))
  :config
  (require 'helm-config)
  (helm-mode 1))
;;
(use-package which-key
  :init
  (require 'which-key)
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.5)
  (which-key-setup-side-window-bottom))

;; Sensible line breaking
(add-hook 'text-mode-hook 'visual-line-mode)
;; Overwrite selected text
(delete-selection-mode t)
;; Scroll to the first and last line of the buffer
(setq scroll-error-top-bottom t)

;;==============================================================================
;; company mode
(use-package company
  :ensure t
  ;; navigate in completion minibuffer with 'C-n' and 'C-p'
  :bind (
         ;;:map global-map
         ;; ("TAB" . company-complete-common-or-cycle)
         ;; ;; use hippie
         ;; ("M-/" . hippie-expand)

      :map company-active-map
      ("TAB" . company-complete-common-or-cycle)
      ;; use hippie
      ("M-/" . hippie-expand)
      ("C-n" . company-selection-next)
      ("C-p" . company-selection-previous))
  :config
  ;; provide instant autocompletion
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 4
        company-selection-wrap-around t)
  ;; use global everywhere
  (global-company-mode t))

;;==============================================================================
;; Org Mode
;(require 'org)
;;(setq org-default-notes-file "~/Documents/help.org"
;;      initial-buffer-choice org-default-notes-file)
(use-package org
  :pin gnu
  :mode (("\\.org$" . org-mode))
  :ensure org-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (progn
    ;; config stuff
    ))

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Increase line spacing
;;(setq-default line-spacing 6)

;; Distraction-free screen
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))

;; Increase size of LaTeX fragment previews
;;(plist-put org-format-latex-options :scale 2)

;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/emacs/org-roam/"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package deft
  :config
  (setq deft-directory org-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))

;; Spell checking (requires the ispell software)
(add-hook 'bibtex-mode-hook 'flyspell-mode)

;; Change fields and format
(setq bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                    ("file" "Link to document file." ":"))
      bibtex-align-at-equal-sign t)

(setq bib-files-directory (directory-files
                           (concat (getenv "HOME") "/emacs/bibliography") t
                           "^[A-Z|a-z].+.bib$")
      pdf-files-directory (concat (getenv "HOME") "/emacs/pdf"))

(use-package helm-bibtex
  :config
  (require 'helm-config)
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path org-directory))

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography bib-files-directory
        org-ref-notes-directory org-directory
        org-ref-notes-function 'orb-edit-notes))

;;(use-package org-roam-bibtex
;;  :after (org-roam helm-bibtex)
;;  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
;;  :config
;;  (require 'org-ref))
;;  (org-roam-bibtex-mode)


;; =========================================
;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
         ;("\\.md\\'" . markdown-mode)
         ;("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"
              markdown-split-window-direction 'right))

(use-package impatient-mode
  :ensure t)

;; =========================================
;;grip-mode (use-package
;; grip-mode :ensure t :hook ((markdown-mode org-mode) . grip-mode))

;; =========================================
;; GIT integration
;; When you press C-c C-g, magit-status runs full-screen, but when you press q,
;; it restores your previous window setup. Very handy.
(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind
  (("C-c C-g" . magit-status)
   :map magit-status-mode-map
   ("q"       . magit-quit-session))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    "Make magit-status run alone in a frame."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package git-commit
  :hook (git-commit-mode . my-american-dict))

(use-package git-messenger
  :bind ("C-x G" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))


;;==============================================================================
;; vhdl-mode
;;(use-package whitespace
;;  :init
;;  (dolist (hook '(prog-mode-hook text-mode-hook vhdl-mode-hook))
;;    (add-hook hook #'whitespace-mode))
;;  (add-hook 'before-save-hook #'whitespace-cleanup)
;;  :config
;;  (setq whitespace-line-column 80) ;; limit line length
;;  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;;==============================================================================
;; whitespace
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80  ;; limit line length
        whitespace-style '(face tabs empty trailing lines-tail)))

;;==============================================================================
;; highlight-indent
;; https://github.com/DarthFennec/highlight-indent-guides/blob/master/README.md
;; method - 'fill, column, character, bitmap
;; auto / custom color hightlight
(use-package highlight-indent-guides
  :init
  ;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-odd-face-perc 15
        highlight-indent-guides-auto-even-face-perc 15
        highlight-indent-guides-auto-character-face-perc 20))

;; Proper line wrapping
;(global-visual-line-mode t)

;; Adds Nice Icons to Emacs so that other themes can use them (required for Doom theme below)
;; run M-x all-the-icons-install-fonts RET to install the fonts
                                        ;(require 'all-the-icons)

;;==============================================================================
;; Folder explorer (file tree) Neotree
;(Global-set-key [f8] 'neotree-toggle) ;; the F8 key toggles the file tree
(use-package neotree
  :init
  :bind (:map global-map
              ("<f8>" . neotree-toggle))
  )
;;;;;==============================================================================
;;;;; speedbar
;;;;; `sr-speedbar-width-x'        The `sr-speedbar' window width under WINDOW system.
;;;;; `sr-speedbar-width-console'  The `sr-speedbar' window width under CONSOLE.
;;;;; `sr-speedbar-max-width'      The max window width allowed remember.
;;;;; `sr-speedbar-delete-windows' Whether delete other window before showing up.
;;;;; `sr-speedbar-skip-other-window-p'
;;;;;      Whether skip `sr-speedbar' window when use
;;;;;      command `other-window'  select window in cyclic ordering of windows.
;;;;; `sr-speedbar-auto-refresh'   Control status of refresh speedbar content.
;;;;; `sr-speedbar-right-side'     Puts the speedbar on the right side if non-nil (else left).
;;;;;
;;;(add-to-list 'load-path "~/.emacs.d/custom/sr-speedbar/")
;;;(require 'sr-speedbar)
;;;;;[SHIFT+F1] vhdl speedbar
;;;(global-set-key (kbd "S-<f1>") 'sr-speedbar-toggle)
;;;(setq sr-speedbar-auto t)
;;;(setq sr-speedbar-width 30)
;;;(setq sr-speedbar-max-width 50)
;;;(setq sr-speedbar-window t)
;;;(setq sr-speedbar-right-side nil)
;;;(setq speedbar-supported-extension-expressions
;;;      '("\\.vhdl?$" "\\.vhd?$" "\\.vhm?$" "\\.vho?$" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?"
;;;        ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?"
;;;        ".ada" ".p[lm]" ".ttl" ".tcl" ".txt" ".m" ".scm" ".pm" ".py" ".s?html" "[Mm]ake.*\\(\\.in\\)?"))
;;;;(setq speedbar-use-images nil)
;;;                                        ;(sr-speedbar-open)
;;==============================================================================
;;==============================================================================
;; Start of codeing section
;;==============================================================================
;; subword
;; allows navigating "sub words" individually in CamelCaseIdentifiers
(use-package subword
  :hook
  (clojure-mode . subword-mode)
  (vhdl-mode . subword-mode))

;;==============================================================================
;; projectile
;; project-relative operations such as searches, navigation, etc.
(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config
  (projectile-global-mode))

;;==============================================================================
;; iedit
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
;; when you hit Ctrl-:, all occurrences of the symbol under the cursor
;; (or the current selection) are highlighted, and any changes you make on one
;; of them will be automatically applied to all others. It’s great for renaming
;; variables in code, but it needs to be used with care, as it has no idea of
;; semantics, it’s a plain string replacement, so it can inadvertently modify
;; unintended parts of the code.
(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

;;==============================================================================
;; eldoc
;; Turn on the online documentation mode for all programming modes
;; (not all of them support it) and for the Clojure REPL cider mode
(use-package eldoc
  :diminish
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode)
  (vhdl-mode       . turn-on-eldoc-mode))

;;==============================================================================
;; flyspell
;; on the fly spell checking
  (use-package flyspell
    :config
    (setq ispell-program-name "hunspell"
          ispell-default-dictionary "en_US")
    :hook (text-mode . flyspell-mode)
    :bind (("M-<f7>" . flyspell-buffer)
           ("<f7>" . flyspell-word)
           ("C-;" . flyspell-auto-correct-previous-word)))

;;==============================================================================
;; perl
(use-package cperl-mode
  :mode "\\.p[lm]\\'"
  :interpreter "perl"
  :config
  (setq cperl-hairy t))

;;==============================================================================
;; python
(use-package company-jedi
  :defer)

(defun my-python-mode-hook-fn ()
  (set (make-local-variable 'company-backends) '(company-jedi))
  (company-mode)
  (smartparens-mode 1)
  (local-set-key (kbd "M-.") #'jedi:goto-definition)
  (local-set-key (kbd "M-,") #'jedi:goto-definition-pop-marker)
  (local-set-key "\C-i" #'company-indent-or-complete-common))

(add-hook 'python-mode-hook #'my-python-mode-hook-fn)


;;==============================================================================
;; vhdl-mode
(use-package vhdl-mode
  :init
  :bind (:map vhdl-mode-map
              ("<S-f1>" . vhdl-speedbar))
  :config
  (setq vhdl-speedbar-update-on-saving t)
  (setq speedbar-supported-extension-expressions
        '("\\.vhdl?$" "\\.vhd?$" "\\.vhm?$" "\\.vho?$" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?"
          ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?"
          ".ada" ".p[lm]" ".ttl" ".tcl" ".txt" ".m" ".scm" ".pm" ".py" ".s?html" "[Mm]ake.*\\(\\.in\\)?"))
  (setq vhdl-clock-name "i_clk"
        vhdl-clock-rising-edge t
        vhdl-clock-edge-condition 'function
        ; RESET
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
        vhdl-inline-comment-column 20
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
        ;; vhdl-underscore-is-part-of-word
        vhdl-align-groups nil
        ;; vhdl-fixup-whitespace-region t
        vhdl-conditions-in-parenthesis t
        ;;vhdl-optional-labels 'process
        ;; PORT MAPS
        vhdl-actual-port-name '(".*" . "\\&")
        ;; INSTANCE
        vhdl-instance-name '(".*" . "\\u_&")
        ;;vhdl-component-instance

        ;; VHDL HEADER
        vhdl-file-header "
-------------------------------------------------------------------------------
-- File       : <file_name>
-- Author     : Dion Barrick x24308
-- Company    : JHU/APL
-- Created    : 2020-04-17
-- Last update: 2020-04-20
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2020
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2020-04-17  1.0      barridd1 Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

cursor>"))
