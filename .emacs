(setq package-check-signature nil)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'projectile)
(require 'popup)
(require 'auto-complete)
(require 'cl)
(require 'whole-line-or-region)
;;(require 'yascroll)

(require 'comint)  ; comint-mode-map
(require 'dired-x) ; dired-jump
(require 'ido)     ; ido-completion-map

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the beep
(setq visible-bell 1)

(ido-mode t)
(setq ido-enable-flex-matching t)

(set-default 'truncate-lines t)
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))
;; Scrollbar
(scroll-bar-mode -1)

;; Enable brackets highlighting
(show-paren-mode 1)
;;(electric-pair-mode 1)

;; Delete selected when start typing
(delete-selection-mode 1)

;;(global-yascroll-bar-mode)

(global-auto-complete-mode t)

(menu-bar-mode -1)

(projectile-mode +1)

;;;;;;;;;;; USEFULL FUNCTIONS

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; just insert tab
(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named ?untitled? or ?untitled<2>?, ?untitled<3>?, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

;; Stealing some stuff from Casey Muratori config
; Bright-red TODOs from Casey config
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-study-face)
 (make-face 'font-lock-important-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
        ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
        ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
    fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
 (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(defun grep-cpp ()
  (interactive)
  (grep (format "findstr -s -n -i -l %s *.h *.cpp" (read-string "Find: ")))
  (other-window 1))

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

;;;;;;;;;;; SETTINGS

(prefer-coding-system 'cp1251)
(setq make-backup-files nil)

(setq-default line-spacing 4)

(add-hook 'before-save-hook 'untabify-except-makefiles)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable git crap
(remove-hook 'find-file-hook 'vc-find-file-hook)

;; Functions highlighting
(add-hook 'c++-mode-hook (lambda ()
   (font-lock-add-keywords nil '(
      ("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face))t)))

(add-hook 'c-mode-hook (lambda ()
   (font-lock-add-keywords nil '(
      ("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face))t)))

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq inhibit-startup-message t)

;; Setting TABS
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8
(setq-default c-basic-offset 4)
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

;; Scroll step
(setq scroll-step 1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

(setq c-default-style "linux"
      c-basic-offset 4)

;; Remove C-d keybinding in c-mode
;; in order to made other C-d keybindings work properly
(add-hook
'c++-mode-hook
(lambda ()
(local-set-key (kbd "C-d") nil)))

(add-hook
'c-mode-hook
(lambda ()
(local-set-key (kbd "C-d") nil)))

(set-variable 'grep-command "findstr -s -n -i -l ")
;;;;;;;;;;; KEYBINDINGS

(global-set-key (kbd "M-<up>") nil)
(global-set-key (kbd "M-<down>") nil)

(global-set-key  (kbd "<left>")  nil)
(global-set-key  (kbd "<right>")  nil)
(global-set-key  (kbd "<up>")  nil)
(global-set-key  (kbd "<down>")  nil)


;; Window setup

(global-set-key (kbd "TAB") 'my-insert-tab-char)
(global-set-key (kbd "C-t") 'indent-region)

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-horizontally)
(global-set-key (kbd "C-3") 'split-window-vertically)

(global-set-key (kbd "M-w") 'other-window)

(global-set-key [f11] 'toggle-menu-bar-mode-from-frame)

(global-set-key (kbd "C-S-b") 'buffer-menu-other-window)

(global-set-key (kbd "C-x C-S-s") 'write-file)

(global-set-key (kbd "C-g") 'goto-line)

(define-key global-map [f7] 'compile-it)
(define-key global-map [f6] 'show-funcs)
(define-key global-map [f8] 'next-error)
(define-key global-map [f9] 'previous-error)
(define-key global-map [f10] 'first-error)

(global-set-key  (kbd "C-h")  'backward-char)
(global-set-key  (kbd "C-l")  'forward-char)
(global-set-key  (kbd "C-k")  'previous-line)
(global-set-key  (kbd "C-j")  'next-line)

(global-set-key  (kbd "M-h")  'backward-word)
(global-set-key  (kbd "M-l")  'forward-word)
(global-set-key  (kbd "M-k")  nil)
(global-set-key  (kbd "M-j")  nil)

(global-set-key  (kbd "C-M-k")  'backward-list)
(global-set-key  (kbd "C-M-j")  'forward-list)
(global-set-key  (kbd "C-M-h")  'backward-sexp)
(global-set-key  (kbd "C-M-l")  'forward-sexp)

;;(global-set-key  (kbd "C-x")  nil)
(global-set-key  (kbd "C-c")  'forward-word)
(global-set-key  (kbd "C-u")  'undo)
(global-set-key  (kbd "C-;")  'universal-argument)

;;(global-set-key  (kbd "C-u")  'undo)
;;(define-key c-mode-base-map (kbd "C-d") nil)
(global-set-key  (kbd "C-d")  'whole-line-or-region-kill-region)
(global-set-key  (kbd "M-<backspace>")  'kill-line)
(global-set-key  (kbd "C-y")  'whole-line-or-region-copy-region-as-kill)

;;(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

(global-set-key  (kbd "C-f")  'projectile-find-file)
(global-set-key  (kbd "M-f")  'ff-find-other-file)
(global-set-key  (kbd "C-n")  'xah-new-empty-buffer)
(global-set-key  (kbd "C-o")  'find-file)
(global-set-key  (kbd "C-<next>")  'next-buffer)
(global-set-key  (kbd "C-<prior>")  'previous-buffer)
(global-set-key  (kbd "C-p")  'yank)
(global-set-key  (kbd "M-p")  'yank-pop)
(global-set-key  (kbd "C-x C-s")  'save-buffer)
(global-set-key  (kbd "C-x s")  'save-some-buffers)
(global-set-key  (kbd "C-b")  'switch-to-buffer)
(global-set-key  (kbd "C-s")  'isearch-forward)
;;(global-set-key (kbd "C-x (") 'kmacro-start-macro)
;;(global-set-key (kbd "C-x )") 'kmacro-end-macro)
;;(global-set-key (kbd "C-x )") 'kmacro-end-and-call-macro)

;;;;;;; WORKING DIRECTORY
(defvar working-dir)

(defun set-working-dir ()
  (interactive)
  (setq working-dir (read-directory-name "Working dir: "))
  )

(defun show-working-dir ()
  (message working-dir))

(set-working-dir)
(show-working-dir)

(setq default-directory working-dir)

(defun compile-it ()
  "Make the current build."
  (interactive)
  (setq default-directory working-dir)
  (save-buffer)
  (compile "build.bat")
  (other-window 1))

;; My color scheme (based on Spacemacs dark theme)

(set-face-attribute 'default nil :family "Consolas" :height 108)
(set-face-bold-p 'bold nil)

(defvar bg1 "#222222")
(defvar bg2 "#212026")
(defvar bg3 "#100a14")
(defvar bg4 "#0a0814")
(defvar base "#b2b2b2")
(defvar base-dim "#686868")
(defvar border "#5d4d7a")
(defvar suc "#86dc2f")
(defvar err "#e0211d")
(defvar keyword "#4f97d7")
(defvar comment "#2aa1ae")
(defvar const "#a45bad")
(defvar meta "#9f8766")
(defvar func "#bc6ec5")
(defvar keyword "#4f97d7")
(defvar str "#2d9574")
(defvar type "#ce537a")
(defvar var "#7590db")
(defvar cursor "#e3dedd")
(defvar war "#dC752F")
(defvar highlight "#444155")
(defvar mat "#86dc2f")
(defvar green-bg-s "#29422d")
(defvar comp "#c56ec3")
(defvar act1 "#222226")
(defvar act2 "#5d4d7a")
(defvar ttip "#9a9aba")
(defvar ttip-bg "#34323e")
(defvar ttip-sl "#5e5079")
(defvar green "#67b11d")
(defvar green-bg "#293235")
(defvar green-bg-s "#29422d")
(defvar yellow "#b1951d")
(defvar comp "c56ec3")

(set-face-attribute 'font-lock-builtin-face nil :foreground keyword)
(set-face-attribute 'font-lock-comment-face nil :foreground comment)
(set-face-attribute 'font-lock-constant-face nil :foreground const)
(set-face-attribute 'font-lock-doc-face nil :foreground meta)
(set-face-attribute 'font-lock-function-name-face nil :foreground func)
(set-face-attribute 'font-lock-keyword-face nil :foreground keyword)
(set-face-attribute 'font-lock-string-face nil :foreground str)
(set-face-attribute 'font-lock-type-face nil :foreground type :bold t)
(set-face-attribute 'font-lock-variable-name-face nil :foreground var)
(set-face-attribute 'cursor nil :background cursor)
(set-face-attribute 'custom-button nil :background bg2 :foreground base)
(set-face-attribute 'default nil :background bg1 :foreground base)
;;(set-face-attribute 'error nil :foreground err)
;;(set-face-attribute 'eval-sexp-fu-flash nil :background suc :foreground bg1)
;;(set-face-attribute 'eval-sexp-fu-flash-error nil :background err :foreground bg1)
(set-face-attribute 'font-lock-negation-char-face nil :foreground const)
(set-face-attribute 'font-lock-preprocessor-face nil :foreground func)
(set-face-attribute 'font-lock-warning-face nil :foreground war :background bg1)
(set-face-attribute 'fringe nil :foreground base :background bg1)
(set-face-attribute 'header-line nil :background bg4)
(set-face-attribute 'highlight nil :foreground base :background highlight)
;;(set-face-attribute 'hl-line nil :foreground bg2)
(set-face-attribute 'isearch nil :foreground bg1 :background mat)
(set-face-attribute 'lazy-highlight nil :foreground base :background green-bg-s)
(set-face-attribute 'link nil :foreground comment :underline t)
(set-face-attribute 'link-visited nil :foreground comp :underline t)
(set-face-attribute 'match nil :background highlight :foreground mat)
;;(set-face-attribute 'page-break-lines nil :foreground act2)
(set-face-attribute 'region nil :background highlight)
(set-face-attribute 'secondary-selection nil :background bg3)
(set-face-attribute 'shadow nil :foreground base-dim)
;;(set-face-attribute 'success nil :foreground suc)
(set-face-attribute 'tooltip nil :background ttip-sl :foreground base :bold nil :italic nil :underline nil)
(set-face-attribute 'vertical-border nil :foreground border)
;;(set-face-attribute 'warning nil :foreground war)
(set-face-attribute 'minibuffer-prompt nil :foreground keyword :bold t)


(set-face-attribute 'mode-line nil :background act1 :foreground base :box border)
(set-face-attribute 'mode-line-buffer-id nil :foreground func)
(set-face-attribute 'mode-line-inactive nil :background bg1 :foreground base :box border)

(set-face-attribute 'ido-first-match nil :foreground comp :bold t)
(set-face-attribute 'ido-only-match nil :foreground mat :bold t)
(set-face-attribute 'ido-subdir nil :foreground keyword)

;;(set-face-attribute 'popup-enu-selection-face nil :background ttip-sl :foreground base)
(set-face-attribute 'popup-face nil :background ttip-bg :foreground ttip)
;;(set-face-attribute 'popup-isearch-match nil :match t)
;;(set-face-attribute 'popup-menu-mouse-face nil :highlight t)
(set-face-attribute 'popup-scroll-bar-background-face nil :background bg2)
(set-face-attribute 'popup-scroll-bar-foreground-face nil :background act2)
(set-face-attribute 'popup-tip-face nil :background ttip-sl :foreground base :bold nil :italic nil :underline nil)

(set-face-attribute 'show-paren-match nil :foreground mat :background highlight :bold t :underline t)
;;(set-face-attribute 'show-paren-match-expression nil :background green-bg-s)
(set-face-attribute 'show-paren-mismatch nil :foreground err :bold t :underline t)

;;(setk-face-attribute 'ido-vertical-match-face nil :foreground comp, :underline nil)


;; irony
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)

;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Windows performance tweaks
;;
;;(when (boundp 'w32-pipe-read-delay)
;;  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
;;(when (boundp 'w32-pipe-buffer-size)
;; (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))
;;(setq company-idle-delay 0)

;;(add-hook 'after-init-hook 'global-company-mode)

;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))




;;(add-hook 'after-init-hook #'global-flycheck-mode)


;;(global-auto-complete-mode t)




;;(evil-mode 1)
;; remove all keybindings from insert-state keymap
;;(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
;;(define-key evil-insert-state-map [escape] 'evil-normal-state)
