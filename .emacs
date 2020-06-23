(setq package-check-signature nil)
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(add-to-list 'load-path "~/.emacs.d/lisp/evil/")

;;(require 'evil)

(require 'popup)
(require 'auto-complete)
(require 'whole-line-or-region)
(require 'yascroll)
(require 'modern-cpp-font-lock)
(require 'csharp-mode)
(require 'rust-mode)
;(require 'racer)
;(require 'company)

(require 'comint)  ; comint-mode-map
(require 'dired-x) ; dired-jump
(require 'ido)     ; ido-completion-map

(require 'base16-based-color-scheme)

;(require 'company-racer)
;
;    (with-eval-after-load 'company
;       (add-to-list 'company-backends 'company-racer))

;; racer
;(add-hook 'rust-mode-hook #'racer-mode)
;(add-hook 'rust-mode-hook #'company-mode)
;(add-hook 'racer-mode-hook #'eldoc-mode)


;;(setq racer-rust-src-path "C:\Users\Dmitry\.rustup\toolchains\stable-x86_64-pc-windows-msvc\lib\rustlib\src\rust\src")

;(setq company-idle-delay 999999)

;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;(define-key company-active-map (kbd "C-j") 'company-select-next)
;(define-key company-active-map (kbd "C-k") 'company-select-previous)
;(define-key company-search-map (kbd "C-j") 'company-select-next)
;(define-key company-search-map (kbd "C-k") 'company-select-previous)
;;(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
;(define-key company-active-map "\e" 'company-abort)
;(setq company-tooltip-align-annotations t)

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

(global-yascroll-bar-mode)

(global-auto-complete-mode t)

(menu-bar-mode -1)

;;;;;;;;;;; USEFULL FUNCTIONS

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

(defun grep-cpp ()
  (interactive)
  (grep (format "findstr -s -n -i -l %s *.h *.cpp" (read-string "Find: ")))
  (other-window 1))

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

;;;;;;;;;;; SETTINGS

;;(prefer-coding-system 'cp1251)
(setq make-backup-files nil)

(setq-default line-spacing 4)

(add-hook 'before-save-hook 'untabify-except-makefiles)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;(add-hook 'c-mode-hook 'modern-cpp-font-lock-mode)
;;(add-hook 'c++-mode-hook 'modern-cpp-font-lock-mode)

;; Disable git crap
(remove-hook 'find-file-hook 'vc-find-file-hook)

(defun my-c++-mode-hook ()
  ;(setq c-basic-offset 4)
  (c-set-offset 'inlambda 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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

(normal-erase-is-backspace-mode 1)

;;;;;;;;;;; KEYBINDINGS

(global-set-key (kbd "M-<up>") nil)
(global-set-key (kbd "M-<down>") nil)

(global-set-key  (kbd "<left>")  nil)
(global-set-key  (kbd "<right>")  nil)
(global-set-key  (kbd "<up>")  nil)
(global-set-key  (kbd "<down>")  nil)

(global-set-key (kbd "RET") 'newline-and-indent)
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
(define-key global-map [f5] 'run-it)
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

(global-set-key  (kbd "C-c")  'forward-word)
(global-set-key  (kbd "C-u")  'undo)
(global-set-key  (kbd "C-;")  'universal-argument)

(global-set-key  (kbd "<delete>")  'delete-char)
(global-set-key  (kbd "C-d")  'whole-line-or-region-kill-region)
(global-set-key  (kbd "M-<backspace>")  'kill-line)
(global-set-key  (kbd "C-y")  'whole-line-or-region-copy-region-as-kill)

;;(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

;;(global-set-key  (kbd "C-f")  'projectile-find-file)
;;(global-set-key  (kbd "M-f")  'ff-find-other-file)
(global-set-key  (kbd "C-n")  'xah-new-empty-buffer)
(global-set-key  (kbd "C-o")  'find-file)
(define-key compilation-mode-map  (kbd "C-o")  'find-file)
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
  (setq working-dir (read-directory-name "Working dir: ")))

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
  (compile "build")
  (other-window 1))

(defun run-it ()
  (interactive)
  (setq default-directory working-dir)
  (save-buffer)
  (compile "build exec")
  (other-window 1))

(set-face-attribute 'default nil :family "Consolas" :height 102)
(set-face-bold-p 'bold nil)
