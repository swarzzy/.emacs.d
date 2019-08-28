(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
 ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-offsets-alist
   (quote
	((brace-list-open . 0)
	 (brace-list-close . 0)
	 (brace-list-intro . 0)
	 (brace-list-entry . 0))))
 '(custom-safe-themes
   (quote
	("ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "152c9642180cb0907bfe7c343ed07d0586c0d84fd8e7279d90566088989a13bb" "1e9001d2f6ffb095eafd9514b4d5974b720b275143fbc89ea046495a99c940b0" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "8150ded55351553f9d143c58338ebbc582611adc8a51946ca467bd6fa35a1075" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "cc71cf67745d023dd2e81f69172888e5e9298a80a2684cbf6d340973dd0e9b75" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "f39257aa5e82e3b9d5aa402a83769c4354de7b1b99eb48a89ffa7bed74fdd358" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "77b826c13b7571d6147d9fbe6fe88661e7fc72b22271cf553346d1ba63034401" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" default)))
 '(global-yascroll-bar-mode t)
 '(package-selected-packages
   (quote
	(helm-core helm-etags-plus helm rust-mode solarized-theme atom-one-dark-theme rg pt base16-theme auto-complete projectile ## company-irony irony mic-paren spacemacs-theme)))
 '(yascroll:delay-to-hide nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq package-check-signature nil)
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;;;;;;;;; MODES

;;(require 'wakib-keys)
;;(wakib-keys 1)

;; TODO, FIXME, ... highlighting
;;(require 'fic-mode)
;;(add-hook 'c++-mode-hook 'turn-on-fic-mode)

;; Turn off the toolbar
(tool-bar-mode -1)

;; Turn off the beep
(setq visible-bell 1)

(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'comint)  ; comint-mode-map
(require 'dired-x) ; dired-jump
(require 'ido)     ; ido-completion-map

(set-default 'truncate-lines t)
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))
;; Scrollbar
(scroll-bar-mode -1)

;; Enable brackets highlighting
(show-paren-mode 1)
(electric-pair-mode 1)

;; Delete selected when start typing
(delete-selection-mode 1)

(require 'cl)
(require 'yascroll)
(global-yascroll-bar-mode)

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

;; Cut region or whole line if nothing selected
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

;; Copy region or whole line if nothing selected. If called again, it'll append-copy next line.
(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2018-09-10"
  (interactive)
  (if current-prefix-arg
      (progn
        (copy-region-as-kill (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (copy-region-as-kill (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn )
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn )
              (progn
                (copy-region-as-kill (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (copy-region-as-kill (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))

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

;;;;;;;;;;; SETTINGS

(prefer-coding-system 'cp1251)
(setq make-backup-files nil)

(load-theme 'spacemacs-dark t)

(set-face-attribute 'default nil :family "Consolas" :height 108)
(set-face-bold-p 'bold nil)
(setq-default line-spacing 4)

;; Functions highlighting
(add-hook 'c++-mode-hook (lambda ()
   (font-lock-add-keywords nil '(
      ("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face))t)))

(add-hook 'c-mode-hook (lambda ()
   (font-lock-add-keywords nil '(
      ("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face))t)))

(defun untabify-c++ ()
  (if (derived-mode-p 'c++-mode)
      (untabify (point-min) (point-max))))

(defun del-trailing-whitespaces-c++ ()
  (if (derived-mode-p 'c++-mode)
    ('delete-trailing-whitespace)))


(add-hook 'write-contents-functions 'untabify-c++)
;;(add-hook 'write-contents-functions 'del-trailing-whitespaces-c++)

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

;; brackets highlighting settings
;;(set-face-background 'show-paren-match-face "#3E4451")
;;(set-face-attribute 'show-paren-match-face nil 
 ;;                   :weight 'normal :underline nil :overline nil ;;:slant 'normal)
;;(paren-activate)

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
(global-set-key (kbd "C-2") 'split-window-right)
(global-set-key (kbd "C-3") 'split-window-below)

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
(global-set-key  (kbd "C-d")  'xah-cut-line-or-region)
(global-set-key  (kbd "M-<backspace>")  'kill-line)
(global-set-key  (kbd "C-y")  'xah-copy-line-or-region)

;;(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

(global-set-key  (kbd "C-f")  'projectile-find-file)
(global-set-key  (kbd "M-f")  'projectile-find-other-file)
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
