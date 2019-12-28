(defvar bg1 "#181818")
(defvar bg2 "#181818")
(defvar bg3 "#100a14")
(defvar bg4 "#0a0814")
(defvar base "#b8b8b8")
(defvar base-dim "#585858")
(defvar border "#383838")
(defvar suc "#a1b56c")
(defvar err "#ab4642")
(defvar keyword "#ba8baf")
(defvar comment "#86c1b9")
(defvar const "#ba8baf")
(defvar meta "#b8b8b8")
(defvar func "#7cafc2")
(defvar str "#a1b56c")
(defvar type "#dc9656")
(defvar var "#b8b8b8")
(defvar cursor "#d8d8d8")
(defvar war "#dc9656")
(defvar highlight "#383838")
(defvar mat "#a1b56c")
(defvar green-bg-s "#29422d")
(defvar comp "#ba8baf")
(defvar act1 "#222226")
(defvar act2 "#5d4d7a")
(defvar ttip "#9a9aba")
(defvar ttip-bg "#34323e")
(defvar ttip-sl "#5e5079")
(defvar green "#67b11d")
(defvar green-bg "#293235")
(defvar green-bg-s "#29422d")
(defvar yellow "#b1951d")

(set-face-attribute 'font-lock-builtin-face nil :foreground keyword)
(set-face-attribute 'font-lock-comment-face nil :foreground comment)
(set-face-attribute 'font-lock-constant-face nil :foreground const)
(set-face-attribute 'font-lock-doc-face nil :foreground meta)
(set-face-attribute 'font-lock-function-name-face nil :foreground func)
(set-face-attribute 'font-lock-keyword-face nil :foreground keyword)
(set-face-attribute 'font-lock-string-face nil :foreground str)
(set-face-attribute 'font-lock-type-face nil :foreground type :bold nil)
(set-face-attribute 'font-lock-variable-name-face nil :foreground var)
(set-face-attribute 'cursor nil :background cursor)
(set-face-attribute 'default nil :background bg1 :foreground base)
(set-face-attribute 'font-lock-negation-char-face nil :foreground const)
(set-face-attribute 'font-lock-preprocessor-face nil :foreground keyword)
(set-face-attribute 'font-lock-warning-face nil :foreground war :background bg1)
(set-face-attribute 'fringe nil :foreground base :background bg1)
(set-face-attribute 'header-line nil :background bg4)
(set-face-attribute 'highlight nil :foreground base :background highlight)
(set-face-attribute 'isearch nil :foreground bg1 :background mat)
(set-face-attribute 'lazy-highlight nil :foreground base :background green-bg-s)
(set-face-attribute 'link nil :foreground comment :underline t)
(set-face-attribute 'link-visited nil :foreground comp :underline t)
(set-face-attribute 'match nil :background highlight :foreground mat)
(set-face-attribute 'region nil :background highlight)
(set-face-attribute 'secondary-selection nil :background bg3)
(set-face-attribute 'shadow nil :foreground base-dim)
(set-face-attribute 'tooltip nil :background ttip-sl :foreground base :bold nil :italic nil :underline nil)
(set-face-attribute 'vertical-border nil :foreground border)
(set-face-attribute 'minibuffer-prompt nil :foreground keyword :bold t)

(set-face-attribute 'mode-line nil :background act1 :foreground base :box border)
(set-face-attribute 'mode-line-buffer-id nil :foreground func)
(set-face-attribute 'mode-line-inactive nil :background bg1 :foreground base :box border)

(set-face-attribute 'ido-first-match nil :foreground comp :bold t)
(set-face-attribute 'ido-only-match nil :foreground mat :bold t)
(set-face-attribute 'ido-subdir nil :foreground keyword)

(set-face-attribute 'popup-face nil :background ttip-bg :foreground ttip)
(set-face-attribute 'popup-scroll-bar-background-face nil :background bg2)
(set-face-attribute 'popup-scroll-bar-foreground-face nil :background act2)
(set-face-attribute 'popup-tip-face nil :background ttip-sl :foreground base :bold nil :italic nil :underline nil)

(set-face-attribute 'show-paren-match nil :foreground mat :background highlight :bold t :underline t)
(set-face-attribute 'show-paren-mismatch nil :foreground err :bold t :underline t)

; Bright-red TODOs from Casey Muratori config
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

(provide 'base16-based-color-scheme)
