
(add-to-list 'load-path "~/.emacs.d/lisp/")

(let ((basedir "~/.emacs.d/themes/"))
      (dolist (f (directory-files basedir))
        (if (and (not (or (equal f ".") (equal f "..")))
                 (file-directory-p (concat basedir f)))
            (add-to-list 'custom-theme-load-path (concat basedir f)))))

(defun my:return ()
  "Call the default bound to RET function. This is intended to be
  used only within the my-keys-minor-mode, othewise this is a
  useless function."
  (interactive)
  (my-keys-minor-mode 0)
  ;; interactively call the function bound to the return key
  (call-interactively (key-binding (kbd "RET") t))
  (my-keys-minor-mode 1))

(defun my:drop-to-new-line ()
  "Move to end of line then call default bound RET function"
  (interactive)
  (move-end-of-line 1)
  (my:return))

;; This doesn't work on the first line of a file
(defun my:rise-to-new-line ()
  "Move to end of previous line then call default bount RET
  function"
  (interactive)
  (previous-line 1)
  (my:drop-to-new-line))

(defun save-macro (name)
  "save a macro. Take a name as argument
   and save the last defined macro under
   this name at the end of your .emacs"
   (interactive "SName of the macro: ")  ; ask for the name of the macro
   (kmacro-name-last-macro name)         ; use this name for the macro
   (find-file user-init-file)            ; open ~/.emacs or other user init file
   (goto-char (point-max))               ; go to the end of the .emacs
   (newline)                             ; insert a newline
   (insert-kbd-macro name)               ; copy the macro
   (newline)                             ; insert a newline
   (switch-to-buffer nil))               ; return to the initial buffer

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun my-quote-it ()
  "Quote the current string"
  (interactive)
  (forward-char)
  (backward-sexp)
  (insert "\"")
  (forward-sexp)
  (insert "\""))

(defun my-pad (&optional n)
  (interactive "P")
  (setq i 0)
  (unless n (setq n 4))
  (setq max (current-column))
  (while (< i (- n max))
    (insert " ")
    (setq i (1+ i))))

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'my:return)                  ;; redundant
    (define-key map (kbd "C-;") 'my:drop-to-new-line)
    (define-key map (kbd "<S-return>") 'my:rise-to-new-line)
    (define-key map (kbd "C-c C-q") 'my-quote-it)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(my-keys-minor-mode 1)

(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(defun my-window-number-select (number)
  "Faster way to select windows using home row."
  (interactive "cEnter Char: ")
  (cond ((char-equal ?j number) (setq number 1))
        ((char-equal ?k number) (setq number 2))
        ((char-equal ?l number) (setq number 3))
        ((char-equal ?\; number) (setq number 4))
        ((char-equal ?a number) (setq number 5))
        ((char-equal ?s number) (setq number 6))
        ((char-equal ?d number) (setq number 7))
        ((char-equal ?f number) (setq number 8))
        ((char-equal ?g number) (setq number 9))
        ((char-equal ?h number) (setq number 0))
        (t (setq number (string-to-number (char-to-string number)))))
  (window-number-select number))

(global-set-key (kbd "C-x j") 'my-window-number-select)

(load-theme `wombat)
;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-title-format "Emacs")
(setq inhibit-splash-screen t)
(transparency 85)

(add-to-list 'org-structure-template-alist
             '("m" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))

(add-to-list 'org-drawers "FUTURE")
(add-to-list 'org-drawers "META")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-hide-leading-stars nil)
(setq org-list-allow-alphabetical t)
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

(set-face-attribute 'org-block-begin-line nil :background "#555" :foreground "#000" :underline "000")
(set-face-attribute 'org-block-end-line nil :background "#555" :foreground "#000" :overline "000")
(set-face-attribute 'org-block-background nil :background "#666")

;; (defface org-block-begin-line
;;   '((t (:underline "#FFF" :foreground "#999" :background "#050505")))
;;   "Face used for the line delimiting the begin of source blocks.")
;; (defface org-block-background
;;   '((t (:background "#AAA")))
;;   "Face used for the source block background.")
;; (defface org-block-end-line
;;   '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
;;   "Face used for the line delimiting the end of source blocks.")

;;   (require 'color)
;; 
;;   (set-face-attribute 'org-block nil :background
;;                       (color-darken-name
;;                        (face-attribute 'default :background) 3))
;; 
;;   (setq org-src-block-faces '(("emacs-lisp" (:background "#EEE2FF"))
;;                               ("python" (:background "#E5FFB8"))))

;;   (org-block-begin-line
;;     ((t (:underline "#F00" :foreground "#0F0" :background "#00F"))))
;;   (org-block-background
;;     ((t (:background "#F0F"))))
;;   (org-block-end-line
;;     ((t (:underline "#0F0" :foreground "#00F" :background "#F00"))))

(global-hl-line-mode 1)
(set-face-underline 'hl-line nil)

(ido-mode)

(global-linum-mode 1)

(column-number-mode)

(require 'paren)
(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-inverse-video 'show-paren-match t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'multiple-cursors)

(require 'magit)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(require 'ess-site)
;; Turn on auto-fill-mode with fill-column of 80 whenever ess-mode begins
;; This is in accordance with the Google R style guide
(add-hook 'ess-mode-hook (progn
                           (auto-fill-mode)
                           (setq fill-column 80)) t)

(require 'tramp)
(setq tramp-default-method "ssh")

(require 'evil-numbers)

(require 'csv-mode)

(require 'icicles)
(icy-mode 0)

(require 'yasnippet)
(yas-global-mode 1)

(require 'origami)
(add-to-list 'origami-parser-alist '(ess-mode . origami-c-style-parser))
(define-key origami-mode-map (kbd "C-c j") 'origami-forward-toggle-node)
(global-origami-mode 1)

(load "~/.emacs-Macaulay2" t)
(global-set-key (kbd "C-c C-l") 'M2-send-to-program)

(autoload 'window-number-mode
  "window-number.el"
  "A global minor mode that enables selection of windows
  according to numbers with the C-x C-j prefix.  Another mode,
  `window-number-meta-mode' enables the use of the M- prefix."
  t)

(window-number-mode)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-x g") 'magit-status)

(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(define-key global-map (kbd "M-s M-j") 'ace-jump-mode)
(define-key global-map (kbd "M-s M-d") 'ace-jump-mode)
(define-key global-map (kbd "C-S-c C-SPC") 'ace-jump-line-mode)

;; Swap M-b with M-j and C-b with C-j to give a more convenient
;; forward backward movement I'm not sure this is the best route since
;; there are times that I would like to use C-j as intended e.g. in
;; helm mode C-j is used to expand the current selection and now I'm
;; forced to use C-b to do so But I don't think I can just use
;; global-set-key to change the behavior the functions called are
;; context dependent

;; (define-key key-translation-map (kbd "C-b") (kbd "C-j"))
;; (define-key key-translation-map (kbd "C-j") (kbd "C-b"))

;; (define-key key-translation-map (kbd "M-b") (kbd "M-j"))
;; (define-key key-translation-map (kbd "M-j") (kbd "M-b"))

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-z") 'jump-to-register)
(global-set-key (kbd "C-M-z") 'window-configuration-to-register)
(global-set-key (kbd "C-S-z") 'point-to-register)

(define-key global-map (kbd "<S-iso-lefttab>") 'origami-forward-toggle-node)

(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)

(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
    (server-start))
