;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "DJS"
      user-mail-address "code@djs.gg")
;; Start in Fullscreen Mode
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use

;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-bisqwit-improved
      doom-font (font-spec :family "JetBrainsMono" :size 16 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 16))
;;(setq doom-font (font-spec :family "More Perfect DOS VGA" :size 16 :weight 'medium))
;;(setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;(doom-themes-treemacs-config)
(setq rainbow-delimiters-max-face-count 3)
(defface heavy-punctuation-face '((t (:foreground "#008000")))
  "Used for extra emphasis on customizable symbols.")
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("[;:,.#]" . 'heavy-punctuation-face))))
      '(emacs-lisp-mode c-mode rjsx-mode typescript-mode))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/Org/")
(setq projectile-project-search-path '("~/Sync/Code/" "~/Code/" "~/Desktop/"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Install lsp servers
(after! lsp-mode
  (lsp-ensure-server 'ts-ls)
  (lsp-ensure-server 'bash-ls)
  (lsp-ensure-server 'emmet-ls)
  (lsp-ensure-server 'html-ls)
  (lsp-ensure-server 'dockerfile-ls)
  (lsp-ensure-server 'svelte-ls)
  (lsp-ensure-server 'yamlls)
  (lsp-ensure-server 'json-ls)
  (lsp-ensure-server 'eslint)
  (lsp-ensure-server 'css-ls)
  (lsp-ensure-server 'vimls)
  (lsp-ensure-server 'clangd)
  (lsp-ensure-server 'mspyls))
;;disable lsp auto formatting
(setq +format-with-lsp nil)
;;disable doom treemacs themes
;; (after! doom-themes
;;   (remove-hook 'doom-load-theme-hook #'doom-themes-treemacs-config))
;;treemacs custom color schemes
(setq doom-themes-treemacs-theme 'doom-colors)
;;change default treemacs color schemes
;; (defface custom-line-highlight '((t (:background "#121212" :foreground "#d4d4d4" :extend t))) "")
;; (add-hook
;;  'treemacs-mode-hook
;;  (defun change-hl-line-mode ()
;;    (setq-local hl-line-face 'custom-line-highlight)
;;    (overlay-put hl-line-overlay 'face hl-line-face)
;;    (treemacs--setup-icon-background-colors)))
;;(setq treemacs-window-background-color '("black"))
;;change treemacs git mode to extended
(setq +treemacs-git-mode 'extended)
;;set treemacs follow mode
;;(treemacs-follow-mode 'toggle)
;;permanently display workspace
(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (face-remap-add-relative '+workspace-tab-selected-face '(:background "#000000"  :foreground "#a9a1e1"))
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))
;;(after! format
;;  (setq +format-on-save-enabled-modes
;;        '(not
;;          ;;emacs-lisp-mode  ; elisp's mechanisms are good enough
;;          sql-mode         ; sqlformat is currently broken
;;          tex-mode         ; latexindent is broken
;;          latex-mode
;;          org-msg-edit-mode)))

;;window management
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  ;;(dired-jump)
  ;;(find-file '\.')
  (consult-buffer))
;;reduce delay time on which-key popups
(setq which-key-idle-delay .5)
;;configure company
(after! company
  (setq company-idle-delay 1
        company-minimum-prefix-length 0)
  (setq company-show-quick-access t))
;;choose extensions to open in web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . web-mode))
;;fix lsp in web-mode for scss
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(web-mode . "scss")))
;;set visual line mode globally
(global-visual-line-mode)
(after! evil
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))
;;
;;tell which-key to behave
(setq which-key-use-C-h-commands t
      prefix-help-command #'which-key-C-h-dispatch)

(defadvice! fix-which-key-dispatcher-a (fn &rest args)
  :around #'which-key-C-h-dispatch
  (let ((keys (this-command-keys-vector)))
    (if (equal (elt keys (1- (length keys))) ?\?)
        (let ((keys (which-key--this-command-keys)))
          (embark-bindings (seq-take keys (1- (length keys)))))
      (apply fn args))))
;;turn off words in rainbow mode

(add-hook 'rainbow-mode-hook
          (defun rainbow-turn-off-words ()
            "Turn off word colours in rainbow-mode."
            (interactive)
            (font-lock-remove-keywords
             nil
             `(,@rainbow-x-colors-font-lock-keywords
               ,@rainbow-latex-rgb-colors-font-lock-keywords
               ,@rainbow-r-colors-font-lock-keywords
               ,@rainbow-html-colors-font-lock-keywords
               ,@rainbow-html-rgb-colors-font-lock-keywords))))
