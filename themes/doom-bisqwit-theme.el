;;; doom-bisqwit-theme.el --- A supremely dark theme inspired by Bisqwit's That editor -*- lexical-binding: t; no-byte-compile: t; -*-
;;;
;;; Commentary:
;;;
;;; This dark theme is inspired by Bisqwit's that_editor
;;; The base color palette is taken from Yegiyan's VS Code theme
;;; Pitch-black defaults are adapted from mskorzhinskiy's doom-homage-black
;;;
;;; Source Material:
;;; [1] https://www.youtube.com/c/Bisqwit.
;;; [2] https://github.com/bisqwit/that_editor
;;; [3] https://github.com/Yegiyan/Rabbit-Terminal-Theme-for-Visual-Studio
;;; [4] https://github.com/mskorzhinskiy

(require 'doom-themes)

;;
(defgroup doom-bisqwit-theme nil
  "Options for the `doom-bisqwit' theme."
  :group 'doom-themes)

(defcustom doom-bisqwit-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-bisqwit-theme
  :type '(choice integer boolean))
;;
(def-doom-theme doom-bisqwit
  "A supremely dark theme inspired by Bisqwit's That editor"

  ;; name        default   256       16
  ((bg              '("#000000" nil       nil            ))
   (bg-alt          '("#000000" nil       nil            ))
   (base0           '("#121212" "#121212"   "black"      ))
   (base1           '("#1c1f24" "#1c1c1c" "brightblack"  ))
   (base2           '("#202328" "#262626" "brightblack"  ))
   (base3           '("#23272e" "#303030" "brightblack"  ))
   (base4           '("#3f444a" "#4e4e4e" "brightblack"  ))
   (base5           '("#5B6268" "#626262" "brightblack"  ))
   (base6           '("#73797e" "#767676" "brightblack"  ))
   (base7           '("#8f8f8e" "#949494" "brightblack"  ))
   (base8           '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg              '("#d4d4d4" "#d0d0d0" "brightwhite"  ))
   (fg-alt          '("#a9a5aa" "#afafaf" "brightwhite"  ))
   (red             '("#b95959" "#d75f5f" "red"          ))
   (orange          '("#cd9731" "#d7af5f" "brightred"    ))
   (green           '("#93e079" "#87d787" "brightgreen"  ))
   (dark-green      '("#008000" "#008000" "brightgreen"  ))
   (teal            '("#4db5bd" "#5fafaf" "brightgreen"  ))
   (yellow          '("#dcdcaa" "#d7d7af" "yellow"       ))
   (bright-yellow   '("#fdb900" "#ffaf00" "yellow"       ))
   (blue            '("#96CBFE" "#87d7ff" "brightblue"   ))
   (dark-blue       '("#2257A0" "#005faf" "blue"         ))
   (magenta         '("#c586c0" "#d787af" "magenta"      ))
   (violet          '("#915bff" "#875fff" "purple"       ))
   (cyan            '("#569cd6" "#5fafd7" "brightcyan"   ))
   (dark-cyan       '("#3d90b6" "#5f87af" "brightcyan"   ))
   (light-cyan      '("#9cdcfe" "#afd7ff" "brightcyan"   ))
   (turquoise       '("#00d7ff" "#00d7ff" "brightblue"   ))
   (grey            '("#5B6268" "#525252" "brightblack"  ))

   ;; face categories -- required for all themes
   (highlight      fg-alt)
   (selection      blue)
   (builtin        yellow)
   (comments       red)
   (vertical-bar   (doom-darken base2 0.1))
   (doc-comments   (doom-darken comments 0.15))
   (constants      cyan)
   (functions      yellow)
   (keywords       magenta)
   (methods        yellow)
   (operators      green)
   (type           dark-green)
   (strings        cyan)
   (variables      fg-alt)
   (numbers        violet)
   (region         `(,(doom-lighten (car base0) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        bright-yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red))


  ;; Base theme face overrides
  (((hl-line &override) :background base0)
   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground bright-yellow :weight 'bold)
   (tooltip :background bg :foreground fg )
   ((secondary-selection &override) :background base0)
   ((highlight-quoted-symbol &override) :foreground yellow)
   ((font-lock-preprocessor-face &override) :inherit 'bold :foreground magenta)
   ((minibuffer-prompt &override) :foreground blue)

   ;; modeline
   (mode-line
    :background bg :foreground fg
    :box `(:line-width -1 :color ,fg))
   (mode-line-inactive
    :background bg :foreground fg
    :box `(:line-width -1 :color ,violet))
   (mode-line-emphasis :foreground fg)
   (doom-modeline-bar :background bg)

   ;; calendar
   ((calendar-month-header &override) :foreground violet :weight 'bold)
   ((calendar-weekday-header &override) :foreground blue :weight 'bold)
   ((calendar-weekend-header &override) :foreground red :weight 'bold)
   ((cfw:face-grid &override) :foreground base0)
   ((cfw:face-title &override) :foreground violet)
   ((cfw:face-today-title &override) :background green)
   ((cfw:face-toolbar-button-on &override) :foreground bright-yellow)
   ((cfw:face-saturday &override) :foreground red)
   ((cfw:face-sunday &override) :foreground red)
   ((org-date-selected &override) :background highlight :foreground bg :weight 'bold)

   ;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)

   ;; company
   ((company-tooltip-common &override) :foreground blue :weight 'bold)
   ((company-tooltip-selection &override) :background base2 )
   ((company-scrollbar-fg &override)   :background red)
   ((company-tooltip-annotation &override) :foreground violet :distant-foreground bg)
   ((company-tooltip-scrollbar-thumb &override) :foreground red)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired/dirvish
   ((diredfl-dir-name &override) :foreground green)

   ;; Doom dashboard
   ((doom-dashboard-menu-title &override) :foreground fg :slant 'italic)
   ((doom-dashboard-menu-desc &override) :foreground violet)
   ((doom-dashboard-footer-icon &override) :foreground red)

   ;; ediff
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))

   ;; embark
   ((embark-keybinding &override) :foreground magenta)

   ;; helm
   (helm-candidate-number :background blue :foreground bg)

   ;; Info-mode
   ((info-xref &override) :inherit 'link :foreground blue)
   ((info-xref-visited &override) :inherit '(info-xref link-visited)  :foreground blue)
   ((info-menu-star &override) :foreground fg)
   ((info-menu-header &override) :inherit 'variable-pitch :weight 'bold :foreground blue)
   ((info-title-4 &override) :inherit 'variable-pitch :weight 'bold :foreground violet :height 1.3)
   ((info-title-3 &override) :inherit 'info-title-4  :height 1.3)
   ((info-title-2 &override) :inherit 'info-title-3  :height 1.3)
   ((info-title-1 &override) :inherit 'info-title-2  :height 1.4)
   ((Info-quoted &override) :inherit 'fixed-pitch-serif :foreground bright-yellow :background bg)

   ;; ivy
   ((ivy-minibuffer-match-face-1 &override) :foreground (doom-lighten base5 0.70))
   (ivy-posframe               :background base0)

   ;; js2-mode
   ((js2-object-property-access &override) :foreground fg)
   ((js2-object-property &override) :foreground light-cyan)

   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)

   ;; magit
   ((git-commit-summary &override) :foreground green)
   ((magit-diff-file-heading           &override) :foreground violet    :background bg-alt :bold bold)
   ((magit-diff-hunk-heading           &override) :foreground orange    :background bg-alt :bold bold)
   ((magit-diff-hunk-heading-highlight &override) :foreground orange :background bg-alt :bold bold)
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.5) :background bg)
   (magit-diff-removed-highlight :foreground red :background bg)
   (magit-diff-added :foreground (doom-darken green 0.5) :background bg)
   (magit-diff-added-highlight :foreground green :background bg)
   (diff-added  :forground green :background (doom-blend green bg 0.35))
   (diff-removed  :forground red :background (doom-blend red bg 0.35))
   (diff-refine-added  :inherit 'diff-added)
   (diff-refine-removed  :inherit 'diff-removed)

   ;; Man
   ((Man-overstrike &override) :inherit 'bold  :height 1.05 :foreground violet)

   ;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)

   ;; mu4e
   (mu4e-highlight-face :background bg :inherit 'bold)
   (mu4e-unread-face :foreground violet :inherit 'bold)
   (mu4e-header-face :foreground fg)
   (mu4e-header-title-face :foreground magenta)
   ((mu4e-title-face &override) :inherit 'outline-1)
   (message-header-name :foreground blue)

   ;; nav-flash
   ((nav-flash-face &override) :foreground fg :background region :weight 'bold)

   ;; orderless-match
   (orderless-match-face-0 :foreground  blue :weight 'bold)
   (orderless-match-face-1 :foreground  green :weight 'bold)
   (orderless-match-face-2 :foreground  yellow :weight 'bold )
   (orderless-match-face-3 :foreground  blue :weight 'bold )

   ;; org
   ((org-todo &override) :foreground bright-yellow)
   ((org-tag &override)   :foreground green :background nil
    :box `(:line-width -1 :color ,green :style 'released-button))
   ((org-date &override)  :foreground bright-yellow :background nil)
   ((org-special-keyword &override)  :foreground fg-alt)
   ((org-drawer          &override)  :foreground base5)
   (org-ellipsis :underline nil :box nil :foreground fg :background bg)
   (org-link :inherit 'link :foreground blue)
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   ((org-quote &override) :background base0)
   ((org-table &override) :foreground fg)
   (org-agenda-structure :inherit `outline-1)
   (org-upcoming-deadline         :foreground base8)
   (org-upcoming-distant-deadline :foreground fg)
   (org-scheduled                 :foreground fg)
   (org-scheduled-today           :foreground fg)
   (org-scheduled-previously      :foreground base8)
   ((org-agenda-date &override) :foreground blue)
   ((org-agenda-date-today &override) :foreground green)
   ((org-agenda-date-weekend &override) :foreground blue)

   ;; outline
   ((outline-1  &override) :foreground violet  :height 1.4 :weight 'bold :extend t)
   ((outline-2  &override) :foreground blue :height 1.3 :weight 'bold :extend t)
   ((outline-3  &override) :foreground yellow :height 1.2 :weight 'bold :extend t)
   ((outline-4  &override) :foreground (doom-lighten violet 0.25)    :weight 'bold :extend t)
   ((outline-5  &override) :foreground (doom-lighten blue 0.25)      :weight 'bold :extend t)
   ((outline-6  &override) :foreground (doom-lighten yellow 0.25)    :weight 'bold :extend t)
   ((outline-7  &override) :foreground (doom-lighten violet 0.25)    :weight 'bold :extend t)
   ((outline-8  &override) :foreground (doom-lighten blue 0.25)      :weight 'bold :extend t)

   ;; rainbow-delimiters
   ((rainbow-delimiters-depth-1-face &override) :foreground bright-yellow)
   ((rainbow-delimiters-depth-2-face &override) :foreground magenta)
   ((rainbow-delimiters-depth-3-face &override) :foreground turquoise)
   ((rainbow-delimiters-depth-4-face &override) :foreground green)

   ;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background bg
    :box bg )
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background bg
    :box bg)

   ;; swiper
   ((swiper-match-face-1 &override) :background fg        :foreground bg)
   ((swiper-line-face    &override) :background dark-blue :foreground fg)

   ;; treemacs
   ((treemacs-root-face &override) :inherit 'variable-pitch :foreground violet :weight 'bold :height 1.3)
   ((treemacs-file-face &override) :inherit 'variable-pitch :foreground fg :weight 'bold)
   ((treemacs-git-unmodified-face &override) :inherit 'variable-pitch :foreground fg :weight 'bold)
   ((treemacs-git-modified-face &override) :inherit 'variable-pitch :foreground orange :weight 'bold)
   ((treemacs-git-ignored-face &override) :inherit 'variable-pitch :foreground fg-alt :weight 'bold)
   ((treemacs-git-conflict-face &override) :inherit 'variable-pitch :foreground bright-yellow :weight 'bold)
   ((treemacs-git-renamed-face &override) :inherit 'variable-pitch :foreground blue :weight 'bold)
   ((treemacs-git-added-face &override) :inherit 'variable-pitch :foreground green :weight 'bold)
   ((treemacs-directory-face &override) :inherit 'variable-pitch :foreground fg :weight 'bold)
   ((doom-themes-treemacs-file-face &override) :inherit 'font-lock-doc-face :foreground blue :weight 'bold :height 1.2)
   ((doom-themes-treemacs-root-face &override) :inherit 'font-lock-doc-face :foreground violet :weight 'bold :height 1.3)

   ;; vc-state
   ((vc-edited-state &override) :foreground orange)
   ((vc-conflict-state &override) :foreground red)

   ;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   (web-mode-html-attr-name-face :foreground yellow)
   (web-mode-html-tag-face :foreground magenta)
   (web-mode-html-tag-bracket-face :foreground dark-green)

   ;; wgrep
   (wgrep-face :background base1)))
  ;;;; Base theme variable overrides-
;;; doom-bisqwit-theme.el ends here
