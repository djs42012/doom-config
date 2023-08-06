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
                 (base0           '("#131313" "#121212"   "black"      ))
                 (base1           '("#272727" "#1c1c1c" "brightblack"  ))
                 (base2           '("#3a3a3a" "#262626" "brightblack"  ))
                 (base3           '("#4d4d4d" "#303030" "brightblack"  ))
                 (base4           '("#606060" "#4e4e4e" "brightblack"  ))
                 (base5           '("#747474" "#626262" "brightblack"  ))
                 (base6           '("#878787" "#767676" "brightblack"  ))
                 (base7           '("#9a9a9a" "#949494" "brightblack"  ))
                 (base8           '("#adadad" "#dfdfdf" "white"        ))
                 (fg              '("#f4f4f4" "#ffffff" "brightwhite"  ))
                 (fg-alt          '("#d4d4d4" "#d0d0d0" "brightwhite"  ))


                 (red             '("#c16b6b" "#af5f5f" "red"          ))
                 (orange          '("#d8ae5f" "#d7af5f" "brightred"    ))
                 (green           '("#93e079" "#87d787" "brightgreen"  ))
                 (dark-green      '("#00d100" "#00d700" "brightgreen"  ))
                 (teal            '("#6ac2c8" "#5fafd7" "brightgreen"  ))
                 (yellow          '("#dcdcaa" "#d7d7af" "yellow"       ))
                 (bright-yellow   '("#fdb900" "#ffaf00" "yellow"       ))
                 (bright-ylw-alt  '("#d7d700" "#d7d700" "brightyellow" ))
                 (blue            '("#96CBFE" "#87d7ff" "brightblue"   ))
                 (magenta         '("#d4a5d0" "#d7afd7" "magenta"      ))
                 (bright-magenta  '("#d670d6" "#d75fd7" "magenta"      ))
                 (violet          '("#c5a8ff" "#d7afff" "purple"       ))
                 (dark-magenta    '("#794183" "#d7afff" "purple"       ))
                 (cyan            '("#88bae2" "#87afd7" "brightcyan"   ))
                 (dark-cyan       '("#85bcd6" "#5fafd7" "brightcyan"   ))
                 (dark-blue       '("#0054a5" "#080808" "brightcyan"   ))
                 (light-cyan      '("#9cdcfe" "#afd7ff" "brightcyan"   ))
                 (turquoise       '("#00d7ff" "#0087ff" "brightblue"   ))
                 (grey            '("#b9b1b6" "#b2b2b2" "brightblack"  ))
                 (dark-grey       '("#363636" "#3a3a3a" "brightblack"  ))

                 ;; face categories -- required for all themes
                 (highlight      dark-grey)
                 (selection      blue)
                 (builtin        yellow)
                 (comments       red)
                 (vertical-bar   base2)
                 (doc-comments   red)
                 (constants      cyan)
                 (functions      yellow)
                 (keywords       magenta)
                 (methods        yellow)
                 (operators      green)
                 (type           dark-green)
                 (strings        light-cyan)
                 (variables      cyan)
                 (numbers        bright-yellow)
                 (region         dark-grey)
                 (error          red)
                 (warning        bright-yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red))


                ;; Base theme face overrides
                (((hl-line &override) :background base1)
                 ((line-number &override) :foreground base5)
                 ((link &override) :foreground blue)
                 (show-paren-match :background dark-magenta  :foreground fg)
                 (cursor :foreground bg :background fg)
                 (lazy-highlight :background magenta :foreground dark-blue )
                 (( evil-ex-search &override) :background dark-blue  :foreground fg)
                 (isearch-group-1 :background dark-magenta :foreground fg )
                 (iedit-occurrence :background dark-magenta :foreground fg )
                 (isearch :background dark-blue :foreground fg )
                 (isearch-group-2 :background dark-blue :foreground fg )
                 (evil-ex-lazy-highlight :background dark-magenta :foreground fg)
                 (( line-number-current-line &override) :foreground bright-yellow :background base0)
                 (tooltip :background base1 :foreground fg )
                 ((secondary-selection &override) :background base1)
                 ((highlight-quoted-symbol &override) :foreground yellow)
                 ((font-lock-preprocessor-face &override) :inherit 'bold :foreground magenta)
                 ((help-key-binding &override) :foreground magenta :background bg :)
                 ((minibuffer-prompt &override) :foreground blue)
                 ((vertical-border &override) :foreground violet :background violet)
                 ((pulse-highlight-face &override) :background base0 :foreground magenta)
                 ((pulse-highlight-start-face &override) :background base0 :foreground magenta)

                 ;; icons
                 ( all-the-icons-completion-dir-face :foreground light-cyan )

                 ;; modeline
                 (mode-line
                  :background bg :foreground fg
                  :box `(:line-width -1 :color ,fg))
                 (mode-line-inactive
                  :background bg :foreground grey
                  )
                 (mode-line-emphasis :foreground fg)
                 (doom-modeline-bar :background bg)

                 ;; calendar
                 ((calendar-month-header &override) :foreground magenta :weight 'bold)
                 ((calendar-weekday-header &override) :foreground blue :weight 'bold)
                 ((calendar-weekend-header &override) :foreground red :weight 'bold)
                 ((cfw:face-grid &override) :foreground fg)
                 ((cfw:face-title &override) :foreground magenta)
                 ((cfw:face-today-title &override) :background green)
                 ((cfw:face-toolbar-button-on &override) :foreground bright-yellow)
                 ((cfw:face-toolbar &override) :foreground bg :background bg)
                 ((cfw:face-saturday &override) :foreground red)
                 ((cfw:face-sunday &override) :foreground red)
                 ((org-date-selected &override) :background region :foreground green :weight 'bold :height 1.2)

                 ;; centaur-tabs
                 ((centaur-tabs-selected &override) :background base0 :underline green :foreground fg :weight 'bold)
                 ((centaur-tabs-unselected &override) :background (doom-darken base0 0.25) :foreground fg )
                 ((centaur-tabs-active-bar-face &override) :background green)
                 ((centaur-tabs-selected-modified &override) :foreground orange :background base3 :weight 'bold)
                 ((centaur-tabs-unselected-modified &override) :foreground orange :background base0)
                 ((centaur-tabs-modified-marker-selected &override) :foreground orange :background base3 :weight 'bold)
                 ((centaur-tabs-modified-marker-unselected &override) :foreground orange :background base0)

                 ;; company
                 ((company-tooltip-common &override) :weight 'normal :foreground green)
                 ((company-tooltip-selection &override) :background base1 )
                 ((company-scrollbar-fg &override)   :background red)
                 ((company-tooltip-annotation &override) :foreground violet :distant-foreground bg)
                 ((company-tooltip &override) :background (doom-darken base0 0.50))
                 (company-tooltip-scrollbar-thumb :background red)
                 (company-tooltip-scrollbar-track :background base0)

                 ;; company-box
                 (company-box-scrollbar  :background red)

                 ;; css-mode / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)

                 ;; dired/dirvish
                 ((diredfl-dir-name &override) :foreground blue)
                 ((dirvish-hl-line &override) :background base1)

                 ;; Doom
                 ((doom-dashboard-menu-title &override) :foreground fg :slant 'italic)
                 ((doom-dashboard-menu-desc &override) :foreground magenta)
                 ((doom-dashboard-footer-icon &override) :foreground red)
                 ((doom-themes-org-hash-tag &override) :foreground green :box nil)

                 ;; ediff
                 (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
                 (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
                 (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
                 (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))

                 ;; evil snipe
                 ((evil-snipe-first-match-face &override)  :background dark-blue :foreground fg )
                 ((evil-snipe-matches-face &override)  :background dark-magenta :foreground fg)


                 ;; embark
                 ((embark-keybinding &override) :foreground magenta)


                 ;; fringe
                 (fringe :foreground fg)

                 ;;flycheck
                 (flycheck-posframe-face :background base0)

                 ;; gnus
                 ((gnus-header-content &override) :foreground green)
                 ((gnus-header-subject &override) :foreground magenta)
                 ((gnus-header-from &override) :foreground violet)
                 ((gnus-header-name &override) :foreground cyan)
                 ((gnus-button &override) :underline t)
                 ((gnus-cite-1 &override) :foreground blue)
                 ((shr-link &override) :foreground cyan :underline t)

                 ;; helm
                 (helm-candidate-number :background blue :foreground red)

                 ;; indent guides
                 ;; TODO Figure out why this only sometimes works
                 ((highlight-indent-guides-character-face &override) :foreground red)
                 (highlight-indent-guides-character-face :background bg :foreground base0)
                 (highlight-indent-guides-stack-character-face  :background bg :foreground base0)
                 (highlight-indent-guides-odd-face  :background bg :foreground base0)
                 (highlight-indent-guides-even-face  :background bg :foreground base0)
                 (highlight-indent-guides-top-odd-face  :background bg :foreground base0)
                 (highlight-indent-guides-top-even-face  :background bg :foreground base0)
                 (highlight-indent-guides-stack-odd-face  :background bg :foreground base0)
                 (highlight-indent-guides-stack-even-face  :background bg :foreground base0)
                 (highlight-indent-guides-top-character-face  :background bg :foreground base0)


                 ;; Info-mode
                 ((info-xref &override) :inherit 'link :foreground blue)
                 ((info-xref-visited &override) :inherit '(info-xref link-visited)  :foreground fg-alt)
                 ((info-menu-star &override) :foreground fg)
                 ((info-menu-header &override) :inherit 'variable-pitch :weight 'bold :foreground blue)
                 ((info-title-4 &override) :inherit 'variable-pitch :weight 'bold :foreground magenta )
                 ((info-title-3 &override) :inherit 'info-title-4  )
                 ((info-title-2 &override) :inherit 'info-title-3  )
                 ((info-title-1 &override) :inherit 'info-title-2  )
                 ((Info-quoted &override)  :inherit 'fixed-pitch :foreground green :background base0)

                 ;; ivy
                 ((ivy-minibuffer-match-face-1 &override) :foreground (doom-lighten base5 0.70))
                 (ivy-posframe               :background base0)

                 ;; js2-mode
                 ((js2-object-property-access &override) :foreground fg)
                 ((js2-object-property &override) :foreground cyan)

                 ;;;; lsp-mode
                 (lsp-ui-doc-background      :background base0)
                 (lsp-face-highlight-read :background dark-blue :foreground fg :underline t)
                 (lsp-face-highlight-textual :background dark-blue :foreground fg )
                 (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)


                 ;; magit
                 ((git-commit-summary &override) :foreground green)
                 ((magit-diff-file-heading &override) :foreground magenta :bg)
                 ((magit-diff-file-heading-highlight &override) :foreground magenta    :background base0  :weight 'bold)
                 ((magit-diff-hunk-heading &override) :foreground orange    :background bg)

                 ((magit-diff-hunk-heading-highlight &override) :foreground orange    :background (doom-blend bright-yellow base0 0.35) :weight 'bold)
                 ((magit-header-line &override) :foreground magenta :background bg  :box `(:line-width -1 :color ,fg))
                 ((magit-section-highlight &override) :background base0)
                 ;;((magit-diff-context-highlight &override) :background base0)
                 (magit-blame-heading     :foreground orange :background bg-alt)
                 (magit-diff-removed :foreground (doom-darken red 0.5) :background bg)
                 (magit-diff-removed-highlight :foreground red :background (doom-darken red 0.8))
                 (magit-diff-added :foreground (doom-darken green 0.5) :background bg)
                 (magit-diff-added-highlight :foreground green :background (doom-darken green 0.8) )
                 (diff-added  :forground green :background (doom-darken green 0.40))
                 (diff-removed  :forground red :background (doom-darken red 0.40))
                 (diff-refine-added  :inherit 'diff-added :foreground fg)
                 (diff-refine-removed  :inherit 'diff-removed :foreground fg)
                 (git-commit-nonempty-second-line :foreground fg :slant 'italic)

                 ;; Man
                 (Man-overstrike :inherit 'outline-1)
                 ((Man-underline &override) :foreground blue)

                 ;; markdown-mode
                 (markdown-markup-face     :foreground fg)
                 (markdown-link-face     :inherit 'org-link)
                 (markdown-url-face     :inherit 'org-link :foreground cyan)
                 (markdown-list-face     :foreground fg-alt)
                 ((markdown-strikethrough-face &override)     :foreground fg-alt)
                 (markdown-reference-face     :inherit 'org-link)
                 (markdown-html-attr-name-face     :inherit 'web-mode-html-attr-name-face)
                 (markdown-html-attr-value-face     :inherit 'web-mode-html-attr-value-face)
                 (markdown-html-tag-delimiter-face     :inherit 'web-mode-html-tag-bracket-face)
                 (markdown-header-face     :inherit 'bold :foreground red)
                 ((markdown-code-face &override)       :background base1)
                 (markdown-header-face :foreground cyan :weight 'bold)
                 ((markdown-header-face-1  &override) :foreground cyan   :weight 'bold)
                 ((markdown-header-face-2  &override) :foreground yellow  :weight 'bold)
                 ((markdown-header-face-3  &override) :foreground magenta  :weight 'bold)
                 ((markdown-header-face-4  &override) :foreground (doom-lighten green 0.25)    :weight 'bold)
                 ((markdown-header-face-5  &override) :foreground (doom-lighten violet 0.25)      :weight 'bold)
                 ((markdown-header-face-6  &override) :foreground (doom-lighten cyan 0.25)    :weight 'bold)
                 (markdown-bold-face :foreground magenta :weight 'bold)
                 (markdown-italic-face :foreground red :slant 'italic)
                 (markdown-header-face :foreground magenta :weight 'bold)
                 (markdown-inline-code-face :foreground bright-yellow)
                 (mmm-default-submode-face :background base0)

                 ;; mu4e
                 (mu4e-highlight-face :background bg :inherit 'bold)
                 ((mu4e-header-highlight-face &override) :inherit 'bold :underline nil :background base1)
                 ((mu4e-flagged-face &override) :foreground bright-yellow)
                 ((mu4e-replied-face &override) :foreground green)
                 (mu4e-unread-face :foreground cyan :inherit 'bold)
                 (mu4e-header-key-face :foreground fg)
                 (mu4e-header-face :foreground fg-alt)
                 (mu4e-related-face :foreground fg-alt)
                 ((mu4e-trashed-face &override) :inherit 'org-done :strike-through 't)
                 (mu4e-header-title-face :foreground cyan)
                 (mu4e-header-title-key :foreground fg)
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
                 ((org-done &override) :strike-through t)
                 ((org-headline-done &override) :strike-through t)
                 ((org-tag &override)   :foreground green :background nil
                  :box `(:line-width -1 :color ,green :style 'released-button))
                 ((org-date &override)  :foreground bright-yellow :background nil)
                 ((org-special-keyword &override)  :foreground fg-alt)
                 ((org-drawer          &override)  :foreground base5)
                 (org-ellipsis :underline nil :box nil :foreground fg-alt :background bg )
                 (org-link :inherit 'link :foreground blue)
                 ((org-block &override) :background bg)
                 ((org-block-begin-line &override) :overline t :foreground bright-yellow :slant 'italic :height 1.0 )
                 ((org-block-end-line &override) :overline nil :underline t :foreground bright-yellow :slant 'italic :height 1.0 )
                 ((org-quote &override) :background bg :extend t)
                 ((org-table &override) :foreground fg)
                 ((org-list-dt &override) :foreground blue)
                 (org-agenda-structure :inherit `outline-1)
                 (org-upcoming-deadline         :foreground base8)
                 (org-upcoming-distant-deadline :foreground fg)
                 (org-scheduled                 :foreground fg)
                 (org-scheduled-today           :foreground fg)
                 (org-scheduled-previously      :foreground base8)
                 ((org-priority &override) :foreground red)
                 ((org-agenda-date &override) :foreground blue )
                 ((org-agenda-date-today &override) :foreground green :slant 'italic )
                 ((org-agenda-date-weekend &override) :foreground blue)
                 ((org-time-grid &override) :foreground fg)
                 ((org-agenda-current-time &override) :foreground bright-yellow)
                 ((org-verbatim &override) :foreground green :background base0)
                 ((org-code &override) :foreground bright-yellow)

                 ;; org-modern
                 (org-modern-label :inherit 'org-todo :foreground bright-yellow :weight 'bold)
                 (org-modern-todo :inherit 'org-modern-label)
                 (org-modern-done  :inherit 'org-done)
                 (org-modern-tag  :inherit 'org-tag   :box nil)
                 (org-modern-bracket-line  :foreground bright-yellow :height 1.0)
                 (org-modern-time-active :foreground bright-yellow :background nil)
                 (org-modern-time-inactive :foreground fg-alt :background nil)
                 ((org-modern-date-active &override)  :foreground bright-yellow :background nil)
                 ((org-modern-date-inactive &override)  :foreground fg-alt :background nil)
                 ((org-modern-priority &override) :background nil)
                 ((org-modern-block-name &override) :foreground bright-yellow)
                 (org-modern-statistics  :foreground nil)

                 ;; outline
                 ((outline-1  &override) :foreground cyan   :weight 'bold)
                 ((outline-2  &override) :foreground yellow  :weight 'bold)
                 ((outline-3  &override) :foreground magenta  :weight 'bold)
                 ((outline-4  &override) :foreground (doom-lighten green 0.25)    :weight 'bold)
                 ((outline-5  &override) :foreground (doom-lighten violet 0.25)      :weight 'bold)
                 ((outline-6  &override) :foreground (doom-lighten cyan 0.25)    :weight 'bold)
                 ((outline-7  &override) :foreground (doom-lighten yellow 0.25)    :weight 'bold)
                 ((outline-8  &override) :foreground (doom-lighten magenta 0.25)      :weight 'bold)

                 ;;pulse-highlight
                 (pulse-highlight-face :background (doom-blend magenta base0 0.05) :foreground magenta)
                 (pulse-highlight-start-face :background(doom-blend magenta base0 0.05) :foreground magenta)

                 ;; rainbow-delimiters
                 ((rainbow-delimiters-depth-1-face &override) :foreground bright-ylw-alt)
                 ((rainbow-delimiters-depth-2-face &override) :foreground magenta)
                 ((rainbow-delimiters-depth-3-face &override) :foreground turquoise)
                 ((rainbow-delimiters-depth-4-face &override) :foreground green)

                 ;; rjsx mode
                 (rjsx-text :foreground fg :slant 'italic)
                 (rjsx-tag :foreground green)
                 (rjsx-tag-bracket-face :foreground (doom-blend green grey 0.25))

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

                 ;; tree-sitter
                 ((tree-sitter-hl-face:operator &override) :foreground green)
                 ((tree-sitter-hl-face:tag &override) :foreground green)
                 ((tree-sitter-hl-face:attribute &override) :foreground yellow :weight 'bold)
                 ((tree-sitter-hl-face:number &override) :foreground bright-yellow :weight 'bold)
                 (( tree-sitter-hl-face:type.builtin &override) :foreground fg :slant 'italic)
                 (( tree-sitter-hl-face:variable.builtin &override) :foreground light-cyan :weight 'bold)
                 (( tree-sitter-hl-face:function.call &override) :foreground yellow :weight 'bold)

                 ;; ((tree-sitter-hl-face:punctuation.delimiter &override) :foreground dark-green)

                 ;; tsx
                 ((typescript-ts-jsx-tag-face &override) :foreground green)
                 ((typescript-ts-jsx-attribute-face &override) :foreground yellow)

                 ((treemacs-root-face &override)  :foreground magenta :weight 'bold )
                 ((treemacs-file-face &override)  :foreground fg :weight 'bold)
                 ((treemacs-git-unmodified-face &override)  :foreground fg :weight 'bold)
                 ((treemacs-git-modified-face &override)  :foreground orange :weight 'bold)
                 ((treemacs-git-ignored-face &override)  :foreground fg-alt :weight 'bold)
                 ((treemacs-git-conflict-face &override)  :foreground bright-yellow :weight 'bold)
                 ((treemacs-git-renamed-face &override)  :foreground blue :weight 'bold)
                 ((treemacs-git-added-face &override)  :foreground green :weight 'bold)
                 ((treemacs-directory-face &override)  :foreground fg :weight 'bold)
                 ((doom-themes-treemacs-file-face &override) :inherit 'font-lock-doc-face :foreground blue :weight 'bold )
                 ((doom-themes-treemacs-root-face &override) :inherit 'font-lock-doc-face :foreground magenta :weight 'bold )

                 ;;vertico
                 (vertico-current :foreground fg :background base1 :weight 'bold)

                 ;; vc-state
                 ((vc-edited-state &override) :foreground orange)
                 ((vc-conflict-state &override) :foreground red)

                 ;; web-mode
                 (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
                 (web-mode-html-attr-name-face :foreground yellow)
                 (web-mode-html-tag-face :foreground green)
                 (web-mode-html-tag-bracket-face :foreground fg)
                 (web-mode-folded-face :background base0)

                 ;; wgrep
                 (wgrep-face :background base1)
                 ;; which-key
                 ((which-key-key-face &override) :inherit 'bold)

                 ))
     ;;;; Base theme variable overrides-
;;; doom-bisqwit-theme.el ends here
