;;; doom-tibetan-theme.el --- A vibrant theme inspired by the colorful culture of Tibet.
;;; lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Tibetan flag colors taken from: https://www.schemecolor.com/flag-of-tibet-colors.php
;;; Other colors chosen using: https://app.contrast-finder.org
;;; ;;; Base colors blended using: https://meyerweb.com/eric/tools/color-blend/
;;; x11 colors found using: https://github.com/jabbalaci/ClosestX11Color/blob/master/README.md
;;; ANSI colors founds using: https://jeffkreeftmeijer.com/vim-16-color/
(require 'doom-themes)

;;
(defgroup doom-tibetan-theme nil
  "Options for the `doom-tibetan' theme."
  :group 'doom-themes)

(defcustom doom-tibetan-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-tibetan-theme
  :type '(choice integer boolean))
;;
(def-doom-theme doom-tibetan
  "A vibarant theme inspired by the colorful culture of tibet"

  ;; name        default   256       16
  ((bg              '("#240d70" "#000080" "blue"         ))
   (bg-alt          '("#240d70" "#000080" "blue"         ))
   (base0           '("#38237d" "#5f0087" "brightblue"   ))
   (base1           '("#4c398a" "#5f5f87" "brightblue"   ))
   (base2           '("#604f97" "#5f5f87" "brightblue"   ))
   (base3           '("#7465a4" "#875faf" "brightblue"   ))
   (base4           '("#887bb1" "#8787af" "brightblue"   ))
   (base5           '("#9b91be" "#8787af" "brightblue"   ))
   (base6           '("#afa7cb" "#8787af" "brightblue"   ))
   (base7           '("#c3bdd8" "#c0c0c0" "brightblue"   ))
   (base8           '("#d7d3e5" "#d7d7d7" "white"        ))
   (fg              '("#FFFFFF" "#ffffff" "brightwhite"  ))
   (fg-alt          '("#f5f4f9" "#eeeeee" "brightwhite"  ))
   (red             '("#db2014" "#ff5f5f" "red"          ))
   (calm-red        '("#fb9b93" "#ff5f5f" "red"          ))
   (orange          '("#e9a95c" "#d7af5f" "brightred"    ))
   (green           '("#4cd00e" "#5fd700" "brightgreen"  ))
   (teal            '("#57cdd7" "#5fd7d7" "cyan"         ))
   (yellow          '("#f5E202" "#ffd700" "yellow"       ))
   (calm-yellow     '("#cab600" "#ffd700" "yellow"       ))
   (blue            '("#96CBFE" "#87d7ff" "cyan"         ))
   (dark-blue       '("#92b7ee" "#87afff" "cyan"         ))
   (magenta         '("#f35bff" "#ff5fff" "brightpurple" ))
   (calm-magenta    '("#e89bf8" "#ff5fff" "brightpurple" ))
   (violet          '("#c4a8ff" "#d7afff" "purple"       ))
   (cyan            '("#78baf0" "#87afff" "brightcyan"   ))
   (dark-cyan       '("#65c0ea" "#5fafd7" "brightcyan"   ))
   (light-cyan      '("#9cdcfe" "#afd7ff" "brightcyan"   ))
   (grey            '("#b3c2d0" "#afafd7" "brightblack"  ))

   ;; face categories -- required for all themes
   (highlight      `(,(doom-lighten (car base0) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (selection      blue)
   (builtin        calm-yellow)
   (comments       (doom-darken orange 0.2))
   (vertical-bar   (doom-darken base2 0.1))
   (doc-comments   calm-red)
   (constants      cyan)
   (functions      calm-yellow)
   (keywords       calm-magenta)
   (methods        calm-yellow)
   (operators      green)
   (type           green)
   (strings        cyan)
   (variables      fg-alt)
   (numbers        violet)
   (region         `(,(doom-lighten (car base0) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red))


  ;; Base theme face overrides
  (((hl-line &override) :background base1)
   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((link &override) :foreground blue)
   (show-paren-match :foreground bg :background red)
   (cursor :foreground bg :background fg)
   (lazy-highlight :background magenta :foreground bg)
   (evil-ex-search :background magenta :foreground bg)
   (line-number-current-line :foreground yellow :weight 'bold)
   (tooltip :background base1 :foreground fg )
   ((secondary-selection &override) :background base1)
   ((highlight-quoted-symbol &override) :foreground calm-yellow)
   ((font-lock-preprocessor-face &override) :inherit 'bold :foreground magenta)
   ((help-key-binding &override) :foreground magenta :background bg :)
   ((minibuffer-prompt &override) :foreground blue)
   ((vertical-border &override) :foreground violet :background violet)

   ;; modeline
   (mode-line
    :background bg :foreground fg
    :box `(:line-width -1 :color ,fg))
   (mode-line-inactive
    :background bg :foreground grey
    :box `(:line-width -1 :color ,grey))
   (mode-line-emphasis :foreground fg)
   (doom-modeline-bar :background bg)

   ;; calendar
   ((calendar-month-header &override) :foreground violet :weight 'bold)
   ((calendar-weekday-header &override) :foreground blue :weight 'bold)
   ((calendar-weekend-header &override) :foreground red :weight 'bold)
   ((cfw:face-grid &override) :foreground fg)
   ((cfw:face-title &override) :foreground violet)
   ((cfw:face-today-title &override) :background green)
   ((cfw:face-toolbar-button-on &override) :foreground yellow)
   ((cfw:face-toolbar &override) :foreground bg :background bg)
   ((cfw:face-saturday &override) :foreground red)
   ((cfw:face-sunday &override) :foreground red)
   ((org-date-selected &override) :background highlight :foreground bg :weight 'bold)

   ;; centaur-tabs
   ((centaur-tabs-selected &override)   :background bg :foreground fg :underline green :weight 'bold)
   ((centaur-tabs-unselected &override) :background bg :foreground grey)

   ;; company
   ((company-tooltip-common &override) :weight 'normal :foreground blue)
   ((company-tooltip-selection &override) :background base2 )
   ((company-scrollbar-fg &override)   :background red)
   ((company-tooltip-annotation &override) :foreground violet :distant-foreground bg)
   ((company-tooltip &override) :background base0)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired/dirvish
   ((diredfl-dir-name &override) :foreground blue)
   ((dirvish-hl-line &override) :background base1)
   ((dired-directory &override) :foreground orange)

   ;; Doom
   ((doom-dashboard-menu-title &override) :foreground fg :slant 'italic)
   ((doom-dashboard-menu-desc &override) :foreground violet)
   ((doom-dashboard-footer-icon &override) :foreground red)
   ((doom-themes-org-hash-tag &override) :foreground green :box nil)

   ;; ediff
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))

   ;; fringe
   (fringe :foreground fg)

   ;; embark
   ((embark-keybinding &override) :foreground magenta)

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
   ;; ((highlight-indent-guides-character-face &override) :foreground red)

   ;; Info-mode
   ((info-xref &override) :inherit 'link :foreground blue)
   ((info-xref-visited &override) :inherit '(info-xref link-visited)  :foreground fg-alt)
   ((info-menu-star &override) :foreground fg)
   ((info-menu-header &override) :inherit 'variable-pitch :weight 'bold :foreground blue)
   ((info-title-4 &override) :inherit 'variable-pitch :weight 'bold :foreground magenta :height 1.3)
   ((info-title-3 &override) :inherit 'info-title-4  :height 1.3)
   ((info-title-2 &override) :inherit 'info-title-3  :height 1.3)
   ((info-title-1 &override) :inherit 'info-title-2  :height 1.5)
   ((Info-quoted &override) :inherit 'fixed-pitch-serif :foreground yellow :background bg)

   ;; ivy
   ((ivy-minibuffer-match-face-1 &override) :foreground (doom-lighten base5 0.70))
   (ivy-posframe               :background base0)

   ;; js2-mode
   ((js2-object-property-access &override) :foreground fg)
   ((js2-object-property &override) :foreground light-cyan)

   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'region)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)


   ;; magit
   ((git-commit-summary &override) :foreground green)
   ((magit-diff-file-heading           &override) :foreground violet    :background bg-alt :bold bold)
   ((magit-diff-hunk-heading           &override) :foreground orange    :background bg-alt :bold bold)
   ((magit-diff-hunk-heading-highlight &override) :foreground orange :background bg-alt :bold bold)
   ((magit-header-line &override) :foreground violet :background bg :height 1.5 :box `(:line-width -1 :color ,fg))
   ((magit-section-highlight &override) :background base1)
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.5) :background bg)
   (magit-diff-removed-highlight :foreground red :background bg)
   (magit-diff-added :foreground (doom-darken green 0.5) :background bg)
   (magit-diff-added-highlight :foreground green :background bg)
   (diff-added  :forground green :background (doom-blend green bg 0.35))
   (diff-removed  :forground red :background (doom-blend red bg 0.35))
   (diff-refine-added  :inherit 'diff-added)
   (diff-refine-removed  :inherit 'diff-removed)
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
   (markdown-header-face :foreground violet :weight 'bold)
   ((markdown-header-face-1  &override) :foreground magenta  :height 1.5 :weight 'bold)
   ((markdown-header-face-2  &override) :foreground cyan :height 1.4 :weight 'bold)
   ((markdown-header-face-3  &override) :foreground orange :height 1.3 :weight 'bold)
   ((markdown-header-face-4  &override) :foreground (doom-lighten yellow 0.25)    :weight 'bold)
   ((markdown-header-face-5  &override) :foreground (doom-lighten green 0.25)      :weight 'bold)
   ((markdown-header-face-6  &override) :foreground (doom-lighten red 0.25)    :weight 'bold)
   (markdown-bold-face :foreground violet :weight 'bold)
   (markdown-italic-face :foreground red :slant 'italic)
   (markdown-header-face :foreground violet :weight 'bold)
   (markdown-inline-code-face :foreground yellow)
   (mmm-default-submode-face :background base0)

   ;; mu4e
   (mu4e-highlight-face :background bg :inherit 'bold)
   ((mu4e-header-highlight-face &override) :inherit 'bold :underline nil :background base1)
   ((mu4e-flagged-face &override) :foreground yellow)
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
   ((org-todo &override) :foreground yellow)
   ((org-done &override) :strike-through t)
   ((org-headline-done &override) :strike-through t)
   ((org-tag &override)   :foreground green :background nil
    :box `(:line-width -1 :color ,green :style 'released-button))
   ((org-date &override)  :foreground yellow :background nil)
   ((org-special-keyword &override)  :foreground fg-alt)
   ((org-drawer          &override)  :foreground base5)
   (org-ellipsis :underline nil :box nil :foreground fg-alt :background bg :height 0.75)
   (org-link :inherit 'link :foreground blue)
   ((org-block &override) :background bg)
   ((org-block-begin-line &override) :foreground yellow :slant 'italic :height 0.8)
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
   ((org-agenda-date &override) :foreground blue :height 1.1)
   ((org-agenda-date-today &override) :foreground green :slant 'italic :height 1.3)
   ((org-agenda-date-weekend &override) :foreground blue)
   ((org-time-grid &override) :foreground fg)
   ((org-agenda-current-time &override) :foreground yellow)

   ;; org-modern
   (org-modern-label :inherit 'org-todo :foreground yellow :weight 'bold)
   (org-modern-todo :inherit 'org-modern-label)
   (org-modern-done  :inherit 'org-done)
   (org-modern-tag  :inherit 'org-tag  :height 0.9 :box nil)
   (org-modern-time-active :foreground yellow :background nil)
   (org-modern-time-inactive :foreground fg-alt :background nil)
   ((org-modern-date-active &override)  :foreground yellow :background nil)
   ((org-modern-date-inactive &override)  :foreground fg-alt :background nil)
   ((org-modern-priority &override) :background nil)
   (org-modern-statistics  :foreground nil)
   (org-modern-symbol  :height 1.5)

   ;; outline
   ((outline-1  &override) :foreground magenta  :height 1.5 :weight 'bold)
   ((outline-2  &override) :foreground cyan :height 1.3 :weight 'bold)
   ((outline-3  &override) :foreground orange :height 1.2 :weight 'bold)
   ((outline-4  &override) :foreground (doom-lighten yellow 0.25)    :weight 'bold)
   ((outline-5  &override) :foreground (doom-lighten green 0.25)      :weight 'bold)
   ((outline-6  &override) :foreground (doom-lighten red 0.25)    :weight 'bold)
   ((outline-7  &override) :foreground (doom-lighten magenta 0.25)    :weight 'bold)
   ((outline-8  &override) :foreground (doom-lighten cyan 0.25)      :weight 'bold)

   ;; rainbow-delimiters
   ((rainbow-delimiters-depth-1-face &override) :foreground magenta)
   ((rainbow-delimiters-depth-2-face &override) :foreground cyan)
   ((rainbow-delimiters-depth-3-face &override) :foreground orange)
   ((rainbow-delimiters-depth-4-face &override) :foreground yellow)
   ;; rjsx mode
   (rjsx-text :foreground fg :slant 'italic)
   (rjsx-tag :foreground green)

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
   ((treemacs-root-face &override) :foreground violet :weight 'bold :height 1.3)
   ((treemacs-file-face &override) :inherit 'fixed-pitch :foreground fg :weight 'bold)
   ((treemacs-git-unmodified-face &override) :inherit 'fixed-pitch :foreground fg :weight 'bold)
   ((treemacs-git-modified-face &override) :inherit 'fixed-pitch :foreground orange :weight 'bold)
   ((treemacs-git-ignored-face &override) :inherit 'fixed-pitch :foreground fg-alt :weight 'bold)
   ((treemacs-git-conflict-face &override) :inherit 'fixed-pitch :foreground yellow :weight 'bold)
   ((treemacs-git-renamed-face &override) :inherit 'fixed-pitch :foreground blue :weight 'bold)
   ((treemacs-git-added-face &override) :inherit 'fixed-pitch :foreground green :weight 'bold)
   ((treemacs-directory-face &override) :inherit 'fixed-pitch :foreground fg :weight 'bold)
   ((doom-themes-treemacs-file-face &override) :inherit 'font-lock-doc-face :foreground blue :weight 'bold :height 1.2)
   ((doom-themes-treemacs-root-face &override) :inherit 'font-lock-doc-face :foreground violet :weight 'bold :height 1.3)

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

   ;; wgrep
   (wgrep-face :background base1)
   ;; which-key
   ((which-key-key-face &override) :inherit 'bold)))

     ;;;; Base theme variable overrides-
;;; doom-tibetan.el ends here
