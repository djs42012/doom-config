;;; doom-homage-black-theme.el --- pitch-black theme version of homage-white -*- lexical-binding: t; no-byte-compile: t; -*-
;;;
;;; Commentary:
;;;
;;; Theme is (manually) inverted homage-white theme with a focus of having
;;; pitch-black backgrounds. I'm also incorporated a several ideas from jbeans
;;; theme (synic/jbeans-emacs).

(require 'doom-themes)

;;
(defgroup doom-homage-black-theme nil
  "Options for the `doom-homage-black' theme."
  :group 'doom-themes)

(defcustom doom-homage-black-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-homage-black-theme
  :type '(choice integer boolean))
;;
(def-doom-theme doom-bisqwit-improved
  "A light theme inspired by Atom One"

  ;; name        default   256       16
  ((bg         '("#000000" nil       nil            ))
   (bg-alt     '("#000000" nil       nil            ))
   (base0      '("#111111" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2light-e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#8f8f8e" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   ;;(fg         '("#ffffff" "#d0d0d0" "brightwhite"  ))
   (fg         '("#d4d4d4" "#d0d0d0" "brightwhite"  ))
   ;;(fg         '("#ccc4ae" "#d0d0d0" "brightwhite"  ))
   ;;(fg         '("#c0c0c0" "#d0d0d0" "brightwhite"  ))
   ;; (fg-alt     '("#c1c1c1" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#a9a5aa" "#bfbfbf" "brightwhite"  ))
   (grey       base5)
   (dark-grey  '("#121212" "black"   "black"        ))
   (light-grey '("#4d4d4d" "#4e4e4e"   "brightwhite"))
   (red        '("#b95959" "#d75f5f" "red"          ))
   (bright-red        '("#b95959" "#d75f5f" "red"          ))
   (orange     '("#cd9731" "#E9C062" "brightred"    ))
   (green      '("#93e079" "#87d787" "brightgreen"  ))
   (olive-green      '("#6a9955" "#87d787" "brightgreen"  ))
   (dark-green      '("#008000" "#87d787" "brightgreen"  ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   ;; (yellow     '("#FFFFB6" "#FFFFB6" "yellow"       ))
   (yellow     '("#dcdcaa" "#FFFFB6" "yellow"       ))
   (msft-yellow     '("#fdb900" "#FFFFB6" "yellow"       ))
   (yellow-grn '("#dcdcaa" "#d7d7af" "yellow"       ))
   (bright-ylw '("#d7d700" "#d7d700" "brightyellow" ))
   (blue       '("#96CBFE" "#96CBFE" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c586c0" "#d787af" "magenta"      ))
   (dark-magenta    '("#915bff" "#875fff" "purple"       ))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
   (cyan       '("#569cd6" "#5fafd7" "brightcyan"   ))
   (dark-cyan       '("#3d90b6" "#5fafd7" "brightcyan"   ))
   ;; (dark-cyan  '("#005fff" "#005fff" "cyan"         ))
   (light-cyan '("#9cdcfe" "#87d7ff" "brightcyan"   ))
   (turquoise  '("#00d7ff" "#00d7ff" "brightblue"   ))
   ;; face categories -- required for all themes
   (highlight      fg-alt)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      blue)
   (builtin        yellow)
   (comments       red)
   (doc-comments   (doom-darken comments 0.15))
   (constants      cyan)
   (functions      yellow)
   (keywords       magenta)
   (methods        yellow)
   (operators      green)
   (type           dark-green)
   (strings        cyan)
   (variables      fg-alt)
   (numbers        dark-magenta)
   (region         `(,(doom-lighten (car dark-grey) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        msft-yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   ;; custom categories
   (-modeline-bright t)
   (-modeline-pad
    (when doom-homage-black-padded-modeline
      (if (integerp doom-homage-black-padded-modeline)
          doom-homage-black-padded-modeline 4)))
   (modeline-fg     fg)
   (modeline-fg-alt violet)

   (modeline-bg "#000000")
   (modeline-bg-l bg)
   (modeline-bg-inactive bg)
   (modeline-bg-inactive-l bg))

  ;;(modeline-bg
  ;;  (if -modeline-bright
  ;;      (doom-darken base2 0.05)
  ;;    base1))
  ;; (modeline-bg-l
  ;;  (if -modeline-bright
  ;;      (doom-darken base2 0.1)
  ;;    base2))
  ;; (modeline-bg-inactive (doom-darken bg 0.1))
  ;; (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))

;;; Base theme face overrides
  (((hl-line &override) :background base0)
   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)
   ((doom-dashboard-menu-title &override) :foreground fg)
   ((doom-dashboard-menu-desc &override) :foreground violet)
   ((doom-dashboard-footer-icon &override) :foreground red)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    ;;:box `(:line-width -1 :color fg)
    )
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    ;;:box `(:line-width -1 :color fg)
    )

   ((info-xref &override) :inherit 'link :foreground blue)
   ((info-xref-visited &override) :inherit '(info-xref link-visited)  :foreground blue)
   ((info-menu-star &override) :foreground fg)
   ((info-menu-header &override) :inherit 'variable-pitch :weight 'bold :foreground blue)
   ((info-title-4 &override) :inherit 'variable-pitch :weight 'bold :foreground violet :height 1.3)
   ((info-title-3 &override) :inherit 'info-title-4  :height 1.3)
   ((info-title-2 &override) :inherit 'info-title-3  :height 1.3)
   ((info-title-1 &override) :inherit 'info-title-2  :height 1.4)
   ((Info-quoted &override) :inherit 'fixed-pitch-serif :background base0 )
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (tooltip :background bg :foreground fg )
   ;;((company-tooltip &override) :box `(:line-width 1 :color ,red))
   ((company-tooltip-common &override) :foreground blue :weight 'bold)
   ((company-tooltip-selection &override) :background base2 )
   ((company-scrollbar-fg &override)   :background red)
   ((secondary-selection &override) :background base0)
   ((company-tooltip-annotation &override) :foreground dark-magenta :distant-foreground bg)
   ((company-tooltip-scrollbar-thumb &override) :foreground red)
   ;;additional override
   ((highlight-quoted-symbol &override) :foreground yellow)
   ;;((paren-face-match &override) :foreground base0 :background violet)
   ;;((show-paren-match &override) :foreground base0 :background violet)
   ;;((sp-show-pair-match-face &override):foreground base0 :background violet)
   (font-lock-add-keywords 'emacs-lisp-mode '(("[#:,.;]" . font-lock-constant-face)))
   ((line-number-current-line &override) :foreground bright-ylw :weight 'bold)
   ((font-lock-preprocessor-face &override) :inherit 'bold :foreground magenta)
   ((rainbow-delimiters-depth-1-face &override) :foreground bright-ylw)
   ((rainbow-delimiters-depth-2-face &override) :foreground magenta)
   ((rainbow-delimiters-depth-3-face &override) :foreground turquoise)
   ((minibuffer-prompt &override) :foreground blue)
   (orderless-match-face-0 :foreground  blue :weight 'bold)
   (orderless-match-face-1 :foreground  green :weight 'bold)
   (orderless-match-face-2 :foreground  yellow :weight 'bold )
   (orderless-match-face-3 :foreground  blue :weight 'bold )
   ;;((rainbow-delimiters-depth-4-face &override) :foreground green)
   ;;dired
   ((diredfl-dir-name &override) :foreground green)
   ((js2-object-property-access &override) :foreground fg)
   ((js2-object-property &override) :foreground light-cyan)
   ((Man-overstrike &override) :inherit 'bold  :height 1.05 :foreground violet)
   ((nav-flash-face &override) :foreground fg :background region :weight 'bold)
   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; ediff <built-in>
   ;;;; embark
   ((embark-keybinding &override) :foreground magenta)
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; magit
   ((git-commit-summary &override) :foreground green)
   ((magit-diff-hunk-heading           &override) :foreground fg    :background bg-alt :bold bold)
   ((magit-diff-hunk-heading-highlight &override) :foreground base8 :background bg-alt :bold bold)
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)
   ;;;; mu4e
   (mu4e-highlight-face :background bg :inherit 'bold)
   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)
   ;;;; ivy
   ;; bg/fg are too close
   ((ivy-minibuffer-match-face-1 &override) :foreground (doom-lighten grey 0.70))
   ;;;; ivy-posframe
   (ivy-posframe               :background base0)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)
   ;;;; outline <built-in>
   ((outline-1  &override) :foreground violet  :height 1.25                      :weight 'bold :extend t)
   ((outline-2  &override) :foreground blue :height 1.1                          :weight 'bold :extend t)
   ((outline-3  &override) :foreground yellow                       :weight 'bold :extend t)
   ((outline-4  &override) :foreground (doom-lighten violet 0.25)    :weight 'bold :extend t)
   ((outline-5  &override) :foreground (doom-lighten blue 0.25)      :weight 'bold :extend t)
   ((outline-6  &override) :foreground (doom-lighten yellow 0.25)    :weight 'bold :extend t)
   ((outline-7  &override) :foreground (doom-lighten violet 0.25)    :weight 'bold :extend t)
   ((outline-8  &override) :foreground (doom-lighten blue 0.25)      :weight 'bold :extend t)
    ;;; org <built-in>
   ;; make unfinished cookie & todo keywords bright to grab attention
   ((org-todo &override) :foreground red)
   ;; make tags and dates to have pretty box around them
   ((org-tag &override)   :foreground green :background nil
    :box `(:line-width -1 :color ,green :style 'released-button))
   ((org-date &override)  :foreground msft-yellow :background nil)
   ;; Make drawers and special keywords (like scheduled) to be very bleak
   ((org-special-keyword &override)  :foreground fg-alt)
   ((org-drawer          &override)  :foreground grey)
   ;; Make ellipsis as bleak as possible and reset underlines/boxing
   (org-ellipsis :underline nil :box nil :foreground fg :background bg)
   ;; Make blocks have a slightly different background
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   ((org-quote &override) :background base0)
   ((org-table &override) :foreground fg)
   ;; org-agendamode: make "unimportant" things like distant deadlines and
   ;; things scheduled for today to be bleak.
   (org-upcoming-deadline         :foreground base8)
   (org-upcoming-distant-deadline :foreground fg)
   (org-scheduled                 :foreground fg)
   (org-scheduled-today           :foreground fg)
   (org-scheduled-previously      :foreground base8)
   ((org-agenda-date &override) :foreground blue)
   ((org-agenda-date-today &override) :foreground green)
   ((org-agenda-date-weekend &override) :foreground blue)
   ;;calendar
   ((calendar-month-header &override) :foreground yellow :weight 'bold)
   ((calendar-weekday-header &override) :foreground blue :weight 'bold)
   ((calendar-weekend-header &override) :foreground red :weight 'bold)
   ((org-date-selected &override) :background highlight :foreground bg :weight 'bold)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; swiper
   ;; bg/fg are too close
   ((swiper-match-face-1 &override) :background fg        :foreground bg)
   ((swiper-line-face    &override) :background dark-blue :foreground fg)
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1))
  ;;;; Base theme variable overrides-
  ())
;;; doom-homage-black-theme.el ends here
