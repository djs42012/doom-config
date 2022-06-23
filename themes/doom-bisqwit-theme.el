;;; doom-ir-black-theme.el --- port of ir_black -*- lexical-bindings: t; no-byte-compile: t; -*-
;;
;; Author: Todd Werth <todd@infinite.red>
;; Ported by: Kevin Kainan Li <legendre6891@users.noreply.github.com>
;; Created: January 10, 2019
;; Version: 2.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Ported from the original ir_black theme in hlissner/emacs-doom-themes#255.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-ir-black-theme nil
  "Options for the `doom-ir-black' theme."
  :group 'doom-themes)

(defcustom doom-ir-black-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ir-black-theme
  :type 'boolean)

(defcustom doom-ir-black-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-ir-black-theme
  :type '(or integer boolean))


;; Theme definition
(def-doom-theme doom-bisqwit
  "A port of the original IR Black colorscheme."

  ;; name        default   256       16
  ((bg         '("#000000" "black"   "black"        ))
   (fg         '("#d4d4d4" "#d0d0d0" "brightwhite"  ))
   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#121212" "black"   "black"        ))
   (fg-alt     '("#4f4f4f" "#4e4e4e" "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))

   (white      '("#ffffff" "#ffffff" "white"))
   (grey       base4)
   (red        '("#b95959" "#d75f5f" "red"          ))
   (orange     '("#E9C062" "#E9C062" "brightred"    ))
   (orange-alt '("#FFD2A7" "#FFD2A7" "brightred"    ))
   (green      '("#A8FF60" "#A8FF60" "green"        ))
   (green-alt  '("#93e079" "#87d787" "brightgreen"  ))
   (teal       '("#00A0A0" "#00A0A0" "brightgreen"  ))
   (yellow     '("#FFFFB6" "#FFFFB6" "yellow"       ))
   (blue       '("#96CBFE" "#96CBFE" "brightblue"   ))
   (blue-alt   '("#87d7ff" "#87d7ff" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c586c0" "#d787af" "magenta"      ))
   (light-cyan '("#9cdcfe" "#87d7ff" "brightcyan"   ))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#569cd6" "#5fafd7" "brightcyan"   ))
   (med-cyan   '("#3d90b6" "#5f87af" "brightcyan"   ))
   ;;(dark-cyan  '("#005fff" "#005fff" "cyan"         ))
   (dark-cyan  '("#3462e5" "#005fff" "cyan"   a      ))
   (blue-mgnta '("#915bff" "#875fff" "purple"       ))
   (dark-green '("#006000" "#005f00" "green"        ))
   (yellow-grn '("#dcdcaa" "#d7d7af" "yellow"       ))
   (bright-ylw '("#d7d700" "#d7d700" "brightyellow" ))
   (turquoise  '("#00d7ff" "#00d7ff" "brightblue"   ))
   (light-grey '("#a9a5aa" "#a8a8a8" "white"        ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base5)
   (selection      cyan)
   (builtin        yellow-grn)
   (comments       red)
   (doc-comments   red)
   (constants      cyan)
   (functions      yellow-grn)
   (keywords       magenta)
   (methods        yellow-grn)
   (operators      green-alt)
   (type           yellow)
   (strings        cyan)
   (variables      fg)
   (numbers        blue-mgnta)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   ;; custom categories
   (-modeline-pad
    (when doom-ir-black-padded-modeline
      (if (integerp doom-ir-black-padded-modeline) doom-ir-black-padded-modeline 4)))

   (modeline-fg          white)
   (modeline-fg-alt      base5)
   (modeline-bg          base4)
   (modeline-bg-inactive base3))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-ir-black-brighter-comments (doom-lighten bg 0.05)))
   ((line-number &override) :foreground base5)
   (font-lock-add-keywords 'emacs-lisp-mode '(("[#:,.;]" . font-lock-constant-face)))
   ((line-number-current-line &override) :foreground "#FFFF00" :weight 'bold)
   ((font-lock-preprocessor-face &override) :inherit 'bold :foreground magenta)
   ((rainbow-delimiters-depth-1-face &override) :foreground bright-ylw)
   ((rainbow-delimiters-depth-3-face &override) :foreground turquoise)
   ;;((rainbow-delimiters-depth-4-face &override) :foreground green)
   ((paren-face-match &override) :foreground base0 :background red)
   ((js2-object-property-access &override) :foreground fg)
   ((js2-object-property &override) :foreground light-cyan)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; evil
   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5)) ;;;; ivy (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05)))
  

  ;;;; Base theme variable overrides-
  ;; ()
  )


;;; doom-ir-black-theme.el ends here
