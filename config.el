;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "DJS"
      user-mail-address "david@djs.gg")


;;
;;; UI/UX
(setq doom-theme 'doom-bisqwit-improved
      doom-font (font-spec :family "Berkeley Mono" :size 16 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Quicksand" :weight 'medium :size 16))

;; Start in Fullscreen Mode
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; I prefer new windows to take me to my dashboard...for now
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  ;;(dired-jump)
  (consult-buffer))
;;(+doom-dashboawrd/open (selected-frame)))

;; Prefer relative line numbers.
(setq display-line-numbers-type 'relative)

;; More than three different color brackets and my eyes get lost
(setq rainbow-delimiters-max-face-count 3)

;; Reduce delay time on which-key popups - because I'm still a noob
(setq which-key-idle-delay 1)

;; Stop embark from taking over which-key paging abilities
(setq which-key-use-C-h-commands t
      prefix-help-command #'which-key-C-h-dispatch)

(defadvice! fix-which-key-dispatcher-a (fn &rest args)
  :around #'which-key-C-h-dispatch
  (let ((keys (this-command-keys-vector)))
    (if (equal (elt keys (1- (length keys))) ?\?)
        (let ((keys (which-key--this-command-keys)))
          (embark-bindings (seq-take keys (1- (length keys)))))
      (apply fn args))))

;; Set visual line mode globally. I can't remember how I got here exactly but
;; doom's word-wrap module wasn't wrapping in all the cases I'd expect it to,
;; particularly in org-mode. I also couldn't figure out why visual-line-mode
;; wasn't causing editing commands to act on visual lines rather than logical
;; lines like the docs say they should, so I changed the keys myself.
(global-visual-line-mode)
(after! evil
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; I like a few punctuation symbols to stand out when coding
;; I would like to move this into my custom theme somehow. In general
;; This strikes me a something I should be doing differently.
(defface heavy-punctuation-face '((t (:foreground "#008000")))
  "Face for extra emphasis on a customizable list of symbols.")
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("[;:,.#]" . 'heavy-punctuation-face))))
      '(emacs-lisp-mode
        c-mode lua-mode
        rjsx-mode
        typescript-mode
        web-mode))

;;
;;; Keybinds
(map! :leader
      :desc "Org Agenda"         "j"     #'org-launch-custom-agenda
      :desc "Doom Splash"        "k"     #'+doom-dashboard/open
      :desc "Kill buffer"        "\\"   #'kill-current-buffer
      :desc "Close window"       "DEL" #'+workspace/close-window-or-workspace
      :desc "Auto complete at point" "-" #'+company/complete
      ;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       (:when (featurep! :completion company)
        :desc "Auto-completion"          "p"     #'+company/toggle-auto-completion)))
;; evil mode (I need to figure out how to get these in the previous call)
(map! :n "[w" #'evil-window-prev
      :n "]w" #'evil-window-next
      :n "[ TAB" #'+workspace/switch-left
      :n "] TAB" #'+workspace/switch-right)


;;
;;; Modules


;;; :lang org
(setq +org-roam-auto-backlinks-buffer t
      org-directory "~/Sync/projects/org/"
      org-roam-directory (concat org-directory "roam/")
      org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-roam-dailies-directory "journal/")

;; Have TODO's switch to done when all subentries are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "[ ]"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(after! org
  (setq ;;org-startup-folded 'show2levels
        org-ellipsis " [...] "
        org-capture-templates
        ;; Personal Todo Templates
        `(("t" "✅ Todo")
          ("tp" "🙋 Personal" entry (file+headline "todo.org" "🙋 Personal") "* TODO %?")
          ("ta" "🐶 Animals" entry (file+headline "todo.org" "🐶 Animals") "* TODO %?")
          ("ts" "🛒 Shopping List" entry (file+headline "todo.org" "Shopping") "* [ ] %?")
          ("th" "🏡 Home" entry (file+headline "todo.org" "🏡 Home") "* TODO %?")
          ("to" "🖥 Office" entry (file+headline "todo.org" "🖥 Office") "* TODO %?")
          ("tm" "⁉ Misc." entry (file+headline "todo.org" "⁉ Inbox") "* TODO %?")
          ("c" "💻 Code")
          ("ce" "🇪 Emacs" entry (file+headline "code.org" "Emacs") "* %?")
          ("ca" "🇦 Awesome" entry (file+headline "code.org" "Awesome") "* %?")
          ("cm" "⁉ Misc" entry (file+headline "code.org" "Inbox") "* %?")
          ("a" "📅 Appointment" entry (file+headline "appt.org" "Inbox") "* %?\n<%(org-read-date)>")
        ;; Default cenralized project templates
          ("g" "🌏 Global Project Files")
          ("gt" "✅ Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("gn" "✏ Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("gc" "🏁 Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
        ;; Default local project templates
          ("l" "🔒 Local Project Files")
          ("lt" "✅ Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend t)
          ("ln" "✏ Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a" :prepend t)
          ("lc" "🏁 Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n%i\n%a" :prepend t)
          )))

(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("r" "thought" plain
           ,(format "#+title: ${title}\n%%[%s/template/thought.org]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        '(("a" "agenda" entry
           ;; TODO figure out how not to hard code this path...
           (file "~/Sync/projects/org/roam/template/agenda.org")
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %B %d, %Y>\n\n"))
          ("d" "dream" entry "* Dream\n%?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %B %d, %Y>\n\n"))
          ("t" "thought" entry "* Thought\n%?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %B %d, %Y>\n\n")))))

;; set org-journal type to daily
(after! org-journal
  (setq org-journal-file-type 'daily
        org-journal-date-format "%A %B %d, %Y"))

;; Start org-agenda at current date and show only 7 day outlook
(after! org-agenda
  (setq org-agenda-start-day "+0d"
        org-agenda-span 7))

;; Create custom block-limited agenda filters
(defun my/org-match-at-point-p (match)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'."
  (funcall (cdr (org-make-tags-matcher match))
           (org-get-todo-state)
           (org-get-tags-at)
           (org-reduced-level (org-current-level))))

(defun my/org-agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.

Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) nil next-headline))))

;; First block shows today's agenda
;; Second block shows 9 day outlook, hiding unwanted chore TODOs
;; Third block shows full list of main TODOs
(setq org-agenda-custom-commands
      '(("j" "Main agenda and TODO list"
         ((agenda "" ((org-agenda-span 1)))
          (agenda "" ((org-agenda-span 9)
                      (org-agenda-start-day "+1d")
                      (org-agenda-skip-function
                       '(my/org-agenda-skip-without-match "-hide"))))
          (tags-todo "+main")))))

;;; ;; Create function to launch custom agenda
;; TODO allow for arguments when launching custom agenda
(defun org-launch-custom-agenda ()
  "Launch the org agenda using the custom command supplied "
  (interactive)
  (org-agenda nil "j"))

;; Hide noisy tag labels in agenda
(setq org-agenda-hide-tags-regexp "main\\|chore\\|hide")

;; Hide certain tags from main agenda
;; No longer needed with custom blocks
;; (defun org-my-auto-exclude-fn (tag)
;;   (if (cond
;;        ((string= tag "hide")))
;;       (concat "-" tag)))
;; (setq org-agenda-auto-exclude-function 'org-my-auto-exclude-fn)


;;; info mode
;; use variable-pitch font by default
(add-hook! 'Info-mode-hook #'variable-pitch-mode)


;;; :completion company
(after! company
  ;; Disable auto completion. We have keybinds to access/toggle completion when needed
  (setq company-idle-delay nil
        company-minimum-prefix-length 0)
  (setq company-show-quick-access t))


;;; :editor evil
;; Focus new windows after splitting
(setq evil-vsplit-window-right t
      evil-split-window-below t)


;;; :ui popup
;; For when I want to keep certain windows around
(set-popup-rules!
  '(("^\\*help"         :ignore t)
    ("^\\*info\\*"      :ignore t)
    ("^\\*Man"          :ignore t)))


;;; :ui treemacs
;; Treemacs theme of choice
(setq doom-themes-treemacs-theme 'doom-colors)

;; I think it's nice to see when directories contain files with changes
(setq +treemacs-git-mode 'extended)


;;; :tools lsp
;; I want to make sure I won't have to manually add lsp serverse on new installs
(after! lsp-mode
  (lsp-ensure-server 'ts-ls)
  (lsp-ensure-server 'bash-ls)
  (lsp-ensure-server 'emmet-ls)
  (lsp-ensure-server 'html-ls)
  (lsp-ensure-server 'dockerfile-ls)
  (lsp-ensure-server 'yamlls)
  (lsp-ensure-server 'json-ls)
  (lsp-ensure-server 'eslint)
  (lsp-ensure-server 'css-ls)
  (lsp-ensure-server 'vimls)
  (lsp-ensure-server 'clangd)
  (lsp-ensure-server 'lua-language-server))

;; Disable lsp auto formatting to prevent interference with tools like prettier
(setq +format-with-lsp nil)


;;; :lang web
;; choose extensions to open in web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . web-mode))

;; Force lsp to to recognize scss files in web-mode
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(web-mode . "scss")))


;;; :lang lua
;; enable rainbow delimiters mode (not sure why this isn't default)
(add-hook! lua-mode
           #'rainbow-delimiters-mode-enable)


;;; :tools rgb
;; Prevent ordinay words from highlight in rainbow mode
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


;;; :ui doom-dashboard
;; For a while I thought it would be nice to default my agenda to zen-mode
;; Not the case anymore but I like leaving the function here
(defun org-agenda-list-zen ()
  "Loads the Org agenda in zen mode"
  (interactive)
  (org-agenda-list)
  (+zen/toggle))

;; My preferred dashboard functions. I think I should be doing this without setq
;; as per the doom FAQ, but this works just fine for now
(setq +doom-dashboard-menu-sections
      '(("Open org-agenda" :icon
         (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
         :when (fboundp 'org-agenda-list)
         :action org-agenda-list)
        ("Recently opened files" :icon
         (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Open project" :icon
         (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Open mail" :icon
         (all-the-icons-octicon "mail" :face 'doom-dashboard-menu-title)
         :action =mu4e)
        ("Jump to bookmark" :icon
         (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)))


;;; :email mu4e
;; I learned the hard way not to use custom domains setting up protonmail
;; even though I'm pretty sure it was no issue when setitng up Thunderbird
;; and the bridge app itself shows your custom domain as the username
(set-email-account! "proton"
                    '((mu4e-sent-folder       . "/proton/Sent")
                      (mu4e-drafts-folder     . "/proton/Drafts")
                      (mu4e-trash-folder      . "/proton/Trash")
                      (mu4e-refile-folder     . "/proton/All Mail")
                      (smtpmail-smtp-user     . "d.sharfi@protonmail.com")
                      (smtpmail-auth-credentials . "~/.authinfo.gpg")
                      (smtpmail-smtp-server   . "127.0.0.1")
                      (smtpmail-smtp-service   . 1025)
                      (smtpmail-stream-type   . starttls)
                      (user-mail-address      . "d.sharfi@protonmail.com")    ;; only needed for mu < 1.4
                      (mu4e-compose-signature . "---\nDavid")
                      (+mu4e-personal-addresses . ("david@djs.gg"
                                                   "catchall@djs.gg"
                                                   "d.sharfi@protonmail.com")))
                    t)
