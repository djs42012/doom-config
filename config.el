;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "DJS"
      user-mail-address "david@djs.gg")


;;
;;; UI/UX
(setq doom-theme 'doom-bisqwit-improved
      doom-font (font-spec :family "Berkeley Mono" :size 16 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 16))

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
      '(emacs-lisp-mode c-mode rjsx-mode typescript-mode web-mode))

;;
;;; Keybinds
(map! :leader
      :desc "Org Agenda"         "j"     #'org-agenda-list
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

(after! org
  (setq org-startup-folded 'show2levels
        org-ellipsis " [...] "
        ;; My org/org-roam capture templates
        ;; org-capture-templates
        ;; '(("t" "todo" entry (file+headline "todo.org" "Unsorted")
        ;;    "* [ ] %?\n%i\n%a"
        ;;    :prepend t)
        ;;   ("d" "deadline" entry (file+headline "todo.org" "Schedule")
        ;;    "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
        ;;    :prepend t)
        ;;   ("s" "schedule" entry (file+headline "todo.org" "Schedule")
        ;;    "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
        ;;    :prepend t))
        ))
(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))
        ;;   ("r" "thought" plain
        ;;    ,(format "#+title: ${title}\n%%[%s/template/thought.org]" org-roam-directory)
        ;;    :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
        ;;    :unnarrowed t)
        ;;   ("p" "project" plain
        ;;    ,(format "#+title: ${title}\n%%[%s/template/project.org]" org-roam-directory)
        ;;    :target (file "project/%<%Y%m%d>-${slug}.org")
        ;;    :unnarrowed t))
        ;; Use human readable dates for dailies titles

        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %B %d, %Y>\n\n"))
          ("a" "agenda" entry
           ;; need to figure out how not to hard code this path...
           (file "~/Sync/projects/org/roam/template/agenda.org")
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%A %B %d, %Y>\n\n")))))

;; set org-journal type to monthly
(after! org-journal
  (setq org-journal-file-type 'daily
        org-journal-date-format "%A %B %d, %Y"))

;; Start org-agenda at current date and show only 7 day outlook
(after! org-agenda
  (setq org-agenda-start-day "+0d"
        org-agenda-span 7))


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
  (lsp-ensure-server 'clangd))

;; Disable lsp auto formatting to prevent interference with tools like prettier
(setq +format-with-lsp nil)


;;; :lang web
;;choose extensions to open in web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . web-mode))

;; Force lsp to to recognize scss files in web-mode
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(web-mode . "scss")))


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
