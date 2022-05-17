;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Very based on https://zzamboni.org/post/beautifying-org-mode-in-emacs/

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Nikita Tchayka"
      user-mail-address "nick@booster.cloud")

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
(setq doom-font (font-spec :family "Fira Code" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 24))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/notes")
(setq +org-capture-todo-file "~/org/inbox.org")


;; Modern org mode


(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
        (setq
         ;; Edit settings
         org-auto-align-tags nil
         org-tags-column 0
         org-catch-invisible-edits 'show-and-error
         org-special-ctrl-a/e t
         org-insert-heading-respect-content t

         ;; Org styling, hide markup etc.
         org-hide-emphasis-markers t
         org-pretty-entities t
         org-ellipsis "…"

         ;; Agenda styling
         org-agenda-block-separator ?─
         org-agenda-time-grid
         '((daily today require-timed)
           (800 1000 1200 1400 1600 1800 2000)
           " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
         org-agenda-current-time-string
         "⭠ now ─────────────────────────────────────────────────"))

(use-package! org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups ;; Each group has an implicit boolean OR operator between its selectors.
        '(
          (:name "Today"  ; Optionally specify section name
                 :time-grid t  ; Items that appear on the time grid
                 :todo "TODAY")  ; Items that have this TODO keyword
          (:name "Important"
                 ;; Single arguments given alone
                 :tag "bills"
                 :priority "A")
          ;; Set order of multiple groups at once
          (:order-multi (2 (:name "Shopping in town"
                                  ;; Boolean AND group matches items that match all subgroups
                                  :and (:tag "shopping" :tag "@town"))
                           (:name "Food-related"
                                  ;; Multiple args given in list with implicit OR
                                  :tag ("food" "dinner"))
                           (:name "Personal"
                                  :habit t
                                  :tag "personal")
                           (:name "Space-related (non-moon-or-planet-related)"
                                  ;; Regexps match case-insensitively on the entire entry
                                  :and (:regexp ("space" "NASA")
                                                ;; Boolean NOT also has implicit OR between selectors
                                                :not (:regexp "moon" :tag "planet")))))
          ;; Groups supply their own section names when none are given
          (:todo "WAITING" :order 8)  ; Set order of this section
          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                 ;; Show this group at the end of the agenda (since it has the
                 ;; highest number). If you specified this group last, items
                 ;; with these todo keywords that e.g. have priority A would be
                 ;; displayed in that group instead, because items are grouped
                 ;; out in the order the groups are listed.
                 :order 9)
          (:priority<= "B"
                       ;; Show this section after "Today" and "Important", because
                       ;; their order is unspecified, defaulting to 0. Sections
                       ;; are displayed lowest-number-first.
                       :order 1))))
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
  

(use-package! mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold))) ;; :foreground ,base-font-color


     (custom-theme-set-faces
      'user
      `(org-level-8 ((t (,@headline ,@variable-tuple))))
      `(org-level-7 ((t (,@headline ,@variable-tuple))))
      `(org-level-6 ((t (,@headline ,@variable-tuple))))
      `(org-level-5 ((t (,@headline ,@variable-tuple))))
      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.5 :underline nil))))))

(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
;;  '(fixed-pitch ((t ( :family "Fira Code" :height 160)))))

(add-hook! 'org-mode-hook #'doom-disable-line-numbers-h)

(after! org
  (setq org-capture-templates
               '(("i" "To Inbox" entry
                  (file +org-capture-todo-file)
                  "** TODO %?\n %i\n"
                  :prepend t :kill-buffer t))))

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
