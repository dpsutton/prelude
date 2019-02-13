;;; summary --- summary

;;; Commentary:

;;; Code:

;;(package-initialize)

(defvar my-packages
  '(geiser
    auto-highlight-symbol
    yasnippet
    s
    ;; CIDER deps
    spinner
    queue
    sesman
    ;; end CIDER deps
    slime
    slime-company
    loccur
    rainbow-delimiters
    paredit
    ivy
    counsel
    swiper
    moody
    minions
    gist
    racer
    rust-mode
    cargo
    flycheck-rust
    company-quickhelp))

(defun my-ensure-installed (package)
  (unless (package-installed-p package)
    (package-install package)))

(mapc #'my-ensure-installed my-packages)

(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'default (selected-frame) :height 160)))

(show-paren-mode)
(global-auto-highlight-symbol-mode t)

(mapc (lambda (mode)
        (add-to-list 'ahs-modes mode))
      '(clojure-mode clojurescript-mode cider-repl-mode))
(yas-global-mode)

;; load things onto exec path
(defvar extra-on-path
  '("/home/dan/.cask/bin/cask"
    "/home/dan/bin"))

(setq exec-path (append exec-path extra-on-path))

(global-company-mode)

(add-to-list 'load-path "~/projects/dev/clojure-mode")
(require 'clojure-mode)

;; load local version of cider
(add-to-list 'load-path "~/projects/dev/cider")
(require 'cider)

(defun cider-copy-jack-in-command ()
  (let ((cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
        (cider-jack-in-lein-plugins (append cider-jack-in-lein-plugins cider-jack-in-cljs-lein-plugins))
        (cider-jack-in-nrepl-middlewares (append cider-jack-in-nrepl-middlewares cider-jack-in-cljs-nrepl-middlewares))
        (orig-buffer (current-buffer)))
    (kill-new (plist-get (cider--update-jack-in-cmd (cider--update-project-dir '()))
                         :jack-in-cmd))))

(add-to-list 'load-path "~/projects/dev/inf-clojure")
(require 'inf-clojure)

;; make cider repl indent and newline on enter and eval on
;; control-enter

(define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
(define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)

;; make inserting eval by default and don't move point to repl

(setq cider-invert-insert-eval-p t)
(setq cider-switch-to-repl-after-insert-p nil)
(setq clojure-toplevel-inside-comment-form t)

(require 'geiser-repl)
(define-key geiser-repl-mode-map (kbd "RET") #'paredit-newline)
(define-key geiser-repl-mode-map [return] 'paredit-newline)
(define-key geiser-repl-mode-map "\C-m" #'paredit-newline)
(define-key geiser-repl-mode-map (kbd "C-<return>") #'geiser-repl--maybe-send)

;; make cider font lock as much as possible
(setq cider-font-lock-dynamically t)

;; don't show the error buffer
(setq cider-show-error-buffer nil)

;; (setq elfeed-feeds
;;       '(("http://endlessparentheses.com/atom.xml" emacs elisp)
;;         ("http://feeds2.feedburner.com/StuartSierra" clojure)
;;         ("https://jeremykun.com/feed/" math programming)
;;         ("http://lambda-the-ultimate.org/rss.xml" languages)
;;         ("http://swannodette.github.io/atom.xml" clojurescript)
;;         ("http://feeds.feedburner.com/SoftwareByRob" programming)
;;         ("https://rjlipton.wordpress.com/feed/" math godel)
;;         ("http://feeds.feedburner.com/Kodeknight" algorithms)
;;         ("http://danluu.com/atom.xml" programming)
;;         ("http://jvns.ca/atom.xml" programming)
;;         ("http://tromey.com/blog/?feed=rss2" emacs java)))

;; turn off the system bell
(setq ring-bell-function 'ignore)

(defvar my-lisps '(clojure lisp emacs-lisp cider-repl geiser geiser-repl racket scheme slime repl))
(defvar my-text-environments '(org markdown))

(defun standard-lisp-environment ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun standard-text-environment ()
  (visual-line-mode)
  (whitespace-mode -1))

(defun append-suffix (suffix phrases)
  "take SUFFIX and append it to each of the PHRASES."
  (mapcar #'(lambda (phrase) (concat (symbol-name phrase) suffix)) phrases))

(defun hook-up-modes (environments hook)
  (mapc (lambda (mode) (add-hook mode hook))
        (mapcar (lambda (env) (intern (format "%s-mode-hook" env)))
                environments)))

(hook-up-modes my-lisps 'standard-lisp-environment)
(hook-up-modes my-text-environments 'standard-text-environment)

;; company tip stuff
(company-quickhelp-mode)
(setq company-quickhelp-use-propertized-text t)
(setq company-quickhelp-delay 0.2)

;; resize window settings
(add-to-list 'load-path "~/projects/dev/resize-window")
(require 'resize-window)

(setq resize-window-swap-capital-and-lowercase-behavior t)
(resize-window-add-choice ?l #'ivy-switch-buffer "switch buffers with ivy")
(resize-window-add-choice ?a #'counsel-git "Search git files")
(resize-window-add-choice ?h (lambda () (dired "~/projects/clojure"))
                          "Visit the clojure directory")
(resize-window-add-choice ?u (lambda () (dired "~/ops/projects"))
                          "Work projects")
(resize-window-add-choice ?d (lambda () (dired "~/projects/dev"))
                          "Visit dev directoryq")
(resize-window-add-choice ?m (lambda () (resize-window--window-push))
                          "Push window state onto window stack")
(resize-window-add-choice ?s #'counsel-git "Counsel git")
(global-set-key (kbd "C-c ;") 'resize-window)
(global-set-key (kbd "C-c C-;") 'resize-window)
;; org mode is stingy with its key mapping
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c ;") 'resize-window)))

;; ivy config
(defun my-ag-at-point ()
  (interactive)
  (let ((current-word (thing-at-point 'symbol)))
    (counsel-ag current-word)))

(setq ivy-initial-inputs-alist '())

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-r") #'my-ag-at-point)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(slime-setup '(slime-company))

;; modeline customizations
(require 'moody)
(setq x-underline-at-descent-line t)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)

(require 'minions)
(minions-mode)


(require 'gist)
(setq gist-list-format
      '((id "Id" 10 nil identity)
        (files "Name" 30 nil identity)
        ;; (created "Created" 20 nil "%D %R")
        (visibility "Visibility" 10 nil
                    (lambda (public)
                      (or (and public "public")
                          "private")))
        (description "Description" 0 nil identity)))

(mapc (lambda (e) (add-to-list 'gist-supported-modes-alist e))
      '((clojurescript-mode "cljs")
        (clojure-mode "cljc")))


;;; ace setting
(global-set-key (kbd "M-p") 'ace-window)

(require 'slime)
(setq slime-contribs '(slime-scratch slime-editing-commands))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


(setq inferior-lisp-program "/usr/bin/sbcl")

;;  off and line numbers
(setq scroll-margin 6)

;; copy things for the web by appending four spaces beforehand
(defun copy-defun-for-web ()
  (interactive)
  (let (beg end)
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (setq beg (point))
      (let ((text (buffer-substring beg end)))
        (with-temp-buffer
          (insert text)
          (indent-rigidly (point-min) (point-max) 4)
          (let ((beginning (point-min)))
            (kill-region beginning (point)))))))
  (message "Defun copied to kill ring and system clip"))

(global-set-key (kbd "C-c C-w") 'copy-defun-for-web)

(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll))


;; loccur settings
(require 'loccur)
(define-key global-map [(control o)] 'loccur-current)


(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; generate tags
(setq path-to-ctags "/usr/local/bin/etags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s  -R %s" path-to-ctags (directory-file-name dir-name))))

;; which key support
(require 'which-key)
(which-key-mode)

;; shutdown server
(defun shutdown-server ()
  "Kill the running emacs server.
When running emacs client, easily kill the server without ps aux,
pkill, etc."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defvar my-clojure-directory "~/projects/clojure/")
(defvar my-go-directory "~/projects/go/")

(defun my-cloned-name (url)
  (s-replace ".git" ""
             (car (last (s-split "/" url)))))

(defun my-directory-for-type (type)
  (case type
    ('clojure my-clojure-directory)
    ('go my-go-directory)))

(defun my-git-clone (type url)
  (let* ((dir (my-directory-for-type type))
         (cmd (format "cd %s && git clone %s" dir url)))
    (shell-command cmd)
    (dired (concat (file-name-as-directory dir) (my-cloned-name url)))))

(defun my-git-clone-clojure (url)
  (interactive "s")
  (my-git-clone 'clojure url))

(defun my-git-clone-go (url)
  (interactive "s")
  (my-git-clone 'go url))

(defun cider-debug-create-local-let (start end)
  "During debugging, grab the locally bound vars and create a let
  binding. Place this let binding in the kill ring for future use."
  (interactive "r")
  (if cider--debug-mode-response
      (nrepl-dbind-response cider--debug-mode-response (locals)
        (let* ((code (buffer-substring-no-properties start end))
               (bindings (apply #'append locals))
               (formatted-bindings (mapconcat 'identity bindings " ")))
          (kill-new (format "(let [%s]\n %s)" formatted-bindings code))
          (message "copied let form to kill ring")))
    (message "No debugging information found.")))



;; export org case briefs to word
(defun export-word ()
  (interactive)
  (let* ((buf (current-buffer))
         (filename (file-name-nondirectory (buffer-file-name buf)))
         (basename (file-name-sans-extension filename))
         (doc-name (format "%s.docx" basename))
         (command (format "pandoc -s %s -o %s" filename doc-name)))
    (shell-command command)))

(defun scrabble-phrase (phrase)
  (interactive "sPhrase to scrabble: ")
  (let* ((characters (s-split "" phrase t))
         (memed (mapcar (lambda (char)
                          (cond ((s-blank-str? char)
                                 ":scrabble-blank:")
                                ((s-matches? "[a-zA-Z]" char)
                                 (format ":scrabble-%s:" (s-downcase char)))
                                (t ":scrabble-blank:")))
                        characters))
         (finished (s-join " " memed)))
    (kill-new finished)
    (message (format "Copied: %s" (s-truncate 60 finished)))))

(defun clap-phrase (phrase)
  (interactive "s")
  (let ((words (s-split-words phrase)))
    (kill-new (s-join " :clap: " words))))

(defun rules-engine-ip-string ()
  (interactive)
  (let ((ip-address (shell-command-to-string  "hostname -I | cut -d ' ' -f 1 | xargs")))
    (kill-new (format "http://%s:8084" (s-replace "\n" "" ip-address)))))

(defun my-phi ()
  (interactive)
  (insert "(def phi (-> server :_cache (get \"phi-fhir-test\") deref :conn))\n")
  (insert "(defn what [id] (d/pull (d/db phi) '[*] id))"))

(setq dired-listing-switches "-alh")
;;; personal.el ends here

