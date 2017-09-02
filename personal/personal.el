;;; summary --- summary

;;; Commentary:

;;; Code:

;; load things onto exec path
(defvar extra-on-path
  '("/home/dan/.cask/bin/cask"
    "/home/dan/bin"
    "/home/dan/racket/bin/racket"))

(setq exec-path (append exec-path extra-on-path))

;; load local version of cider
(add-to-list 'load-path "~/projects/cider")
(require 'cider)

;; make cider font lock as much as possible
(setq cider-font-lock-dynamically t)

;; load local version of elfeed
(add-to-list 'load-path "~/projects/elfeed")
(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds
      '(("http://endlessparentheses.com/atom.xml" emacs elisp)
        ("http://feeds2.feedburner.com/StuartSierra" clojure)
        ("https://jeremykun.com/feed/" math programming)
        ("http://lambda-the-ultimate.org/rss.xml" languages)
        ("http://swannodette.github.io/atom.xml" clojurescript)
        ("http://feeds.feedburner.com/SoftwareByRob" programming)
        ("https://rjlipton.wordpress.com/feed/" math godel)
        ("http://feeds.feedburner.com/Kodeknight" algorithms)
        ("http://danluu.com/atom.xml" programming)
        ("http://jvns.ca/atom.xml" programming)
        ("http://tromey.com/blog/?feed=rss2" emacs java)))

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

(defun multiple-mode-add-hook (modes hook)
  "Given a list of x-mode-hook symbols in MODE, add the HOOK to them."
  (mapc (lambda (mode) (add-hook mode hook)) modes))

(defun hook-up-modes (environments hook)
  (let ((modes (mapcar #'intern (append-suffix "-mode-hook" environments))))
    (mapc (lambda (mode) (add-hook mode hook))
          modes)))

;; clj-refactor
(defun additional-clojure-environment ()
  (yas-minor-mode 1)
  ; (cljr-add-keybindings-with-prefix "C-c C-j")
  )

(add-hook 'clojure-mode-hook 'additional-clojure-environment)

(hook-up-modes my-lisps 'standard-lisp-environment)
(hook-up-modes my-text-environments 'standard-text-environment)

;; elpy for python
(package-initialize)
(elpy-enable)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers 'slime-company)

;; resize window settings
(add-to-list 'load-path "~/projects/resize-window")
(require 'resize-window)

(defun my-projectile-find-file ()
  (resize-window--delete-overlays)
  (funcall-interactively 'helm-projectile-find-file)
  (resize-window--create-overlay))

(setq resize-window-swap-capital-and-lowercase-behavior t)
(resize-window-add-choice ?l #'helm-mini "helm mini")
(resize-window-add-choice ?t #'my-projectile-find-file "Projectile find file")
(resize-window-add-choice ?h (lambda () (dired "~/projects/clojure"))
                          "Visit the clojure directory")
(global-set-key (kbd "C-c ;") 'resize-window)
(global-set-key (kbd "C-c C-;") 'resize-window)
;; org mode is stingy with its key mapping
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c ;") 'resize-window)))

(slime-setup '(slime-company))
(slime-setup '(slime-repl))
(add-to-list 'slime-contribs 'slime-repl)




;; helm projectile mode
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;; ace setting
(global-set-key (kbd "M-p") 'ace-window)

;;; tern auto complete for javascript
(require 'tern)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(require 'slime)
(setq slime-contribs '(slime-scratch slime-editing-commands))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


;;(setq inferior-lisp-program "/usr/bin/sbcl")
(setq inferior-lisp-program "/usr/bin/clisp")

;;  off and line numbers
(setq scroll-margin 6)
;; (global-linum-mode t)
;; (setq linum-format " %3i")

;;; helm mode info
(require 'helm-config)
(require 'helm-swoop)
(helm-mode 1)

;; bind helm mode to M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

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

;; fuzzy searches
(setq helm-M-x-fuzzy-match nil)
(setq helm-buffers-fuzzymatching nil)


;; loccur settings
(require 'loccur)
(define-key global-map [(control o)] 'loccur-current)

;; yas for helm
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c l") 'helm-yas-complete)
(yas-global-mode 1)

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

(defvar my-clojure-directory "/home/dan/projects/clojure/")
(defun my-cloned-name (url)
  (s-replace ".git" ""
             (car (last (s-split "/" url)))))

(defun my-git-clone-clojure (url)
  (interactive "s")
  (let* ((cmd (format "cd %s && git clone %s"
                     my-clojure-directory url)))
    (shell-command cmd)
    (dired (concat (file-name-as-directory my-clojure-directory)
                   (my-cloned-name url)))))

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
         (reference "~/law/1L/reference.docx")
         (command (format "pandoc -s %s -o %s --reference-docx %s" filename doc-name
                          reference)))
    (shell-command command)))
;;; personal.el ends here
