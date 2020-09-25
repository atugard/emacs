
;;=================definitions=================
(defun pkg-init ()
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
  (package-initialize))
(defun install (p)
  (unless (package-installed-p p)
    (package-install p))
  (require p))

(defun install-packages (pkgs)
  (when (car pkgs)
    (progn (install (car pkgs))
	   (install-packages (cdr pkgs)))))
(defun my-sd ()
  (interactive)
  (let* ((initial-key ?s)
	 (final-key ?d)
	 (timeout 0.5)
	 (event (read-event nil nil timeout)))
    (if event
	;; timeout met
	(if (and (characterp event) (= event final-key))
	    (evil-normal-state)
	  (insert initial-key)
	  (push event unread-command-events))
      ;; timeout exceeded
      (insert initial-key))))
(defun linum-init ()
  (require 'linum-relative)
  (global-linum-mode 1)
  (linum-relative-on)
  (setq linum-relative-current-symbol ""))

(defun custom ()
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ansi-color-faces-vector
     [default default default italic underline success warning error])
   '(ansi-color-names-vector
     ["#484e54" "#f11235" "#02d849" "#ffb627" "#00a6fb" "#e834f9" "#1de9b6" "#f8f8f2"])
   '(custom-enabled-themes nil)
   '(custom-safe-themes
     (quote
      ("e4b0b3402412fa850ae8575e761a5f8b02a1c108fb2359ab4f4977537bb64531" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" default)))
   '(hl-todo-keyword-faces
     (quote
      (("TODO" . "#e834f9")
       ("NEXT" . "#e834f9")
       ("THEM" . "#0ed1d1")
       ("PROG" . "#1de9b6")
       ("OKAY" . "#00a6fb")
       ("DONT" . "#ff9505")
       ("FAIL" . "#f11235")
       ("DONE" . "#02d849")
       ("NOTE" . "#ffb627")
       ("KLUDGE" . "#fb6107")
       ("HACK" . "#a359fe")
       ("TEMP" . "#c0c0bd")
       ("FIXME" . "#e834f9")
       ("XXX" . "#0ed1d1")
       ("XXXX" . "#0ed1d1")
       ("\\?\\?\\?+" . "#0ed1d1"))))
   '(package-selected-packages
     (quote
      (humanoid-themes linum-relative evil)))
   '(pdf-view-midnight-colors (quote ("#f8f8f2" . "#232629"))))
  (custom-set-faces))

(defun keys ()
  (evil-define-key 'normal ibuffer-mode-map
    (kbd "l") 'ibuffer-visit-buffer)
  (evil-define-key 'normal bookmark-bmenu-mode-map
    (kbd "l") 'bookmark-bmenu-select)
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map
    (kbd "l") 'dired-find-file)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key evil-insert-state-map (kbd "s") 'my-sd)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key evil-insert-state-map (kbd "TAB") 'dabbrev-expand)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-h")  'windmove-left)
  (global-set-key (kbd "C-l") 'windmove-right)
  (global-set-key (kbd "C-k")    'windmove-up)
  (global-set-key (kbd "C-j")  'windmove-down)
  (global-set-key (kbd "C-S-k") 'enlarge-window)
  (global-set-key (kbd "C-S-j") 'shrink-window))

(defun graphics ()
  (if (display-graphic-p)
      (setq initial-frame-alist
	    '(
	      (font . "iosevka")
	      (alpha . 90)
	      (menu-bar-lines . 0)
	      (tool-bar-lines . 0)
	      (width . 106)
	      (height . 60)
	      (left . 50)
	      (top . 50)))
    (setq initial-frame-alist '( (tool-bar-lines . 0))))
  (setq default-frame-alist initial-frame-alist))
(defun add-pretty-lambda ()
  (setq prettify-symbols-alist
	'(
	  ("lambda" . 955) ; λ
	  ("->" . 8594)    ; →
	  ("=>" . 8658)    ; ⇒
	  ("map" . 8614)    ; ↦
	  )))
(defun hooks ()
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'racket-mode-hook 'add-pretty-lambda)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'add-pretty-lambda))
(defun misc-options ()
  (setq inhibit-startup-screen t)
  (evil-set-initial-state 'ibuffer-mode 'normal) 
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal) 
  (ac-config-default)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (show-paren-mode 1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (projectile-mode +1)
  (evil-mode 1)
  (helm-mode 1)
  (desktop-save-mode 1)
  ;;Scroll one line at a time
  (setq scroll-step 1)
  (global-prettify-symbols-mode 1)
  (let ((basedir "~/.emacs.d/themes/"))
      (dolist (f (directory-files basedir))
        (if (and (not (or (equal f ".") (equal f "..")))
                 (file-directory-p (concat basedir f)))
            (add-to-list 'custom-theme-load-path (concat basedir f)))))

  (load-theme 'hello t)
  )

;;=================init=================
(pkg-init)
(install-packages '(haskell-mode racket-mode evil helm projectile ibuffer-projectile ag ggtags auto-complete hydra js2-mode org-bullets tablist))
;;(custom)
(keys)
(graphics)
(hooks)
(linum-init)
(misc-options)

;;=================load hydras=================
(load-file "./.emacs.d/hydras/hydras.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c30b896176fc5e33cb5d6e2bb63faeeb1ef64ce5e4d30bc65f17c78415eae0ac" "e4b0b3402412fa850ae8575e761a5f8b02a1c108fb2359ab4f4977537bb64531" "c6c47de581524f53ad7b0ee2b42a245811fdfc82d549b98fef0615e69746a29a" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
