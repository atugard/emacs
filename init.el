(load-file "~/.emacs.d/themes/dunno.el")
(load-file "~/.emacs.d/snippets/html.el")
(defun pkg-init ()
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
  (package-initialize)
  )
(pkg-init)

(defun install (p)
  (unless (package-installed-p p)
    (package-install p))
  (require p))

(install 'evil)
(evil-mode 1)

(install 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(install 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)

(install 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(install 'ibuffer-projectile)
(install 'ag)
(install 'ggtags)

(install 'auto-complete)
(ac-config-default)

(install 'hydra)

(install 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(install 'typescript-mode)

;;jk to exit insert mode
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
(define-key evil-insert-state-map (kbd "s") 'my-sd)
(defun linum-init ()
  (require 'linum-relative)
  (global-linum-mode 1)
  (linum-relative-on)
  (setq linum-relative-current-symbol ""))
(linum-init)


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
    (typescript-mode humanoid-themes linum-relative evil)))
 '(pdf-view-midnight-colors (quote ("#f8f8f2" . "#232629"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun graphics-init ()
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
(graphics-init)

(setq inhibit-startup-screen t)

;;show matching paren
(show-paren-mode 1)

;;Scroll one line at a time
(setq scroll-step 1)

(global-set-key (kbd "C-c s") (kbd "M-! st RET"))


;;Load hydras
(load-file "./.emacs.d/hydras/hydras.el")

