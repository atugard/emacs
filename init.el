(load-file "~/.emacs.d/themes/cyberpunk-theme.el")
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
 '(package-selected-packages (quote (linum-relative evil))))
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
	      (background-color . "#150f1c")
	      (background-mode "dark")
	      (foreground-color . "#E0DFDB")
	      (left . 50)
	      (top . 50)))
    (setq initial-frame-alist '( (tool-bar-lines . 0))))
  (setq default-frame-alist initial-frame-alist))
(graphics-init)


;;show matching paren
(show-paren-mode 1)

;;Scroll one line at a time
(setq scroll-step 1)

;;Load hydras
(load-file "./.emacs.d/hydras/hydras.el")

