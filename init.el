;; close welcome page
(setq inhibit-startup-screen t)
(setq apropos-sort-by-scores t)
;; change backup and cache directory
(setq backup-directory-alist '(("." . "~/.cache/emacs/")))
(setq auto-save-file-name-transforms
  `((".*" "~/.cache/emacs/" t)))

(save-place-mode)
(load-theme 'tango-dark t)
(setq custom-safe-themes t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(windmove-default-keybindings)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(show-paren-mode 1)

;; show buffer line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; define sudo function
(defun sudo ()
    "Use TRAMP to `sudo' the current buffer"
    (interactive)
    (when buffer-file-name
	(find-alternate-file
	(concat "/sudo:root@localhost:"
	    buffer-file-name))))








;; Set up package.el to work with MELPA
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(require 'use-package) 
(setq use-package-always-ensure t)


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package swiper)
;; (use-package counsel)
(use-package popup)
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-file-jump-from-find)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(require 'popup)


(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer yancy/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))



(yancy/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "w"  '(save-buffer :which-key "save buffer")
  "q"  '(save-buffers-kill-terminal :which-key "save buffer and exit buffer")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(defun ivy-display-function-popup (text)
  (with-ivy-window
    (popup-tip
     (setq ivy-insert-debug
           (substring text 1))
     :nostrip t)))

;; (setq ivy-display-functions-alist
      ;; '((counsel-M-x . ivy-display-function-popup)
        ;; (ivy-completion-in-region . ivy-display-function-overlay)))
(defun yancy/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)

;;  :hook (evil-mode . yancy/evil-hook)

  :config ;; tweak evil after loading it
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (define-key evil-motion-state-map ";" 'evil-ex))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package undo-fu
  :ensure t)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(yancy/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 8)))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package doom-themes
  :init (load-theme 'doom-gruvbox))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Workspace")
    (setq projectile-project-search-path '("~/Workspace")))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode 1))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" default)))
 '(package-selected-packages
   (quote
    (counsel-projectile projectile general doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline all-the-icons popup counsel swiper ivy evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


