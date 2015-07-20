;;
;; Casey Zednick's init.el  
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      auto-complete
                      color-theme
                      twilight-theme
                      markdown-mode
		      haskell-mode
                      p4)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
     (package-install p)))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(put 'downcase-region 'disabled nil)
(setq-default ispell-program-name "/usr/bin/env aspell")

(setq c-basic-indent 4)
(setq tab-width 4)
(setq-default indent-tabs-mode nil) ;; No tabs anywhere!

(set-face-attribute 'default nil :font "-apple-Menlo-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1" :height 140)
;; No visual bell
(setq ring-bell-function 'ignore)
(menu-bar-mode 1)

(setq color-theme-is-global t)
(load-theme 'twilight t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq compilation-window-height 16) ;; Normaly takes half the window.
(setq gdb-window-height 16) ;; Normaly takes half the window.
