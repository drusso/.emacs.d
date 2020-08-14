(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages '(
   clojure-mode
   csv-mode
   diff-hl
   ess
   expand-region
   flx-ido
   flx-isearch
   haml-mode
   highlight-indent-guides
   ido-completing-read+
   js2-mode
   json-mode
   magit
   molokai-theme
   monokai-theme
   markdown-mode
   multiple-cursors
   projectile
   rich-minority
   rust-mode
   smart-mode-line
   smex
   swift-mode
   yaml-mode
   ))

(package-initialize)
(package-install-selected-packages)
