(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages '(
   clojure-mode
   csv-mode
   diff-hl
   ess
   expand-region
   haml-mode
   highlight-indent-guides
   js2-mode
   json-mode
   magit
   molokai-theme
   monokai-theme
   markdown-mode
   multiple-cursors
   projectile
   rust-mode
   smex
   swift-mode
   yaml-mode

   ;; The ivy trio + a counsel integration with projectile.
   ivy
   swiper
   counsel
   counsel-projectile
   ))

(package-initialize)
(package-install-selected-packages)
