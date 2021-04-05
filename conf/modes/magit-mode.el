(require 'magit)

;; Display magit buffers in the current window, rather than a new window.
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))

;; Don't show the diff buffer when authoring a commit. To manually bring up the
;; diff, use C-c C-d.
;;
;; The `server-switch-hook' change is documented here:
;;
;;   <https://magit.vc/manual/magit/Performance.html>
;;
(setq magit-commit-show-diff nil)
(remove-hook 'server-switch-hook 'magit-commit-diff)

;; Display 20 commits, for example, in the recent commits section.
(setq magit-log-section-commit-count 20)

;; The magit status buffer sections are controlled by
;; `magit-status-sections-hook'. By default, it includes
;; `magit-insert-unpushed-to-upstream-or-recent', which displays the unpushed
;; commits OR the recent commits. Instead, always dispaly the recent commits
;; section (in addition to the unpushed commits section).
;;
;; (1) Replace the default `magit-insert-unpushed-to-upstream-or-recent' with
;; the dedicated upstream commits section...
(magit-add-section-hook
 'magit-status-sections-hook
 'magit-insert-unpushed-to-upstream
 'magit-insert-unpushed-to-upstream-or-recent
 'replace)

;; (2) Add the dedicated recent commits section...
(magit-add-section-hook
 'magit-status-sections-hook
 'magit-insert-recent-commits
 (car (last magit-status-sections-hook)) ;; Insert at the end.
 t)

;; (3) Because the default `magit-insert-unpushed-to-upstream-or-recent' is
;; removed, the transient actions `pu` is no longer valid. We have to replace
;; the action to jump to the dedicated upstream commits section.
(transient-replace-suffix 'magit-status-jump "pu"
  '("pu" "Unmerged into upstream" magit-jump-to-unpushed-to-upstream
     :if (lambda ()
           (memq 'magit-insert-unpushed-to-upstream
                 magit-status-sections-hook))))
