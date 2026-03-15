;; Enable enhanced automatic buffer saving
(require 'auto-save-buffers-enhanced)

(auto-save-buffers-enhanced t)

;; Do not restrict auto-save to version-controlled files
(auto-save-buffers-enhanced-include-only-checkout-path nil)

;; Toggle auto-save activity with C-x x w
(global-set-key "\C-xxw" 'auto-save-buffers-enhanced-toggle-activity)
