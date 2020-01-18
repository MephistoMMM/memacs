;;; lang/nix/autoload.el -*- lexical-binding: t; -*-

(defun +nix--options-action (candidate)
  (switch-to-buffer-other-window
   (nixos-options-doc-buffer
    (nixos-options-get-documentation-for-option candidate))))

;;;###autoload
(defun +nix/lookup-option (&optional initial-input)
  "Look up documentation on a nix option."
  (interactive
   (list
    ;; REVIEW Must be a better way to do this
    (when (and (looking-at-p "[a-zA-Z0-9-_\\.]")
               (not (doom-point-in-string-or-comment-p)))
      (buffer-substring-no-properties
       (save-excursion
         (skip-chars-backward "^ ")
         (point))
       (save-excursion
         (skip-chars-forward "^ ")
         (point))))))
  (cond ((featurep! :completion helm)
         (require 'helm-nixos-options)
         ;; REVIEW We reimplment `helm-nixos-options' so we can supply
         ;; `initial-input'. Maybe use `helm-attrset' instead?
         (helm :sources `(,(helm-source-nixos-options-search))
               :buffer "*helm-nixos-options*"
               :input initial-input))
        ((featurep! :completion ivy)
         (require 'nixos-options)
         (ivy-read "NixOS options: "
                   nixos-options
                   :require-match t
                   :initial-input initial-input
                   :action #'+nix--options-action
                   :caller '+nix/options))
        ;; TODO Add general `completing-read' support
        ((user-error "No search engine is enabled. Enable helm or ivy!")))
  ;; Tell lookup module to let us handle things from here
  'deferred)
