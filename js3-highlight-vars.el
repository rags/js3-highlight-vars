;;; js3-highlight-vars.el --- highlight occurrences of the variable under cursor

;; Copyright (C) 2009  Free Software Foundation, Inc.

;; Author:  Mihai Bazon <mihai.bazon@gmail.com>

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'js3-mode)

(deflocal js3-highlight-vars-mode nil)

(defface js3-highlight-vars-face
  `((((class color) (background light))
     (:background "light green"))
    (((class color) (background dark))
     (:background "royal blue")))
  "Face for highlighting variables"
  :group 'js3-mode)

(defface js3-highlight-vars-second-face
  `((((class color) (background light))
     (:background "light pink"))
    (((class color) (background dark))
     (:background "blue violet")))
  "Face for highlighting variables"
  :group 'js3-mode)

(defvar js3-highlight-vars-local-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n")       'js3-highlight-vars-next)
    (define-key map (kbd "C-<down>")  'js3-highlight-vars-next)
    (define-key map (kbd "M-p")       'js3-highlight-vars-prev)
    (define-key map (kbd "C-<up>")    'js3-highlight-vars-prev)
    (define-key map (kbd "M-r")       'js3-highlight-vars-rename)
    map))

(deflocal js3--highlight-vars-tokens nil)
(deflocal js3--highlight-vars-current-token nil)
(deflocal js3--highlight-vars-current-token-name nil)
(deflocal js3--highlight-vars-post-command-timer nil)

(defun js3--do-highlight-vars ()
  "Highlight variable under cursor within the defining scope"
  (interactive)
  (setq js3--highlight-vars-post-command-timer nil)
  (unless js3--highlight-vars-tokens
    (let ((node (js3-node-at-point))
          (tokens nil)
          name
          scope)
      (unless (js3-name-node-p node)
        (setq node (js3-node-at-point (- (point) 1))))
      (when (and node (js3-name-node-p node))
        (setq scope (js3-node-get-enclosing-scope node)
              name (js3-name-node-name node)
              js3--highlight-vars-current-token (js3-node-abs-pos node)
              js3--highlight-vars-current-token-name name)
        (setq scope (js3-get-defining-scope scope name))
        (js3-with-unmodifying-text-property-changes
          (js3-visit-ast
           scope
           (lambda (node end-p)
             (when (and (not end-p)
                        (js3-name-node-p node)
                        (string= name (js3-name-node-name node)))
               (let* ((beg (js3-node-abs-pos node))
                      (end (+ beg (js3-node-len node)))
                      (new-scope (js3-node-get-enclosing-scope node))
                      (new-scope (js3-get-defining-scope new-scope name))
                      (ovl (make-overlay beg end)))
                 (add-to-list 'tokens beg t)
                 (overlay-put ovl 'keymap js3-highlight-vars-local-keymap)
                 (overlay-put ovl 'face
                              (if (eq new-scope scope)
                                  'js3-highlight-vars-face
                                'js3-highlight-vars-second-face))
                 (overlay-put ovl 'evaporate t)
                 (overlay-put ovl 'js3-highlight-vars t)))
             t)))
        (setq js3--highlight-vars-tokens tokens)
        (top-level)))))

(defun js3-highlight-vars-next ()
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (diff (- (point) js3--highlight-vars-current-token))
        (next (catch 'done
                (dolist (pos js3--highlight-vars-tokens)
                  (when (> pos (point))
                    (throw 'done pos))))))
    (when next
      (setq js3--highlight-vars-current-token next)
      (goto-char next)
      (forward-char diff))))

(defun js3-highlight-vars-prev ()
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (diff (- (point) js3--highlight-vars-current-token))
        (prev (catch 'done
                (dolist (pos (reverse js3--highlight-vars-tokens))
                  (when (and (< pos (point))
                             (not (= pos js3--highlight-vars-current-token)))
                    (throw 'done pos))))))
    (when prev
      (setq js3--highlight-vars-current-token prev)
      (goto-char prev)
      (forward-char diff))))

(defun js3-highlight-vars-rename (new-name)
  (interactive "*sRename variable to: ")
  (let ((len (length js3--highlight-vars-current-token-name))
        (inhibit-point-motion-hooks t)
        (ovl (make-overlay 1 1))
        (all nil)
        doit)
    (overlay-put ovl 'face 'highlight)
    (dolist (pos (mapcar (lambda(pos)
                           (let ((m (make-marker)))
                             (set-marker m pos))) js3--highlight-vars-tokens))
      (goto-char pos)
      (move-overlay ovl pos (+ pos len))
      (setq doit (if all
                     ?y
                   (read-char "Replace this occurrence? (y/n/!)")))
      (when (= doit ?!)
        (setq all t
              doit ?y))
      (when (= doit ?y)
        (insert new-name)
        (delete-char len)))
    (delete-overlay ovl)))

(defun js3--unhighlight-vars (&rest ignore)
  (setq js3--highlight-vars-tokens nil
        js3--highlight-vars-current-token nil)
  (remove-overlays (point-min) (point-max)
                   'js3-highlight-vars t))

(defun js3-highlight-vars-post-command-hook ()
  (ignore-errors
    (let* ((overlays (overlays-at (point)))
           (ovl (and overlays
                     (catch 'found
                       (dolist (ovl overlays)
                         (when (overlay-get ovl 'js3-highlight-vars)
                           (throw 'found ovl)))
                       nil))))
      (if (and ovl
               (string= js3--highlight-vars-current-token-name
                        (buffer-substring (overlay-start ovl)
                                          (overlay-end ovl))))
          (setq js3--highlight-vars-current-token (overlay-start ovl))
        (js3--unhighlight-vars)
        (when js3--highlight-vars-post-command-timer
          (cancel-timer js3--highlight-vars-post-command-timer))
        (setq js3--highlight-vars-post-command-timer
              (run-with-timer 0.5 nil 'js3--do-highlight-vars))))))

;;;###autoload
(define-minor-mode js3-highlight-vars-mode
  "Minor mode that highlights occurrences of the variable under
cursor in js3-mode buffers"
  nil " vars" nil
  (if js3-highlight-vars-mode
      (progn
        (add-hook 'post-command-hook 'js3-highlight-vars-post-command-hook nil t))
    (remove-hook 'post-command-hook 'js3-highlight-vars-post-command-hook t)
    (js3--unhighlight-vars)
    (kill-local-variable js3--highlight-vars-tokens)
    (kill-local-variable js3--highlight-vars-current-token)))

(provide 'js3-highlight-vars)
