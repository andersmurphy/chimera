;;; chimera.el --- Monstrous modal text editing for emacs.

;; Copyright © 2017 Anders Murphy <andersmurphy@gmail.com>

;; Author: Anders Murphy <andersmurphy@gmail.com>
;; Version: 0.1
;; Keywords: chimera, modal
;; URL: https://github.com/andersmurphy/chimera

;;; Commentary:

;;; Code:

(require 'hydra)
(require 'avy)

(global-set-key (kbd "f") 'chimera-change-state/body)

(defcustom chimera-leader-function 'show-no-leader-function-set-message
  "Optional leader function that can be bound to space."
  :group 'chimera
  :type 'function)

(defhydra chimera-change-state (:color blue
                              :body-pre (insert "f")
                              :timeout 0.4
                              :idle 0.5)
  ("d" (progn
         (delete-char -1)
         (setq deactivate-mark nil)
         (chimera/body))))

(defhydra chimera
  (:foreign-keys warn
   :columns 3
   :body-pre   (progn
                 (set-cursor-color "#EEAD0E")
                 (setq-default cursor-type 'box)
                 (chimera-region-current-char))
   :post  (progn
            (set-cursor-color "#66CD00")
            (setq-default cursor-type 'bar)))
  "Normal"
  ("a" chimera-insert-after-region "insert after" :exit t)
  ("d" chimera-delete-region "delete region")
  ("g" chimera-goto-word "goto word")
  ("h" chimera-region-previous-char "move left")
  ("j" chimera-region-below-char "move down")
  ("k" chimera-region-above-char "move up")
  ("l" chimera-region-next-char "move right")
  ("i" chimera-insert-before-region "insert before" :exit t)
  ("<SPC>" (funcall chimera-leader-function) "leader" :exit t))

(defun chimera-region-previous-char ()
  "Create a region on the char behind the current point.
Does nothing if the point is at the beginning of the buffer"
  (interactive)
  (when (not (bobp))
    (call-interactively 'set-mark-command)
    (call-interactively 'backward-char)))

(defun chimera-region-next-char ()
  "Create a region on the char in front of the current point.
Does nothing if the point is at the end of the buffer."
  (interactive)
  (when (not (eobp))
    (call-interactively 'forward-char)
    (call-interactively 'forward-char)
    (call-interactively 'chimera-region-previous-char)))

(defun chimera-region-current-char ()
  "Create a region on the char in front of the current point.
Does nothing if the point is at the end of the buffer."
  (interactive)
  (when (not (eobp))
    (call-interactively 'forward-char)
    (call-interactively 'chimera-region-previous-char)))

(defun chimera-region-above-char ()
  "Create a region on the char above and in front of the current point."
  (interactive)
  (when (not (chimera-is-first-line-in-buffer))
    (call-interactively 'previous-line)
    (call-interactively 'forward-char)
    (call-interactively 'chimera-region-previous-char)))

(defun chimera-region-below-char ()
  "Create a region on the char below and in front of the current point."
  (interactive)
  (when (not (chimera-is-last-line-in-buffer))
    (call-interactively 'next-line)
    (if (not (eobp))
        (progn
           (call-interactively 'forward-char)
           (call-interactively 'chimera-region-previous-char))
      (call-interactively 'set-mark-command))))

(defun chimera-is-first-line-in-buffer ()
  "Return true if point is on the first line in the buffer."
  (equal (count-lines (point-min) (point)) 0))

(defun chimera-is-last-line-in-buffer ()
  "Return true if point is on the last line in the buffer."
  (equal (count-lines (point) (point-max)) 0))

(defun chimera-insert-after-region ()
  "Move point to end of region."
  (interactive)
  (if (region-active-p)
      (goto-char (region-end))
    (goto-char (point)))
  (deactivate-mark t))

(defun chimera-insert-before-region ()
  "Move point to end of region."
  (interactive)
  (if (region-active-p)
      (goto-char (region-beginning))
    (goto-char (point)))
  (deactivate-mark t))

(defun chimera-goto-word ()
  "Deactivate mark and then call avy go to word."
  (interactive)
  (deactivate-mark t)
  (call-interactively 'avy-goto-word-1))

(defun chimera-delete-region ()
  "Delete region current region.
Then create region around next char."
  (interactive)
  (call-interactively 'kill-region)
  (if (eobp)
      (call-interactively 'chimera-region-previous-char)
    (call-interactively 'chimera-region-current-char)))

(provide 'chimera)

;;; chimera.el ends here
