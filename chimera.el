;;; chimera.el --- Monstrous modal text editing for emacs.

;; Copyright © 2017 Anders Murphy <andersmurphy@gmail.com>

;; Author: Anders Murphy <andersmurphy@gmail.com>
;; Version: 0.1
;; Keywords: chimera, modal
;; URL: https://github.com/andersmurphy/chimera

;;; Commentary:

;;; Code:

(require 'hydra)
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
         (chimera/body)
         (region-current-char))))

(defhydra chimera
  (:foreign-keys warn
   :columns 3
   :body-pre   (progn
                 (set-cursor-color "#EEAD0E")
                 (setq-default cursor-type 'box))
   :post  (progn
            (set-cursor-color "#66CD00")
            (setq-default cursor-type 'bar)))
  "NORMAL"
  ("h" region-previous-char "move left")
  ("k" region-above-char "move up")
  ("l" region-next-char "move right")
  ("i" (deactivate-mark t) "insert" :exit t)
  ("<SPC>" (funcall chimera-leader-function) :exit t))

(defun region-previous-char ()
  "Create a region on the char behind the current point.
Does nothing if the point is at the beginning of the buffer"
  (interactive)
  (when (not (bobp))
    (call-interactively 'set-mark-command)
    (call-interactively 'backward-char)))

(defun region-next-char ()
  "Create a region on the char in front of the current point.
Does nothing if the point is at the end of the buffer."
  (interactive)
  (when (not (eobp))
    (call-interactively 'forward-char)
    (call-interactively 'forward-char)
    (call-interactively 'region-previous-char)))

(defun region-current-char ()
  "Create a region on the char in front of the current point.
Does nothing if the point is at the end of the buffer."
  (interactive)
  (when (not (eobp))
    (call-interactively 'forward-char)
    (call-interactively 'region-previous-char)))

(defun region-above-char ()
  "Create a region on the char above and in front of the current point."
  (interactive)
  (call-interactively 'previous-line)
  (call-interactively 'forward-char)
  (call-interactively 'region-previous-char))

(provide 'chimera)

;;; chimera.el ends here
