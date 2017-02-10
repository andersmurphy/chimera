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
         (chimera/body))))

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
  ("i" (deactivate-mark t) "insert" :exit t))

(provide 'chimera)

;;; chimera.el ends here
