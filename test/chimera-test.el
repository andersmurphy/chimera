;;; chimera-test.el --- tests chimera.el

;;; Commentary:

;;; Code:
(load-file "chimera.el")

;; Ensure transient mark mode is active. This is needed so that
;; region-active-p can be used to determine whether the buffer
;; is active or not.
(setq transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; region-previous-char ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-previous-char-creates-region ()
  "Creates a region around the previous character."
  (with-temp-buffer
    (insert "Text")
    (chimera-region-previous-char)
    (should (equal (region-beginning) 4))
    (should (equal (region-end) 5))))

(ert-deftest region-previous-char-moves-point ()
  "Moves point to before previous character."
  (with-temp-buffer
    (insert "Text")
    (chimera-region-previous-char)
    (should (equal (point) 4))))

(ert-deftest region-previous-char-handles-beginning-of-buffer ()
  "Doesn't create region if point is at beginning of buffer."
  (with-temp-buffer
    (insert "Text")
    (goto-char 1)
    (chimera-region-previous-char)
    (should-not (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; region-next-char ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-next-char-creates-region ()
  "Creates a region around the previous character."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-next-char)
    (should (equal (region-beginning) 2))
    (should (equal (region-end) 3))))

(ert-deftest region-next-char-moves-point ()
  "Moves point to before next character."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-next-char)
    (should (equal (point) 2))))

(ert-deftest region-next-char-handles-end-of-buffer ()
  "Doesn't create region if point is at end of buffer."
  (with-temp-buffer
    (insert "Text")
    (chimera-region-next-char)
    (should-not (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; region-current-char ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-current-char-creates-region ()
  "Creates a region around the character in front of the point."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest region-current-char-doesnt-move-point ()
  "Doesn't move point."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (should (equal (point) 1))))

(ert-deftest region-current-char-handles-end-of-buffer ()
  "Doesn't create region if point is at end of buffer."
  (with-temp-buffer
    (insert "Text")
    (goto-char 5)
    (chimera-region-current-char)
    (should-not (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;; region-above-char ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-above-char-creates-region ()
  "Creates region around character below current character."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 9)
    (chimera-region-above-char)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest region-above-char-moves-point ()
  "Moves point to before character below current character."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 9)
    (chimera-region-above-char)
    (should (equal (point) 1))))

(ert-deftest region-above-char-handles-first-line-in-buffer ()
  "Doesn't create region if on first line in buffer."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char (point-min))
    (chimera-region-above-char)
    (should-not (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; region-below-char   ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-below-char-creates-region ()
  "Creates region around character above current character."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 1)
    (chimera-region-below-char)
    (should (equal (region-beginning) 9))
    (should (equal (region-end) 10))))

(ert-deftest region-below-char-moves-point ()
  "Moves point to before character above current character."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 1)
    (chimera-region-below-char)
    (should (equal (point) 9))))

(ert-deftest region-below-char-handles-last-line-in-buffer ()
  "Doesn't create region if on last line in buffer."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char (point-max))
    (chimera-region-below-char)
   (should-not (region-active-p))))

(ert-deftest region-below-char-handles-last-line-in-buffer-being-empty ()
  "Doesn't create region from current character to last line if last line is empty."
  (with-temp-buffer
    (insert "Text on\ntwo lines\n")
    (goto-char 10)
    (chimera-region-current-char)
    (chimera-region-below-char)
    (should (equal (region-beginning) 19))
    (should (equal (region-end) 19))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; insert-after-region ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest insert-after-region-moves-pointn ()
  "Moves point to beginning of region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (chimera-insert-after-region)
    (should (equal (point) 2))))

(ert-deftest insert-after-deactivates-region ()
  "Deactivate region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (chimera-insert-after-region)
    (should-not (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; insert-before-region ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest insert-before-region-moves-point ()
  "Moves point to beginning of region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (chimera-insert-before-region)
    (should (equal (point) 1))))

(ert-deftest insert-before-deactivates-region ()
  "Deactivates region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (chimera-insert-before-region)
    (should-not (region-active-p))))

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; delete-region ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(ert-deftest delete-region-kills-region ()
  "Kills active region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (chimera-delete-region)
    (should (equal (buffer-string) "ext"))))

(ert-deftest delete-region-creates-region ()
  "Creates region around current character after killing region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (chimera-region-current-char)
    (chimera-delete-region)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest delete-region-handles-end-of-buffer ()
  "Creates region around previous character if at end of buffer."
  (with-temp-buffer
    (insert "Text")
    (goto-char 4)
    (chimera-region-current-char)
    (chimera-delete-region)
    (should (equal (region-beginning) 3))
    (should (equal (region-end) 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;; store-region & restore-region ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest store-and-restore-region-creates-region ()
  "Restores region to stored state."
  (with-temp-buffer
    (insert "Text")
    (chimera-region-previous-char)
    (chimera-store-region)
    (chimera-region-previous-char)
    (chimera-restore-region)
    (should (equal (region-beginning) 4))
    (should (equal (region-end) 5))))

(ert-deftest store-and-restore-region-moves-point ()
  "Restores point to before the region."
   (with-temp-buffer
    (insert "Text")
    (chimera-region-previous-char)
    (chimera-store-region)
    (chimera-region-previous-char)
    (chimera-restore-region)
    (should (equal (point) 4))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ;;
;; paste-after-region ;;
;;                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest paste-after-region-inserts-text ()
  "Pastes last kill ring item after the current region."
  (with-temp-buffer
    (insert "Text")
    (kill-whole-line)
    (insert "Paste")
    (chimera-region-previous-char)
    (chimera-paste-after-region)
    (should (equal (buffer-string) "PasteText"))))

(ert-deftest paste-after-region-stays-where-it-is ()
  "Region stays where it is."
  (with-temp-buffer
    (insert "Text")
    (kill-whole-line)
    (insert "Paste")
    (chimera-region-previous-char)
    (chimera-paste-after-region)
    (should (equal (region-beginning) 5))
    (should (equal (region-end) 6))))

(ert-deftest paste-after-region-point-stays-at-beginning-of-region ()
  "Point stays at beginning of region."
  (with-temp-buffer
    (insert "Text")
    (kill-whole-line)
    (insert "Paste")
    (chimera-region-previous-char)
    (chimera-paste-after-region)
    (should (equal (point) 5))))

;; chimera-test.el ends here
