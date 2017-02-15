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
    (region-previous-char)
    (should (equal (region-beginning) 4))
    (should (equal (region-end) 5))))

(ert-deftest region-previous-char-moves-point ()
  "Moves point to before previous character."
  (with-temp-buffer
    (insert "Text")
    (region-previous-char)
    (should (equal (point) 4))))

(ert-deftest region-previous-char-handles-beginning-of-buffer ()
  "Doesn't create region if point is at beginning of buffer."
  (with-temp-buffer
    (insert "Text")
    (goto-char 1)
    (region-previous-char)
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
    (region-next-char)
    (should (equal (region-beginning) 2))
    (should (equal (region-end) 3))))

(ert-deftest region-next-char-moves-point ()
  "Moves point to before next character."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-next-char)
    (should (equal (point) 2))))

(ert-deftest region-next-char-handles-end-of-buffer ()
  "Doesn't create region if point is at end of buffer."
  (with-temp-buffer
    (insert "Text")
    (region-next-char)
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
    (region-current-char)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest region-current-char-doesnt-move-point ()
  "Doesn't move point."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-current-char)
    (should (equal (point) 1))))

(ert-deftest region-current-char-handles-end-of-buffer ()
  "Doesn't create region if point is at end of buffer."
  (with-temp-buffer
    (insert "Text")
    (goto-char 5)
    (region-current-char)
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
    (region-above-char)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest region-above-char-moves-point ()
  "Moves point to before character below current character."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 9)
    (region-above-char)
    (should (equal (point) 1))))

(ert-deftest region-above-char-handles-first-line-in-buffer ()
  "Doesn't create region if on first line in buffer."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char (point-min))
    (region-above-char)
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
    (region-below-char)
    (should (equal (region-beginning) 9))
    (should (equal (region-end) 10))))

(ert-deftest region-below-char-moves-point ()
  "Moves point to before character above current character."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 1)
    (region-below-char)
    (should (equal (point) 9))))

(ert-deftest region-below-char-handles-last-line-in-buffer ()
  "Doesn't create region if on last line in buffer."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char (point-max))
    (region-below-char)
   (should-not (region-active-p))))

(ert-deftest region-below-char-handles-last-line-in-buffer-being-empty ()
  "Doesn't create region from current character to last line if last line is empty."
  (with-temp-buffer
    (insert "Text on\ntwo lines\n")
    (goto-char 10)
    (region-current-char)
    (region-below-char)
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
    (region-current-char)
    (insert-after-region)
    (should (equal (point) 2))))

(ert-deftest insert-after-deactivates-region ()
  "Deactivate region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-current-char)
    (insert-after-region)
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
    (region-current-char)
    (insert-before-region)
    (should (equal (point) 1))))

(ert-deftest insert-before-deactivates-region ()
  "Deactivates region."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-current-char)
    (insert-before-region)
    (should-not (region-active-p))))

;; chimera-test.el ends here
