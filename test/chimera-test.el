;;; chimera-test.el --- tests chimera.el

;;; Commentary:

;;; Code:
(load-file "chimera.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; region-previous-char ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-previous-char-creates-correct-region ()
  "If the point is after the 't' in 'Text'.
This method will create a region around the 't'"
  (with-temp-buffer
    (insert "Text")
    (region-previous-char)
    (should (equal (region-beginning) 4))
    (should (equal (region-end) 5))))

(ert-deftest region-previous-char-moves-point ()
  "If the point is after the 't' in 'Text'.
This method will move the point to before 't'."
  (with-temp-buffer
    (insert "Text")
    (region-previous-char)
    (should (equal (point) 4))))

(ert-deftest region-previous-char-handles-begining-of-buffer ()
  "If the point is before the 'T' in 'Text'.
This method will not create a region."
  (with-temp-buffer
    (insert "Text")
    (goto-char 1)
    (region-previous-char)
    (should-error (region-beginning))))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; region-next-char ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-next-char-creates-correct-region ()
  "If the point is before the 'T' in Text'.
This method will create a region around the 'e'"
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-next-char)
    (should (equal (region-beginning) 2))
    (should (equal (region-end) 3))))

(ert-deftest region-next-char-moves-point ()
  "If the point is before the 'T' in 'Text'.
This method will move the point to before 'e'."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-next-char)
    (should (equal (point) 2))))

(ert-deftest region-next-char-handles-end-of-buffer ()
  "If the point is after the 't' in 'Text'.
This method will not create a region."
  (with-temp-buffer
    (insert "Text")
    (region-next-char)
    (should-error (region-beginning))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; region-current-char ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-current-char-creates-correct-region ()
  "If the point is before the 'T' in 'Text'.
This method will create a region around the 'T'."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-current-char)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest region-current-char-doesnt-move-point ()
  "If the point is before the 'T' in 'Text'.
This method will not move the point."
  (with-temp-buffer
    (insert "Text")
    (beginning-of-line)
    (region-current-char)
    (should (equal (point) 1))))

(ert-deftest region-current-char-handles-end-of-buffer ()
  "If the point is after the 't' in 'Text'.
This method will create a region around the 't'."
  (with-temp-buffer
    (insert "Text")
    (goto-char 5)
    (should-error (region-beginning))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; region-above-char   ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-above-char-creates-correct-region ()
  "If the point is before the 't' in 'two'.
This method will create a region around the 'T' in 'Text'."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 9)
    (region-above-char)
    (should (equal (region-beginning) 1))
    (should (equal (region-end) 2))))

(ert-deftest region-above-char-moves-point ()
  "If the point is before the 't' in 'two'.
This method will move the point to before the 'T' in 'Text'."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 9)
    (region-above-char)
    (should (equal (point) 1))))

(ert-deftest region-above-char-handles-first-line-in-buffer ()
  "If the point is before the 'T' in 'Text'.
This method will not create a region."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char (point-min))
    (region-above-char)
    (should-error (region-beginning))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; region-below-char   ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest region-below-char-creates-correct-region ()
  "If the point is before the 'T' in 'Text'.
This method will create a region around the 't' in 'two'."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 1)
    (region-below-char)
    (should (equal (region-beginning) 9))
    (should (equal (region-end) 10))))

(ert-deftest region-below-char-moves-point ()
  "If the point is before the 'T' in 'Text'.
This method will move the point to before the 't' in 'two'."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char 1)
    (region-below-char)
    (should (equal (point) 9))))

(ert-deftest region-below-char-handles-last-line-in-buffer ()
  "If the point is after the 'o' in 'two'.
This method will not create a region."
  (with-temp-buffer
    (insert "Text on\ntwo lines")
    (goto-char (point-max))
    (region-below-char)
   (should-error (region-beginning))))

(ert-deftest region-below-char-handles-last-line-in-buffer-being-empty ()
  "If the region is around the 'w' in 'two'.
This method will not create a region from 't' until the end of the buffer."
  (with-temp-buffer
    (insert "Text on\ntwo lines\n")
    (goto-char 10)
    (region-current-char)
    (region-below-char)
    (should (equal (region-beginning) 19))
    (should (equal (region-end) 19))))

;; chimera-test.el ends here
