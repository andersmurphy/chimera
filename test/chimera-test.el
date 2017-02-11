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
    (should (equal (region-active-p) nil))))

;;; chimera-test.el ends here
