(defun timvisher/rtm-decoded-days-later (days-later)
  (decode-time (time-add (current-time) (days-to-time days-later))))

(defun timvisher/rtm-due-date-string (hour decoded-date)
  (format-time-string "^'%d/%m %I%p'" (apply 'encode-time (cons 0 (cons 0 (cons hour (cdddr decoded-date)))))))

(defun timvisher/insert-memorization-task ())
