;; Copyright 2007-2008, Matthew Welland. matt@kiatoa.com All rights reserved.
;; 
;; maint/control.scm
;;
(s:load-model "maint")

;; remember that the system will call the function <pagename>-action with the action as a parameter
(define (maint-action action)
  (let ((asym (string->symbol action)))
    (s:log "Doing action! " action)
    (case asym
      ('update_tables
       (maint:update-tables)))))
