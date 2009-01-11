;; This doesn't work yet
;;
(use spiffy cgi-handler)

(spiffy-debug-mode #t)

(spiffy-file-ext-handlers 
 `(("drcdb" . ,(cgi-handler* "/path/to/drcdb"))))

(spiffy-root-path "/path/to/web")

(start-server location: (get-host-name)
                init: noop)
