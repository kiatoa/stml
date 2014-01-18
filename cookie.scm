;;;
;;; cookie.scm - parse and construct http state information
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  Ported to Chicken by Reed Sheridan
;;;

;; Parser and constructor of http "Cookies" defined in
;; RFC 2965 HTTP state managemnet mechanism
;;   <ftp://ftp.isi.edu/in-notes/rfc2965.txt>
;; See also
;; RFC 2964 Use of HTTP state management
;;   <ftp://ftp.isi.edu/in-notes/rfc2964.txt>
;; The parser also supports the old Netscape spec
;;   <http://www.netscape.com/newsref/std/cookie_spec.html>

(declare (unit cookie))
(require-extension srfi-1 srfi-13 srfi-14 regex)
;; (use  srfi-1 srfi-13 srfi-14 regex)
;; (declare (export parse-cookie-string construct-cookie-string))

;; #>
;; #include <time.h>
;; <#
;; 
;; (define fmt-time
;;   (foreign-lambda* c-string ((long secs_since_epoch))
;;     "static char buf[256];"
;;     "time_t t = (time_t) secs_since_epoch;"
;;     "strftime(buf, sizeof(buf), \"%a, %d-%b-%Y %H:%M:%S GMT\", gmtime(&t));"
;;     "return(buf);"))


(define (fmt-time seconds)
   (time->string (seconds->utc-time seconds) "%D"))

 ;; utility fn.  breaks  ``attr=value;attr=value ... '' into alist.
 ;; version is a cookie version.  if version>0, we allow comma as the
 ;; delimiter as well as semicolon.
 (define (parse-av-pairs input version)
   (define attr-regexp
     (if (= version 0)
         (regexp "\\s*([\\w$_-]+)\\s*([=\\;]\\s*)?")
         (regexp "\\s*([\\w$_-]+)\\s*([=\\;,]\\s*)?")))
   (define attr-delim
     (if (= version 0) #\; (char-set #\, #\\ #\;)))
   
   (define (read-attr input r)
     (cond ((string-null? input) (reverse! r))
           ((string-search attr-regexp input)
            => (lambda (m)
                 (if (and-let* ((delimiter (third m))) ;;is an attr_value pai
 		      (string-prefix? "=" delimiter))
                     (let ((attr (second m))
                           (rest (string-search-after attr-regexp input)))
                       (if (string-prefix? "\"" rest)
                           (read-token-quoted attr (string-drop rest 1) r)
                           (read-token attr rest r)))
                     (read-attr (string-search-after attr-regexp input) ;; Skip ahead if broken input?
                                (alist-cons (second m) #f r)))))
           (else
            ;; the input is broken; for now, we ignore the rest.
            (reverse! r))))
   (define (read-token attr input r)
     (cond ((string-index input attr-delim)
            => (lambda (i)
                 (read-attr (string-drop input (+ i 1))
                            (alist-cons attr
 				       (string-trim-right (string-take input i))
 				       r))))
           (else
            (reverse! (alist-cons attr (string-trim-right input) r)))))
   (define (read-token-quoted attr input r)
     (let loop ((input input)
                (partial '()))
       (cond ((string-index input (char-set #\\ #\"))
              => (lambda (i)
                   (let ((c (string-ref input i)))
                     (if (char=? c #\\)
                         (if (< (string-length input) (+ i 1))
                             (error-unterminated attr)
                             (loop (string-drop input (+ i 2))
                                   (cons* (string (string-ref input (+ i 1)))
                                          (string-take input i)
                                          partial)))
                         (read-attr (string-drop input (+ i 1))
                                    (alist-cons attr
 					       (string-concatenate-reverse
 						(cons (string-take input i)
 						      partial))
 					       r))))))
             (else (error-unterminated attr)))))
   (define (error-unterminated attr)
     (error "Unterminated quoted value given for attribute" attr))
 
   (read-attr input '()))
 
 ;; Parses the header value of "Cookie" request header.
 ;; If cookie version is known by "Cookie2" request header, it should
 ;; be passed to version (as integer).  Otherwise, it figures out
 ;; the cookie version from input.
 ;;
 ;; Returns the following format.
 ;;   ((<name> <value> [:path <path>] [:domain <domain>] [:port <port>])
 ;;    ...)
 
 (define (parse-cookie-string input #!optional version)
   (let ((ver (cond ((integer? version) version)
                    ((string-search "^\\s*\\$Version\\s*=\\s*(\\d+)" input)
                     => (lambda (m)
                          (string->number (cadr m))))
                    (else 0))))
     (let loop ((av-pairs (parse-av-pairs input ver))
                (r '())
                (current '()))
       (cond ((null? av-pairs)
              (if (null? current)
                  (reverse r)
                  (reverse (cons (reverse current) r))))
             ((string-ci=? "$path" (caar av-pairs))
              (loop (cdr av-pairs) r (cons* (cdar av-pairs) path: current)))
             ((string-ci=? "$domain" (caar av-pairs))
              (loop (cdr av-pairs) r (cons* (cdar av-pairs) domain: current)))
             ((string-ci=? "$port" (caar av-pairs))
              (loop (cdr av-pairs) r (cons* (cdar av-pairs) port: current)))
             (else
              (if (null? current)
                  (loop (cdr av-pairs) r (list (cdar av-pairs) (caar av-pairs)))
                  (loop (cdr av-pairs)
                        (cons (reverse current) r)
                        (list (cdar av-pairs) (caar av-pairs)))))))))
 
 ;; Construct a cookie string suitable for Set-Cookie or Set-Cookie2 header.
 ;; specs is the following format.
 ;;
 ;;   ((<name> <value> [:comment <comment>] [:comment-url <comment-url>]
 ;;                    [:discard <bool>] [:domain <domain>]
 ;;                    [:max-age <age>] [:path <value>] [:port <port-list>]
 ;;                    [:secure <bool>] [:version <version>] [:expires <date>]
 ;;    ) ...)
 ;;
 ;; Returns a list of cookie strings for each <name>=<value> pair.  In the
 ;; ``new cookie'' implementation, you can join them by comma and send it
 ;; at once with Set-cookie2 header.  For the old netscape protocol, you
 ;; must send each of them by Set-cookie header.
 
 
 (define (construct-cookie-string specs #!optional (version 1))
   (map (lambda (spec) (construct-cookie-string-1 spec version))
        specs))
 
 (define (construct-cookie-string-1 spec ver)
   (when (< (length spec) 2)
     (error "bad cookie spec: at least <name> and <value> required" spec))
   (let ((name (car spec))
         (value (cadr spec)))
     (let loop ((attr (cddr spec))
                (r    (list (if value
                                (string-append name "="
                                               (quote-if-needed value))
                                name))))
       (define (next s) (loop (cddr attr) (cons s r)))
       (define (ignore) (loop (cddr attr) r))
       (cond
        ((null? attr) (string-join (reverse r) ";"))
        ((null? (cdr attr))
         (error (conc "bad cookie spec: attribute " (car attr) " requires value" )))
        ((eqv? comment: (car attr))
         (if (> ver 0)
 	    (next (string-append "Comment=" (quote-if-needed (cadr attr))))
             (ignore)))
        ((eqv? comment-url: (car attr))
         (if (> ver 0)
             (next (string-append "CommentURL=" (quote-value (cadr attr))))
             (ignore)))
        ((eqv? discard: (car attr))
         (if (and (> ver 0) (cadr attr)) (next "Discard") (ignore)))
        ((eqv? domain: (car attr))
         (next (string-append "Domain=" (cadr attr))))
        ((eqv? max-age: (car attr))
         (if (> ver 0)
             (next (sprintf "Max-Age=~a" (cadr attr)))
             (ignore)))
        ((eqv? path: (car attr))
         (next (string-append "Path=" (quote-if-needed (cadr attr)))))
        ((eqv? port: (car attr))
         (if (> ver 0)
             (next (string-append "Port=" (quote-value (cadr attr))))
             (ignore)))
        ((eqv? secure: (car attr))
         (if (cadr attr) (next "Secure") (ignore)))
        ((eqv? version: (car attr))
         (if (> ver 0)
             (next (sprintf "Version=~a" (cadr attr)))
             (ignore)))
        ((eqv? expires: (car attr))
         (if (> ver 0)
             (ignore)
             (next (make-expires-attr (cadr attr)))))
        (else (error "Unknown cookie attribute" (car attr))))
       ))
   )
 
 
 ;; (define (quote-value value)
 ;;   (string-append "\"" (regexp-replace-all #/\"|\\/ value "\\\\\\0") "\""))
 
 (define (quote-value value)
   (string-append "\"" (string-substitute* value '(("\\\"" . "\\\"") ("\\\\" . "\\\\"))) "\""))
 
 (define quote-if-needed
   (let ((rx (regexp "[\\\",;\\\\ \\t\\n]")))
     (lambda (value)
       (if (string-search rx value)
 	  (quote-value value)
 	  value))))
 
 (define (make-expires-attr time)
   (sprintf "Expires=~a"
 	   (if (number? time)
 	       (fmt-time time)
 	       time)))
 
 ;;;; Added support functions from my utils, split this out
 
 (define (string-search-after r s #!optional (start 0))
   (and-let* ((match-indices (string-search-positions r s start))
 	     (right-match (second (first match-indices))))
     (substring s right-match)))
