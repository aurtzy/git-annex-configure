;;; Copyright © 2023 aurtzy <aurtzy@gmail.com>
;;;
;;; This file is part of git-annex-configure.
;;;
;;; git-annex-configure is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; git-annex-configure is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with git-annex-configure.  If not, see <http://www.gnu.org/licenses/>.

(define-module (git-annex-configure spec)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure git annex remote)
  #:use-module (git-annex-configure logging)
  #:use-module (git-annex-configure records)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 exceptions)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<repository-configuration>
            repository-configuration repository-configuration?
            this-repository-configuration
            repository-configuration-uuid
            repository-configuration-disabled?
            repository-configuration-description
            repository-configuration-wanted
            repository-configuration-required
            repository-configuration-groups
            repository-configuration-remotes
            repository-configuration-config
            repository-configuration-hooks
            configuration-uuid
            configuration-disabled?
            configuration-description
            configuration-wanted
            configuration-required
            configuration-groups
            configuration-remotes
            configuration-config
            configuration-hooks

            <global-configuration>
            global-configuration global-configuration?
            this-global-configuration
            global-configuration-annex-config
            global-configuration-groupwanted
            global-configuration-repositories
            configuration?
            configuration-annex-config
            configuration-groupwanted
            configuration-repositories

            <local-configuration>
            local-configuration local-configuration?
            this-local-configuration
            local-configuration-remotes
            local-configuration-config
            local-configuration-hooks))

;;; Sanitization procedures provide assertions with formatted error messages.
;;; Return values of procedures should be the sanitized value, which may or may
;;; not be the same. Procedures are expected to have the #:accept-false? key for
;;; cases where #f is not acceptable.

(define ((sanitize-with-deprecation-warning msg sanitize-field)
         raw)
  (when raw
    (format-log $warning "~a" msg))
  (sanitize-field raw))

(define ((proc-with-deprecation-warning proc) . args)
  (format-log $warning
              "Use of deprecated procedure: ~a"
              (procedure-name proc))
  (apply proc args))

(define* ((sanitize-self field
                         #:key
                         (accept-false? #t))
          raw)
  "Accepts any value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   (raw
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a")
      (make-exception-with-irritants
       (list field
             "field value cannot be false")))))))

(define* ((sanitize-string field
                           #:key
                           (accept-false? #t))
          raw)
  "Accepts only string value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((string? raw)
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a: ~s")
      (make-exception-with-irritants
       (list field
             "field value not a string"
             raw)))))))

(define* (((sanitize-list sanitize-value)
           field
           #:key (accept-false? #t))
          raw)
  "Accepts only a list where each element is sanitized with sanitize-value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((list? raw)
    (map
     (lambda (value)
       ((sanitize-value field #:accept-false? #f) value))
     raw))
   (else 
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a field value not a list: ~s")
      (make-exception-with-irritants
       (list field
             raw)))))))

(define* (((sanitize-alist sanitize-key sanitize-value)
           field
           #:key
           (accept-false? #t))
          raw)
  "Accepts only an alist where each pair has its key and value sanitized with
sanitize-key and sanitize-value, respectively."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((and (list? raw)
         (every pair?
                raw))
    (map
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (cons ((sanitize-key field #:accept-false? #f)
                key)
               ((sanitize-value field #:accept-false? #f)
                value))))
     raw))
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a field value not an alist: ~s")
      (make-exception-with-irritants
       (list field
             raw)))))))

(define* ((sanitize-group field
                          #:key
                          (accept-false? #t))
          raw)
  "Accepts <group> value; otherwise attempt to coerce value into a <group>."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <group>)
    raw)
   (else
    (with-exception-handler
     (lambda (exn)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          "~a field value not a valid group: ~s")
         (make-exception-with-irritants
          (list field
                raw))
         exn)))
     (lambda ()
       (group raw))))))

(define* ((sanitize-groups field
                           #:key
                           (accept-false? #t))
          raw)
  "Accepts <groups> value; otherwise assumes value is a list of <group> objects
(or objects that can be coerced to <group>) and attempts to apply it to the
`groups' constructor."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <groups>)
    raw)
   (else
    (with-exception-handler
     (lambda (exn)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          "~a ~a: ~s")
         (make-exception-with-irritants
          (list field
                "field value not a valid groups type"
                raw))
         exn)))
     (lambda ()
       (apply groups raw))))))

(define* ((sanitize-remote field
                           #:key
                           (accept-false? #t))
          raw)
  "Accepts <remote> value; otherwise raises an assertion exception."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <remote>)
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a: ~s")
      (make-exception-with-irritants
       (list field
             "field value not a valid remote type"
             raw)))))))

(define* ((sanitize-remotes field
                            #:key
                            (accept-false? #t))
          raw)
  "Accepts <remotes> value; otherwise assumes raw value is a list of <remote>
objects and tries to apply it to the `remotes' constructor."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((is-a? raw <remotes>)
    raw)
   (else
    (with-exception-handler
     (lambda (exn)
       (raise-exception
        (make-exception
         (make-assertion-failure)
         (make-exception-with-message
          "~a ~a: ~s")
         (make-exception-with-irritants
          (list field
                "field value not a valid remotes type"
                raw))
         exn)))
     (lambda ()
       (apply remotes raw))))))

(define-record-type* <repository-configuration>
  repository-configuration make-repository-configuration
  repository-configuration?
  this-repository-configuration
  (uuid repository-configuration-uuid
        (sanitize (sanitize-string "repository uuid")))
  (disabled? repository-configuration-disabled?
             (default #f))

  ;; These configurations can be applied from any repository.
  (description repository-configuration-description
               (default #f)
               (sanitize (sanitize-string "description")))
  (wanted repository-configuration-wanted
          (default #f)
          (sanitize (sanitize-string "wanted matchexpression")))
  (required repository-configuration-required
            (default #f)
            (sanitize (sanitize-string "required matchexpression")))
  (groups repository-configuration-groups
          (default #f)
          (sanitize (sanitize-groups "groups")))

  ;; TODO Below fields are deprecated in favor of fields in local-configuration.
  ;;
  ;; These configurations can only be applied locally - there appears to be no
  ;; method of syncing state for these settings.
  (remotes configuration-remotes
           (default #f)
           (sanitize
            (sanitize-with-deprecation-warning
             (format #f
                     (string-append
                      "Remotes field for ~a is deprecated in favor of being "
                      "declared via ~a")
                     <repository-configuration>
                     <local-configuration>)
             (sanitize-remotes "remotes"))))
  (config configuration-config
          (default #f)
          (sanitize
           (sanitize-with-deprecation-warning
            (format #f
                    (string-append
                     "Git config field for ~a is deprecated in favor of being "
                     "declared via ~a")
                    <repository-configuration>
                    <local-configuration>)
            ((sanitize-alist sanitize-string
                             sanitize-string)
             "git config"))))
  (hooks configuration-hooks
         (default #f)
         (sanitize
          (sanitize-with-deprecation-warning
           (format #f
                   (string-append
                    "Hooks field for ~a is deprecated in favor of being "
                    "declared via ~a")
                   <repository-configuration>
                   <local-configuration>)
           ((sanitize-alist sanitize-string
                            sanitize-self)
            "hooks")))))

;;; TODO Deprecated repository-configuration aliases
(define configuration-uuid
  (proc-with-deprecation-warning
   repository-configuration-uuid))
(define configuration-disabled?
  (proc-with-deprecation-warning
   repository-configuration-disabled?))
(define configuration-description
  (proc-with-deprecation-warning
   repository-configuration-description))
(define configuration-wanted
  (proc-with-deprecation-warning
   repository-configuration-wanted))
(define configuration-required
  (proc-with-deprecation-warning
   repository-configuration-required))
(define configuration-groups
  (proc-with-deprecation-warning
   repository-configuration-groups))

(define* ((sanitize-repository-configuration field
                                             #:key
                                             (accept-false? #t))
          raw)
  "Accept only a <repository-configuration> value."
  (cond
   ((and accept-false?
         (not raw))
    #f)
   ((repository-configuration? raw)
    raw)
   (else
    (raise-exception
     (make-exception
      (make-assertion-failure)
      (make-exception-with-message
       "~a ~a: ~s")
      (make-exception-with-irritants
       (list field
             "field value not a valid repository configuration"
             raw)))))))

(define-record-type* <global-configuration>
  global-configuration make-global-configuration
  global-configuration?
  this-global-configuration
  (annex-config global-configuration-annex-config
                (default #f)
                (sanitize ((sanitize-alist sanitize-string
                                           sanitize-string)
                           "global git-annex config")))
  (groupwanted global-configuration-groupwanted
               (default #f)
               (sanitize ((sanitize-alist sanitize-group
                                          sanitize-string)
                          "global groupwanted matchexpressions")))
  (repositories global-configuration-repositories
                (default '())
                (sanitize ((sanitize-list sanitize-repository-configuration)
                           "global repository configurations"))))

;;; TODO Deprecated global-configuration aliases
(define-syntax-rule (configuration field ...)
  (begin
    (format-log $warning
                "~a; ~a"
                "configuration is a deprecated macro"
                "use global-configuration instead")
    (global-configuration field ...)))
(define configuration?
  (proc-with-deprecation-warning
   global-configuration?))
(define configuration-annex-config
  (proc-with-deprecation-warning
   global-configuration-annex-config))
(define configuration-groupwanted
  (proc-with-deprecation-warning
   global-configuration-groupwanted))
(define configuration-repositories
  (proc-with-deprecation-warning
   global-configuration-repositories))

(define-record-type* <local-configuration>
  local-configuration make-local-configuration
  local-config?
  this-local-configuration
  (remotes local-configuration-remotes
           (default #f)
           (sanitize (sanitize-remotes "remotes")))
  (config local-configuration-config
          (default #f)
          (sanitize ((sanitize-alist sanitize-string
                                     sanitize-string)
                     "git config")))
  (hooks local-configuration-hooks
         (default #f)
         (sanitize ((sanitize-alist sanitize-string
                                    sanitize-self)
                    "hooks"))))
