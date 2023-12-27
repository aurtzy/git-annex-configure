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

(define-module (git-annex-configure main)
  #:use-module (git-annex-configure git repository)
  #:use-module (git-annex-configure git config)
  #:use-module (git-annex-configure git remote)
  #:use-module (git-annex-configure git annex repository)
  #:use-module (git-annex-configure git annex config)
  #:use-module (git-annex-configure git annex group)
  #:use-module (git-annex-configure git annex matchexpression)
  #:use-module (git-annex-configure git annex remote)
  #:use-module (git-annex-configure git annex remotes)
  #:use-module (git-annex-configure logging)
  #:use-module (git-annex-configure spec)
  #:use-module (git-annex-configure utils)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (oop goops)
  #:use-module (rnrs conditions)
  #:export (CONFIGURATION-FILE
            with-module-load
            annex-configure-global
            annex-configure-local
            annex-configure))

;; version.scm should be managed as a build-time variable
(define VERSION (primitive-load-path "git-annex-configure/version.scm" #f))

(define-method (annex-configure-global (local-repo <annex-repository>)
                                       global-config)
  "Configure global git-annex repository settings."
  (define local-repo-uuid (config-ref local-repo "annex.uuid"))

  (let ((annex-config-items (global-configuration-annex-config global-config)))
    (when annex-config-items
      (format-log $info "Configuring global git-annex config...")
      (for-each
       (lambda (item)
         (let ((key (car item))
               (value (cdr item)))
           (unless (equal? value
                           (annex-config-ref local-repo key))
             (annex-config-set! local-repo key value))))
       annex-config-items)
      (newline)))
  (let ((groupwanted-items (global-configuration-groupwanted global-config)))
    (when groupwanted-items
      (format-log $info "Configuring global git-annex groupwanted...")
      (for-each
       (lambda (item)
         (let ((group (car item))
               (expr (cdr item)))
           (groupwanted-set! local-repo group expr)))
       groupwanted-items)
      (newline)))

  ;; Apply non-disabled repo-specific configs for each uuid specified in config
  (for-each
   (lambda (repo-config)
     (define uuid (repository-configuration-uuid repo-config))

     (format-log $info
                 "~a: ~s~a\n"
                 "Configuring repository uuid"
                 uuid
                 (if (equal? local-repo-uuid uuid)
                     " [self]"
                     ""))
     (let ((description (repository-configuration-description repo-config)))
       (when description
         (format-log $info "Configuring git-annex description...")
         (description-set! local-repo description #:remote uuid)
         (newline)))
     (let ((wanted (repository-configuration-wanted repo-config)))
       (when wanted
         (format-log $info "Configuring git-annex wanted...")
         (wanted-set! local-repo wanted #:remote uuid)
         (newline)))
     (let ((required (repository-configuration-required repo-config)))
       (when required
         (format-log $info "Configuring git-annex required...")
         (required-set! local-repo required #:remote uuid)
         (newline)))
     (let ((groups (repository-configuration-groups repo-config)))
       (when groups
         (format-log $info "Configuring git-annex groups...")
         (groups-set! local-repo groups #:remote uuid)
         (newline)))

     ;; TODO Deprecated; should only be done with local-configuration
     ;;
     ;; The following configurations cannot be set from another repository, thus
     ;; we only run them on the current one.

     (when (equal? local-repo-uuid uuid)
       (let ((config-items (configuration-config repo-config)))
         (when config-items
           (format-log $info "Configuring git config...")
           (for-each
            (lambda (item)
              (let ((key (car item))
                    (value (cdr item)))
                (config-set! local-repo key value)))
            config-items)
           (newline)))
       (let ((remotes (configuration-remotes repo-config)))
         (when remotes
           (format-log $info "Configuring git remotes...")
           (remotes-set! local-repo remotes)
           (newline)))
       (let ((hooks (configuration-hooks repo-config)))
         (when hooks
           (format-log $info "Configuring hooks...")
           (for-each
            (lambda (hook)
              (let* ((name (car hook))
                     (script (cdr hook))
                     (file-path (string-join
                                 (list
                                  (repository-git-dir-ref local-repo)
                                  "hooks"
                                  name)
                                 "/")))
                ;; Only write if hook script has changed
                (unless (false-if-exception
                         (equal? script
                                 (call-with-port (open-input-file
                                                  file-path)
                                   (lambda (port)
                                     (read port)))))
                  (call-with-port (open-output-file file-path)
                    (lambda (port)
                      (format port
                              "~a\n~a\n\n"
                              "#!/usr/bin/env -S guile -s"
                              "!#")
                      (pretty-print script port)
                      (chmod port
                             (logior #o111 (stat:perms
                                            (stat port)))))))))
            hooks)
           (newline)))))
   (filter
    (lambda (repo-config)
      (not (repository-configuration-disabled? repo-config)))
    (global-configuration-repositories global-config))))

(define-method (annex-configure-local (local-repo <annex-repository>)
                                      local-config)
  "Configure local git-annex repository settings."
  (let ((config-items (local-configuration-config local-config)))
    (when config-items
      (format-log $info "Configuring git config...")
      (for-each
       (lambda (item)
         (let ((key (car item))
               (value (cdr item)))
           (config-set! local-repo key value)))
       config-items)
      (newline)))
  (let ((remotes (local-configuration-remotes local-config)))
    (when remotes
      (format-log $info "Configuring git remotes...")
      (remotes-set! local-repo
                    remotes
                    #:name-prefix (local-configuration-remotes-name-prefix
                                   local-config))
      (newline)))
  ;; TODO perhaps write compiled hook scripts instead to remove need
  ;; for propagated guile input?
  (let ((hooks (local-configuration-hooks local-config)))
    (when hooks
      (format-log $info "Configuring hooks...")
      (for-each
       (lambda (hook)
         (let* ((name (car hook))
                (script (cdr hook))
                (file-path (string-join
                            (list (repository-git-dir-ref local-repo)
                                  "hooks"
                                  name)
                            "/")))
           ;; Only write if hook script has changed
           (unless (false-if-exception
                    (equal? script
                            (call-with-port (open-input-file
                                             file-path)
                              (lambda (port)
                                (read port)))))
             (call-with-port (open-output-file file-path)
               (lambda (port)
                 (format port
                         "~a\n~a\n\n"
                         "#!/usr/bin/env -S guile -s"
                         "!#")
                 (pretty-print script port)
                 ;; Make sure script is executable
                 (chmod port
                        (logior #o111 (stat:perms
                                       (stat port)))))))))
       hooks)
      (newline))))

(define (with-module-load name file)
  "Load a file with current module set to the specified name."
  (with-exception-handler
   (lambda (exn)
     (raise-exception
      (make-exception
       (make-external-error)
       (make-exception-with-message "Unable to load file")
       exn)))
   (lambda ()
     (save-module-excursion
      (lambda ()
        (set-current-module (resolve-module name))
        (primitive-load file))))))

(define-method (annex-configure file)
  "Load a configuration file and apply the relevant procedure to the git-annex
repository it is contained in."
  (let* ((local-repo (annex-repository (dirname file)))
         (config (with-module-load '(git-annex-configure spec) file)))
    (format-log $info
                "Repository to configure: ~s\n"
                (or (repository-toplevel-ref local-repo)
                    (repository-git-dir-ref local-repo)))
    (cond
     ((global-configuration? config)
      (annex-configure-global local-repo config))
     ((local-configuration? config)
      (annex-configure-local local-repo config))
     (else
      (raise-exception
       (make-exception
        (make-external-error)
        (make-exception-with-message
         (format #f
                 (string-append
                  "Evaluation of file not a <global-configuration> or "
                  "<local-configuration>: ~s")))
        (make-exception-with-irritants
         (list file))))))))

(define (format-usage usage . rest)
  (string-join
   (cons
    (string-append "Usage: "$prog-name" "usage)
    rest)
   "\n"))

(define main-help
  (format-usage
   "--help | [OPTION] ... FILE"
   "Read the configuration file from a git-annex repository path and apply the"
   "configuration."
   ""
   "Options:"
   "  -h, --help"
   "      Display this help message."
   "  -q, --quiet"
   "      Suppress logs. Only warnings and error messages will be printed."
   "  --version"
   "      Display the current version of this program."))

(define (main-getopts args)
  (getopt-long args
               '((help (single-char #\h))
                 (quiet (single-char #\q))
                 (version))))

(define-public (git-annex-configure args)
  (let* ((options (main-getopts args))
         (args (option-ref options '() #f))
         (file (if (or (null? args)
                       (> (length args) 1))
                   #f
                   (car args))))
    ;; Order of conditions for setting log level is important here; prioritize
    ;; more verbosity
    (log-level-set! (cond
                     ((getenv "DEBUG") $debug)
                     ((option-ref options 'quiet #f) $warning)
                     (else $info)))
    (cond
     ((option-ref options 'version #f)
      (display VERSION)
      (newline))
     ((or (option-ref options 'help #f)
          (not file))
      (display main-help)
      (newline))
     (else
      (when (not (eq? 'regular (stat:type (stat file))))
        (raise-exception
         (make-exception
          (make-exception-with-message (format #f
                                               "Not a file: ~s"
                                               file)))))
      (annex-configure file)))))

(define-public (main args)
  "Entry point."
  (log-level-set! $debug)
  (let ((exception-with-kind-and-args?
         (exception-predicate &exception-with-kind-and-args))
        (last-exn
         (make-exception-with-message "this message should never be seen")))
    (guard
     (_ ;; for non-continuable exceptions
      ((or (programming-error? last-exn)
           (equal? $debug (log-level-ref)))
       (raise-exception last-exn))
      (else
       (exit 1)))
     (with-exception-handler
      ;; for continuable exceptions
      (lambda (exn)
        ;; if non-continuable is intercepted here, pass it up
        (when (non-continuable-error? exn)
          (raise-exception exn))
        
        (when (exception-with-message? exn)
          (let ((exns (simple-exceptions exn)))
            (apply format-log
                   $error
                   (string-join
                    (map exception-message
                         (filter exception-with-message? exns))
                    "\n |- ")
                   (apply append
                          (map exception-irritants
                               (filter exception-with-irritants?
                                       exns))))))
        (set! last-exn exn))
      (lambda ()
        (git-annex-configure args))))))
