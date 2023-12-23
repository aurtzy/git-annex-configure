(use-modules (guix packages)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix download)
             (guix build-system guile)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages haskell-apps)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages version-control))

(package
  (name "git-annex-configure")
  (version "1.0.0-dev")
  (source (local-file (dirname (current-filename))
                      #:recursive? #t))
  (build-system guile-build-system)
  (arguments
   (list
    #:source-directory "modules"
    #:phases
    #~(modify-phases %standard-phases
        (add-before 'build 'add-build-configurations
          (lambda* (#:key outputs #:allow-other-keys)
            (symlink #$(scheme-file "version.scm" version)
                     "modules/git-annex-configure/version.scm")))
        (add-after 'install 'install-scripts
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir (string-append out "/bin"))
              (symlink #$(program-file
                          "git-annex-configure"
                          '(begin
                             (use-modules (git-annex-configure main))
                             (main (command-line))))
                       (string-append out "/bin/git-annex-configure"))))))))
  (native-inputs
   ;; Specify guile-3.0 here for guile-build-system and also in
   ;; propagated-inputs for runtime features (git hook scripts)
   (list guile-3.0))
  (propagated-inputs
   (list guile-3.0
         git
         git-annex))
  (synopsis "Declarative git-annex configuration with Guile Scheme")
  (description
   "git-annex-configure is a git-annex addon command that enables declarative
configuration of git-annex repositories using Guile Scheme.  With a few
limitations, settings for repositories can be applied with just a single
execution of the command.")
  (home-page "https://github.com/aurtzy/git-annex-configure")
  (license license:gpl3+))

