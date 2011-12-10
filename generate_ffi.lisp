(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :verrazano))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cl-git))
(in-package :cl-git-user)

;;; verrazano lowlevel generator
(defun generate-binding* (name headers &rest args
                          &key (working-directory (verrazano::system-relative-pathname
                                                   :cl-git ""))
                          (debug nil)
                          (gccxml-flags "-I/usr/include")
                          &allow-other-keys)
  (format *debug-io* "~%~%; *** Processing binding ~S~%" name)
  (alexandria:remove-from-plistf args :working-directory :gccxml-flags :debug)
  (block try
    (handler-bind ((serious-condition
                    (lambda (error)
                      (unless debug
                        (warn "Failed to generated binding for ~S, error: ~A" name error)
                        (return-from try)))))
      (let ((*print-right-margin* 100))
        (verrazano:generate-binding (append
                           (list :cffi
                                 :package-name name
                                 :input-files headers
                                 :working-directory working-directory
                                 :gccxml-flags gccxml-flags)
                           args)
                          :keep-temporary-files nil))))
  (values))

(defun generate-libgit2-bindings ()
  (generate-binding*
   :libgit2
   '("git2/net.h"
     "git2/config.h"
     "git2/repository.h"
     "git2/types.h"
     "git2/reflog.h"
     "git2/revwalk.h"
     "git2/common.h"
     "git2/commit.h"
     "git2/blob.h"
     "git2/thread-utils.h"
     "git2/oid.h"
     "git2/refs.h"
     "git2/index.h"
     "git2/transport.h"
     "git2/errors.h"
     "git2/indexer.h"
     "git2/tree.h"
     "git2/branch.h"
     "git2/zlib.h"
     "git2/signature.h"
     "git2/odb.h"
     "git2/remote.h"
     "git2/refspec.h"
     "git2/status.h"
     "git2/object.h"
     "git2/tag.h")))

(generate-libgit2-bindings)
(in-package :cl-user)
(quit)
