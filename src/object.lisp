;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package #:cl-git)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type object-type ()
  ((git-type :reader git-type :initarg :git-type))
  (:actual-type :pointer))

(define-parse-method %object (&key (type :any))
  (make-instance 'object-type :git-type type))

(defcenum git-object-type
  (:any -2)       ; Object can be any of the following
  (:bad -1)       ; Object is invalid.
  (:commit 1)     ; A commit object.
  (:tree 2)       ; A tree (directory listing) object.
  (:blob 3)       ; A file revision object.
  (:tag 4)        ; An annotated tag object.
  (:ofs_delta 6)  ; A delta, base is given by an offset.
  (:ref_delta 7)) ; A delta, base is given by object id.

(defcfun ("git_object_id" git-object-id)
    %oid
  "Returns the oid identifying `object'"
  (object %object))

(defcfun ("git_object_type" git-object-type)
    git-object-type
  "Returns the type of the git object."
  (object %object))

(defcfun ("git_object_lookup" %git-object-lookup)
    %return-value
  (object %object)
  (repo :pointer)
  (oid %oid)
  (type git-object-type))

(defcfun ("git_object_free" git-object-free)
    :void
  "Free the git object."
  (object %object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign type translation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass object ()
  ((%object :accessor pointer :initarg :pointer :initform (null-pointer))
   (%repository :accessor %repository :initarg :repository-pointer)))

(defmethod translate-to-foreign (value (type object-type))
  "No-op translation in case the value is already a cffi pointer"
  (if (pointerp value)
      value
      (error "Cannot convert object ~S to C-pointer to git object" value)))

(defmethod translate-to-foreign ((value object) (type object-type))
  "Converts the value to a cffi pointer.  Note that rudimentary libgit type checking is done.
If the type of the `object' is known and the type specifier `type' requires a specific
type, the types are checked for compatibility."
  (pointer value))

(defmethod translate-from-foreign (value (type object-type))
  "Convers a pointer to a libgit type to a CLOS instance wrapping this pointer.
If the `type' specifier is :any, an additional call into libgit will be made
to get the actual type of the git object.  If the libgit type is specified in `type'
no extra call will be made.

The CLOS object has a finalize method to free the libgit underlying object."
  (let* ((git-type (git-type type))
	 (obj-type (case (or (unless (eq git-type :any) git-type
			       (git-object-type value)))
		     (:commit 'commit)
		     (:tag 'tag)
		     (:tree 'tree)
		     (t 'object)))
	 (object (make-instance obj-type
				:pointer value
				:repository-pointer *git-repository*)))
    (finalize object (lambda ()
		       (git-object-free value)))
    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+nil (defun make-instance-object (&key object-ptr (repository-ptr *git-repository*) type)
  (let* ((obj-type (case (or (unless (eq type :any) type)
                             (git-object-type object-ptr))
                     (:commit 'commit)
                     (:tag 'tag)
                     (:tree 'tree)
                     (t 'object)))
         (object (make-instance obj-type
                                :pointer object-ptr
                                :repository-pointer repository-ptr)))
    (finalize object
	      (lambda ()
		(git-object-free object-ptr)))
    object))

(defun git-object-lookup (oid type)
  "Returns a reference to the git odb (object) which is identified by the oid.
The type argument specifies which type is expected.  If the found
object is not of the right type, an error will be signaled.  The type
is one of :any, :bad, :commit :tree :blob :tag :ofs_delta :refs_delta.
:any and :bad are special cases.  :any means return the object found,
do not do a typecheck and is a valid type, but should typically not
occur."

  (assert (not (null-or-nullpointer *git-repository*)))

  (with-foreign-object (obj-ptr :pointer)
    (%git-object-lookup obj-ptr *git-repository* oid type)
    (convert-from-foreign (mem-ref obj-ptr :pointer) '(%object :type type))))
