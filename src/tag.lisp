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

(defparameter *tag-values* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Low-level interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("git_tag_list" %git-tag-list)
    %return-value
  (strings :pointer)
  (repository %repository))

(defcfun ("git_tag_foreach" %git-tag-foreach)
    %return-value
  (repository %repository)
  (callback :pointer)
  (payload :pointer))

(defcfun ("git_tag_create" %git-tag-create)
    %return-value
  (oid %oid)
  (repo %repository)
  (tag-name :string)
  (target %object)
  (tagger %git-signature)
  (message :string)
  (force :boolean))

(defcfun ("git_tag_target" %git-tag-target)
    %return-value
  (target-out :pointer)
  (tag %tag))

(defcfun ("git_tag_peel" %git-tag-peel)
    %return-value
  (reference :pointer)
  (tag %tag))

(defcfun ("git_tag_tagger" git-tag-tagger)
    %git-signature
  (tag %tag))

(defcfun ("git_tag_name" git-tag-name)
    :string
  "Returns the name of the tag"
  (tag %tag))

(defcfun ("git_tag_message" git-tag-message)
    :string
  "Returns the message of the tag"
  (tag %tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlevel Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tag (object)
  ())

(defgeneric tag-p (tag)
  (:documentation
   "Return T if the reference is within the git tag namespace.")
  (:method ((tag string))
    (when (eq 0 (search reference-tags-dir tag))
      t))
  (:method ((tag tag))
    (tag-p (full-name tag))))

(defun make-tag (name message &key
                                repository
                                target
                                tagger
                                force)
  ;; TODO (RS) should catch exists error and offer a continue
  (with-foreign-object (newoid '(:struct git-oid))
    (with-foreign-strings ((%name name)
                           (%message message))
      (%git-tag-create newoid repository %name target tagger %message force))
    (get-object 'tag (convert-from-foreign newoid '%oid) repository)))


(defmethod list-objects ((class (eql 'tag)) repository &key test test-not)
  "Returns a list of tag for the repository.  If the tag is an
annotated tag then a TAG object will be returned, otherwise it will be
a ref with the in the tag namespace."
  (with-foreign-object (string-array '(:struct git-strings))
    (%git-tag-list string-array repository)
    (let ((refs
            (mapcar (lambda (ref-name)
                      (let* ((ref (make-reference-from-name
                                   (concatenate 'string reference-tags-dir ref-name)
                                   repository))
                             (target (target ref)))
                        (case (type-of target)
                          ('tag target)
                          (t ref))))
                    (prog1
                        (convert-from-foreign string-array '%git-strings)
                      (free-translated-object string-array '%git-strings t)))))
      (cond (test
                (remove-if-not test refs))
            (test-not
             (remove-if test-not refs))
            (t
             refs)))))

(defmethod get-object ((class (eql 'tag)) oid repository)
  (git-object-lookup oid class repository))

(defmethod full-name ((tag tag))
  (concatenate 'string reference-tags-dir (short-name tag)))

(defmethod short-name ((tag tag))
  (git-tag-name tag))

(defgeneric tagger (object)
  (:documentation "Returns the signature of the tagger of OBJECT.

The return value is a signature (a property list with
keys :NAME, :EMAIL and :TIME.  If the tag is not annotated then nil
will be returned.")
  (:method ((tag tag))
    (git-tag-tagger tag))
  (:method tagger ((tag reference))
    nil))

(defmethod message ((tag tag))
  (git-tag-message tag))

(defmethod target ((tag tag))
  "Returns the target of a tag."
  (with-foreign-object (%object :pointer)
    (%git-tag-target %object tag)
    (make-instance-object :pointer (mem-ref %object :pointer)
                          :type :any
                          :facilitator (facilitator tag))))

(defmethod git-peel ((tag tag))
  "Peels layers of the tag until the resulting object is not a tag anymore.
Basically calls GIT-TARGET on tag and if the result of that is a TAG,
repeat the process."
  (with-foreign-object (%object :pointer)
    (%git-tag-peel %object tag)
    (make-instance-object :pointer (mem-ref %object :pointer)
                          :type :any
                          :facilitator (facilitator tag))))
