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


(in-package #:cl-git-tests)

(in-suite :cl-git)


(defun write-string-to-file (repo-path filename content)
  (let ((test-file (concatenate 'string repo-path "/" filename)))
    (with-open-file (stream test-file :direction :output :if-exists :supersede)
      (format stream content))))

(defun make-commit1 (repo-path)
  (cl-git:with-git-repository-index
    (write-string-to-file repo-path "test1"
                          "Some test data~%...~%line 3 still nothing..~%getting on~%")
    (cl-git:git-index-add "test1")
    (cl-git:git-index-write)
    (cl-git:make-commit
     (cl-git:git-oid-from-index)
     "Committing test file test1"
     :author (list :name "Joe Blogs"
                   :email "test@example.com"
                   :time (local-time:unix-to-timestamp 1338033679))
     :committer (list :name "Jim Blogs"
                      :email "test1@example.com"
                      :time (local-time:unix-to-timestamp 1338044479)))))

(test create-commit
      "create a repository and add a file to it."
      (tempory-repository
       (path)
       (cl-git:with-git-repository (path)
         (cl-git:with-git-revisions
             (commit :sha (make-commit1 path))
           ;; check the commit message
           (is (equal (cl-git:commit-message commit)
                      (format-string "Committing test file test1~%")))
           ;; check the author
           (let ((author (cl-git:commit-author commit)))
             (is (equal (getf author :name)
                        "Joe Blogs"))
             (is (equal (getf author :email)
                        "test@example.com"))
             (is (equal (local-time:timestamp-to-unix (getf author :time))
                        1338033679)))
           ;; check the committer
           (let ((committer (cl-git:commit-committer commit)))
             (is (equal (getf committer :name)
                        "Jim Blogs"))
             (is (equal (getf committer :email)
                        "test1@example.com"))
             (is (equal (local-time:timestamp-to-unix (getf committer :time))
                        1338044479)))))))