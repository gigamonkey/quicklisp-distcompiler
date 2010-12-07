;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :gigamonkey-distcompiler)

(defmacro with-dist-file-output ((name version what &key verbose) &body body)
  (with-gensyms (file)
    `(let ((,file (make-pathname :directory `(:relative ,,name ,,version) :name ,what :type "txt")))
       (with-output-to-file (*standard-output* ,file :ensure-directories t)
         (when ,verbose (format *trace-output* "~&Writing ~a" (truename *standard-output*)))
         ,@body))))

(defun compile-dist (file &key verbose)
  "Compile a dist defined in a file. The file contains a define-dist
expression that provides the name, version, base-url, and list of
projects in (name version) tuples. For each project the .tgz archive
name-version.tgz should exist in archives/."
  (let ((*package* #.*package*))
    (destructuring-bind (tag name (&key version base-url) &rest projects) (file->sexp file)
      (assert (eql tag 'define-dist))
      (write-dist-files (string-downcase name) version base-url projects :verbose verbose))))

(defun write-dist-files (name version base-url projects &key verbose)
  (setf base-url (string-right-trim "/" base-url))
  (write-distinfo name version base-url :verbose verbose)
  (write-releases name version base-url projects :verbose verbose)
  (write-systems name version projects :verbose verbose))

(defun write-distinfo (name version base-url &key verbose)
  "Write the distinfo file assuming a standard relation between the parts."
  (with-dist-file-output (name version "distinfo" :verbose verbose)
    (let ((web-dir (format nil "~a/~a" base-url name)))
      (format t "~&name: ~a" name)
      (format t "~&version: ~a" version)
      (format t "~&system-index-url: ~a/~a/systems.txt" web-dir version)
      (format t "~&release-index-url: ~a/~a/releases.txt" web-dir version)
      (format t "~&canonical-distinfo-url: ~a/~a/distinfo.txt" web-dir version)
      (format t "~&distinfo-subscription-url: ~a/current.txt" web-dir))))

(defun write-releases (name version base-url releases &key verbose)
  "Write the releases.txt file containing information about the
  released software available as part of the dist."
  (with-dist-file-output (name version "releases" :verbose verbose)
    (format t "~&# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]")
    (loop for (project version) in releases do (write-one-release base-url project version))))
  
(defun write-one-release (base-url project version)
  "Write out the line for a given version of a project. (Assumes the code is available in software/ and the archive in archives/.)"
  (let* ((prefix (format nil "~a-~a" project version))
         (archive-file (archive-name prefix))
         (code-directory (unpack-tgz archive-file "unpacked/"))
         (url (format nil "~a/archives/~a" base-url archive-file))
         (size (file-bytes archive-file))
         (md5 (file-md5 archive-file))
         (sha1 (file-sha1 archive-file))
         (system-files (find-asd-files code-directory)))
  (format t "~&~{~a~^ ~}" (list* project url size md5 sha1 prefix system-files))))

(defun write-systems (name version releases &key verbose)
  "Write the systems.txt file containing information about which
  systems the systems in the dist depend on."
  (with-dist-file-output (name version "systems" :verbose verbose)
    (write-line "# project system-file system-name [dependency1..dependencyN]")
    (loop for (project version) in releases do (write-project-systems project version))))

(defun write-project-systems (project version)
  (let ((prefix (format nil "~a-~a" project version)))
    (walk-directory 
     (unpacked-dir prefix)
     (lambda (file) 
       (when (string-equal (pathname-type file) "asd")
         (write-systems/asd-file project file))))))
  
(defun write-systems/asd-file (project file)
  "Emit the systems.txt lines for a given project and a single .asd file."
  (loop for system in (parse-asdf file) do
       (format t "~&~a ~a ~(~{~a~^ ~}~)" project (pathname-name file) system)))

(defun unpack-tgz (tgz dir)
  (let ((tar (tmp-tar tgz dir))
        (dir (pathname-as-directory dir)))
    (ql-gunzipper:gunzip tgz tar)
    (ql-minitar:unpack-tarball tar :directory dir)
    (when (probe-file tar)
      (ignore-errors (delete-file tar)))
    (let ((expected-dir (top-level-dir tgz dir)))
      (cond
        ((file-exists-p expected-dir) expected-dir)
        (t (error "~a didn't unpack into expected dir ~a" tgz expected-dir))))))
       
(defun unpacked-dir (prefix)
  (merge-pathnames (pathname-as-directory prefix) (pathname-as-directory "unpacked")))

(defun tmp-tar (tgz dir)
  (ensure-directories-exist
   (make-pathname 
    :directory (pathname-directory dir)
    :type "tar"
    :defaults tgz)))

(defun top-level-dir (tgz dir)
  (merge-pathnames (pathname-as-directory (pathname-name tgz)) dir))

(defun file-md5 (file)
  (ironclad:byte-array-to-hex-string (ironclad:digest-file :md5 file)))

(defun file-sha1 (file)
  (quicklisp-tarhash:content-hash file))

(defun archive-name (prefix)
  (make-pathname :directory '(:relative "archives") :name prefix :type "tgz"))



(defun find-asd-files (project-dir)
  (let ((files ())
        (project-dir (merge-pathnames project-dir)))
    (walk-directory 
     project-dir 
     (lambda (file)
       (when (string-equal (pathname-type file) "asd")
         (push (enough-namestring file project-dir) files))))
    files))


;;; Simple-minded ASDF parsing to extract system names and dependencies

(defun parse-asdf (file &key verbose)
  (let ((*package* (find-package :asdf)))
    (loop for form in (file->list file)
       when (not (consp form)) do
         (when verbose (warn "Ignoring non-cons form in ~a: ~s" file form))
       else when (not (eql (car form) 'asdf:defsystem)) do
         (when verbose (warn "Ignoring non-DEFSYSTEM form in ~a: (~s ...)" file (car form)))
       else collect (parse-defsystem form))))

(defun parse-defsystem (form)
  "Parse the name and dependencies out of a DEFSYSTEM form."
  (destructuring-bind (defsystem name &rest options) form
    (assert (eql defsystem 'asdf:defsystem))
    (cons name
          (loop for (k v) on options by #'cddr
             when (eql k :depends-on) nconc (dependencies v)))))

(defun dependencies (depends-on)
  (remove-if-not (lambda (x) (or (symbolp x) (stringp x))) depends-on))
