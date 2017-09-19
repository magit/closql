;;; closql.el --- store EIEIO objects using EmacSQL  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/closql
;; Package-Requires: ((emacs "25.1") (emacsql-sqlite "2.0.2"))
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Commentary:

;; Store uniform EIEIO objects in an EmacSQL database.  SQLite is used
;; as backend.  This library imposes some restrictions on what kind of
;; objects can be stored; it isn't intended to store arbitrary objects.
;; All objects have to share a common superclass and subclasses cannot
;; add any additional instance slots.

;; This is an alpha release.  Everything might change.  You probably
;; shouldn't use this in your own packages yet.  I am releasing this
;; now anyway because it is needed by Epkg, for which it was written.
;; Documentation will be written once the API has been stabilized.

;;; Code:

(require 'eieio)
(require 'emacsql-sqlite)

;;; Objects

(defclass closql-object ()
  ((closql-database :initarg :closql-database))
  :abstract t)

;;;; Oref

(defun eieio-oref--closql-oref (fn obj slot)
  (if (closql-object--eieio-childp obj)
      (closql-oref obj slot)
    (funcall fn obj slot)))

(advice-add 'eieio-oref :around #'eieio-oref--closql-oref)

(defun closql--oref (obj slot)
  (aref obj (eieio--slot-name-index (eieio--object-class obj) slot)))

(defun closql-oref (obj slot)
  (cl-check-type slot symbol)
  (cl-check-type obj (or eieio-object class))
  (let* ((class (cond ((symbolp obj)
                       (error "eieio-oref called on a class: %s" obj)
                       (let ((c (cl--find-class obj)))
                         (if (eieio--class-p c) (eieio-class-un-autoload obj))
                         c))
                      (t (eieio--object-class obj))))
         (c (eieio--slot-name-index class slot)))
    (if (not c)
        (if (setq c (eieio--class-slot-name-index class slot))
            (aref (eieio--class-class-allocation-values class) c)
          (slot-missing obj slot 'oref))
      (cl-check-type obj eieio-object)
      (let ((value (aref obj c))
            (columns (closql--slot-get obj slot :columns)))
        (if columns
            (if (eq value eieio-unbound)
                (pcase-let ((db (closql--oref obj 'closql-database))
                            (`(,where . ,select) (cl-coerce columns 'list)))
                  (setq value
                        (emacsql db [:select $i1 :from $i2
                                     :where (= $i3 $s4)
                                     :order-by [(asc $i3)]]
                                 (vconcat select) slot where
                                 (closql--oref
                                  obj (oref-default db primary-key))))
                  (aset obj c (if (= (length select) 1)
                                  (mapcar #'car value)
                                value)))
              value)
          (eieio-barf-if-slot-unbound value obj slot 'oref))))))

;;;; Oset

(defun eieio-oset--closql-oset (fn obj slot value)
  (if (closql-object--eieio-childp obj)
      (closql-oset obj slot value)
    (funcall fn obj slot value)))

(advice-add 'eieio-oset :around #'eieio-oset--closql-oset)

(defun closql--oset (obj slot value)
  (aset obj (eieio--slot-name-index (eieio--object-class obj) slot) value))

(defun closql-oset (obj slot value)
  (cl-check-type obj eieio-object)
  (cl-check-type slot symbol)
  (let* ((class (eieio--object-class obj))
         (c (eieio--slot-name-index class slot)))
    (if (not c)
        (if (setq c (eieio--class-slot-name-index class slot))
            (progn (eieio--validate-class-slot-value class c value slot)
                   (aset (eieio--class-class-allocation-values class) c value))
          (slot-missing obj slot 'oset value))
      (eieio--validate-slot-value class c value slot)
      (unless (eq slot 'closql-database)
        (let ((db (closql--oref obj 'closql-database)))
          (unless (or (not db) (eq db eieio-unbound))
            (closql--dset db obj slot value))))
      (aset obj c value))))

(defun closql--dset (db obj slot value)
  (let* ((primary-table (oref-default db primary-table))
         (primary-key   (oref-default db primary-key))
         (object-id (closql--oref obj primary-key))
         (columns (closql--slot-get obj slot :columns)))
    (if columns
        (emacsql-with-transaction db
          (setq columns (cl-coerce columns 'list))
          ;; Caller might have modified value in place.
          (closql--oset obj slot eieio-unbound)
          (let ((list1 (closql-oref obj slot))
                (list2 value)
                elt1 elt2)
            (when (= (length columns) 2)
              (setq list1 (mapcar #'list list1))
              (setq list2 (mapcar #'list list2)))
            ;; `list2' may not be sorted at all and `list1' has to
            ;; be sorted because Elisp and SQLite sort differently.
            (setq list1 (cl-sort list1 'string< :key #'car))
            (setq list2 (cl-sort list2 'string< :key #'car))
            (while (progn (setq elt1 (car list1))
                          (setq elt2 (car list2))
                          (or elt1 elt2))
              (let ((key1 (car elt1))
                    (key2 (car elt2)))
                (cond
                 ((and elt1 (or (not elt2) (string< key1 key2)))
                  (apply #'emacsql db
                         (vconcat
                          [:delete-from $i1 :where]
                          (closql--where-equal (cons object-id elt1) 1))
                         slot
                         (cl-mapcan #'list columns (cons object-id elt1)))
                  (pop list1))
                 ((string= key1 key2)
                  (unless (equal elt1 elt2)
                    (cl-mapcar
                     (lambda (col val1 val2)
                       (unless (equal val1 val2)
                         (emacsql db [:update $i1 :set (= $i2 $s3)
                                      :where (and (= $i4 $s5) (= $i6 $s7))]
                                  slot col val2
                                  (car  columns) object-id
                                  (cadr columns) key2)))
                     (cddr columns)
                     (cdr  elt1)
                     (cdr  elt2)))
                  (pop list1)
                  (pop list2))
                 (t
                  (emacsql db [:insert-into $i1 :values $v2]
                           slot (vconcat (cons object-id elt2)))
                  (pop list2)))))))
      (emacsql db [:update $i1 :set (= $i2 $s3) :where (= $i4 $s5)]
               primary-table slot
               (if (eq value eieio-unbound) 'eieio-unbound value)
               primary-key object-id))))

;;;; Slot Properties

(defun closql--slot-get (object-or-class slot prop)
  (let ((s (car (cl-member slot
                           (eieio-class-slots
                            (cond ((eieio-object-p object-or-class)
                                   (eieio--object-class object-or-class))
                                  ((eieio--class-p object-or-class)
                                   object-or-class)
                                  (t
                                   (find-class object-or-class 'error))))
                           :key #'cl--slot-descriptor-name))))
    (and s (cdr (assoc prop (cl--slot-descriptor-props s))))))

(defconst closql--slot-properties '(:columns))

(defun eieio-defclass-internal--set-closql-slot-props
    (cname _superclasses slots _options)
  (let ((class (cl--find-class cname)))
    (when (child-of-class-p class 'closql-object)
      (pcase-dolist (`(,name . ,slot) slots)
        (let ((slot-obj
               (car (cl-member name
                               (cl-coerce (eieio--class-slots class) 'list)
                               :key (lambda (elt) (aref elt 1))))))
          (dolist (prop closql--slot-properties)
            (let ((val (plist-get slot prop)))
              (when val
                (setf (alist-get prop (cl--slot-descriptor-props slot-obj))
                      val)))))))))

(advice-add 'eieio-defclass-internal :after
            #'eieio-defclass-internal--set-closql-slot-props)

(defun eieio--slot-override--set-closql-slot-props (old new _)
  (dolist (prop closql--slot-properties)
    (when (alist-get prop (cl--slot-descriptor-props new))
      (setf (alist-get prop (cl--slot-descriptor-props old))
            (alist-get prop (cl--slot-descriptor-props new))))))

(advice-add 'eieio--slot-override :after
            #'eieio--slot-override--set-closql-slot-props)

;;; Database

(defclass closql-database (emacsql-sqlite-connection)
  ((primary-table :allocation :class)
   (primary-key   :allocation :class)
   (object-class  :allocation :class)))

(cl-defmethod closql-db ((class (subclass closql-database))
                         &optional variable file debug)
  (or (let ((db (and variable (symbol-value variable))))
        (and db (emacsql-live-p db) db))
      (let ((db-init (not (and file (file-exists-p file))))
            (db (make-instance class :file file)))
        (set-process-query-on-exit-flag (oref db process) nil)
        (when debug
          (emacsql-enable-debugging db))
        (when db-init
          (closql--db-init db))
        (when variable
          (set variable db))
        db)))

(cl-defmethod closql--db-init ((db closql-database))
  (let* ((primary-table (oref-default db primary-table))
         (primary-key   (oref-default db primary-key))
         (object-class  (oref-default db object-class))
         (slots (mapcar #'cl--slot-descriptor-name
                        (eieio--class-slots (cl--find-class object-class)))))
    (emacsql-with-transaction db
      (emacsql db [:create-table $i1 $S2] primary-table
               (list (vconcat (list (list 'class :not-null)
                                    (list primary-key :not-null :primary-key))
                              (cddr slots))))
      (dolist (slot (cddr slots))
        (let ((columns (closql--slot-get object-class slot :columns)))
          (when columns
            (pcase-let ((`(,foreign ,key . ,rest) (cl-coerce columns 'list)))
              (emacsql
               db [:create-table $i1 $S2] slot
               (list (vconcat `((,foreign :not-null)
                                (,key     :not-null))
                              rest)
                     (list :primary-key (vector foreign key))
                     (list :foreign-key (vector foreign)
                           :references primary-table (vector primary-key)
                           :on-delete :cascade))))))))))

(cl-defmethod emacsql ((connection closql-database) sql &rest args)
  (mapcar #'closql--extern-unbound
          (apply #'cl-call-next-method connection sql
                 (mapcar (lambda (arg)
                           (if (stringp arg)
                               (let ((copy (copy-sequence arg)))
                                 (set-text-properties 0 (length copy) nil copy)
                                 copy)
                             arg))
                         args))))

(cl-defmethod closql-insert ((db closql-database) obj)
  (aset obj 1 db)
  (let (alist)
    (dolist (slot (eieio-class-slots (eieio--object-class obj)))
      (setq  slot (cl--slot-descriptor-name slot))
      (let ((columns (closql--slot-get obj slot :columns)))
        (when columns
          (push (cons slot (closql-oref obj slot)) alist)
          (closql--oset obj slot eieio-unbound))))
    (emacsql-with-transaction db
      (emacsql db [:insert-into $i1 :values $v2]
               (oref-default db primary-table)
               (let ((value (closql--intern-unbound (cl-coerce obj 'list))))
                 (vconcat (cons (closql--class-to-sql db (car value))
                                (cddr value)))))
      (pcase-dolist (`(,slot . ,value) alist)
        (closql--dset db obj slot value))))
  obj)

(cl-defmethod closql-delete ((db closql-database) (obj closql-object))
  (closql-delete db (eieio-oref obj (oref-default db primary-key))))

(cl-defmethod closql-delete ((db closql-database) ident)
  (emacsql db [:delete-from $i1 :where (= $i2 $s3)]
           (oref-default db primary-table)
           (oref-default db primary-key)
           ident))

(cl-defmethod closql-get ((db closql-database) ident)
  (let ((obj (car (emacsql db [:select * :from $i1
                               :where (= $i2 $s3)
                               :order-by [(asc $i2)]]
                           (oref-default db primary-table)
                           (oref-default db primary-key)
                           ident))))
    (and obj (closql--remake-instance db obj t))))

(cl-defmethod closql-entries ((db closql-database) &optional classes)
  (mapcar (apply-partially #'closql--remake-instance db)
          (closql-select db '* classes)))

(cl-defmethod closql-select ((db closql-database) select &optional classes)
  (emacsql db
           (vconcat (if (eq select '*)
                        [:select * :from $i2]
                      [:select $i1 :from $i2])
                    (and classes
                         [:where class :in $v3])
                    [:order-by [(asc $i4)]])
           select
           (oref-default db primary-table)
           (and classes (closql-where-class-in db classes))
           (oref-default db primary-key)))

(cl-defmethod closql--remake-instance ((db closql-database) row &optional resolve)
  (pcase-let ((`(,class . ,rest)
               (closql--extern-unbound row)))
    (let ((obj (vconcat (list (closql--sql-to-class db class)
                              db)
                        rest)))
      (when resolve
        (closql--resolve-slots obj))
      obj)))

(cl-defmethod closql--resolve-slots ((obj closql-object))
  (dolist (slot (eieio-class-slots (eieio--object-class obj)))
    (setq  slot (cl--slot-descriptor-name slot))
    (unless (slot-boundp obj slot)
      (closql--oset obj slot (closql-oref obj slot)))))

(defun closql--intern-unbound (row)
  (mapcar (lambda (elt)
            (if (eq elt eieio-unbound) 'eieio-unbound elt))
          row))

(defun closql--extern-unbound (row)
  (mapcar (lambda (elt)
            (if (eq elt 'eieio-unbound) eieio-unbound elt))
          row))

(defun closql--list-subclasses (class &optional result)
  (unless (class-abstract-p class)
    (cl-pushnew class result))
  (dolist (child (eieio--class-children (cl--find-class class)))
    (setq result (closql--list-subclasses child result)))
  result)

(defun closql--where-equal (value offset)
  (vector
   (cons 'and
         (mapcar (lambda (v)
                   (if v
                       (list '=
                             (intern (format "$i%i" (cl-incf offset)))
                             (intern (format "$s%i" (cl-incf offset))))
                     (list 'isnull
                           (intern (format "$i%i" (1- (cl-incf offset 2)))))))
                 value))))

(defun closql-where-class-in (db classes)
  (vconcat
   (mapcar (apply-partially #'closql--class-to-sql db)
           (cl-mapcan (lambda (sym)
                        (let ((str (symbol-name sym)))
                          (cond ((string-match-p "--eieio-childp\\'" str)
                                 (closql--list-subclasses
                                  (intern (substring str 0 -14)) nil))
                                ((string-match-p "-p\\'" str)
                                 (list (intern (substring str 0 -2))))
                                (t
                                 (list sym)))))
                      (if (listp classes) classes (list classes))))))

(cl-defmethod closql--class-to-sql ((_db closql-database) value)
  value)

(cl-defmethod closql--sql-to-class ((_db closql-database) value)
  value)

(cl-defmethod closql--set-object-class ((db closql-database) obj class)
  (let* ((primary-table (oref-default db primary-table))
         (primary-key   (oref-default db primary-key))
         (object-id (closql--oref obj primary-key)))
    (aset obj 0 (intern (format "eieio-class-tag--%s" class)))
    (emacsql db [:update $i1 :set (= class $s2) :where (= $i3 $s4)]
             primary-table
             (closql--class-to-sql db class)
             primary-key object-id)))

(provide 'closql)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; closql.el ends here
