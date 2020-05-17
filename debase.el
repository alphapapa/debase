;;; debase.el --- DBus<->EIEIO interface           -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: lisp, unix
;; URL: https://github.com/ieure/debase
;; Version: 0.5
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lol

;;; Code:

(require 'dbus)
(require 'gv)
(require 'dom)

(defconst debase--prefix "debase-"
  "Default prefix for Debase-generated symbols.")

(defvar debase--class-cache nil
  "Cache for already-created classes.  The interface name is the key.")

(defvar debase--ignore-interfaces
  '("org.freedesktop.DBus.Properties"
    "org.freedesktop.DBus.Introspectable"
    "org.freedesktop.DBus.Peer")
  "Interfaces to ignore.")

(defun debase--name-mangle (dbus-name &optional options)
  "Mangle DBUS-NAME into something Lispier.

   ex. FooBARQuux -> foo-bar-quux."
  (let ((case-fold-search)
        (prefix (cond ((memq :prefix options) (plist-get options :prefix))
                      (t debase--prefix))))
    (concat prefix (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" dbus-name)))))

(defun debase-interface-names (xml)
  "Return a list of supported D-Bus interface names in XML."
  (cl-loop for child in (dom-non-text-children xml)
           when (eq 'interface (dom-tag child))
           collect (debase-interface-name child)))

(defun debase-interface-name (interface-def)
  "Return the name of the interface in INTERFACE-DEF XML."
  (cdr (assoc 'name (dom-attributes interface-def))))

(defun debase--interface (xml interface-name)
  "Return definition of interface INTERFACE-NAME from introspected XML."
  (cl-loop for child in (dom-non-text-children xml)
           when (and (eq 'interface (dom-tag child))
                     (string= interface-name (cdr (assoc 'name (dom-attributes child)))))
           collect child))

(defun debase--interface-properties (interface-def)
  "Return properties for D-Bus interface INTERFACE-DEF."
  (thread-first (lambda (child) (eq 'property (dom-tag child)))
    (cl-remove-if-not (dom-non-text-children interface-def))))

(defun debase--interface-methods (interface-def)
  "Return methods for D-Bus interface INTERFACE-DEF."
  (thread-first (lambda (child) (eq 'method (dom-tag child)))
    (cl-remove-if-not (dom-non-text-children interface-def))))

(defun debase--interface->name (interface-def &optional options)
  "Return the EIEIO class name for D-Bus interface INTERFACE-DEF."
  (debase--name-mangle
   (thread-last (dom-attributes interface-def)
     (assoc 'name)
     cdr
     (replace-regexp-in-string "^org\\.freedesktop\\." "")
     (replace-regexp-in-string "\\." "-"))
   options))

(defun debase--interface-method->arglist (method-def)
  "Return the CL argument list for METHOD-DEF."
  (cl-loop for child in (dom-non-text-children method-def)
           with i = 0
           when (eq 'arg (dom-tag child))
           when (string= "in" (cdr (assoc 'direction (dom-attributes child))))
           collect (intern (or (cdr (assoc 'name (dom-attributes child)))
                               (format "arg%d" i)))
           do (incf i)))

(defun debase--interface-method->defmethod (options class-name interface-name method-def)
  "Return the EIEIO method definition for method METHOD-DEF.

   The method will be dispatched on EIEIO class CLASS-NAME."
  (let ((method-name (cdr (assoc 'name (dom-attributes method-def))))
        (args (debase--interface-method->arglist method-def)))
    `(cl-defmethod ,(intern (debase--name-mangle method-name options)) ((obj ,class-name) ,@args)
       (with-slots (bus service path) obj
         (dbus-call-method bus service path ,interface-name
                           ,method-name
                           ,@args)))))

(defun debase--interface->methods (options class-name interface-def)
  "Return EIEIO methods for INTERFACE-DEF, bound to CLASS-NAME."
  (mapcar (apply-partially #'debase--interface-method->defmethod options class-name
                           (cdr (assoc 'name (dom-attributes interface-def))))
          (debase--interface-methods interface-def)))

(defun debase--property-readable? (property-def)
  "Is the property specified in PROPERTY-DEF writable?"
  (let ((access (cdr (assoc 'access (dom-attributes property-def)))))
    (or (string= "read" access)
        (string= "readwrite" access))))

(defun debase--property-writeable? (property-def)
  "Is the property specified in PROPERTY-DEF writeable?"
  (let ((access (cdr (assoc 'access (dom-attributes property-def)))))
    (or (string= "write" access)
        (string= "readwrite" access))))

(defun debase--property->slotdef (property-def &optional options)
  "Return slot definition for property PROPERTY-DEF."
  (let ((property-name (cdr (assoc 'name (dom-attributes property-def)))))
    ;; Ignore the prefix for the property name's slot.
    `(,(intern (debase--name-mangle property-name))
      :type t                           ; lol ugh FIXME
      ;; But use it for the accessor.
      :accessor ,(intern (debase--name-mangle (concat "prop-" property-name) options)))))

(defun debase--property->dbus-accessor (class-name interface accessor-symbol property-name)
  "Return a default (D-Bus) property accessor.

Creates a generic method template named ACCESSOR-SYMBOL, which
returns the value of PROPERTY-NAME, and binds it to the
CLASS-NAME class."
  `(cl-defmethod ,accessor-symbol ((this ,class-name))
     (with-slots (bus service path) this
       (dbus-get-property bus service path ,interface
                          ,property-name))))

(defun debase--property->error-accessor (class-name interface accessor-symbol property-name)
  "Return an error property accessor.

This is used for write-only D-Bus propertues.

Creates a generic method template named ACCESSOR-SYMBOL, which
attempts to access PROPERTY-NAME, but returns an error; and binds
it to the CLASS-NAME class."
  `(cl-defmethod ,accessor-symbol ((this ,class-name))
     (error "Property `%s' isn't readable" ,property-name)))

(defun debase--property->slot (class-name interface property-def &optional options)
  "Return slot def & helpers for D-Bus PROPERTY-DEF in CLASS-NAME.

   Returns a list of (SLOT-DEF [HELPER...])"

  ;; There's always a slot definition.
  (let* ((slot-and-helpers (list (debase--property->slotdef property-def options)))
         (slotname (caar slot-and-helpers))
         (accessor (plist-get (cdar (last slot-and-helpers)) :accessor))
         (property-name (cdr (assoc 'name (dom-attributes property-def)))))

    ;; There's always a reader, but it might be one that just errors.
    (push (funcall (if (debase--property-readable? property-def)
                       'debase--property->dbus-accessor
                     'debase--property->error-accessor)
                   class-name
                   interface
                   accessor
                   property-name)
          slot-and-helpers)

    ;; There's sometimes a writer.
    (when (debase--property-writeable? property-def)
      ;; Clear the setter, if there is one, otherwise `gv-setter' complains.
      (put accessor 'gv-expander nil)
      (push
       `(gv-define-setter ,accessor (val obj)
          (backquote (with-slots (bus service path interface) ,',obj
            (dbus-set-property bus service path interface ,property-name ,',val)
            (oset ,',obj ,slotname ,',val))))
       slot-and-helpers))

    (reverse slot-and-helpers)))

(defclass debase--dbus ()
  ((interface :type string
              :documentation "D-Bus interface this class implements."))
  :abstract t
  :documentation "Base class for D-Bus interface classes.")

(defclass debase--dbus-object ()
  ((bus :initarg :bus
        :type symbol
        :documentation "Bus the D-Bus object is on.")
   (service :initarg :service
            :type string
            :documentation "D-Bus service.")
   (path :initarg :path
         :type string
         :documentation "Path to D-Bus object.")
   (interfaces :type list
               :documentation "Interfaces this object implements."))
  :abstract t
  :documentation "Base class for D-Bus objects.")

(defun define-debase-interface** (interface-def &rest options)
  "Define an EIEIO class and methods for D-Bus interface INTERFACE-DEF.

OPTIONS is an alist supporting the following keywords:

  :prefix - Generated symbols will be prefixed by this string, instead of `debase-'.
  :name - The generated class will be named this."
  (let* ((interface-name (cdr (assoc 'name (dom-attributes interface-def))))
         (class-name (pcase (or (plist-get options :name)
                                (debase--interface->name interface-def options))
                       ((and s (pred symbolp)) s)
                       ((and s (pred stringp)) (intern s))))
         (properties (debase--interface-properties interface-def))
         (methods (debase--interface-methods interface-def))
         (slots-and-helpers (mapcar (lambda (prop-def) (debase--property->slot class-name interface-name prop-def options)) properties)))
    `(prog1
         (defclass ,class-name
           (debase--dbus)             ; Inherit from this base

           ((interface :initform ,interface-name)
           ,@(mapcar #'car slots-and-helpers)))
       ;; TODO:
       ;; - Maybe(?) override constructor with one that fetches property values.
       ;; - Override constructor with one that subscribes to property updates.

       ;; Interface methods
       ,@(debase--interface->methods options class-name interface-def)

       ;; Slot helpers -- getters and setf support.
       ,@(apply #'append (mapcar #'cdr slots-and-helpers)))))

(defun define-debase-interface* (interface-def &rest options)
  (let ((interface-name (debase-interface-name interface-def)))
    (cdr (or (and (not (plist-get options :no-cache))
                  (assoc interface-name debase--class-cache))
             (car (push (cons interface-name
                              (eval (apply 'define-debase-interface** interface-def options)))
                        debase--class-cache))))))

(defun define-debase-interface (bus service path interface &rest options)
  "Define class and methods for SERVICE, PATH, and INTERFACE, on BUS.

The class name is generated using a prefix and the name of the interface.

OPTIONS allows specification of paramaters which control the generated class.

`:prefix' is a string which will be prepended to symbols.  The
 default is \"debase-\".

`:no-cache', when non-nil, will generate a new class instead of
 looking for one in `debase--class-cache'."
  (if-let ((interface-def (thread-first
                              (dbus-introspect-xml bus service path)
                            (debase--interface interface))))
      (apply #'define-debase-interface* interface-def options)
    (error "Introspection failed")))

(defun debase--object-interfaces (xml &optional interfaces)
  (cl-loop for interface in
           (let ((object-interfaces (debase-interfaces xml)))
             (cond
              ((eq interfaces :all) object-interfaces)
              ((consp interfaces) interfaces)
              (t (cl-loop for interface in object-interfaces
                 unless (member interface debase--ignore-interfaces)
                 collect interface))))

           collect (debase--interface xml interface)))

(defun debase-make-instance (bus service path &rest options)
  (let* ((classes (thread-last (plist-get options :interfaces)
                    (debase--object-interfaces (dbus-introspect-xml bus service path))
                    (mapcar (lambda (interface-def)
                              (apply #'define-debase-interface* interface-def options)))
                    (cons 'debase--dbus-object)))
         (class-name
          (gensym (concat (or (plist-get options :prefix) debase--prefix) "composite--" (mapconcat #'symbol-name classes "&")))))

    (message "Defining class %s, composite of %s" class-name classes)

    (eval `(defclass ,class-name ,classes nil))
    (make-instance class-name
                   :bus bus
                   :service service
                   :path path)))

(provide 'debase)
;;; debase.el ends here
