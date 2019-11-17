;;; debase.el --- DBus<->EIEIO interface           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ian Eure

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

(cl-defun debase--name-mangle (dbus-name &key (prefix "db-"))
  "Mangle DBUS-NAME into something Lispier.

   ex. FooBARQuux -> foo-bar-quux."
  (let ((case-fold-search))
    (concat prefix (downcase (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" dbus-name)))))

(defun debase-interfaces (xml)
  "Return a list of supported D-Bus interfaces in XML."
  (cl-loop for child in (dom-non-text-children xml)
           when (eq 'interface (dom-tag child))
           collect (cdr (assoc 'name (dom-attributes child)))))

(defun debase--interface (xml interface-name)
  "Return definition of interface INTERFACE-NAME from introspected XML."
  (cl-loop for child in (dom-non-text-children xml)
           when (and (eq 'interface (dom-tag child))
                     (string= interface-name (cdr (assoc 'name (dom-attributes child)))))
           collect child))

(defun debase--interface-properties (interface-def)
  "Return properties for D-Bus interface INTERFACE-DEF."
  (cl-loop for child in (dom-non-text-children interface-def)
           when (eq 'property (dom-tag child))
           collect child))

(defun debase--interface-methods (interface-def)
  "Return methods for D-Bus interface INTERFACE-DEF."
  (cl-loop for child in (dom-non-text-children interface-def)
           when (eq 'method (dom-tag child))
           collect child))

(defun debase--interface->name (interface-def)
  "Return the EIEIO class name for D-Bus interface INTERFACE-DEF."
  (thread-last (dom-attributes interface-def)
    (assoc 'name)
    cdr
    (replace-regexp-in-string "^org\\.freedesktop\\." "")
    (replace-regexp-in-string "\\." "-")
    debase--name-mangle))

(defun debase--interface-method->arglist (method-def)
  "Return the CL argument list for METHOD-DEF."
  (cl-loop for child in (dom-non-text-children method-def)
           with i = 0
           when (eq 'arg (dom-tag child))
           when (string= "in" (cdr (assoc 'direction (dom-attributes child))))
           collect (intern (or (cdr (assoc 'name (dom-attributes child)))
                               (format "arg%d" i)))
           do (incf i)))

(defun debase--interface-method->defmethod (class-name interface-name method-def)
  "Return the EIEIO method definition for method METHOD-DEF.

   The method will be dispatched on EIEIO class CLASS-NAME."
  (let ((method-name (cdr (assoc 'name (dom-attributes method-def))))
        (args (debase--interface-method->arglist method-def)))
    `(cl-defmethod ,(intern (debase--name-mangle method-name)) ((obj ,class-name) ,@args)
       (with-slots (bus service path) obj
         (dbus-call-method bus service path ,interface
                           ,method-name
                           ,@args)))))

(defun debase--interface->methods (class-name interface-def)
  "Return EIEIO methods for INTERFACE-DEF, bound to CLASS-NAME."
  (mapcar (apply-partially #'debase--interface-method->defmethod class-name
                           (cdr (assoc 'name (dom-attributes interface-def))))
          (debase--interface-methods interface-def)))

(defun debase--property-readable? (property-def)
  "Is the property specified in PROPERTY-DEF writable?"
  (let ((access (cdr (assoc 'access (dom-attributes property-def)))))
    (or (string= "read" access)
        (string= "readwrite" access))))

(defun debase--property-writeable? (property-def)
  "Is the property specified in PROPERTY-DEF readable?"
  (let ((access (cdr (assoc 'access (dom-attributes property-def)))))
    (or (string= "write" access)
        (string= "readwrite" access))))

(defun debase--property->slotdef (property-def)
  "Return slot definition for property PROPERTY-DEF."
  (let ((property-name (cdr (assoc 'name (dom-attributes property-def)))))
    `(,(intern (debase--name-mangle property-name :prefix nil))
      :type t                           ; lol ugh FIXME
      :accessor ,(intern (debase--name-mangle (concat "prop-" property-name))))))

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

(defun debase--property->slot (class-name interface property-def)
  "Return slot def & helpers for D-Bus PROPERTY-DEF in CLASS-NAME.

   Returns a list of (SLOT-DEF [HELPER...])"

  ;; There's always a slot definition.
  (let* ((slot-and-helpers (list (debase--property->slotdef property-def)))
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
  ((bus :initarg :bus
        :type symbol
        :documentation "Bus the D-Bus object is on.")
   (service :initarg :service
            :type string
            :documentation "D-Bus service.")
   (path :initarg :path
         :type string
         :documentation "Path to D-Bus object.")
   (interface :initarg :interface
              :type string
              :documentation "D-Bus interface this class implements."))
  :abstract t
  :documentation "Base class for D-Bus objects.")

(defun define-debase-interface* (interface-def &optional bus service path)
  "Define an EIEIO class and methods for D-Bus interface INTERFACE-DEF.

   When BUS, SERVICE, or PATH are non-nil, they specify the
   default target of the interface.  This is useful for
   well-known D-Bus services which always have a single instance
   at a well-known location."
  (let* ((interface-name (cdr (assoc 'name (dom-attributes interface-def))))
         (class-name (intern (debase--interface->name interface-def)))
         (properties (debase--interface-properties interface-def))
         (methods (debase--interface-methods interface-def))
         (slots-and-helpers (mapcar (apply-partially #'debase--property->slot class-name interface-name) properties)))
    `(prog1
         (defclass ,class-name
           (debase--dbus)             ; Inherit from this base

           (,(when bus `(bus :initform ,bus))
            ,(when service `(service :initform ,service))
            ,(when path `(path :initform ,path))
            (interface :initform ,interface-name)
           ,@(mapcar #'car slots-and-helpers)))
       ;; TODO:
       ;; - Maybe(?) override constructor with one that fetches property values.
       ;; - Override constructor with one that subscribes to property updates.

       ;; Interface methods
       ,@(debase--interface->methods class-name interface-def)

       ;; Slot helpers -- getters and setf support.
       ,@(apply #'append (mapcar #'cdr slots-and-helpers)))))

(defun define-debase-interface (bus service path interface)
  "Define class and methods for SERVICE, PATH, and INTERFACE, on BUS.

   The class name is taken from the interface, and cannot be specified."
  (if-let ((interface-def (thread-first
                              (dbus-introspect-xml bus service path)
                            (debase--interface interface))))
      (eval (define-debase-interface* interface-def bus service path))
    (error "Introspection failed")))

 ;; Tests

(ert-deftest debase--test-property-readable? ()
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "read")))))
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))
  (should (null (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "write")))))))


(ert-deftest debase--test-property-writeable? ()
  (should (null (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "read"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "write"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))))

(ert-deftest debase--test-name-mangle ()
  (should (string= "db-foo-bar" (debase--name-mangle "FooBar")))
  (should (string= "foo-bar" (debase--name-mangle "FooBar" :prefix nil))))


(defun debase--interface->name (interface-def)
  "Return the EIEIO class name for D-Bus interface INTERFACE-DEF."
  (thread-last (dom-attributes interface-def)
    (assoc 'name)
    cdr
    (replace-regexp-in-string "^org\\.freedesktop\\." "")
    (replace-regexp-in-string "\\." "-")
    debase--name-mangle))

(ert-deftest debase--test-interface->name ()
  (should (string= "db-network-manager"
                   (debase--interface->name '((interface
                                                 ((name . "org.freedesktop.NetworkManager"))))))))

(ert-deftest debase--test-property->slotdef ()
  (should (equal
           '(global-dns-configuration :type t :accessor db-prop-global-dns-configuration)
           (debase--property->slotdef '(property
                                          ((type . "a{sv}")
                                           (name . "GlobalDnsConfiguration")
                                           (access . "readwrite")))))))

(ert-deftest debase--test--interface-method->arglist ()
  (should (equal '(arg0) (debase--interface-method->arglist '(method ((name . "Reload")) "\n      " (arg ((type . "u") (direction . "in"))) "\n    "))))

  (should (equal '(flags) (debase--interface-method->arglist '(method ((name . "Reload")) "\n      " (arg ((type . "u") (name . "flags") (direction . "in"))) "\n    ")))))

(provide 'debase)
;;; debase.el ends here
