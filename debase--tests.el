;;; debase--tests.el --- Tests for debase            -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: internal

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

;;

;;; Code:

(require 'debase)
(require 'ert)

(ert-deftest debase-test--property-readable? ()
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "read")))))
  (should (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))
  (should (null (debase--property-readable? '(property ((type . "ao") (name . "Devices") (access . "write")))))))


(ert-deftest debase-test--property-writeable? ()
  (should (null (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "read"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "write"))))))
  (should (eq t (debase--property-writeable? '(property ((type . "ao") (name . "Devices") (access . "readwrite")))))))

(ert-deftest debase-test--name-mangle ()
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

(ert-deftest debase-test--interface->name ()
  (should (string= "db-network-manager"
                   (debase--interface->name '((interface
                                               ((name . "org.freedesktop.NetworkManager"))))))))

(ert-deftest debase-test--property->slotdef ()
  (should (equal
           '(global-dns-configuration :type t :accessor db-prop-global-dns-configuration)
           (debase--property->slotdef '(property
                                        ((type . "a{sv}")
                                         (name . "GlobalDnsConfiguration")
                                         (access . "readwrite")))))))

(ert-deftest debase-test---interface-method->arglist ()
  (should (equal '(arg0) (debase--interface-method->arglist '(method ((name . "Reload")) "\n      " (arg ((type . "u") (direction . "in"))) "\n    "))))

  (should (equal '(flags) (debase--interface-method->arglist '(method ((name . "Reload")) "\n      " (arg ((type . "u") (name . "flags") (direction . "in"))) "\n    ")))))

(provide 'debase--tests)
;;; debase--tests.el ends here
