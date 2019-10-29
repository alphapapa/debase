# Debase, the DBus<->EIEIO bridge.

![img](sorry.jpg)

D-Bus is an [IPC system](https://en.wikipedia.org/wiki/Inter-process_communication) which is ubiquitous on Linux, and (in this author’s opinion) not very good. Emacs has bindings for interfacing with it (see the former point), which are annoying to use (see the latter point).

These days, numerous common system management tasks are implemented as D-Bus services rather than tradidional \*nix commands, and many of the command-line tools themselves are now front-ends which communicate via D-Bus. Mounting and unmounting disks, monitoring battery status, controlling display brightness, connecting to wireless networks and more are now handled with D-Bus services.

It makes no sense to shell out to the tools when one could interact with them directly via D-Bus, if only it was less annoying to do so.

Debase frees you from writing repetitive, annoying boilerplate code to drive D-Bus services by throwing another pile of abstraction at the problem, in the form of unreadably dense, macro-heavy, profoundly cursed Lisp.


## Usage

While debase is mildly useful on its own, it’s best used as a base layer for building domain-specific functionality on top of.

Debase creates one EIEIO class per D-Bus interface. The interface is discovered using reflection, by examining a particular service and path on a bus.

```emacs-lisp
(require 'debase)

(define-debase-interface
  :system                               ; The bus to define the interface for
  "org.freedesktop.NetworkManager"      ; D-Bus service to call
  ;; Path to the D-Bus object
  "/org/freedesktop/NetworkManager"
  ;; Interface to define an EIEIO implementation of.
  ;; Note that nearly every D-Bus object implements multiple interfaces.
  "org.freedesktop.NetworkManager")
```

    db-network-manager

All names are fixed, derived from the D-Bus interface specification, prefixed with `db-` where appropriate, and cannot be specified or changed.

Even though the `db-network-manager` class is reusable & may be targeted at any D-Bus object, the bus/service/path values from the definition are used as defaults, which is handy for well-known interfaces like UPower, UDisks2, NetworkManager, etc.

```emacs-lisp
(setq my/nm (db-network-manager))
```

D-Bus methods are implemented as CL generic methods:

```emacs-lisp
(db-get-permissions my/nm)
```

    (("org.freedesktop.NetworkManager.enable-disable-network" "yes") ("org.freedesktop.NetworkManager.sleep-wake" "no") ("org.freedesktop.NetworkManager.enable-disable-wifi" "yes") ("org.freedesktop.NetworkManager.enable-disable-wwan" "yes") ("org.freedesktop.NetworkManager.enable-disable-wimax" "yes") ("org.freedesktop.NetworkManager.network-control" "yes") ("org.freedesktop.NetworkManager.wifi.share.protected" "yes") ("org.freedesktop.NetworkManager.wifi.share.open" "yes") ("org.freedesktop.NetworkManager.settings.modify.system" "yes") ("org.freedesktop.NetworkManager.settings.modify.own" "yes") ("org.freedesktop.NetworkManager.settings.modify.hostname" "auth") ("org.freedesktop.NetworkManager.settings.modify.global-dns" "auth") ("org.freedesktop.NetworkManager.reload" "auth") ("org.freedesktop.NetworkManager.checkpoint-rollback" "auth") ("org.freedesktop.NetworkManager.enable-disable-statistics" "yes") ("org.freedesktop.NetworkManager.enable-disable-connectivity-check" "yes"))

Each D-Bus property has an accessor function with a `db-prop-` prefix.

```emacs-lisp
(db-prop-wwan-enabled my/nm)
```

    nil

> **Tip**
> 
> The `*Help*` buffer produced with `C-h f db-network-manager RET` lists all available properties and methods.

> **Aside for those who know about EIEIO and/or CLOS**
> 
> While properties are also slots in the object, do not use `oref` or `slot-value` to access them. They only return the locally cached (possbly unbound) value from the object; the accessor is required to actually fetch the current value from D-Bus.

For properties specified as writable in the D-Bus interface, they’re `setf`-able:

```emacs-lisp
(setf (db-prop-wwan-enabled my/nm) nil) ; returns the value that was set, nil
(db-prop-wwan-enabled my/nm)            ; now returns nil
```

    nil