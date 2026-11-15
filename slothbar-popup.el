;;; slothbar-popup.el --- Backend-neutral popup surfaces  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;;; Commentary:

;; Small backend-neutral popup handle.  The backend owns the surface,
;; buffers, and protocol objects.  Callers use this handle for
;; lifecycle and querying configure/dismiss state.

;;; Code:

(require 'cl-lib)
(require 'slothbar)
(require 'slothbar-backend)

(cl-defstruct (slothbar-popup
               (:constructor slothbar-popup--make)
               (:copier nil))
  "A popup owned by a display backend."
  backend
  bar
  x
  y
  width
  height
  state)

(defun slothbar-popup-create (bar x y width height)
  "Create a popup anchored to BAR at X,Y with WIDTH and HEIGHT.
Return a `slothbar-popup', or nil when the backend has no popup support."
  (let ((backend (slothbar-display-backend bar)))
    (when backend
      (when-let* ((state (slothbar-backend-create-popup
                          backend bar x y width height)))
        (slothbar-popup--make
         :backend backend
         :bar bar
         :x x :y y :width width :height height
         :state state)))))

(defun slothbar-popup-configured-p (popup)
  "Return non-nil when POPUP has been configured."
  (and popup
       (slothbar-backend-popup-configured-p
        (slothbar-popup-backend popup)
        (slothbar-popup-state popup))))

(defun slothbar-popup-done-p (popup)
  "Return non-nil when POPUP has been dismissed."
  (and popup
       (slothbar-backend-popup-done-p
        (slothbar-popup-backend popup)
        (slothbar-popup-state popup))))

(defun slothbar-popup-destroy (popup)
  "Destroy POPUP."
  (when popup
    (slothbar-backend-destroy-popup
     (slothbar-popup-backend popup)
     (slothbar-popup-state popup))
    (setf (slothbar-popup-state popup) nil))
  nil)

(provide 'slothbar-popup)
;;; slothbar-popup.el ends here
