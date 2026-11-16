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
  state
  motion-function
  button-function
  done-function)

(defvar slothbar-popup--active nil
  "Popups currently eligible for pointer input.")

(defun slothbar-popup--at-surface (backend surface)
  "Return the active popup on BACKEND at SURFACE."
  (when surface
    (cl-find-if
     (lambda (popup)
       (and (eq backend (slothbar-popup-backend popup))
            (eq surface
                (slothbar-backend-popup-surface
                 backend (slothbar-popup-state popup)))))
     slothbar-popup--active)))

(defun slothbar-popup-dispatch-motion (backend surface x y)
  "Forward motion at X,Y on SURFACE to an active popup."
  (when-let* ((popup (slothbar-popup--at-surface backend surface))
              (function (slothbar-popup-motion-function popup)))
    (funcall function x y)
    t))

(defun slothbar-popup-dispatch-done (backend state)
  "Forward a popup-done event for BACKEND's popup STATE."
  (when-let* ((popup (cl-find-if
                      (lambda (candidate)
                        (and (eq backend (slothbar-popup-backend candidate))
                             (eq state (slothbar-popup-state candidate))))
                      slothbar-popup--active))
              (function (slothbar-popup-done-function popup)))
    (funcall function)
    t))

(defun slothbar-popup-dispatch-button (backend surface x y button)
  "Forward a BUTTON press at X,Y on SURFACE to an active popup."
  (when-let* ((popup (slothbar-popup--at-surface backend surface))
              (function (slothbar-popup-button-function popup)))
    (funcall function x y button)
    t))

(defun slothbar-popup-create (bar x y width height)
  "Create a popup anchored to BAR at X,Y with WIDTH and HEIGHT.
Return a `slothbar-popup', or nil when the backend has no popup support."
  (let ((backend (slothbar-display-backend bar)))
    (when backend
      (when-let* ((state (slothbar-backend-create-popup
                          backend bar x y width height)))
        (let ((popup (slothbar-popup--make
                      :backend backend
                      :bar bar
                      :x x :y y :width width :height height
                      :state state)))
          (push popup slothbar-popup--active)
          popup)))))

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

(defun slothbar-popup-present (popup frame)
  "Present FRAME in POPUP."
  (when popup
    (slothbar-backend-present-popup
     (slothbar-popup-backend popup)
     (slothbar-popup-state popup)
     frame)))

(defun slothbar-popup-close-for-bar (bar)
  "Close active popups anchored to BAR."
  (dolist (popup (copy-sequence slothbar-popup--active))
    (when (eq bar (slothbar-popup-bar popup))
      (if-let* ((function (slothbar-popup-done-function popup)))
          (funcall function)
        (slothbar-popup-destroy popup)))))

(defun slothbar-popup-close-all ()
  "Close every active popup."
  (dolist (popup (copy-sequence slothbar-popup--active))
    (if-let* ((function (slothbar-popup-done-function popup)))
        (funcall function)
      (slothbar-popup-destroy popup))))

(defun slothbar-popup-destroy (popup)
  "Destroy POPUP."
  (when popup
    (setq slothbar-popup--active (delq popup slothbar-popup--active))
    (slothbar-backend-destroy-popup
     (slothbar-popup-backend popup)
     (slothbar-popup-state popup))
    (setf (slothbar-popup-state popup) nil))
  nil)

(provide 'slothbar-popup)
;;; slothbar-popup.el ends here
