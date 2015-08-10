;;; magit-filenotify.el --- Refresh status buffer when git tree changes -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Created: 17 Jul 2013
;; Keywords: tools
;; Package-Requires: ((magit "1.3.0") (emacs "24.4") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module comes with a minor mode `magit-filenotify' which tracks
;; changes in the source tree using `file-notify' and refreshes the magit
;; status buffer.  Emacs 24.4 with `file-notify-support' is required for it to
;; work.

;;; Code:

(require 'magit)
(require 'cl-lib)
(require 'filenotify)

(defgroup magit-filenotify nil
  "Refresh status buffer if source tree changes"
  :prefix "magit-filenotify"
  :group 'magit-extensions)

(defcustom magit-filenotify-ignored '("\\`\\.#" "\\`flycheck_")
  "A list of regexp for filenames that will be ignored by the callback."
  :group 'magit-filenotify
  :type '(repeat regexp))

(defun magit-filenotify--directories ()
  "List all directories containing files watched by git."
  (cons
   default-directory
   (cl-remove-duplicates
    (cl-loop for file in (magit-git-lines "ls-files")
             for dir = (file-name-directory (magit-decode-git-path file))
             when dir
             collect (expand-file-name dir))
    :test #'string=)))

;; Use #'equal as test because watch-descriptors aren't always integers.  With
;; inotify, descriptors can be lists like (17).
(defvar magit-filenotify-data (make-hash-table :test #'equal)
  "A hash table to map watch-descriptors to a list (DIRECTORY STATUS-BUFFER).")

(defvar magit-filenotify--idle-timer nil
  "Timer which will refresh buffers when emacs becomes idle.")

(defvar magit-filenotify-idle-delay 1.57
  "Number of seconds to wait before refreshing.")

(defvar magit-filenotify--buffers nil
  "List of magit status buffers to be refreshed.
Those will be refreshed after `magit-filenotify-idle-delay' seconds.")

(defun magit-filenotify--refresh ()
  (dolist (b magit-filenotify--buffers)
    (when (buffer-live-p b)
      (with-current-buffer b
        ;; `magit-refresh' runs the functions in `magit-pre-refresh-hook' which
        ;; contains `magit-maybe-save-repository-buffers'.  This function
        ;; queries the user to save repository buffers.  That's nice for
        ;; interactive use but it's bad here because when you edit, save, and
        ;; start editing again, you'll get that query after
        ;; `magit-filenotify-idle-delay'.
        (defvar magit-pre-refresh-hook) ; Workaround Emacs bug#21311.
        (let ((magit-pre-refresh-hook nil))
          (magit-refresh)))))
  (setq magit-filenotify--buffers nil))

(defun magit-filenotify--register-buffer (buffer)
  (cl-pushnew buffer magit-filenotify--buffers)
  (if magit-filenotify--idle-timer
      (progn
        (cancel-timer magit-filenotify--idle-timer)
        (timer-activate-when-idle magit-filenotify--idle-timer t))
    (setq magit-filenotify--idle-timer
          (run-with-idle-timer magit-filenotify-idle-delay
                               nil #'magit-filenotify--refresh))))

(defun magit-filenotify--callback (ev)
  "Handle file-notify callbacks.
Argument EV contains the watch data."
  (unless
      (let ((file (car (last ev))) res)
        (dolist (rx magit-filenotify-ignored res)
          (when (string-match rx (file-name-nondirectory file))
            (setq res t))))
    (let* ((wd (car ev))
           (data (gethash wd magit-filenotify-data))
           (buffer (cadr data)))
      (if (buffer-live-p buffer)
          (magit-filenotify--register-buffer buffer)
        (file-notify-rm-watch wd)
        (remhash wd magit-filenotify-data)))))

(defun magit-filenotify-start ()
  "Start watching for changes to the source tree using filenotify.
This can only be called from a magit status buffer."
  (unless (derived-mode-p 'magit-status-mode)
    (error "Only works in magit status buffer"))
  (dolist (dir (magit-filenotify--directories))
    (when (file-exists-p dir)
      (puthash (file-notify-add-watch dir
                                      '(change attribute-change)
                                      #'magit-filenotify--callback)
               (list dir (current-buffer))
               magit-filenotify-data))))

(defun magit-filenotify-stop ()
  "Stop watching for changes to the source tree using filenotify.
This can only be called from a magit status buffer."
  (unless (derived-mode-p 'magit-status-mode)
    (error "Only works in magit status buffer"))
  (maphash
   (lambda (k v)
     (when (or (equal (cadr v) (current-buffer)) ; or use buffer?
               ;; Also remove watches for source trees where the magit status
               ;; buffer has been killed.
               (not (buffer-live-p (cadr v))))
       (file-notify-rm-watch k)
       (remhash k magit-filenotify-data)))
   magit-filenotify-data))

(defun magit-filenotify-watching-p ()
  "Return non-nil if current source tree is watched."
  (unless (derived-mode-p 'magit-status-mode)
    (error "Only works in magit status buffer"))
  (let (ret)
    (maphash (lambda (_k v)
               (when (and (not ret)
                          (equal (cadr v) (current-buffer)))
                 (setq ret t)))
             magit-filenotify-data)
    ret))

(defcustom magit-filenotify-lighter " MagitFilenotify"
  "String to display in mode line when `magit-filenotify-mode' is active."
  :group 'magit-filenotify
  :type 'string)

;;;###autoload
(define-minor-mode magit-filenotify-mode
  "Refresh status buffer if source tree changes."
  :lighter magit-filenotify-lighter
  :group 'magit-filenotify
  (if magit-filenotify-mode
      (progn
        (magit-filenotify-start)
        (add-hook 'kill-buffer-hook #'magit-filenotify-stop nil t))
    (magit-filenotify-stop)))

(defun magit-filenotify-stop-all ()
  "Stop watching for changes in all git trees."
  (interactive)
  (maphash
   (lambda (k _v) (file-notify-rm-watch k))
   magit-filenotify-data)
  (clrhash magit-filenotify-data))

;;; Loading
(easy-menu-add-item magit-mode-menu nil
                    ["Auto Refresh" magit-filenotify-mode
                     :style toggle
                     :selected (magit-filenotify-watching-p)
                     :help "Refresh magit status buffer when source tree updates"]
                    "Refresh")

(custom-add-option 'magit-status-mode-hook #'magit-filenotify-mode)

(defun magit-filenotify-unload-function ()
  "Cleanup when module is unloaded."
  (magit-filenotify-stop-all)
  (easy-menu-remove-item magit-mode-menu nil "Auto Refresh"))

(provide 'magit-filenotify)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-filenotify.el ends here
