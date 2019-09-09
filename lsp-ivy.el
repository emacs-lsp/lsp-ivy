;;; lsp-ivy.el --- LSP ivy integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski, Sebastian Sturm

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

;; Author: Sebastian Sturm
;; Keywords: languages, debug
;; URL: https://github.com/emacs-lsp/lsp-ivy
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "5.0") (ivy "0.12.0"))
;; Version: 0.1
;;

(require 'ivy)
(require 'dash)
(require 'lsp-mode)

(defun ivy-lsp--format-symbol-match (match)
  (format "%s.%s"
          (gethash "containerName" match)
          (gethash "name" match)))

(defun ivy-lsp--workspace-symbol-action (candidate)
  (-let* (((&hash "uri" "range" (&hash "start" (&hash "line" "character")))
           (gethash "location" candidate)))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(defun ivy-lsp--workspace-symbol (workspaces prompt initial-input)
  "Search against WORKSPACES with PROMPT and INITIAL-INPUT."
  (let (;; contains current user input, followed by the string representations
        ;; of all currently available candidates
        (candidates)
        (current-request-id))
    (ivy-read
     prompt
     (lambda (user-input &rest args)
       (if (string= user-input (car candidates))
           (--map (ivy-lsp--format-symbol-match it) (cdr candidates))
         (ignore
          (with-lsp-workspaces workspaces
            (-let (((request &as &plist :id request-id)
                    (lsp-make-request
                     "workspace/symbol"
                     (list :query user-input))))
              (when current-request-id
                (lsp--cancel-request current-request-id))
              (setq current-request-id request-id)
              (lsp-send-request-async
               request
               (lambda (incoming-candidates)
                 (setq candidates (cons user-input incoming-candidates))
                 (let (ivy--old-text)
                   (ivy--exhibit)))
               :mode 'detached))))))
     :dynamic-collection t
     :require-match t
     :initial-input initial-input
     :action (lambda (result)
               (let ((match
                      (--find
                       (string-equal result (ivy-lsp--format-symbol-match it))
                       ;; KLUDGE: remove current query, find candidate
                       ;; corresponding to selected candidate by linear search
                       (-drop 1 candidates))))
                 (when match (ivy-lsp--workspace-symbol-action match)))))))

;;;###autoload
(defun ivy-lsp-workspace-symbol (arg)
  "`ivy' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (ivy-lsp--workspace-symbol (lsp-workspaces)
                             "Workspace symbol: "
                             (when arg (thing-at-point 'symbol))))

;;;###autoload
(defun ivy-lsp-global-workspace-symbol (arg)
  "`ivy' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (ivy-lsp--workspace-symbol
   (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session)))))
   "Global workspace symbols: "
   (when arg (thing-at-point 'symbol))))

(provide 'lsp-ivy)
