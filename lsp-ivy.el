;;; lsp-ivy.el --- LSP ivy integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sebastian Sturm

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
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "5.0") (ivy "0.13.0"))
;; Version: 0.1
;;

;;; Commentary:

;;

;;; Code:

(require 'ivy)
(require 'dash)
(require 'lsp-mode)

(defun lsp-ivy--format-symbol-match (match)
  "Convert the (hash-valued) MATCH returned by `lsp-mode` into a candidate string."
  (let ((container-name (gethash "containerName" match))
        (name (gethash "name" match)))
    (if (or (null container-name) (string-empty-p container-name))
        name
      (format "%s.%s" container-name name))))

(defun lsp-ivy--workspace-symbol-action (candidate)
  "Jump to selected CANDIDATE."
  (-let* (((&hash "uri" "range" (&hash "start" (&hash "line" "character")))
           (gethash "location" candidate)))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(defun lsp-ivy--workspace-symbol (workspaces prompt initial-input)
  "Search against WORKSPACES with PROMPT and INITIAL-INPUT."
  (let ((current-request-id nil))
    (ivy-read
     prompt
     (lambda (user-input)
       (with-lsp-workspaces workspaces
         (let ((request (lsp-make-request
                         "workspace/symbol"
                         (list :query user-input))))
           (when current-request-id
             (lsp--cancel-request
              current-request-id))
           (setq current-request-id
                 (plist-get request :id))
           (lsp-send-request-async
            request
            #'ivy-update-candidates
            :mode 'detached)))
       0)
     :dynamic-collection t
     :require-match t
     :initial-input initial-input
     :action #'lsp-ivy--workspace-symbol-action
     :caller 'lsp-ivy-workspace-symbol)))

(ivy-configure 'lsp-ivy-workspace-symbol
  :display-transformer-fn #'lsp-ivy--format-symbol-match)

;;;###autoload
(defun lsp-ivy-workspace-symbol (arg)
  "`ivy' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (lsp-ivy--workspace-symbol (lsp-workspaces)
                             "Workspace symbol: "
                             (when arg (thing-at-point 'symbol))))

;;;###autoload
(defun lsp-ivy-global-workspace-symbol (arg)
  "`ivy' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (lsp-ivy--workspace-symbol
   (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session)))))
   "Global workspace symbols: "
   (when arg (thing-at-point 'symbol))))

(provide 'lsp-ivy)
;;; lsp-ivy.el ends here
