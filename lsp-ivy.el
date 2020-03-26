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

;; Authors: Sebastian Sturm
;;          Oliver Rausch
;; Keywords: languages, debug
;; URL: https://github.com/emacs-lsp/lsp-ivy
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "5.0") (ivy "0.13.0"))
;; Version: 0.2
;;

;;; Commentary:

;; This package provides an interactive ivy interface to the workspace symbol
;; functionality offered by lsp-mode. For an alternative implementation based on
;; helm, see https://github.com/emacs-lsp/helm-lsp

;;; Code:

(require 'ivy)
(require 'dash)
(require 'lsp-mode)

(defgroup lsp-ivy nil
  "LSP support for ivy-based symbol completion"
  :group 'lsp-mode)

(defcustom lsp-ivy-show-symbol-kind
  t
  "Whether to show the symbol's kind when showing lsp symbols"
  :group 'lsp-ivy
  :type 'boolean)

(defcustom lsp-ivy-filter-symbol-kind
  nil
  "A list of LSP SymbolKind's to filter out"
  :group 'lsp-ivy
  :type '(repeat integer))

(defcustom lsp-ivy-symbol-kind-to-string
  [("    " . "red") ;; Unknown - 0
   ("File" . "red") ;; File - 1
   ("Modu" . "red") ;; Module - 2
   ("Nmsp" . "red") ;; Namespace - 3
   ("Pack" . "red") ;; Package - 4
   ("Clss" . "red") ;; Class - 5
   ("Meth" . "violet") ;; Method - 6
   ("Prop" . "violet") ;; Property - 7
   ("Fld " . "violet") ;; Field - 8
   ("Cons" . "red") ;; Constructor - 9
   ("Enum" . "red") ;; Enum - 10
   ("Intf" . "red") ;; Interface - 11
   ("Func" . "darkgreen") ;; Function - 12
   ("Var " . "blue") ;; Variable - 13
   ("Cnst" . "blue") ;; Constant - 14
   ("Str " . "blue") ;; String - 15
   ("Num " . "blue") ;; Number - 16
   ("Bool " . "blue") ;; Boolean - 17
   ("Arr " . "blue") ;; Array - 18
   ("Obj " . "blue") ;; Object - 19
   ("Key " . "blue") ;; Key - 20
   ("Null" . "red") ;; Null - 21
   ("EmMm" . "violet") ;; EnumMember - 22
   ("Srct" . "red") ;; Struct - 23
   ("Evnt" . "red") ;; Event - 24
   ("Op  " . "red") ;; Operator - 25
   ("TPar" . "red")] ;; TypeParameter - 26
  "A vector of 26 cons cells, where the ith cons cell contains the string representation and foreground color to use for the i+1th SymbolKind (defined in the LSP)"
  :group 'lsp-ivy
  :type '(vector
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)
          (cons string color)))


(defun lsp-ivy--format-symbol-match (match)
  "Convert the (hash-valued) MATCH returned by `lsp-mode` into a candidate string."
  (let* ((container-name (gethash "containerName" match))
         (name (gethash "name" match))
         (type (elt lsp-ivy-symbol-kind-to-string (gethash "kind" match) ))
         (typestr (if lsp-ivy-show-symbol-kind
                      (propertize (format "[%s] " (car type)) 'face `(:foreground ,(cdr type)))
                    "")))
    (concat typestr (if (or (null container-name) (string-empty-p container-name))
                        (format "%s" name)
                      (format "%s.%s" container-name name)))))

(defun lsp-ivy--workspace-symbol-action (candidate)
  "Jump to selected CANDIDATE."
  (-let* (((&hash "uri" "range" (&hash "start" (&hash "line" "character")))
           (gethash "location" candidate)))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(defun lsp-ivy--filter-func (candidate)
  (member (gethash "kind" candidate) lsp-ivy-filter-symbol-kind))

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
            (lambda (result)
              (ivy-update-candidates (-remove 'lsp-ivy--filter-func result)))
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
