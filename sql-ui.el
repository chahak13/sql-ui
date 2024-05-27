(require 'f)
(require 'dired)
(require 'eglot)
(require 'json)
(require 'cl-lib)

(defvar sql-ui-default-directory "~/.local/sql-ui/")
(defvar sql-ui-saved-directory (f-join sql-ui-default-directory "saved/"))
(defvar sql-ui-playground-file nil)
(defvar cm/sql-ui-connection-file "~/.local/sql-ui/.sql-ls/config.json")

(defun sql-ui-save-buffer-as-file ()
  "Prompt for a new file name and save the current buffer to that file."
  (interactive)
  (unless (f-dir-p sql-ui-saved-directory)
    (make-directory sql-ui-saved-directory t))
  (let ((file-name (read-file-name "Save query as: " sql-ui-saved-directory))
        (content (buffer-string)))
    (with-current-buffer (find-file-noselect file-name)
      (insert content)
      (save-buffer))
    (message "Query saved to %s" file-name))
  (with-current-buffer (get-buffer-create sql-ui-saved-directory)
    (dired sql-ui-saved-directory))
  (switch-to-buffer (get-file-buffer sql-ui-playground-file)))


(defun sql-ui-open-saved-query ()
  "Open file at point in BUFFER."
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (with-current-buffer (get-buffer-create (f-filename sql-ui-playground-file))
      (insert-file-contents file-name nil nil nil t)
      )))


(defun sql-ui--make-temp-file (directory)
  "Create a temporary file in the specified DIRECTORY and return its path."
  (let ((temporary-file-directory directory))  ;; Set the temporary directory
    (unless (f-dir-p directory)
      (make-directory directory t))
    (make-temp-file "lsp-virtual-" nil ".sql")))


(defun sql-ui--create-editor-buffer ()
  "Create the query editor buffer.

This creates a temporary sql file in `sql-ui-default-directory' and
visits it in the editor buffer"
  (let ((tmp-file (sql-ui--make-temp-file (f-join sql-ui-default-directory "tmp"))))
    (setq sql-ui-playground-file tmp-file)
    (set-buffer (create-file-buffer tmp-file))
    (funcall 'sql-mode)
    (setq-local default-directory sql-ui-default-directory)
    (setq-local buffer-file-name tmp-file)
    (eglot-ensure)
    (current-buffer)))


(defun sql-ui-default-layout ()
  "Create the default layout for SQL-UI in Emacs."
  (interactive)
  (let ((queries-buffer (if (f-dir-p sql-ui-saved-directory)
                            (dired sql-ui-saved-directory)
                          (progn
                            (make-directory sql-ui-saved-directory t)
                            (dired sql-ui-saved-directory))))
        (editor-buffer (sql-ui--create-editor-buffer))
        (results-buffer (get-buffer-create "*sql-ls-eglot results*")))
    (delete-other-windows)
    (set-window-buffer (frame-first-window) editor-buffer)
    (display-buffer-in-side-window results-buffer '((side . bottom)))
    (display-buffer-in-side-window queries-buffer '((side . left)))
    (with-current-buffer queries-buffer
      (dired-hide-details-mode t)
      (keymap-local-set "C-M-<return>" 'sql-ui-open-saved-query)
      )
    (switch-to-buffer editor-buffer)
    (keymap-local-set "C-x C-s" 'sql-ui-save-buffer-as-file)))


(defun cm/sql-ui--connection-exists (alias connections)
  "Check if the ALIAS already exists in CONNECTIONS."
  (cl-some (lambda (connection)
             (and (eq (car connection) alias) connection))
           connections))


(defun cm/sql-ui-add-connection (alias new-db-connection)
  "Load a JSON file at FILE-PATH, check if KEY exists,
   and add NEW-JSON-OBJECT if the key is missing.
   Save the updated JSON to the file."
  (unless (f-directory-p (f-dirname cm/sql-ui-connection-file))
    (make-directory (f-dirname cm/sql-ui-connection-file) t))
  (let* ((db-connections (if (file-exists-p cm/sql-ui-connection-file)
                             (with-temp-buffer
                               (insert-file-contents cm/sql-ui-connection-file)
                               (json-read))
                           (list 'connections '())))
         (existing-connections (cdr (assoc 'connections db-connections)))
         (new-connection (push alias new-db-connection)))
    (if (cm/sql-ui--connection-exists alias existing-connections)
        (message "Connection already exists: %s" alias)
      (message "Adding new connection")
      (add-to-list 'existing-connections new-connection)
      (setq db-connections (json-add-to-object (json-new-object) "connections" existing-connections))
      (with-temp-file cm/sql-ui-connection-file
        (insert (json-encode db-connections))))))
