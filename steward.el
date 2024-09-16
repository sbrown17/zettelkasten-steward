(setq org-capture-templates
  '(("z" "Zettelkasten Entry" entry (file create-zettelkasten-entry)
     "* %?\nEntered on %U\n  %i\n  %a")
    ("f" "Fleeting Entry" entry (file create-fleeting-entry)
     "* %?\nEntered on %U\n  %i\n  %a"))
  )

(defun create-zettelkasten-entry ()
  "Create a dated filename for zettelkasten identification purposes."
  (let ((name (read-string "Enter the file name: ")))
    (expand-file-name (format "%s_%s.org"
      (format-time-string "%Y%m%d%H%M%S")
      name)
    "~/Development/notes/")))

(defun create-fleeting-entry ()
  "Create a dated filename for zettelkasten identification purposes."
  (let ((name (read-string "Enter the file name: ")))
    (expand-file-name (format "%s_%s.org"
      (format-time-string "%Y%m%d%H%M%S")
      name)
		      "~/Development/notes/fleeting/")))

(defun get-undated-filenames (directory)
  "Return a list of filenames in the given Directory."
    (directory-files directory nil "^[^0-9].*\.org$" t))

;; create-new-name -> get current datetime if current_datetime exists, decrement and check again else append datetime to the old name with an underscore
;; get datetimes at beginning of each file that has one
;; (directory-files "~/Development/notes/" nil "^[0-9]\\{14\\}_.*\\.org" t)
(defun get-14-digit-prefixes-as-numbers (directory)
  "Get a list of 14-digit prefixes as numbers from filenames in DIRECTORY."
  (let* ((files (directory-files directory nil "^[0-9]\\{14\\}_.*\\.org" t))
         (digits-list '()))
    (dolist (file files digits-list)
      (when (string-match "^\\([0-9]\\{14\\}\\)" file)
        (push (string-to-number (match-string 1 file)) digits-list)))))

(defun rename-file-safe (new-name old-name)
  "Rename file old-name to new-name safely."
  (condition-case err
      (progn
	(when (file-exists-p new-name)
	  (error "File '%s' already exists" new-name))
      (rename-file old-name new-name)
      (message "File renamed from '%s' to '%s'" old-name new-name))
    (error (message "Error renaming file: %s" (error-message-string err)))))

(defun file-name-with-unique-id (file-name)
  "Add datetime to the filename"
  (let* ((base (file-name-sane-extension file-name))
	 (datetime-id (generate-datetime-id))
	 (format "%s-%s.org" base datetime-id))))

(defun sort-numbers-descending (numbers)
  "Sort a list of NUMBERS from highest to lowest."
  (sort (copy-sequence numbers) '>))

(defun generate-datetime-id ()
  "Generate new datetime based id for file name."
  (+ 1 (car (sort-numbers-descending (get-14-digit-prefixes-as-numbers "." )))))
  
(defun id-files-in-directory (directory)
  "Give ids to files that dont have ids in a given directroy."
  (let ((files (get-undated-filenames directory t)))
    (dolist (file files)
      (when (file-regular-p file)
        (let* ((old-name (file-name-nondirectory file))
               (new-name (file-name-with-unique-id old-name))
               (new-path (expand-file-name new-name directory)))
          (unless (string= old-name new-name)
            (rename-file file new-path)
            (message "Renamed: %s -> %s" old-name new-name)))))))
