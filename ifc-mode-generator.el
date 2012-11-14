(load-file "ifc-mode-common.el")

(defun ifc-mode-parse-schema-file (filename)
  (let (types entities enumerations)
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (while (re-search-forward "^TYPE \\([[:word:]]+\\)" nil t)
        (push (match-string 1) types))
      (goto-char (point-min))
      (while (re-search-forward "^ENTITY \\([[:word:]]+\\)" nil t)
        (push (match-string 1) entities))
      (goto-char (point-min))
      (while (re-search-forward "ENUMERATION OF[^(]+(\\([^)]+\\)" nil t)
        (let ((start (match-beginning 1))
              (end (match-end 1)))
          (goto-char start)
          (while (re-search-forward "[_[:word:]]+" end t)
            (push (match-string 0) enumerations)))))
    (values types entities enumerations)))

(defun ifc-mode-generate-syntax-from-schema (schema output)
  "Generates definitions for all types and enumerations from an EXPRESS
schema definition."
  (multiple-value-bind (types entities enumerations)
      (ifc-mode-parse-schema-file schema)
    (with-temp-file output
      (let ((standard-output (current-buffer)))
        (print `(setf ifc-mode-spf-objects
                      ',(mapcar #'upcase (append types entities))))
        (print `(setf ifc-mode-spf-constants
                      '(".T." ".F."
                        ,@(mapcar (lambda (string)
                                    (format ".%s." (upcase string)))
                                  enumerations)))))))
  (values))

(defvar ifc-mode-syntax-files
  '("http://www.steptools.com/support/stdev_docs/express/ifc2x3/ifc2x3_tc1.exp"
    "http://www.steptools.com/support/stdev_docs/express/ifc2x2/ifc2x2_add1.exp"))

(defun ifc-mode-download-syntax-files (directory)
  (make-directory directory t)
  (dolist (exp-file ifc-mode-syntax-files)
    (url-copy-file exp-file (expand-file-name (url-file-nondirectory exp-file) directory) t)))

(defvar ifc-mode-toc-files
  '("alphabeticalorder_definedtype.htm"
    "alphabeticalorder_entities.htm"
    "alphabeticalorder_enumtype.htm"
    "alphabeticalorder_selecttype.htm"))

(defun ifc-mode-download-toc-files (directory)
  (make-directory directory t)
  (dolist (html-file ifc-mode-toc-files)
    (url-copy-file (concat ifc-mode-base-url html-file)
                   (expand-file-name html-file directory)
                   t)))

(defun ifc-mode-parse-toc-files ()
  (setf ifc-mode-names-to-interfaces nil
        ifc-mode-resources nil
        ifc-mode-names nil)
  (dolist (html-file ifc-mode-toc-files)
    (with-current-buffer (find-file-noselect (expand-file-name html-file "html"))
      (goto-char (point-min))
      (while (re-search-forward "A HREF=\"\\(.+?\\)/lexical/\\(.+?\\)\\.htm\"" nil t)
        (let* ((interface (upcase (match-string 1)))
               (type (upcase (match-string 2))))
          (pushnew interface ifc-mode-resources :test #'string=)
          (push (cons type interface) ifc-mode-names-to-interfaces)))))
  (setf ifc-mode-names (append (mapcar #'car ifc-mode-names-to-interfaces)
                               ifc-mode-resources))
  (values))

(defun ifc-mode-generate-tocs (output)
  (ifc-mode-parse-toc-files)
  (with-temp-file output
    (let ((standard-output (current-buffer)))
      (print `(setf ifc-mode-names-to-interfaces ',ifc-mode-names-to-interfaces))
      (print `(setf ifc-mode-resources ',ifc-mode-resources))
      (print `(setf ifc-mode-names ',ifc-mode-names))))
  (values))
