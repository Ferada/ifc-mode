;;; ifc-mode.el --- syntax highlighting for IFC SPF files

;;; in general, this could be implemented on top of a not-yet written STEP
;;; schema framework, but i doubt that will happen any time soon, thus,
;;; this file defines a useful subset to highlight and use files found in
;;; my vicinity

;;; schema definitions and some examples files are downloaded from
;;; http://www.buildingsmart-tech.org/ifc/IFC2x3/TC1/
;;; and http://www.buildingsmart-tech.org/ifc/IFC2x2/Add1/

;;; more example files from
;;; http://www.buildingsmartalliance.org/index.php/projects/commonbimfiles

;;; see also http://www.ifcwiki.org/index.php/Examples

;; TODO:
;; - parse file version before loading syntax
;; - mark invalid syntax for file version
;;     i.e. for each version have a list from the other versions, which
;;     are invalid in this version, the mark them with an error or warning
;;     face
;; - jumping to tags and back
;;     relatively simple: run etags --regex=@ifc.tags example.ifc
;;     then use visit-tags-table and find-tag
;;     however, on large files (two digit mega-byte range) etags most likely
;;     crashes, so this approach won't work
;;     otherwise special commands would've to be used to sequentially
;;     search for occurences, not particularly efficient, but usable
;;     nonetheless
;; - make the existing search wrap around like isearch, or use that anyway
;; - generating the jump targets for all symbols
;;     enumerations need more schema parsing, also multiple jump targets
;;     (i.e. for UNDEFINED it's basically useless)

(defvar ifc-base-url "http://www.buildingsmart-tech.org/ifc/IFC2x3/TC1/html/")
(defvar ifc-docs-root ifc-base-url)

(defvar ifc-spf-objects)
(defvar ifc-spf-constants)

(defvar ifc-names-to-interfaces)
(defvar ifc-resources)
(defvar ifc-names)

;; load definitions for ifc-spf-objects and ifc-spf-constants
(load-file "generated/ifc2x3_tc1.el")
;;(load-file "generated/ifc2x2_add1.el")
(load-file "generated/ifc2x3_tc1.toc.el")

(defvar step-file-keywords
  '("HEADER" "FILE_DESCRIPTION" "FILE_NAME" "FILE_SCHEMA" "ENDSEC" "DATA"
    "ISO-10303-21" "END-ISO-10303-21")
  "STEP-File keywords.")

(defvar step-file-keywords-regexp (regexp-opt step-file-keywords 'symbol))
(defvar ifc-spf-objects-regexp (regexp-opt ifc-spf-objects 'symbols))
(defvar ifc-spf-constants-regexp (regexp-opt ifc-spf-constants 'symbols))

(defvar ifc-spf-font-lock-keywords
  `((,step-file-keywords-regexp . font-lock-keyword-face)
    ;; more likely overkill
    (,ifc-spf-objects-regexp . font-lock-function-name-face)
    (,ifc-spf-constants-regexp . font-lock-constant-face)))

(defvar ifc-spf-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 14" syntax-table)
    (modify-syntax-entry ?* ". 23" syntax-table)
    (modify-syntax-entry ?' "\"" syntax-table)
    (modify-syntax-entry ?. "_" syntax-table)
    (modify-syntax-entry ?\# "_" syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    syntax-table))

(define-derived-mode ifc-spf-mode fundamental-mode
  "IFC-SPF"
  "Major mode for editing IFC (Industrial Foundation Classes) SPF
\(STEP-File) files."
  :syntax-table ifc-spf-syntax-table
  (setf font-lock-defaults '((ifc-spf-font-lock-keywords))))

(defun parse-schema-file (filename)
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

(defun generate-syntax-from-schema (schema output)
  "Generates definitions for all types and enumerations from an EXPRESS
schema definition."
  (multiple-value-bind (types entities enumerations)
      (parse-schema-file schema)
    (with-temp-file output
      (let ((standard-output (current-buffer)))
        (print `(setf ifc-spf-objects
                      ',(mapcar #'upcase (append types entities))))
        (print `(setf ifc-spf-constants
                      ',(mapcar (lambda (string)
                                  (format ".%s." (upcase string)))
                                enumerations))))))
  (values))

(defun alist-for-element-urls (interface names)
  (mapcar (lambda (name)
            (cons name interface))
          names))

(defun resolve-element-url (name)
  (let* ((name (upcase name))
         (found (assoc name ifc-names-to-interfaces)))
    (if found
        ;; downcase here just because the url looks nicer
        (concat ifc-docs-root
                (format "%s/lexical/%s.htm"
                        (downcase (cdr found))
                        (downcase name)))
      (let ((found (member name ifc-resources)))
        (if found
            (concat ifc-docs-root
                    (let ((downcased (downcase name)))
                      (format "%s/%s.htm"
                              downcased downcased))))))))

(defun ifc-find-tag (name)
  (interactive
   (let ((symbol (thing-at-point 'symbol)))
     (list symbol)))
  (if (not (char-equal ?# (aref name 0)))
      (message "`%s' isn't a valid object reference" name)
    (goto-char (point-min))
    ;; "\\([[:space:]]\\|^\\|\\s(\\)\\(%s\\)\\([[:space:]]\\|\\s.\\|\\s)\\)*="
    (if (re-search-forward (format "\\_<\\(%s\\)\\_>[[:space:]]*=" name) nil t)
        (goto-char (match-beginning 1))
      (message "Couldn't find object `%s'" name))))

(defun ifc-find-usages (name)
  (interactive
   (let ((symbol (thing-at-point 'symbol)))
     (list symbol)))
  (if (not (char-equal ?# (aref name 0)))
      (message "`%s' isn't a valid object reference" name)
    (goto-char (1+ (point)))
    ;; "\\([[:space:]]\\|^\\|\\s(\\|\\s.\\)\\(%s\\)\\([[:space:]]\\|\\s.\\|\\s)\\)"
    (if (re-search-forward (format "\\_<\\(%s\\)\\_>" name) nil t)
        (goto-char (match-beginning 1))
      (message "Couldn't find object `%s'" name)
      (goto-char (1- (point))))))

(defvar ifc-toc-files
  '("alphabeticalorder_definedtype.htm"
    "alphabeticalorder_entities.htm"
    "alphabeticalorder_enumtype.htm"
    "alphabeticalorder_selecttype.htm"))

(defun download-ifc-toc-files ()
  (make-directory "html" t)
  (dolist (html-file ifc-toc-files)
    (url-copy-file (concat ifc-base-url html-file) (concat "html/" html-file))))

(defun parse-ifc-toc-files ()
  (setf ifc-names-to-interfaces nil
        ifc-resources nil
        ifc-names nil)
  (dolist (html-file ifc-toc-files)
    (with-current-buffer (find-file-noselect (concat "html/" html-file))
      (goto-char (point-min))
      (while (re-search-forward "A HREF=\"\\(.+?\\)/lexical/\\(.+?\\)\\.htm\"" nil t)
        (let* ((interface (upcase (match-string 1)))
               (type (upcase (match-string 2))))
          (pushnew interface ifc-resources :test #'string=)
          (push (cons type interface) ifc-names-to-interfaces)))))
  (setf ifc-names (append (mapcar #'car ifc-names-to-interfaces)
                          ifc-resources))
  (values))

(defun generate-ifc-tocs (output)
  (parse-ifc-toc-files)
  (with-temp-file output
    (let ((standard-output (current-buffer)))
      (print `(setf ifc-names-to-interfaces ',ifc-names-to-interfaces))
      (print `(setf ifc-resources ',ifc-resources))
      (print `(setf ifc-names ',ifc-names))))
  (values))

;;;###autoload
(defun ifc-docs-lookup (name)
  "View the documentation on NAME from the IFC documentation. "
  (interactive
   (let ((symbol (thing-at-point 'symbol))
         (enable-recursive-minibuffers t)
         (completion-ignore-case t)
         val)
     (if (and symbol (member (upcase symbol) ifc-names))
         (setf val symbol)
       (setf val (ido-completing-read
                  (if symbol
                      (format "Lookup IFC element (default %s): " symbol)
                    "Lookup IFC element: ")
                  ifc-names nil t symbol nil)))
     (list (if (equal val "")
               symbol val))))
  (if (null name)
      (message "You didn't specify an IFC name")
    (let ((url (resolve-element-url name)))
      (if url
          (browse-url url)
        (error "Couldn't find URL for `%s'." name)))))

(provide 'ifc-spf-mode)
