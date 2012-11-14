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

(load-file "ifc-mode-common.el")

(defvar ifc-mode-spf-objects)
(defvar ifc-mode-spf-constants)

(defvar ifc-mode-names-to-interfaces)
(defvar ifc-mode-resources)
(defvar ifc-mode-names)

(eval-when-compile
  (defun ifc-mode-load-generated ()
    ;; load definitions for ifc-spf-objects and ifc-spf-constants
    (load-file (expand-file-name "generated/ifc2x3_tc1.el" ifc-mode-path))
    (load-file (expand-file-name "generated/ifc2x3_tc1.toc.el" ifc-mode-path)))
  (ifc-mode-load-generated))

(defvar ifc-mode-step-file-keywords
  '("HEADER" "FILE_DESCRIPTION" "FILE_NAME" "FILE_SCHEMA" "ENDSEC" "DATA"
    "ISO-10303-21" "END-ISO-10303-21")
  "STEP-File keywords.")

(defvar ifc-mode-step-file-keywords-regexp (regexp-opt ifc-mode-step-file-keywords 'symbol))
(defvar ifc-mode-spf-objects-regexp (regexp-opt ifc-mode-spf-objects 'symbols))
(defvar ifc-mode-spf-constants-regexp (regexp-opt ifc-mode-spf-constants 'symbols))

(defvar ifc-mode-spf-font-lock-keywords
  `((,ifc-mode-step-file-keywords-regexp . font-lock-keyword-face)
    ;; more likely overkill
    (,ifc-mode-spf-objects-regexp . font-lock-function-name-face)
    (,ifc-mode-spf-constants-regexp . font-lock-constant-face)))

(defvar ifc-mode-spf-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 14" syntax-table)
    (modify-syntax-entry ?* ". 23" syntax-table)
    (modify-syntax-entry ?' "\"" syntax-table)
    (modify-syntax-entry ?. "_" syntax-table)
    (modify-syntax-entry ?\# "_" syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    syntax-table))

(defvar ifc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d h") 'ifc-mode-docs-lookup)
    (define-key map (kbd "M-.") 'ifc-mode-find-tag)
    (define-key map (kbd "M-,") 'ifc-mode-pop-find-tag)
    (define-key map (kbd "M-_") 'ifc-mode-find-uses)
    (define-key map (kbd "M-?") 'ifc-mode-find-uses)
    map))

;;;###autoload
(define-derived-mode ifc-spf-mode fundamental-mode
  "IFC-SPF"
  "Major mode for editing IFC SPF files."
  :syntax-table ifc-mode-spf-syntax-table
  (setf font-lock-defaults '((ifc-mode-spf-font-lock-keywords)))
  (use-local-map ifc-mode-map))

(defun ifc-mode-resolve-element-url (name)
  (let* ((name (upcase name))
         (found (assoc name ifc-mode-names-to-interfaces)))
    (if found
        ;; downcase here just because the url looks nicer
        (concat ifc-mode-docs-root
                (format "%s/lexical/%s.htm"
                        (downcase (cdr found))
                        (downcase name)))
      (let ((found (member name ifc-mode-resources)))
        (if found
            (concat ifc-mode-docs-root
                    (let ((downcased (downcase name)))
                      (format "%s/%s.htm"
                              downcased downcased))))))))

(defun ifc-mode-push-find-tag ()
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))

(defun ifc-mode-pop-find-tag ()
  (interactive)
  (pop-tag-mark))

(defun ifc-mode-find-tag (name)
  (interactive
   (let ((symbol (thing-at-point 'symbol)))
     (list symbol)))
  (if (not (char-equal ?# (aref name 0)))
      (message "`%s' isn't a valid object reference" name)
    (ifc-mode-push-find-tag)
    (goto-char (point-min))
    ;; "\\([[:space:]]\\|^\\|\\s(\\)\\(%s\\)\\([[:space:]]\\|\\s.\\|\\s)\\)*="
    (if (re-search-forward (format "\\_<\\(%s\\)\\_>[[:space:]]*=" name) nil t)
        (goto-char (match-beginning 1))
      (ifc-mode-pop-find-tag)
      (message "Couldn't find object `%s'" name))))

(defun ifc-mode-find-uses (name)
  (interactive
   (let ((symbol (thing-at-point 'symbol)))
     (list symbol)))
  (if (not (char-equal ?# (aref name 0)))
      (message "`%s' isn't a valid object reference" name)
    (ifc-mode-push-find-tag)
    (goto-char (1+ (point)))
    ;; "\\([[:space:]]\\|^\\|\\s(\\|\\s.\\)\\(%s\\)\\([[:space:]]\\|\\s.\\|\\s)\\)"
    (if (re-search-forward (format "\\_<\\(%s\\)\\_>" name) nil t)
        (goto-char (match-beginning 1))
      (ifc-mode-pop-find-tag)
      (message "Couldn't find object `%s'" name))))

;;;###autoload
(defun ifc-mode-docs-lookup (name)
  "View the documentation on NAME from the IFC documentation. "
  (interactive
   (let ((symbol (thing-at-point 'symbol))
         (enable-recursive-minibuffers t)
         (completion-ignore-case t)
         val)
     (if (and symbol (member (upcase symbol) ifc-mode-names))
         (setf val symbol)
       (setf val (ido-completing-read
                  (if symbol
                      (format "Lookup IFC element (default %s): " symbol)
                    "Lookup IFC element: ")
                  ifc-mode-names nil t symbol nil)))
     (list (if (equal val "")
               symbol val))))
  (if (null name)
      (message "You didn't specify an IFC name")
    (let ((url (ifc-mode-resolve-element-url name)))
      (if url
          (browse-url url)
        (error "Couldn't find URL for `%s'." name)))))

(provide 'ifc-spf-mode)
