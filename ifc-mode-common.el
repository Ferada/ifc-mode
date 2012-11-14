(eval-and-compile
  (defvar ifc-mode-path
    (let ((path (or (locate-library "ifc-mode") load-file-name)))
      (and path (file-name-directory path)))))

(defvar ifc-mode-base-url "http://www.buildingsmart-tech.org/ifc/IFC2x3/TC1/html/")
(defvar ifc-mode-docs-root ifc-mode-base-url)
