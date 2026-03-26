(in-package :hactar)

(defun get-all-md-src-blocks (markdown-string)
  "Parse MARKDOWN-STRING and return a list of all source block contents and their languages."
  (let ((doc-tree (cmark:parse-document markdown-string))
        (blocks nil))
    (cmark:walk-tree doc-tree
                     (lambda (node event)
                       (when (and (eq event 'cmark:enter)
                                  (typep node 'cmark:code-block-node))
                         (push (list :language (cmark::node-fence-info node)
                                     :content (cmark:node-literal node))
                               blocks))))
    (nreverse blocks)))

(defun render-markdown-to-html (markdown-string)
  "Render a MARKDOWN-STRING to an HTML string."
  (libcmark:markdown-to-html markdown-string 
                             (length (babel:string-to-octets markdown-string)) 
                             0))

(defun insert-md-sibling (node sibling)
  "Insert SIBLING node after NODE."
  (cmark:insert-node-after node sibling))

(defun insert-md-child (node child)
  "Append CHILD node to NODE."
  (cmark:append-child-node node child))
