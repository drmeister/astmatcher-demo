;;; -*- tabwidth:2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the C++ files to analyze
;;; Parse and load their ASTs in *ASTS*
;;;
(progn
  (format t "---- 1. Parsing C++ source in test.cpp~%")
  (require 'clang-tool)
  (load-compilation-database
   "compile_commands.json"
   :main-source-filename "test.cpp")
  (unless (every #'probe-file $*)
    (error "compile_commands.json contains a file which does not exist; this probably means you haven't run update.sh (see README.md for more information).~%"))
  (load-asts $*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the class/method/field matcher
;;;
(progn
  (format t "---- 2. Defining the matcher for class/field/method~%")
  (defparameter *matcher*
    '(:record-decl                                          ; Class declarations...              
      (:for-each                                            ; match their contents, once per item
       (:any-of 
        (:bind :field                                       ; Store field (member) declarations in :field
               (:field-decl
                (:unless                                    ; We don't care about
                    (:any-of
                     (:has-ancestor                         ; template specializations
                      (:class-template-specialization-decl))
                     (:is-implicit)))))                     ; or implicit fields
        (:bind :method                                      ; Store method declarations in :method
               (:method-decl                                ; and their return types in :ret-type
                             (:unless (:is-implicit))
                             (:returns (:bind :ret-type (:qual-type)))))))))
  ;; Matcher for method parameters; may be easily extended to functions
  (defparameter *method-parm-matcher*
    (compile-matcher 
     '(:method-decl 
       (:for-each-descendant 
        (:bind :param 
         (:parm-var-decl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define some helper functions
;;;
(progn
  (format t "---- 3. Define some helper functions~%")
  (defun mtag-node-or-null (tag)
    "Return a matched node, or NIL if that node does not exist."
    (handler-case (mtag-node tag)
      (no-node-for-tag-error (err) nil)))

  (defun ast-node-typed-decl (node-or-tag)
    "Return (cons type name) from a value-decl or tag"
    (let ((node (if (typep node-or-tag 'clang-ast:value-decl)
                    node-or-tag
                    (mtag-node node-or-tag))))
      (cons (clang-ast:get-as-string
             (clang-ast:get-type node))
            (clang-ast:get-name node))))

  (defmacro sub-match-collect (matcher node &body body)
    (let ((buf (gensym)))
      `(let ((,buf ())) (sub-match-run ,matcher ,node
                                       (lambda () (push (progn ,@body) ,buf)))
         ,buf)))

  (defstruct cxx-class name fields methods))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define the search function
;;;
(progn
  (format t "---- 4. Define the search function~%")
  (defun get-classes ()
    (let ((classes (make-hash-table :test 'equal)))
      (match-run
       *matcher*
       :code (lambda ()
               ;; This code is run for each match.
               (let* ((field-node (mtag-node-or-null :field))
                      (method-node (mtag-node-or-null :method))
                      (ast-name (mtag-name :whole))
                      (obj (or (gethash ast-name classes)
                               (progn (format t "Matched a new class: ~a~%" ast-name)
                                      (setf (gethash ast-name classes)
                                            (make-cxx-class :name ast-name))))))
                 (cond (method-node
                        (format t "Matched a method-node: ~a~%" method-node)
                        (push (list (clang-ast:get-as-string
                                     (mtag-node :ret-type))
                                    (mtag-name :method)
                                    (sub-match-collect *method-parm-matcher* method-node
                                                       (ast-node-typed-decl :param)))
                              (cxx-class-methods obj))
                        (format t "Matched parameters~%"))
                       (field-node
                        (format t "Matched a field-node: ~a~%" field-node)
                        (push (ast-node-typed-decl field-node)
                              (cxx-class-fields obj)))))))
      classes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run and print info
;;;

(defparameter *classes* (get-classes))

(loop for v being the hash-values in *classes*
      do (format t "#S(~A :NAME ~S~{~@[~&   :~A ~S~]~})~&"
                 (type-of v) (cxx-class-name v)
                 (list :fields (cxx-class-fields v)
                       :methods (cxx-class-methods v))))


(print *asts*)
