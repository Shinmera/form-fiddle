(in-package #:org.shirakumo.form-fiddle)

(defun declaration-p (form)
  (and (listp form) (eql 'declare (first form))))

(defun docstring-p (form)
  (stringp form))

(defun lambda-function (lambda-form)
  (first lambda-form))

(defun lambda-name (lambda-form)
  (unless (listp (second lambda-form))
    (second lambda-form)))

(defun lambda-qualifiers (lambda-form)
  (unless (listp (second lambda-form))
    (loop for item in (cddr lambda-form)
          until (listp item)
          collect item)))

(defun lambda-lambda-list (lambda-form)
  (loop for item in lambda-form
        when (listp item)
        do (return item)))

(defun lambda-body (lambda-form)
  (loop for body on lambda-form
        ;; Once we encounter the lambda-list, return the following.
        when (listp (first body))
        do (return (cdr body))))

(defun lambda-docstring (lambda-form)
  (let ((body (lambda-body lambda-form)))
    (loop ;; The last expression in a form cannot be a docstring.
          ;; (lambda (foo) "bar") => no docstring, returns "bar".
          for i from 0 below (1- (length body))
          for form in body
          while (declaration-p form)
          finally (when (stringp form)
                    (return form)))))

(defun lambda-declarations (lambda-form)
  (let ((body (lambda-body lambda-form)))
    (when (stringp (first body))
      (setf body (cdr body)))
    (loop for form in body
          while (or (declaration-p form)
                    (docstring-p form))
          when (declaration-p form)
          collect form)))

(defun lambda-forms (lambda-form)
  (let* ((body (lambda-body lambda-form)))
    (loop for forms on body
          for form = (car body)
          for i from 0 below (1- (length body))
          while (or (declaration-p form)
                    (docstring-p form))
          finally (return
                    (if (declaration-p (car forms))
                        (cdr forms)
                        forms)))))

(defun split-lambda-form (lambda-form)
  (let ((body lambda-form)
        (lambda-p (eql (first lambda-form) 'lambda)))
    (append
     (list (pop body))
     ;; First is name, rest are qualifiers
     ;; until we reach a list, which must be
     ;; the lambda-list of the form.
     (if lambda-p
         (list NIL)
         (list (pop body)))
     (loop for form = (pop body)
           until (listp form)
           collect form into qualifiers
           finally (return
                     (list qualifiers
                           form)))
     ;; Now we go on to parsing the body we reached.
     ;; This is annoying because of the interleaving
     ;; of docstrings and declarations.
     ;; F.e. the following is a body with two
     ;; declarations, a docstring and a form:
     ;;
     ;;   (declare) "foo" (declare) "bar"
     ;;
     ;; And this is a body with one declaration, a
     ;; docstring and a form:
     ;;
     ;;   (declare) "foo" "bar"
     ;;
     ;; But this is only a declaration and a form:
     ;;
     ;;   (declare) "foo"
     ;;
     ;; To make matters even more complicated, the
     ;; following has three decls, one docstring, and
     ;; a form. The consequences of multiple docstrings
     ;; like this are undefined as per the hyperspec,
     ;; but it seems allowed so we have to deal.
     ;;
     ;;   (declare) "foo" (declare) "bar" (declare) "baz"
     ;;
     ;; On the other hand, something like this has
     ;; one decl, one docstring and three forms.
     ;;
     ;;   (declare) "foo" "bar" (declare) "baz"
     ;;
     ;; Or at least that's what I'm guessing is the
     ;; intended behaviour suggested by 3.4.11 .
     ;;
     ;; We opt to choose the first occurring docstring.
     (loop with docstring = NIL
           for forms on body
           for form = (car forms)
           for i from 0 below (1- (length body))
           while (or (and (docstring-p form)
                          (or (not docstring)
                              (not (docstring-p (car (cdr forms))))))
                     (declaration-p form))
           if (docstring-p form)
           do (setf docstring (or docstring form))
           else
           collect form into declarations
           finally (return
                     (if (declaration-p (car forms))
                         (list docstring (append declarations (list (car forms))) (cdr forms))
                         (list docstring declarations forms)))))))

(defmacro with-destructured-lambda-form ((&key function name qualifiers lambda-list docstring declarations forms) expression &body body)
  (let ((bindings (list (or function (gensym "FUNCTION"))
                        (or name (gensym "NAME"))
                        (or qualifiers (gensym "QUALIFIERS"))
                        (or lambda-list (gensym "LAMBDA-LIST"))
                        (or docstring (gensym "DOCSTRING"))
                        (or declarations (gensym "DECLARATIONS"))
                        (or forms (gensym "FORMS")))))
    `(destructuring-bind ,bindings
         (split-lambda-form ,expression)
       (declare (ignore ,@(loop for symb in bindings
                                unless (symbol-package symb)
                                collect symb)))
       ,@body)))

(defun split-body-options (body)
  (values (loop for list = body then rest
                for (key val . rest) = list
                while (and (cdr list) (keywordp key))
                collect key collect val
                finally (setf body list))
          body))

(defun removef (plist &rest args)
  (loop for (key val) on plist by #'cddr
        unless (find key args) collect key
        unless (find key args) collect val))

(defmacro with-body-options ((body other-options &rest options) form &body body-forms)
  (flet ((unlist (opt) (if (listp opt) (first opt) opt))
         (kw (opt) (intern (string opt) "KEYWORD")))
    (let ((all-options (gensym "OPTIONS"))
          (option-keywords (loop for opt in options collect (kw (unlist opt)))))
      `(multiple-value-bind (,all-options ,body) (split-body-options ,form)
         (destructuring-bind (&key ,@options &allow-other-keys) ,all-options
           (let ((,other-options (removef ,all-options ,@option-keywords)))
             ,@body-forms))))))
