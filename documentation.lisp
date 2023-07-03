(in-package #:org.shirakumo.form-fiddle)

(docs:define-docs
  (function declaration-p
    "Returns T if the given thing is a declaration expression.")

  (function docstring-p
    "Returns T if the given thing is a docstring.")

  (function lambda-function
    "Returns the defining FUNCTION of the lambda-form.
     v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-name
    "Returns the NAME of the lambda-form, if any.

             v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-qualifiers
    "Returns the QUALIFIERS of the lambda-form.

                      v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-lambda-list
    "Returns the LAMBDA-LIST of the lambda-form.

                                  v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-body
    "Returns all BODY forms of the lambda-form.

                                         |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯v¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-docstring
    "Returns the DOCSTRING of the lambda-form, if any.

                                               v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-declarations
    "Returns the DECLARATIONS of the lambda-form, if any.

                                                             v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function lambda-forms
    "Returns the actual body forms of the lambda-form, if any.

                                                                         v
 (function [name] qualifier* lambda-list [[docstring? | declaration*]] form*)")

  (function split-lambda-form
    "Returns all parts of a lambda-form as a list in the following order:
 FUNCTION NAME QUALIFIERS LAMBDA-LIST DOCSTRING DECLARATIONS FORMS")

  (function with-destructured-lambda-form
    "Destructures the given EXPRESSION into its lambda-form parts.")

  (function split-body-options
    "Parses the body into two separate lists of forms and options.
This is found in some expressions like in the clause body of RESTART-CASE.

BODY   ::= OPTION* FORM*
OPTION ::= KEYWORD EXPRESSION")

  (function with-body-options
    "Destructures the body according to split-body-kargs.

OTHER-OPTIONS will be bound to contain all the options that occur in the body but
were not explicitly requested in OPTIONS. BODY will be bound to the remaining
body forms. Each option in OPTIONS can be either a symbol or a list of symbol and
default. The symbol is automatically converted to a keyword to match against the
body options."))
