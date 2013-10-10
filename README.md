# formatting

Format things as strings:

    format ("Person's name is " %text% ", age is " %hex) "Dave" 54

or with short-names:

    format ("Person's name is " %x% ", age is " %x) "Dave" 54

Similar to C's `printf`:

    printf("Person's name is %s, age is %x","Dave",54);

and Common Lisp's `FORMAT`:

    (format nil "Person's name is ~a, age is ~x" "Dave" 54)
