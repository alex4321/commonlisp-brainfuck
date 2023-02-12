#!/bin/env clisp

(defun memory-new ()
    '()
)

(defun memory-padding-list (size default-value) (
    if (= size 0)
        (list default-value)
        (append (list default-value) (memory-padding-list (- size 1) default-value))
))

(defun memory-get-cell-value (memory address) (
    if (< address (list-length memory))
        (nth address memory)
        0
))

(defun memory-padded-if-required (memory address) (
    if (>= address (list-length memory))
        (append
            memory
            (memory-padding-list
                (- (+ address 1) (list-length memory))
                0
            ))
        memory
))

(defun memory-set-cell-value (memory address value)
    (
        let ((memory-padded (memory-padded-if-required memory address)))
            (setf (elt memory-padded address) value)
            memory-padded
    )
)

(defun get-operator-by-index (program counter) 
    (string (char program counter))
)

(defun get-operator-next-stack-level (operator current-level) (
    if (string= operator "+")
    current-level
    (
        if (string= operator "-")
        current-level
        (
            if (string= operator ">")
            current-level
            (
                if (string= operator "<")
                current-level
                (
                    if (string= operator ".")
                    current-level
                    (
                        if (string= operator ",")
                        current-level
                        (
                            if (string= operator "[")
                            (+ current-level 1)
                            (
                                if (string= operator "]")
                                (- current-level 1)
                            )
                        )
                    )
                )
            )
        )
    )
))

(defun symbol-stack-levels-inner (program counter current-level) (
    if (>= counter (length program))
        '()
        (
            let
                ((operator (get-operator-by-index program counter)))
            (
                append
                (list current-level)
                (
                    symbol-stack-levels-inner
                    program
                    (+ counter 1)
                    (get-operator-next-stack-level operator current-level)
                )
            )
        )
))

(defun get-stack-levels (program) (
    symbol-stack-levels-inner program 0 0
))

(defun iterate-list-inner (data handler index) (
    if (= (list-length data) 0)
        '()
        (
            let
                ((head (car data)) (tail (cdr data)))
            (
                append
                (list (funcall handler index head))
                (iterate-list-inner tail handler (+ 1 index))
            )
        )
))

(defun iterate-list (data handler) (
    iterate-list-inner data handler 0
))

(defun first-index-inner (data condition index) (
    if (= (list-length data) 0)
        nil
        (
            if (funcall condition index (car data))
                index
                (first-index-inner (cdr data) condition (+ index 1))
        )
))

(defun first-index (data condition) (
    first-index-inner data condition 0
))

(defun last-index-inner (data condition index) (
    if (= (list-length data) 0) 
        '()
        (
            let 
                (
                    (item-data (
                        if (funcall condition index (car data))
                            index
                            nil
                    ))
                    (tail-data (last-index-inner (cdr data) condition (+ index 1)))
                )
                (append (list item-data) tail-data)
        )
))

(defun reverse-list (data) (
    if (= (list-length data) 0)
        '()
        (
            let ((head (car data)) (tail (cdr data)))
            (
                append
                (reverse-list tail)
                (list head)
            )
        )
))

(defun last-index (data condition) (
    let
        ((indices (last-index-inner data condition 0)))
    (
        let
            ((indices-descending (reverse-list indices)))
            indices-descending
        (
            nth
            (
                first-index
                indices-descending
                (lambda (index data) data)
            )
            indices-descending
        )
    )
))

(defun operator-print (program stack-depth memory current-stack-depth current-code-cell current-memory-cell) (
    let ((value (memory-get-cell-value memory current-memory-cell)))
    (
        progn
        (format T (string (code-char value)))
        (list memory current-stack-depth (+ current-code-cell 1) current-memory-cell)
    )
))

(defun operator-input (program stack-depth memory current-stack-depth current-code-cell current-memory-cell) (
    let ((value (char-code (read-char))))
    (
        let
            ((memory-new (memory-set-cell-value memory current-memory-cell value)))
        (list memory-new current-stack-depth (+ current-code-cell 1) current-memory-cell)
    )
))

(defun operator-plus (program stack-depth memory current-stack-depth current-code-cell current-memory-cell) (
    let
        ((
            memory-new 
            (
                memory-set-cell-value 
                memory 
                current-memory-cell 
                (+ (memory-get-cell-value memory current-memory-cell) 1)
            )
        ))
    (list memory-new current-stack-depth (+ current-code-cell 1) current-memory-cell)
))

(defun operator-minus (program stack-depth memory current-stack-depth current-code-cell current-memory-cell) (
    let
        ((
            memory-new 
            (
                memory-set-cell-value 
                memory 
                current-memory-cell 
                (- (memory-get-cell-value memory current-memory-cell) 1)
            )
        ))
    (list memory-new current-stack-depth (+ current-code-cell 1) current-memory-cell)
))

(defun operator-next (program stack-depth memory current-stack-depth current-code-cell current-memory-cell)
    (list memory current-stack-depth (+ current-code-cell 1) (+ current-memory-cell 1))
)

(defun operator-previos (program stack-depth memory current-stack-depth current-code-cell current-memory-cell)
    (list memory current-stack-depth (+ current-code-cell 1) (- current-memory-cell 1))
)

(defun operator-condition (program stack-depth memory current-stack-depth current-code-cell current-memory-cell) (
    let
        ((value (memory-get-cell-value memory current-memory-cell)))
        (
            if (= value 0)
                (
                    let (
                        (go-to-cell (
                            first-index
                            stack-depth
                            (lambda (index depth) (
                                and
                                (= current-stack-depth depth)
                                (> index current-code-cell)
                            ))
                        ))
                    )
                    (list memory current-stack-depth go-to-cell current-memory-cell)
                )
                (list memory (+ current-stack-depth 1) (+ current-code-cell 1) current-memory-cell)
        )
))

(defun operator-condition-end (program stack-depth memory current-stack-depth current-code-cell current-memory-cell) (
    let 
        ((value (memory-get-cell-value memory current-memory-cell)))
        (
            if (not (= value 0))
                (
                    let (
                        (open-cell (
                            last-index
                            stack-depth
                            (lambda (index depth) (
                                let ((operator (get-operator-by-index program index)))
                                (
                                    and
                                    (string= operator "[")
                                    (= depth (- current-stack-depth 1))
                                )
                            ))
                        ))
                    )
                    (
                        let (
                            (go-to-cell (+ open-cell 1))
                        )
                        (list memory current-stack-depth go-to-cell current-memory-cell)
                    )
                )
                (list memory (- current-stack-depth 1) (+ current-code-cell 1) current-memory-cell)
        )
))

(defun get-operator-inner (pairs operator) (
    if (= (list-length pairs) 0)
        nil
        (
            let (
                (head-pair (car pairs))
                (tail-pairs (cdr pairs))
            )
            (
                let (
                    (head-pair-operator (nth 0 head-pair))
                    (head-pair-function (nth 1 head-pair))
                )
                (
                    if (string= head-pair-operator operator)
                        head-pair-function
                        (get-operator-inner tail-pairs operator)
                )
            )
        )
))

(defun get-operator-function (operator) (
    let ((pairs 
        (list
            (list "." (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-print program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list "," (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-input program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list "+" (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-plus program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list "-" (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-minus program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list ">" (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-next program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list "<" (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-previos program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list "[" (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-condition program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
            (list "]" (lambda (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (operator-condition-end program stack-levels memory current-stack-depth current-code-cell current-memory-cell)) )
        )
    ))
    (
        get-operator-inner
        pairs
        operator
    )
))

(defun interpreter-next-state-inner (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (
    let ((operator (get-operator-by-index program current-code-cell)))
    (
        let ((operator-function (get-operator-function operator)))
        (
            let ((new-machine-state (
                funcall 
                operator-function 
                program 
                stack-levels 
                memory 
                current-stack-depth 
                current-code-cell 
                current-memory-cell
            )))
            (
                let (
                    (new-memory (nth 0 new-machine-state))
                    (new-stack-depth (nth 1 new-machine-state))
                    (new-code-cell (nth 2 new-machine-state))
                    (new-memory-cell (nth 3 new-machine-state))
                )
                (list program stack-levels new-memory new-stack-depth new-code-cell new-memory-cell)
            )
        )
    )
))

(defun interpreter-next-state (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (
    if (>= current-code-cell (length program))
        nil
        (
            interpreter-next-state-inner
            program
            stack-levels
            memory
            current-stack-depth
            current-code-cell
            current-memory-cell
        )
))

(defun run-bf-program-inner (program stack-levels memory current-stack-depth current-code-cell current-memory-cell) (
    let 
        ((next-machine-state (
            interpreter-next-state
            program
            stack-levels
            memory
            current-stack-depth
            current-code-cell
            current-memory-cell
        )))
    (
        if next-machine-state
        (
            let (
                (next-program (nth 0 next-machine-state))
                (next-stack-levels (nth 1 next-machine-state))
                (next-memory (nth 2 next-machine-state))
                (next-stack-depth (nth 3 next-machine-state))
                (next-code-cell (nth 4 next-machine-state))
                (next-memory-cell (nth 5 next-machine-state))
            )
            (
                run-bf-program-inner
                next-program
                next-stack-levels
                next-memory
                next-stack-depth
                next-code-cell
                next-memory-cell
            )
        )
    )
))

(defun run-filtered-bf-program (program) (
    let (
        (stack-levels (get-stack-levels program))
        (memory (memory-new))
    )
    (
        run-bf-program-inner
        program
        stack-levels
        memory
        0
        0
        0
    )
))

(defun concatenate-strings (values) (
    concatenate 'string values
))

(defun filter-operator-inner (operator) (
    let ((operator-string (string operator))) 
    (
        or
        (string= operator-string "+")
        (string= operator-string "-")
        (string= operator-string ".")
        (string= operator-string ",")
        (string= operator-string "<")
        (string= operator-string ">")
        (string= operator-string "[")
        (string= operator-string "]")
    )
))

(defun filter-program-inner (program counter) (
    if (>= counter (length program))
        '()
        (
            let ((operator (char program counter)))
            (
                let (
                    (head (
                        if (filter-operator-inner operator)
                        (list operator)
                        (list)
                    ))
                    (tail (filter-program-inner program (+ counter 1)))
                )
                (append head tail)
            )
        )
))

(defun filter-program (program) (
    concatenate-strings (filter-program-inner program 0)
))

(defun run-bf-program (program) (
    run-filtered-bf-program (filter-program program)
))

(defun file-get-contents (filename) (
    with-open-file (stream filename)
        (
            let ((contents (make-string (file-length stream))))
            (read-sequence contents stream)
        contents
    )
))

(defun main(file-name) (
    run-bf-program (file-get-contents file-name)
))

(main (nth 0 *args*))