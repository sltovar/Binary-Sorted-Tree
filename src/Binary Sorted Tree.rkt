#lang racket
;------------------------------------------------------------
; BINARY SORTED TREE
;------------------------------------------------------------
;------------------------------------------------------------
; FUNCTION 1: insertInTree
; Takes the values from the list-to-tree, and sorts them
; based upon if they are less than or equal, or greater than,
; the root value. Recurses through the left and right
; subtrees until the list is empty.

; treeValue is the value from the given list that is sorted
; tree is the binary search tree that is being built 
;------------------------------------------------------------
(define (insertInTree treeValue tree)
  (cond
    ;-----------------------------------------
    ; If the tree is empty, return the list
    ;-----------------------------------------
    [(empty? tree)
     ; Places the first value given as the root node
     (list treeValue)]
    ;-----------------------------------------
    ; If the tree is too small (length of 1)
    ;-----------------------------------------
    [(equal? 1 (length tree))
     ; If the value is less than or equal to the first node
     (if (<= treeValue (car tree))
             ; Appending the root of the tree,
             (append [list (car tree)
                     ; the left node,
                     (list treeValue)
                     ; and the empty list on the right
                     '()])
             ; Appending the root of the tree,
             (append [list (car tree)
                     ; the empty list on the left
                     '()
                     ; and the right node
                     (list treeValue)])
             )]
    ;----------------------------------------------------
    ; To sort ALL of the given values from userList
    ;----------------------------------------------------
    [else
     ; If the value is less than or equal to the first node
     (if (<= treeValue (car tree))
         ;--------------------------------------------------------
         ; If the value is LESS than or EQUAL, put on LEFT subtree
         ;--------------------------------------------------------
         ; Appending the root of the tree,
         (append [list (car tree)]
                 ; the left side of the tree (cadr),
                 [list (insertInTree treeValue (cadr tree))]
                 ; and the right side of the tree (caddr).
                 [list (caddr tree)])
         ;--------------------------------------------------------
         ; If the value is GREATER than, put on RIGHT subtree
         ;--------------------------------------------------------
         ; Appending the root of the tree,
         (append [list (car tree)]
                 ; the left side of the tree (cadr),
                 [list (cadr tree)]
                 ; and the right side of the tree (caddr).
                 [list (insertInTree treeValue (caddr tree))])
         )] 
    ))
 
;--------------------------------------------------------------
; FUNCTION 2: list-to-tree
; In this function, the tree takes a list and converts it into
; a tree using insertInTree's function.
; The if statement asks if the initial list is empty. If it is
; not, call the list-to-tree function with the first value of
; the initial list, and the insertInTree function call with the
; parameters to match the insertInTree function.
;--------------------------------------------------------------
(define (list-to-tree userList tree) 
  (if (empty? userList)
      ;---------------------------------------------------------
      ; Returns the entire binary search tree (final step)
      ;---------------------------------------------------------
      tree
      ;---------------------------------------------------------
      ; Otherwise, call list-to-tree recursively to 'loop' the
      ; insertInTree function call. First, the rest of the list
      ; is called, then the first value of the list, to fill
      ; the tree.
      ;---------------------------------------------------------
      (list-to-tree (cdr userList) (insertInTree (car userList) tree)) 
     ))

;---------------------------------------------------------------
; TEST YOUR FINISHED FUNCTION WITH THE FOLLOWING CODE:
;---------------------------------------------------------------
(list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3) '())

; Which should return the following:
;'(22 ; ROOT
;  (7 (7 (4 (3) (5)) ()) (16 (8 (8) ()) (17))) ; LEFT SUBTREE OF 22
;  (25 () (34 (32) (67))) ) ; RIGHT SUBTREE OF 22

 