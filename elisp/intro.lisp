
'(rose
  violet
  daisy
  buttercup)

(+ 2 2)

'(this list has (a list inside of it))

'(this list includes "text between quotation marks"

'(this list looks like 
       this list)

(this is an unquoted list)

(+ 2 2 (+ 3 3))

(fill-column)

(concat "abc" "def")

(substring "The quick brown fox" 16 19)

(+ 2 fill-column)

(concat "The " (number-to-string (+ 2 fill-column)) " red foxes.")

( + )

( * )

(+ 2 'hello)

(message "This message appears in the echo area!")

(message "The name of this buffer is: %s." (buffer-name))

(message "The value of fill-column is %d." fill-column)

(message "There are %d %s in the office!"
              (- fill-column 14) "pink elephants")


 (message "He saw %d %s"
              (- fill-column 32)
              (concat "red "
                      (substring
                       "The quick brown foxes jumped." 16 21)
                      " leaping."))

(set 'flowers '(rose  violet  daisy  buttercup))

flowers

'flowers

(setq carnivores '(lion tiger leopard))

(setq trees '(pine fir oak maple)
           herbivores '(gazelle antelope zebra))

trees
herbivores


(setq counter 0)                ; Let's call this the initializer.
     
(setq counter (+ counter 2))    ; This is the incrementer.
     
counter                         ; This is the counter.


counter

(tiger)

(setq my-message "This is my message")

my-message


(buffer-name)

(buffer-file-name)"/Users/shawnpage/lang/lisp/elisp/intro.lisp"

(current-buffer)

(other-buffer)

(switch-to-buffer (other-buffer))




(buffer-size)




(point)

(point-min)
(point)

