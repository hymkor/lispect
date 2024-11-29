(spawn "cmd.exe")
(let ((ctrlc (format nil "~A" (convert 3 <character>))))
  (block main
    (while t
      (case (expect* "xxx" "yyy")
        ((0)
         (sendln (string-append ctrlc "exit"))
         (return-from main nil)
         )
        ((1)
         (sendln (string-append ctrlc "rem"))
         )
        ))))
