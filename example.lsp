(spawn "cmd.exe")
(let ((ctrlc (format nil "~A" (convert 3 <character>))))
  (block main
    (while t
      (expect*
        ("xxx"
         (send ctrlc)
         (sendln "exit")
         (return-from main nil)
         )
        (("yyy" "zzz")
         (send ctrlc)
         (sendln "rem")
         )
        (10 ; timeout second
         (send ctrlc)
         (sendln "exit")
         (return-from main nil)
        )
        )
      )
    )
  )
