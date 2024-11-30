(if (equal (getenv "OS") "Windows_NT")
  (spawn "cmd.exe" "/k" "set PROMPT=$$$S")
  (spawn "bash"))

(defglobal ctrlc (create-string 1 (convert 3 <character>)))
(block main
  (while t
    (expect*
      ("xxx"
       (send ctrlc)
       (expect "$ ")
       (sendln "exit")
       (return-from main nil)
       )
      (("yyy" "zzz")
       (send ctrlc)
       (expect "$ ")
       (sendln "echo test")
       )
      (10 ; timeout second
       (send ctrlc)
       (expect "$ ")
       (sendln "exit")
       (return-from main nil)
       )
      )
    )
  )
