(block main
  (if (< (length args) 2)
    (progn
      (format (error-output) "Usage: ~A ~A HOSTNAME PASSWORD~%" $EXECUTABLE_NAME $PROGRAM_NAME)
      (return-from main nil)
      )
    )
  (defglobal host (car args))
  (defglobal password (cadr args))

  (spawn "ssh" host)
  (expect*
    ("[fingerprint])?"
     (sendln "yes")
     (expect "password: ")
     (sendln password)
     )
    ("password: "
     (sendln password)
     )
    ("Connection refused"
     (return-from main nil)
     )
    (30
     (format (error-output) "TIME OUT~%")
     (return-from main nil)
     )
    )
  (expect "$ ")
  (sendln "echo YOU CAN CALL SOME COMMAND HERE")
  (expect "$ ")
  (sendln "exit")
  )
