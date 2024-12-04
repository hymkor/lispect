(block main
  (if (< (length args) 2)
    (progn
      (format (error-output) "Usage: ~A ~A USERNAME@DOMAIN PASSWORD~%" $EXECUTABLE_NAME $PROGRAM_NAME)
      (return-from main nil)))
  (defglobal account (car args))
  (defglobal password (cadr args))

  (with-handler
    (lambda (c)
      (format (error-output) "ssh is not found~%")
      (return-from main nil))
    (spawn "ssh" account))

  (expect*
    ("[fingerprint])?"
     (sendln "yes")
     (expect "password: ")
     (sendln password))
    ("password: "
     (sendln password))
    (("Connection refused"
      "Could not resolve hostname")
     (return-from main nil))
    (30
     (format (error-output) "TIME OUT~%")
     (return-from main nil)))

  (expect*
    ("Permission denied"
     (return-from main nil)
     )
    ("$ "))
  (sendln "echo YOU CAN CALL SOME COMMAND HERE")
  (expect "$ ")
  (sendln "exit"))
