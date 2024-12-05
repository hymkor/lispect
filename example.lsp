(block main
  (if (< (length args) 2)
    (progn
      (format (error-output) "Usage: ~A ~A USERNAME@DOMAIN PASSWORD~%" $EXECUTABLE_NAME $PROGRAM_NAME)
      (return-from main nil)))

  (let ((account (car args))
        (password (cadr args))
        (sshpid nil))

    (with-handler
      (lambda (c)
        (format (error-output) "ssh is not found~%")
        (return-from main nil))
      (setq sshpid (spawn "ssh" account)))

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
    (sendln "exit")
    (wait sshpid)
    )
  )
