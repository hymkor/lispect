(block main
  (if (< (length *argv*) 2)
    (progn
      (format (error-output) "Usage: ~A ~A USERNAME@DOMAIN PASSWORD~%" *executable-name* *program-name*)
      (return-from main nil)))

  (let ((account (car *argv*))
        (password (cadr *argv*))
        (sshpid nil))

    (with-handler
      (lambda (c)
        (format (error-output) "ssh is not found~%")
      (setq sshpid (spawn "ssh" account)))
        (return-from main nil))

    (expect*
      ("[fingerprint])?"
       (sendln "yes")
       (expect "password:")
       (sendln password))
      ("password:"
       (sendln password))
      (("Connection refused"
        "Could not resolve hostname")
       (return-from main nil))
      (30
       (format (error-output) "TIME OUT~%")
       (return-from main nil)))

    (expect*
      ("Permission denied"
       (expect "password:")
       (send #\U3)
       (wait sshpid)
       (return-from main nil)
       )
      ("$ "))
    (sendln "echo YOU CAN CALL SOME COMMAND HERE")
    (expect "$ ")
    (sendln "exit")
    (wait sshpid)
    )
  )
