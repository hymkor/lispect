(block main
  (if (< (length ARGV) 2)
    (progn
      (format (error-output) "Usage: ~A ~A USERNAME@DOMAIN PASSWORD~%" EXECUTABLE-NAME PROGRAM-NAME)
      (return-from main nil)))

  (let ((account (car ARGV))
        (password (cadr ARGV))
        (sshpid nil))

    (with-handler
      (lambda (c)
        (format (error-output) "ssh is not found~%")
        (return-from main nil))
      (setq sshpid (spawn "ssh" account)))

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
