(catch 'fail
  (if (< (length ARGV) 2)
    (progn
      (format (error-output) "Usage: ~A ~A USERNAME@DOMAIN PASSWORD~%" EXECUTABLE-NAME PROGRAM-NAME)
      (throw 'fail nil)))

  (let ((account (car ARGV))
        (password (cadr ARGV))
        (sshpid nil))

    (with-handler
      (lambda (c)
        (format (error-output) "ssh is not found~%")
        (throw 'fail nil))
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
       (throw 'fail nil))
      (30
       (format (error-output) "TIME OUT~%")
       (throw 'fail nil)))

    (expect*
      ("Permission denied"
       (expect "password:")
       (send #\U3)
       (wait sshpid)
       (throw 'fail nil)
       )
      ("$ "))
    (sendln "echo YOU CAN CALL SOME COMMAND HERE")
    (expect "$ ")
    (sendln "exit")
    (wait sshpid)
    )
  )
