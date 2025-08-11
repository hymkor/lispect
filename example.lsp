(block main
  (if (< (length *argv*) 2)
    (progn
      (format (error-output) "Usage: ~A ~A {SSH-OPTIONS} USERNAME@DOMAIN PASSWORD~%" *executable-name* *program-name*)
      (return-from main nil)))

  (let* ((ssh-param (subseq *argv* 0 (- (length *argv*) 1)))
         (password (elt *argv* (- (length *argv*) 1)))
         (sshpid nil))

    (with-handler
      (lambda (c)
        (format (error-output) "ssh is not found~%")
        (return-from main nil))
      (setq sshpid (apply #'spawn "ssh" ssh-param)))

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
