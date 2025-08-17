(if (< (length *argv*) 2)
  (progn
    (format (error-output) "Usage: ~A ~A {SSH-OPTIONS} USERNAME@DOMAIN PASSWORD~%" *executable-name* *program-name*)
    (quit 1)))

(let* ((ssh-param (subseq *argv* 0 (- (length *argv*) 1)))
       (password (elt *argv* (- (length *argv*) 1)))
       (sshpid nil))

  (with-handler
    (lambda (c)
      (format (error-output) "ssh is not found~%")
      (quit 2))
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
     (quit 3))

    (30
     (format (error-output) "TIME OUT~%")
     (quit 4)))

  (expect*
    ("Permission denied"
     (expect "password:")
     (send #\U3)
     (wait sshpid)
     (quit 5))

    ("$ ")
    )
  (sendln "echo YOU CAN CALL SOME COMMAND HERE")
  (expect "$ ")
  (sendln "exit")
  (wait sshpid)
  )
