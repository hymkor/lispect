Lispect
=======

- A text-terminal automation tool similar to [expect(1)](https://linux.die.net/man/1/expect) of UNIX and Linux
- It runs on Windows 10 and later, and Linux
- The script is written in the subset of ISLisp ([gmnlisp](https://github.com/hymkor/gmnlisp))

```example.lsp
(if (equal (getenv "OS") "Windows_NT")
  (spawn "cmd.exe" "/k" "set PROMPT=$$$S")
  (spawn "bash"))

(defglobal ctrlc (format nil "~A" (convert 3 <character>)))
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
```

#### Technologies used

- [Creating a Pseudoconsole session - Windows Console | Microsoft Learn](https://learn.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session)
- [aymanbagabas/go-pty: Cross platform Go Pty interface](https://github.com/aymanbagabas/go-pty)
- [hymkor/gmnlisp: gmnlisp - the subset of ISLisp](https://github.com/hymkor/gmnlisp)
