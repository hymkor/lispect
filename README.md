[![GoDev](https://pkg.go.dev/badge/github.com/hymkor/lispect)](https://pkg.go.dev/github.com/hymkor/lispect)

Lispect
=======

- A text-terminal automation tool similar to [expect(1)] of UNIX and Linux
- It runs on Windows 10 and later, and Linux
- The script is written in the subset of [ISLisp] ([gmnlisp])

[expect(1)]: https://linux.die.net/man/1/expect

**example.lsp**

$ `lispect example.lsp USERNAME@DOMAIN PASSWORD`  
→ login `USERNAME@DOMAIN` with `ssh` and input `PASSWORD` automatically

> [!NOTE]
> Some variable names have been updated for compatibility with recent versions of gmnlisp, but the old names remain available for now.

```example.lsp
(catch 'fail
  (if (< (length ARGV) 2)
    (progn
      (format (error-output) "Usage: ~A ~A USERNAME@DOMAIN PASSWORD~%" *executable-name* *program-name*)
      (throw 'fail nil)))

  (let ((account (car *argv*))
        (password (cadr *argv*))
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
```

Install
-------

Download the binary package from [Releases](https://github.com/hymkor/lispect/releases) and extract the executable.

### go install

```
go install github.com/hymkor/lispect/cmd/lispect@latest
```

### scoop-installer

```
scoop install https://raw.githubusercontent.com/hymkor/lispect/master/lispect.json
```

or

```
scoop bucket add hymkor https://github.com/hymkor/scoop-bucket
scoop install lispect
```

Functions and variables
-----------------------

Parameters enclosed in curly braces {...} are optional and can be omitted.

- `(send {'interval MS} "STRING")`
    - Send STRING to the terminal
    - Specify the `'interval MS` option to make the program wait MS milliseconds before outputting each character
- `(sendln {'interval MS} "STRING")`
    - send STRING and Enter-key to the terminal
- `(expect {'timeout SEC} "STRING-0" "STRING-1" ...)`
    - Wait until STRINGs are found on the terminal
    - When STRING-n is found on the terminal, it will return n
    - If a timeout occurs after SEC seconds have elapsed, it returns -1
- `(expect* ("STRING-0" COMMANDS-0...) ("STRING-1"...)... (SEC COMMANDS-FOR-TIMEOUT...))`
    - When STRING-n is found on the terminal, COMMANDS-n will be executed
    - After SEC seconds have elapsed, COMMANDS-FOR-TIMEOUT will be executed
- `(spawn "COMMANDNAME" "ARG-1" ...)`
    - Start the executable file and it returns Process-ID
- `(wait PID)`
    - Wait the process specified with PID
- `(getenv "NAME")`
    - Get the value of the environment variable NAME
- `(setenv "NAME" "VALUE")`
    - Set the value of the environment variable NAME as VALUE
- `*argv*` (formerly `ARGV`, now deprecated)[^1]  
  The list of command-line arguments.
- `*program-name*` (formerly `PROGRAM-NAME`, now deprecated)[^1]  
  The path to the script.
- `*executable-name*` (formerly `EXECUTABLE-NAME`, now deprecated)[^1]  
  The path to the Lispect executable.
- `*match*` (formerly `MATCH`, now deprecated)[^1]  
  The matched string in an `(expect*)` block.
- `(catch TAG-FORM FORM...)` and `(throw TAG-FORM RESULT-FORM)`
    - Non-local exits. See also [ISLISP draft - 14.7. Non-local exits](https://islisp-dev.github.io/ISLispHyperDraft/islisp-v23.html#non_local_exits)
- `(with-handler HANDLER FORM...)`
    - When an error occurs in `FORMS...`, call `HANDLER`.
      See also [ISLISP Draft - Operations relating to condition handling](https://islisp-dev.github.io/ISLispHyperDraft/islisp-v23.html#s_with_handler)

[^1]: These variable names were changed to accommodate gmnlisp v0.7.10 and later, in which symbol names are case-insensitive. The new format avoids potential name conflicts.

Other functions are defined on [ISLisp]

Comparison with [Expect-lua] and [expect(1)]
--------------------------------------------

|                       | [Expect-lua]          | Lispect       |[expect(1)]
|-----------------------|-----------------------|---------------|-----------
| Started               | 2017                  | 2024          | 1990?
| Script                | Lua ([GophaLua])      | ISLisp ([gmnlisp]) | tcl
| Windows 7/8           | Supported             | Not Supported | Not Supported
| Windows 10/11         | Supported             | Supported     | Not Supported
| Linux                 | Not Supported         | Supported     | Supported
| Mechanisms            | [ReadConsoleOutputW]  |[PseudoConsole]| PseudoConsole
| Read Misses           | Possible[^1]          | Unlikely      | Never
| Stdout Redirection    | Unavailable           | Available     | Available
| Status                | Stable                |Work in Progress| Stable

[^1]: When the output is too excessive, there might be some dropped data

[ReadConsoleOutputW]: https://github.com/hymkor/expect/issues/34
[PseudoConsole]: https://learn.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session
[Expect-lua]: https://github.com/hymkor/expect

Technologies used
-----------------

- [Creating a Pseudoconsole session - Windows Console | Microsoft Learn](https://learn.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session)
- [aymanbagabas/go-pty: Cross platform Go Pty interface](https://github.com/aymanbagabas/go-pty)
- [hymkor/gmnlisp: gmnlisp - the subset of ISLisp][gmnlisp]

[ISLisp]: http://islisp.org
[gmnlisp]: https://github.com/hymkor/gmnlisp
[GophaLua]: https://github.com/yuin/gopher-lua

Acknowledgements
----------------

Thanks to the following contributors:

- [@rwinkhart](https://github.com/rwinkhart) - [#1](https://github.com/hymkor/lispect/pull/1) : modularization support

Author
------

This project is developed and maintained by [@hymkor](https://github.com/hymkor)
