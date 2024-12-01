Lispect
=======

- A text-terminal automation tool similar to [expect(1)](https://linux.die.net/man/1/expect) of UNIX and Linux
- It runs on Windows 10 and later, and Linux
- The script is written in the subset of [ISLisp] ([gmnlisp])

```example.lsp
(if (equal (getenv "OS") "Windows_NT")
  (spawn "cmd.exe" "/k" "set PROMPT=$$$S")
  (spawn "bash"))

(defglobal ctrlc (create-string 1 (convert 3 <character>)))
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
       (sendln 'interval 200 "echo test")
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

### Functions

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
    - Start the executable file

Other functions are defined on [ISLisp]

### Technologies used

- [Creating a Pseudoconsole session - Windows Console | Microsoft Learn](https://learn.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session)
- [aymanbagabas/go-pty: Cross platform Go Pty interface](https://github.com/aymanbagabas/go-pty)
- [hymkor/gmnlisp: gmnlisp - the subset of ISLisp][gmnlisp]

[ISLisp]: http://islisp.org
[gmnlisp]: https://github.com/hymkor/gmnlisp
[GophaLua]: https://github.com/yuin/gopher-lua

### Comparison with [Expect-lua]

|                       | [Expect-lua]          | Lispect       |
|-----------------------|-----------------------|---------------|
| Started               | 2017                  | 2024          |
| Script                | Lua ([GophaLua])      | ISLisp ([gmnlisp]) |
| Windows 7/8           | Supported             | Not Supported |
| Windows 10/11         | Supported             | Supported     |
| Linux                 | Not Supported         | Supported     |
| Mechanisms            | [ReadConsoleOutputW]  |[PseudoConsole]|
| Read Misses           | Possible[^1]          | Unlikely      |
| Stdout Redirection    | Unavailable           | Available     |
| Status                | Stable                |Work in Progress|

[^1]: When the output is too excessive, there might be some dropped data

[ReadConsoleOutputW]: https://github.com/hymkor/expect/issues/34
[PseudoConsole]: https://learn.microsoft.com/en-us/windows/console/creating-a-pseudoconsole-session
[Expect-lua]: https://github.com/hymkor/expect
