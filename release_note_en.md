v0.6.2
======
Jul 21, 2025

- Dependency: Bump gmnlisp to [v0.7.19]

[v0.7.19]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.19

v0.6.1
======
Jun 9, 2025

- Dependency: Bump gmnlisp to [v0.7.14]

[v0.7.14]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.14

v0.6.0
======
May 8, 2025

- Refactor: renamed the structure type managing Pseudoconsole session, goroutine communication channels, and other related information from `Global` to `Env`.
- Dependency: Bump gmnlisp to [v0.7.10]
- In [gmnlisp v0.7.10], symbol names have officially become case-insensitive in accordance with the ISLisp standard.  
  To ensure consistent behavior, Lispect has adopted the canonical names `*argv*`, `*program-name*`, `*executable-name*`, and `*match*` for built-in global variables.  
  The previous names remain available as aliases for backward compatibility but are now deprecated.

[v0.7.10]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.10

v0.5.0
======
May 3, 2025

- Refactor: split code into two packages: (Thanks to [@rwinkhart])
    - `main` package for the CLI: `github.com/hymkor/lispect/cmd/lispect`
    - `lispect` package for library use: `github.com/hymkor/lispect`
- Dependency: Bump `gmnlisp` to v0.7.9

[@rwinkhart]: https://github.com/rwinkhart

v0.4.1
======
Jan 16, 2025

- Update gmnlisp to v0.7.8

v0.4.0
======
Dec.8, 2024

- Rename system variables (**Breaking changes**)
    - `$MATCH` → `MATCH`
- Update gmnlisp to v0.7.4

v0.3.0
=======
Dec.5, 2024

- `(spawn)` returns a process-id now
- Implement `(wait PROCESS-ID)`
- Rename system variables (**Breaking changes**)
    - `args` → `ARGV`
    - `$PROGRAM_NAME` → `PROGRAM-NAME`
    - `$EXECUTABLE_NAME` → `EXECUTABLE-NAME`
- Fix: `(expect "B")` after `(expect "A")` sometimes missed B when `"A\nB"` was output at once time
- Changes on gmnlisp
    - Fix: panic when the token is only `#\`

v0.2.0
======
Dec.4, 2024

- Fix: on Linux, `~%` feeded line, but it did not carriage return
- On gmnlisp changes
    - Support unicode character literal `#\U3042` like CommonLisp
- Implement `(setenv "NAME" "VALUE")`
- Implement `$MATCH` - the matching string in the block of `(expect*)`

v0.1.0
======
Dec.2, 2024

- First Release
