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
