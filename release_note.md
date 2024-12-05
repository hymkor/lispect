- `(spawn)` returns a process-id now
- Implement `(wait PROCESS-ID)`

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
