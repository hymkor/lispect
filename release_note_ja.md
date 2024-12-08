- システム変数名を変更 (**Breaking changes**)
    - `$MATCH` → `MATCH`
- gmnlisp を v0.7.4 相当へ

v0.3.0
=======
Dec.5, 2024

- `(spawn)` はプロセスIDを返すようにした
- `(wait プロセスID)` を実装
- システム変数名を変更 (**Breaking changes**)
    - `args` → `ARGV`
    - `$PROGRAM_NAME` → `PROGRAM-NAME`
    - `$EXECUTABLE_NAME` → `EXECUTABLE-NAME`
- `"A\nB"` を一括して出力された時、`(expect "A")` の後の `(expect "B")` が B を見逃す不具合を修正
- gmnlisp 側修正
    - `#\` だけのトークンがあるとクラッシュする不具合を修正

v0.2.0
======
Dec.4, 2024

- Linux で、`~%` が改行だけで、復帰をしていなかったのを修正
- gmnlisp の変更のとりこみ:
    - CommonLisp のような文字リテラル `#\U3042` をサポート
- `(setenv "NAME" "VALUE")` を実装
- `(expect*)` 内のブロックでマッチ文字列をさす `$MATCH` を実装

v0.1.0
======
Dec.2, 2024

- 初版
