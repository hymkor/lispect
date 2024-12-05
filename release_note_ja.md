- `(spawn)` はプロセスIDを返すようにした
- `(wait プロセスID)` を実装
- システム変数名を変更 (**Breaking changes**)
    - `args` → `ARGV`
    - `$PROGRAM_NAME` → `PROGRAM-NAME`
    - `$EXECUTABLE_NAME` → `EXECUTABLE-NAME`
- expect: 監視語がたてつづけに出力された時、後のものを見逃す時があった点を修正

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
