- 疑似端末情報やゴルーチン通信用チャンネルなどの情報を管理する構造体名を `Global` から `Env` に改めた。
- ISLisp の仕様に従うため、将来的に gmnlisp ではシンボルの英大文字・小文字の区別がなくなることを見越し、グローバル変数の名前を `*argv*`、`*program-name*`、`*executable-name*`、`*match*` のように変更した。従来の名前も非推奨ながら当面は利用可能。

v0.5.0
======
May 3, 2025

- ソースを二つのパッケージへ分割した (Thanks to [@rwinkhart])
    - `main` パッケージ(実行ファイル用): `github.com/hymkor/lispect/cmd/lispect`
    - `lispect` パッケージ(ライブラリ用): `github.com/hymkor/lispect`
- gmnlisp を v0.7.9 へバージョンアップした

[@rwinkhart]: https://github.com/rwinkhart

v0.4.1
======
Jan 16, 2025

- gmnlisp を v0.7.8 へ

v0.4.0
======
Dec.8, 2024

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
