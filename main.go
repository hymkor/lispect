package lispect

import (
	"context"
	"os"

	"github.com/hymkor/gmnlisp"
)

// RunFile loads and executes a Lisp script from the specified file.
// - `fname`: the path to the Lisp source file to be executed.
// - `args`: a slice of arguments typically corresponding to os.Args[1:].
// - `args[0]` is set to the Lisp variable `*program-name*`.
// - `args[1:]` is set to the Lisp variable `*argv*`.
func RunFile(fname string, args []string) error {
	script, err := os.ReadFile(fname)
	if err != nil {
		return err
	}
	return RunString(string(script), args)
}

// New creates a new Env instance that embeds a gmnlisp.World and
// sets up a pseudoterminal session and related communication structures.
// This allows users to customize and control the Lisp environment flexibly.
func New() (*Env, error) {
	term, err := NewTerm()
	if err != nil {
		return nil, err
	}

	watcher := NewWatcher(term)

	env := &Env{
		term: term,
		w:    watcher,
	}

	gmnlisp.NewLineOnFormat = []byte{'\r', '\n'}

	lisp := gmnlisp.New()

	env.World = lisp.Flet(
		gmnlisp.Functions{
			gmnlisp.NewSymbol("send"):    &gmnlisp.Function{Min: 1, F: env.send},
			gmnlisp.NewSymbol("sendln"):  &gmnlisp.Function{Min: 1, F: env.sendln},
			gmnlisp.NewSymbol("spawn"):   &gmnlisp.Function{Min: 1, F: env.spawn},
			gmnlisp.NewSymbol("expect*"): gmnlisp.SpecialF(env.expectX),
			gmnlisp.NewSymbol("expect"):  gmnlisp.SpecialF(env.expect),
			gmnlisp.NewSymbol("getenv"):  gmnlisp.Function1(env.getenv),
			gmnlisp.NewSymbol("setenv"):  gmnlisp.Function2(env.setenv),
			gmnlisp.NewSymbol("wait"):    gmnlisp.Function1(env.wait),
		})

	return env, nil
}

// RunString executes a Lisp script given directly as a string.
// - `script`: the Lisp source code to be executed (equivalent to the contents of a file).
// - `args`: a slice of arguments typically corresponding to os.Args[1:].
// - `args[0]` is set to the Lisp variable `*program-name*`.
// - `args[1:]` is set to the Lisp variable `*argv*`.
func RunString(script string, args []string) error {
	env, err := New()
	if err != nil {
		return err
	}
	defer env.Close()

	var posixArgv gmnlisp.Node = gmnlisp.Null
	for i := len(args) - 1; i >= 1; i-- {
		posixArgv = &gmnlisp.Cons{Car: gmnlisp.String(args[i]), Cdr: posixArgv}
	}
	executable := os.Args[0]
	if value, err := os.Executable(); err == nil {
		executable = value
	}
	lisp := env.Let(gmnlisp.Variables{
		gmnlisp.NewSymbol("ARGV"):              posixArgv,
		gmnlisp.NewSymbol("*argv*"):            posixArgv,
		gmnlisp.NewSymbol("PROGRAM-NAME"):      gmnlisp.String(args[0]),
		gmnlisp.NewSymbol("*program-name*"):    gmnlisp.String(args[0]),
		gmnlisp.NewSymbol("EXECUTABLE-NAME"):   gmnlisp.String(executable),
		gmnlisp.NewSymbol("*executable-name*"): gmnlisp.String(executable),
	})

	ctx, cancel := context.WithCancel(context.Background())
	_, err = lisp.Interpret(ctx, script)
	cancel()
	return err
}
