package lispect

import (
	"context"
	"os"

	"github.com/hymkor/gmnlisp"
)

// RunFile loads and executes a Lisp script from the specified file.
//
// - fname: the path to the Lisp source file to be executed.
// - args: typically corresponds to os.Args[1:].
// - args[0]: the script file name (not used directly).
// - args[1:]: arguments passed to the script.
func RunFile(fname string, args []string) error {
	script, err := os.ReadFile(fname)
	if err != nil {
		return err
	}
	return RunString(string(script), args)
}

// RunString executes a Lisp script given directly as a string.
//
// - script: the Lisp source code to be executed (equivalent to the contents of a file).
// - args: typically corresponds to os.Args[1:].
// - args[0]: the script file name (not used directly).
// - args[1:]: arguments passed to the script.
func RunString(script string, args []string) error {
	term, err := NewTerm()
	if err != nil {
		return err
	}
	defer term.Close()

	watcher := NewWatcher(term)

	env := &Env{
		term: term,
		w:    watcher,
	}
	defer env.Close()

	gmnlisp.NewLineOnFormat = []byte{'\r', '\n'}

	lisp := gmnlisp.New()

	lisp = lisp.Flet(
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
	posixArgv := []gmnlisp.Node{}
	for _, s := range args[1:] {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	executable := os.Args[0]
	if value, err := os.Executable(); err == nil {
		executable = value
	}
	lisp = lisp.Let(gmnlisp.Variables{
		gmnlisp.NewSymbol("ARGV"):              gmnlisp.List(posixArgv...),
		gmnlisp.NewSymbol("*argv*"):            gmnlisp.List(posixArgv...),
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
