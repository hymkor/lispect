package main

import (
	"context"
	"fmt"
	"os"
	"runtime"

	"github.com/hymkor/gmnlisp"
)

var version string

func mains(args []string) error {
	term, err := NewTerm()
	if err != nil {
		return err
	}
	defer term.Close()

	watcher := NewWatcher(term)

	g := &Global{
		term: term,
		w:    watcher,
	}
	defer g.Close()

	gmnlisp.NewLineOnFormat = []byte{'\r', '\n'}

	lisp := gmnlisp.New()

	lisp = lisp.Flet(
		gmnlisp.Functions{
			gmnlisp.NewSymbol("send"):    &gmnlisp.Function{Min: 1, F: g.send},
			gmnlisp.NewSymbol("sendln"):  &gmnlisp.Function{Min: 1, F: g.sendln},
			gmnlisp.NewSymbol("spawn"):   &gmnlisp.Function{Min: 1, F: g.spawn},
			gmnlisp.NewSymbol("expect*"): gmnlisp.SpecialF(g.expectX),
			gmnlisp.NewSymbol("expect"):  gmnlisp.SpecialF(g.expect),
			gmnlisp.NewSymbol("getenv"):  gmnlisp.Function1(g.getenv),
		})

	if len(args) <= 0 {
		return fmt.Errorf("%s %s-%s-%s: script path required",
			os.Args[0],
			version,
			runtime.GOOS,
			runtime.GOARCH)
	}
	script, err := os.ReadFile(args[0])
	if err != nil {
		return err
	}

	posixArgv := []gmnlisp.Node{}
	for _, s := range args[1:] {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	executable := os.Args[0]
	if value, err := os.Executable(); err == nil {
		executable = value
	}
	lisp = lisp.Let(gmnlisp.Variables{
		gmnlisp.NewSymbol("args"):             gmnlisp.List(posixArgv...),
		gmnlisp.NewSymbol("$PROGRAM_NAME"):    gmnlisp.String(args[0]),
		gmnlisp.NewSymbol("$EXECUTABLE_NAME"): gmnlisp.String(executable),
	})

	ctx, cancel := context.WithCancel(context.Background())
	_, err = lisp.Interpret(ctx, string(script))
	cancel()
	return err
}

func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
