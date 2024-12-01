package main

import (
	"context"
	"errors"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

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
		return errors.New("script path required")
	}
	script, err := os.ReadFile(args[0])
	if err != nil {
		return err
	}

	posixArgv := []gmnlisp.Node{}
	for _, s := range args {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	lisp = lisp.Let(&gmnlisp.Pair{Key: gmnlisp.NewSymbol("args"), Value: gmnlisp.List(posixArgv...)})

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
