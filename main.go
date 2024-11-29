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
			gmnlisp.NewSymbol("sendln"):  gmnlisp.Function1(g.sendln),
			gmnlisp.NewSymbol("spawn"):   &gmnlisp.Function{Min: 1, F: g.spawn},
			gmnlisp.NewSymbol("expect*"): &gmnlisp.Function{Min: 1, F: g.expect},
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

	_, err = lisp.Interpret(context.Background(), string(script))
	return err
}

func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
