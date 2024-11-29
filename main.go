package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/hymkor/gmnlisp"
)

type Global struct {
	w      *Watcher
	term   *Term
	closer []func()
}

func (g *Global) Close() {
	for i := len(g.closer) - 1; i >= 0; i-- {
		g.closer[i]()
	}
	g.closer = nil
}

func (g *Global) sendln(ctx context.Context, w *gmnlisp.World, arg gmnlisp.Node) (gmnlisp.Node, error) {
	io.WriteString(g.term, arg.String())
	g.term.Write([]byte{'\r'})
	return gmnlisp.Null, nil
}

func (g *Global) spawn(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {

	argStrings := make([]string, 0, len(args))
	for _, s := range args {
		argStrings = append(argStrings, s.String())
	}

	sh := g.term.Command(argStrings[0], argStrings[1:]...)
	if err := sh.Start(); err != nil {
		return nil, err
	}
	g.closer = append(g.closer, func() { sh.Wait() })
	return gmnlisp.Null, nil
}

func (g *Global) expect(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
	patterns := make([]string, 0, len(args))
	for _, arg := range args {
		patterns = append(patterns, arg.String())
	}
	result := g.w.Expect(patterns...)
	return gmnlisp.Integer(result), nil
}

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
