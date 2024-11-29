package main

import (
	"context"
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

func mains() error {
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

	_, err = lisp.Interpret(context.Background(), `
		(spawn "cmd.exe")
		(block main
			(while t
				(case (expect* "xxx" "yyy")
					((0)
						(sendln (format nil "~Aexit" (convert 3 <character>)))
						(return-from main nil)
						)
					((1)
						(sendln (format nil "~Arem" (convert 3 <character>)))
						)
					)))`)
	return err
}

func main() {
	if err := mains(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
