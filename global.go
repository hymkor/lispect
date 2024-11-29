package main

import (
	"context"
	"io"

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
