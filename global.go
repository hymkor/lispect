package main

import (
	"context"
	"io"
	"time"

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

func (g *Global) send(ctx context.Context, w *gmnlisp.World, arg gmnlisp.Node) (gmnlisp.Node, error) {
	io.WriteString(g.term, arg.String())
	return gmnlisp.Null, nil
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

var symTimeout = gmnlisp.NewSymbol("timeout")

func (g *Global) expect(ctx context.Context, w *gmnlisp.World, node gmnlisp.Node) (gmnlisp.Node, error) {
	patterns := []string{}
	timeOut := 0

	for gmnlisp.IsSome(node) {
		var value gmnlisp.Node
		var err error

		value, node, err = w.ShiftAndEvalCar(ctx, node)
		if err != nil {
			return nil, err
		}
		if symTimeout.Equals(value, gmnlisp.EQUALP) {
			value, node, err = w.ShiftAndEvalCar(ctx, node)
			if err != nil {
				return nil, err
			}
			_timeout, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, value)
			if err != nil {
				return nil, err
			}
			timeOut = int(_timeout)
		} else {
			patterns = append(patterns, value.String())
		}
	}
	var result int
	if timeOut == 0 {
		result = g.w.Expect(patterns...)
	} else {
		result = g.w.ExpectWithTimeout(time.Second*time.Duration(timeOut), patterns...)
	}
	return gmnlisp.Integer(result), nil
}
