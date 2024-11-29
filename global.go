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

func (g *Global) expectX(ctx context.Context, w *gmnlisp.World, node gmnlisp.Node) (gmnlisp.Node, error) {
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

func (g *Global) expect(ctx context.Context, w *gmnlisp.World, node gmnlisp.Node) (gmnlisp.Node, error) {

	patterns := []string{}
	actions := []gmnlisp.Node{}

	timeoutSec := 0
	var timeoutAct gmnlisp.Node

	for gmnlisp.IsSome(node) {
		var err error
		var _patAndAct gmnlisp.Node
		_patAndAct, node, err = gmnlisp.Shift(node)
		if err != nil {
			return nil, err
		}
		patAndAct, err := gmnlisp.ExpectClass[*gmnlisp.Cons](ctx, w, _patAndAct)
		if err != nil {
			return nil, err
		}
		pat := patAndAct.Car
		act := patAndAct.Cdr

		if patInt, ok := pat.(gmnlisp.Integer); ok {
			timeoutSec = int(patInt)
			timeoutAct = act
		} else if _, ok := pat.(*gmnlisp.Cons); ok {
			for gmnlisp.IsSome(pat) {
				var pat1 gmnlisp.Node

				pat1, pat, err = gmnlisp.Shift(pat)
				if err != nil {
					return nil, err
				}
				actions = append(actions, act)
				patterns = append(patterns, pat1.String())
			}
		} else {
			actions = append(actions, act)
			patterns = append(patterns, pat.String())
		}
	}

	var result int
	if timeoutSec == 0 {
		result = g.w.Expect(patterns...)
	} else {
		result = g.w.ExpectWithTimeout(time.Second*time.Duration(timeoutSec), patterns...)
	}
	if result >= 0 {
		return gmnlisp.Progn(ctx, w, actions[result])
	} else {
		return gmnlisp.Progn(ctx, w, timeoutAct)
	}
}
