package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/hymkor/gmnlisp"
)

var (
	symTimeout  = gmnlisp.NewSymbol("timeout")
	symInterval = gmnlisp.NewSymbol("interval")
)

var ErrCtrlC = errors.New("^C")

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

func (g *Global) send(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
	interval := 0
	for len(args) > 0 {
		arg := args[0]
		args = args[1:]
		if symInterval.Equals(arg, gmnlisp.EQUALP) {
			if len(args) <= 0 {
				return nil, errors.New("too few arguments")
			}
			_interval, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, args[0])
			if err != nil {
				return nil, err
			}
			args = args[1:]
			interval = int(_interval)
			continue
		}
		if interval > 0 {
			for _, c := range arg.String() {
				fmt.Fprintf(g.term, "%c", c)
				if interval > 0 {
					time.Sleep(time.Millisecond * time.Duration(interval))
				}
			}
		} else {
			io.WriteString(g.term, arg.String())
		}
	}
	return gmnlisp.Null, nil
}

func (g *Global) sendln(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
	args = append(args, gmnlisp.Node(gmnlisp.String("\r")))
	g.send(ctx, w, args)
	return gmnlisp.Null, nil
}

func (g *Global) spawn(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {

	argStrings := make([]string, 0, len(args))
	for _, s := range args {
		argStrings = append(argStrings, s.String())
	}

	sh := g.term.CommandContext(ctx, argStrings[0], argStrings[1:]...)
	if err := sh.Start(); err != nil {
		return nil, err
	}
	g.closer = append(g.closer, func() { sh.Wait() })
	return gmnlisp.Null, nil
}

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
	if result == EventCtrlC {
		return nil, ErrCtrlC
	}
	return gmnlisp.Integer(result), nil
}

func (g *Global) expectX(ctx context.Context, w *gmnlisp.World, node gmnlisp.Node) (gmnlisp.Node, error) {

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

	if result == EventCtrlC {
		return nil, ErrCtrlC
	} else if result >= 0 {
		return gmnlisp.Progn(ctx, w, actions[result])
	} else {
		return gmnlisp.Progn(ctx, w, timeoutAct)
	}
}

func (g *Global) getenv(ctx context.Context, w *gmnlisp.World, arg gmnlisp.Node) (gmnlisp.Node, error) {
	name, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	value, ok := os.LookupEnv(string(name))
	if !ok {
		return gmnlisp.Null, nil
	}
	return gmnlisp.String(value), nil
}
