package lispect

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
	symMatch    = gmnlisp.NewSymbol("MATCH")
)

var ErrCtrlC = errors.New("^C")

type Env struct {
	w      *Watcher
	term   *Term
	closer []func()
}

func (env *Env) Close() {
	for i := len(env.closer) - 1; i >= 0; i-- {
		env.closer[i]()
	}
	env.closer = nil
}

func (env *Env) send(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
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
				fmt.Fprintf(env.term, "%c", c)
				if interval > 0 {
					time.Sleep(time.Millisecond * time.Duration(interval))
				}
			}
		} else {
			io.WriteString(env.term, arg.String())
		}
	}
	return gmnlisp.Null, nil
}

func (env *Env) sendln(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
	args = append(args, gmnlisp.Node(gmnlisp.String("\r")))
	env.send(ctx, w, args)
	return gmnlisp.Null, nil
}

func (env *Env) spawn(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {

	argStrings := make([]string, 0, len(args))
	for _, s := range args {
		argStrings = append(argStrings, s.String())
	}

	cmd := env.term.CommandContext(ctx, argStrings[0], argStrings[1:]...)
	if err := cmd.Start(); err != nil {
		return nil, err
	}
	env.closer = append(env.closer, func() { cmd.Wait() })
	if p := cmd.Process; p != nil {
		return gmnlisp.Integer(p.Pid), nil
	}
	return gmnlisp.Null, nil
}

func (env *Env) expect(ctx context.Context, w *gmnlisp.World, node gmnlisp.Node) (gmnlisp.Node, error) {
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
		result = env.w.Expect(patterns...)
	} else {
		result = env.w.ExpectWithTimeout(time.Second*time.Duration(timeOut), patterns...)
	}
	if result == EventCtrlC {
		return nil, ErrCtrlC
	}
	return gmnlisp.Integer(result), nil
}

func (env *Env) expectX(ctx context.Context, w *gmnlisp.World, node gmnlisp.Node) (gmnlisp.Node, error) {

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
		result = env.w.Expect(patterns...)
	} else {
		result = env.w.ExpectWithTimeout(time.Second*time.Duration(timeoutSec), patterns...)
	}

	if result == EventCtrlC {
		return nil, ErrCtrlC
	} else if result >= 0 {
		_w := w.Let(&gmnlisp.Pair{Key: symMatch, Value: gmnlisp.String(patterns[result])})
		return gmnlisp.Progn(ctx, _w, actions[result])
	} else {
		return gmnlisp.Progn(ctx, w, timeoutAct)
	}
}

func (env *Env) getenv(ctx context.Context, w *gmnlisp.World, arg gmnlisp.Node) (gmnlisp.Node, error) {
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

func (env *Env) setenv(ctx context.Context, w *gmnlisp.World, __key, __val gmnlisp.Node) (gmnlisp.Node, error) {
	_key, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, __key)
	if err != nil {
		return nil, err
	}
	key := string(_key)

	if gmnlisp.IsNull(__val) {
		return gmnlisp.Null, os.Unsetenv(key)
	}
	_val, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, __val)
	if err != nil {
		return nil, err
	}
	val := string(_val)

	return gmnlisp.Null, os.Setenv(key, val)
}

func (env *Env) wait(ctx context.Context, w *gmnlisp.World, pidNode gmnlisp.Node) (gmnlisp.Node, error) {
	pidInteger, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, pidNode)
	if err != nil {
		return nil, err
	}
	pid := int(pidInteger)

	process, err := os.FindProcess(pid)
	if err != nil {
		return gmnlisp.Null, nil
	}
	_, err = process.Wait()
	return gmnlisp.Null, err
}
