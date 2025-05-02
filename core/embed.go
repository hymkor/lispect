package core

import (
	"context"
	"fmt"
	"os"
	"runtime"

	"github.com/hymkor/gmnlisp"
)

// version is set by Makefile, thus it is not compatible with
// RunEmbedded unless handled by the downstream developer.
var version string

func RunEmbedded(script string, additionalArgs []string) {
	if err := mains(append([]string{script}, additionalArgs...), true); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}

func Run() {
	if err := mains(os.Args[1:], false); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}

func mains(args []string, embedded bool) error {
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
			gmnlisp.NewSymbol("setenv"):  gmnlisp.Function2(g.setenv),
			gmnlisp.NewSymbol("wait"):    gmnlisp.Function1(g.wait),
		})

	if len(args) <= 0 {
		return fmt.Errorf("%s %s-%s-%s: script path required",
			os.Args[0],
			version,
			runtime.GOOS,
			runtime.GOARCH)
	}
	var script []byte
	if !embedded {
		script, err = os.ReadFile(args[0])
		if err != nil {
			return err
		}
	} else {
		script = []byte(args[0])
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
		gmnlisp.NewSymbol("ARGV"):            gmnlisp.List(posixArgv...),
		gmnlisp.NewSymbol("PROGRAM-NAME"):    gmnlisp.String(args[0]),
		gmnlisp.NewSymbol("EXECUTABLE-NAME"): gmnlisp.String(executable),
	})

	ctx, cancel := context.WithCancel(context.Background())
	_, err = lisp.Interpret(ctx, string(script))
	cancel()
	return err
}
