package main

import (
	"os"

	"golang.org/x/term"

	"github.com/aymanbagabas/go-pty"

	"github.com/hymkor/go-windows1x-virtualterminal"
)

type Term struct {
	pty.Pty
	closers []func()
}

func (T *Term) Close() {
	for i := len(T.closers) - 1; i >= 0; i-- {
		T.closers[i]()
	}
	T.closers = nil
}

func NewTerm() (*Term, error) {
	T := &Term{}

	disableStdout, err := virtualterminal.EnableStdout()
	if err == nil {
		T.closers = append(T.closers, disableStdout)
	}

	disableStdin, err := virtualterminal.EnableStdin()
	if err != nil {
		T.Close()
		return nil, err
	}
	T.closers = append(T.closers, disableStdin)

	T.Pty, err = pty.New()
	if err != nil {
		T.Close()
		return nil, err
	}
	T.closers = append(T.closers, func() { T.Pty.Close() })

	width, height, err := term.GetSize(int(os.Stdout.Fd()))
	if err == nil {
		T.Pty.Resize(width, height)
	}

	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		T.Close()
		return nil, err
	}
	T.closers = append(T.closers, func() { term.Restore(int(os.Stdin.Fd()), oldState) })
	return T, nil
}
