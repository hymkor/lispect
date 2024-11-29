package main

import (
	"fmt"
	"io"
	"os"

	"golang.org/x/term"

	"github.com/aymanbagabas/go-pty"

	"github.com/hymkor/go-windows1x-virtualterminal"
)

func loop(ptmx pty.Pty) error {
	watcher := NewWatcher(ptmx)

	sh := ptmx.Command("cmd.exe")
	if err := sh.Start(); err != nil {
		return err
	}

	_ = watcher.Expect("100")
	io.WriteString(ptmx, "exit\r")
	return sh.Wait()
}

func mains() error {
	disableStdout, err := virtualterminal.EnableStdout()
	if err != nil {
		return err
	}
	defer disableStdout()

	disableStdin, err := virtualterminal.EnableStdin()
	if err != nil {
		return err
	}
	defer disableStdin()

	ptmx, err := pty.New()
	if err != nil {
		return err
	}
	defer ptmx.Close()

	width, height, err := term.GetSize(int(os.Stdout.Fd()))
	if err != nil {
		return err
	}
	ptmx.Resize(width, height)

	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		return err
	}
	defer term.Restore(int(os.Stdin.Fd()), oldState)

	return loop(ptmx)
}

func main() {
	if err := mains(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
