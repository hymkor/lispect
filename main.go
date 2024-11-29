package main

import (
	"fmt"
	"io"
	"os"
	"time"
)

func mains() error {
	term, err := NewTerm()
	if err != nil {
		return err
	}
	defer term.Close()

	watcher := NewWatcher(term)

	sh := term.Command("cmd.exe")
	if err := sh.Start(); err != nil {
		return err
	}

	i := watcher.ExpectWithTimeout(time.Duration(10*time.Second), "100")
	// i := watcher.Expect("100")
	io.WriteString(term, "\x03exit\r")
	println(i)
	return sh.Wait()
}

func main() {
	if err := mains(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
