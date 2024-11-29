package main

import (
	"io"
	"os"
	"strings"

	"github.com/aymanbagabas/go-pty"
)

type Watcher struct {
	ch       <-chan string
	lastline string
}

func NewWatcher(ptmx pty.Pty) *Watcher {
	pipeline := make(chan string, 1024)
	go io.Copy(ptmx, os.Stdin)
	go func() {
		for {
			var buffer [1024]byte
			n, err := ptmx.Read(buffer[:])
			if err != nil {
				close(pipeline)
				return
			}
			// If the code below spends a lot of time,
			// It hangs up to io.Copy(ptmx, os.Stdin)
			// The reason is unknown.
			os.Stdout.Write(buffer[:n])
			pipeline <- string(buffer[:n])
		}
	}()

	return &Watcher{ch: pipeline}
}

func (W *Watcher) updateLastLine() {
	newLinePos := strings.LastIndexByte(W.lastline, '\n')
	if newLinePos >= 0 {
		W.lastline = W.lastline[newLinePos+1:]
	} else {
		W.lastline = ""
	}
}

func (W *Watcher) Expect(words ...string) int {
	for frag := range W.ch {
		W.lastline += frag
		for i, word := range words {
			if strings.Contains(W.lastline, word) {
				W.updateLastLine()
				return i
			}
		}
		W.updateLastLine()
	}
	return -1
}
