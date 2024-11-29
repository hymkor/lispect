package main

import (
	"io"
	"os"
	"strings"
	"time"

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

func (W *Watcher) checkWords(token string, words []string) int {
	W.lastline += token
	for i, word := range words {
		if strings.Contains(W.lastline, word) {
			W.updateLastLine()
			return i
		}
	}
	W.updateLastLine()
	return -1

}

func (W *Watcher) Expect(words ...string) int {
	for token := range W.ch {
		if found := W.checkWords(token, words); found >= 0 {
			return found
		}
	}
	return -1
}

func (W *Watcher) ExpectWithTimeout(d time.Duration, words ...string) int {
	timer := time.NewTimer(d)
	defer timer.Stop()

	for {
		select {
		case token := <-W.ch:
			if found := W.checkWords(token, words); found >= 0 {
				return found
			}
		case <-timer.C:
			return -1
		}
	}
}
