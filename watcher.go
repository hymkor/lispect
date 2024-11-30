package main

import (
	"io"
	"os"
	"strings"
	"time"
)

type Watcher struct {
	ch       <-chan string
	lastline string
}

func NewWatcher(pty io.ReadWriter) *Watcher {
	pipeline := make(chan string, 1024)
	go io.Copy(pty, os.Stdin)
	go func() {
		for {
			var buffer [1024]byte
			n, err := pty.Read(buffer[:])
			if err != nil {
				close(pipeline)
				return
			}
			// If the code below spends a lot of time,
			// It hangs up to io.Copy(pty, os.Stdin)
			// The reason is unknown.
			b := buffer[:n]
			os.Stdout.Write(b)
			pipeline <- string(b)
		}
	}()

	return &Watcher{ch: pipeline}
}

func (W *Watcher) updateLastLine() {
}

func (W *Watcher) checkWords(token string, words []string) int {
	W.lastline += token
	for i, word := range words {
		if pos := strings.Index(W.lastline, word); pos >= 0 {
			W.lastline = W.lastline[pos+len(word):]
			return i
		}
	}
	newLinePos := strings.LastIndexByte(W.lastline, '\n')
	if newLinePos >= 0 {
		W.lastline = W.lastline[newLinePos+1:]
	}
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
