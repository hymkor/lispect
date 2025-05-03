package main

import (
	"fmt"
	"os"
	"runtime"

	"github.com/hymkor/lispect"
)

var version string

func mains(args []string) error {
	if len(args) < 1 {
		return fmt.Errorf("%s %s-%s-%s: script path required",
			os.Args[0],
			version,
			runtime.GOOS,
			runtime.GOARCH)
	}
	return lispect.RunFile(args[0], args)
}
func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
