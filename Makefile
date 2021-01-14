-include .env

# source: https://github.com/azer/go-makefile-example/blob/master/Makefile

BUILD := $(shell git rev-parse --short HEAD)
PROJECTNAME := $(shell basename "$(PWD)")

# Go related variables.
GOBASE := $(shell pwd)
GOFILES := $(wildcard *.go)
GOROOT := $(shell go env GOROOT)

# -X add string value definition of the form importpath.name=value
RELEASE := -ldflags "-s -w -X project.name=goVectrex"
PACKAGES := ./pkg/cpu ./pkg/logger
PACKAGES_TO_TEST := ./$(PACKAGES)
DISTDIR := ./dist

## build: build all the things (size unoptimized build)
build: build-native

## release: release build (size optimized build)
release: build-native-release

## build-native: build go SDL2 binary
build-native:
	@echo "  >  BUILD debug version"
	@go build -o "$(DISTDIR)/main" $(PACKAGES)
	@echo "  DONE! run main in the dist directory"

build-native-release:
	@echo "  >  BUILD release version"
	@env go build -o "$(DISTDIR)/main.release" $(RELEASE) $(PACKAGES)
	@echo "  DONE! run main.release in the dist directory"

## format: format code using go fmt
format:
	@gofmt -w .

## test: run unit tests
test:
	@go test -cover -v $(PACKAGES_TO_TEST)

## lint: static analyze source
lint:
	#@env GOARCH=wasm GOOS=js go vet ./wasm/...
	@go vet "$(PACKAGES)"

## doc: create project documentation
doc:
	@go doc -all $(PACKAGES)

## clean: removes build files
clean:
	@go clean
	@rm -fr ./dist/*

all: help
help: Makefile
	@echo
	@echo " Choose a command run in "$(PROJECTNAME)":"
	@echo
	@sed -n 's/^##//p' $< | column -t -s ':' |  sed -e 's/^/ /'
	@echo

.PHONY: build release build-native build-native-release wasm-common build-wasm format test lint doc update-go-deps clean help