PROJECT = logjam
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LFE = _build/$(REBAR_PROFILE)/lib/lfe/bin/lfe
REBAR3 = PATH=.:$(PATH) rebar3
REBAR_PROFILE ?= dev

include priv/make/code.mk
include priv/make/docs.mk


