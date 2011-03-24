#lang racket

(require "safe-scheme.rkt")

(apply safe-files->csharp-files (vector->list (current-command-line-arguments)))
