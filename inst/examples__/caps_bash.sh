#!/bin/bash

pandoc -f markdown -t json caps.md | ./caps_bash.R | pandoc -f json -t html

