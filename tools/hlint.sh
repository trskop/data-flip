#!/bin/bash

main()
{

    local -a hlint_flags=()
    local -a dirs=()

    # Colorize output if stdout is a terminal.
    if [ -t 1 ]; then
        hlint_flags=("${hlint_flags[@]}" '--colour')
    fi

    local dir=''
    for dir in 'src' 'test' 'tests'; do
        if [ -d "$dir" ]; then
            dirs=("${dirs[@]}" "$dir")
        fi
    done

    # Use current directory if no known subdirectories with source code were
    # found.
    if (( "${#dirs}" == 0 )); then
        dirs=('.')
    fi

    hlint "${hlint_flags[@]}" "${dirs[@]}" "$@"
}

main "$@"
