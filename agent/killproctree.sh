#!/bin/sh

# based on the code examples from this thread:
# http://stackoverflow.com/questions/392022/best-way-to-kill-all-child-processes

killtree() {
    local parent=$1 child
    local sig=${2:-TERM}
    echo parent: $parent sig: $sig
    for child in $(ps -e -o ppid= -o pid= | awk "\$1==$parent {print \$2}"); do
        killtree ${child} ${sig}
    done
    kill -${sig} ${parent}
}

if [ $# -eq 0 -o $# -gt 2 ]; then
    echo "Usage: $(basename $0) <pid> [signal]"
    exit 1
fi

killtree $@
