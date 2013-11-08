#!/bin/sh

# based on the code examples from this thread:
# http://stackoverflow.com/questions/392022/best-way-to-kill-all-child-processes

killtree() {
    local parent=$1 child
    local sig=${2:-TERM}
    kill -stop ${parent} # needed to stop the quickly forking parent from producing child between the moment we killed all the children and the parent killing
    for child in $(ps -e -o ppid= -o pid= | awk "\$1==$parent {print \$2}"); do
        killtree ${child} ${sig}
    done
    kill -${sig} ${parent}
    kill -cont ${parent} # resume the parent, because stopped processes don't get killed by SIGTERM
}

if [ $# -eq 0 -o $# -gt 2 ]; then
    echo "Usage: $(basename $0) <pid> [signal]"
    exit 1
fi

killtree $@
