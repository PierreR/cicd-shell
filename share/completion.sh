#!/usr/bin/env bash
set -o pipefail

zone=$1

_pep () {
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
        orch)
            COMPREPLY=( $(compgen -W "-s -h" -- "$cur" ) )
            return 0
            ;;
        data)
            COMPREPLY=( $(compgen -W "-s -k -n -g -h" -- "$cur" ) )
            return 0
            ;;
        facts|sync|ping)
            COMPREPLY=( $(compgen -W "-s -a -n -g -h" -- "$cur" ) )
            return 0
            ;;
        runpuppet|du)
            COMPREPLY=( $(compgen -W "-s -n -g -h" -- "$cur" ) )
            return 0
            ;;
        results)
            COMPREPLY=( $(compgen -W "-j -n -h" -- "$cur" ) )
            return 0
            ;;
        service)
            COMPREPLY=( $(compgen -W "status reload" -- "$cur" ) )
            return 0
            ;;
        "-n")
            local nodes=$(cat "$HOME/.local/share/cicd/.nodes-${zone}")
            COMPREPLY=( $(compgen -W "$nodes" -- $cur ) )
            return 0
            ;;
        "-s")
            COMPREPLY=( $(compgen -W "bas bos brucat fidus fmx genericservices gis hms iam irisbox nova middleware plone smartcity tms urbisaddress" -- "$cur" ) )
            return 0
            ;;
    esac

    ret=0;
} &&
complete -F _pep pep data runpuppet du facts

# end
