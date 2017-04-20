#!/usr/bin/env bash
set -o pipefail

zone=$1

_pep () {
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    cur="${COMP_WORDS[COMP_CWORD]}"

    case "${prev}" in
        pep)
            local nodes=$(cat "$HOME/.local/share/cicd/.nodes-${zone}")
            COMPREPLY=( $(compgen -W "$nodes" -- $cur ) )
            return 0
            ;;
        *)
            local topics=$(cat "$HOME/.local/share/cicd/.topics")
            COMPREPLY=( $(compgen -W "$topics" -- $cur ) )
            return 0
            ;;
    esac

 }

_cmdalias () {
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
        facts)
            COMPREPLY=( $(compgen -W "-s --all -n -g -h --down" -- "$cur" ) )
            return 0
            ;;
        sync|ping)
            COMPREPLY=( $(compgen -W "-s --all -n -g -h" -- "$cur" ) )
            return 0
            ;;
        runpuppet|du)
            COMPREPLY=( $(compgen -W "-s -n -g -h" -- "$cur" ) )
            return 0
            ;;
        result)
            COMPREPLY=( $(compgen -W "-j -n -h" -- "$cur" ) )
            return 0
            ;;
        service)
            COMPREPLY=( $(compgen -W "status restart" -- "$cur" ) )
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
complete -F _cmdalias data runpuppet du facts result service sync ping
complete -F _pep pep

# end
