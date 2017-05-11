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
            local mods=$(cat "$HOME/.local/share/cicd/.modlist")
            COMPREPLY=( $(compgen -W "$mods" -- $cur ) )
            return 0
            ;;
    esac

 }

_cmdalias () {
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
        data)
            COMPREPLY=( $(compgen -W "-k -n" -- "$cur" ) )
            return 0
            ;;
        doc)
            COMPREPLY=( $(compgen -W "html mod modules" -- "$cur" ) )
            return 0
            ;;
        facts)
            COMPREPLY=( $(compgen -W "-n" -- "$cur" ) )
            return 0
            ;;
        sync|ping)
            COMPREPLY=( $(compgen -W "-n" -- "$cur" ) )
            return 0
            ;;
        runpuppet|du)
            COMPREPLY=( $(compgen -W "-n" -- "$cur" ) )
            return 0
            ;;
        result)
            COMPREPLY=( $(compgen -W "-j -n" -- "$cur" ) )
            return 0
            ;;
        service)
            COMPREPLY=( $(compgen -W "status restart" -- "$cur" ) )
            return 0
            ;;
        mod)
            local mods=$(cat "$HOME/.local/share/cicd/.modlist")
            COMPREPLY=( $(compgen -W "$mods" -- $cur ) )
            return 0
            ;;
        setfacts)
            local mods=$(cat "$HOME/.local/share/cicd/.modlist")
            COMPREPLY=( $(compgen -W "-n" -- $cur ) )
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

    if [[ "$cur" == -* ]]; then
        COMPREPLY=( $( compgen -W "-g -s -v -h" -- $cur ) )
    fi

    if [[ "$cur" == --* ]]; then
        COMPREPLY=( $( compgen -W "--raw --verbose --all --down --subgroup --role --hostgroup --zone" -- $cur ) )
    fi
    ret=0;
} &&
complete -F _cmdalias data runpuppet du facts result service sync ping doc setfacts
complete -F _pep pep

# end
