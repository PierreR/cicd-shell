#!/usr/bin/env bash

# Naming convention:
# - "on" means on a target/node
# - "for" means for a key/topic
set -o pipefail
set -e

zone=$1
pgserver="$PGSERVER_URL"
puppetdb="$PUPPETDB_URL"
salt_user="$SALTAPI_USER"
hostgroup="$STACK"

jq0='.return[0]'

commands () {
    local topic=${1:-'execution'}
    pepper --client=runner "doc.${topic}" | jq "${jq0} | keys"
}

help_for () {
    local topic=$1
    local key=$2
    if [ -z "$1" ]; then echo "Expect a help topic"; return; fi
    if [ -z "$2" ]; then echo "Expect a help key"; return; fi
    pepper --client=runner "doc.${topic}" | jq -r "${jq0}.\"${key}\""
}

stats () {
    pepper --client=runner manage.status | jq '.return[0] | [.up , .up + .down | length] as $stats | {up, down, stats: "\($stats[0]) up of \($stats[1])"}'
}

stack_ping () {
    local hostgroup=${1:-"$STACK"}
    pepper -G "hostgroup:${hostgroup}" test.ping | jq "${jq0}"
}
stack_ping_on () {
    local role=${1}
    local hostgroup=${2:-"$STACK"}
    pepper -C "G@role:${role} and G@hostgroup:${hostgroup}" test.ping | jq "${jq0}"
}

stack_facts () {
    local hostgroup=${1:-"${STACK}"}
    pepper -G "hostgroup:${hostgroup}" grains.item os osrelease fqdn fqdn_ip4 subgroup role | jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[], os:  "\(.os) \(.osrelease)", subgroup, role }'
}

stack_sync () {
    local hostgroup=${1:-"$STACK"}
    pepper -G "hostgroup:${hostgroup}" saltutil.sync_all
}

# launch an orchestration command on your stack (async)
stack-orchestrate () {
    local cmd=$1
    local hostgroup=${2:-"$STACK"}
    if [ -z "$cmd" ]; then echo "expect a orchestration command"; return; fi
    pepper state.orchestrate --client=runner mods="orch.${cmd}" saltenv="${hostgroup}"
}

# dynamic info for all nodes given a specific key (ie: 'docker::version')
stack_data_for () {
    if [ -z "$1" ]; then echo "Expect a pillar key"; return; fi
    local hostgroup=${2:-"$STACK"}
    ( pepper -G "hostgroup:${hostgroup}" grains.item fqdn subgroup role \
      ; pepper -G "hostgroup:${hostgroup}" pillar.item "$1" delimiter='/' ) | jq -s '.[0].return[0] * .[1].return[0]' | jq ".[] | { fqdn, subgroup, role, \"$1\"}"
}

stack_runpuppet_on () {
    local role=${1}
    local hostgroup=${2:-"${STACK}"}
    read -p "Run puppet on all ${role} in ${hostgroup} ? (y/n)"
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
	      pepper -C "G@role:{role} and G@hostgroup:${hostgroup}" --client=local_async puppetutils.run_agent | jq '.return'
    fi
}

node_facts () {
    if [ -z "$1" ]; then echo "expect a target"; return; fi
    pdbquery -t remote  -l "$puppetdb" facts "$1" | jq 'map({"key": .name, value}) | from_entries | {hostgroup, subgroup, role, "os": "\(.operatingsystem) \(.operatingsystemrelease)", "ip": .ipaddress_eth0, uptime}'
}

node_du () {
    if [ -z "$1" ]; then echo "expect a target"; return; fi
    pepper "$1" disk.percent | jq "${jq0}"
}

# dynamic info from configuration
node_data () {
    if [ -z "$1" ]; then echo "Expect a target"; return; fi
    pepper  "$1" pillar.items delimiter='/' | jq "${jq0}"
}

node_runpuppet () {
    local target=$1
    if [ -z "$target" ]; then echo "expect a target"; return; fi
	  pepper "$target" puppetutils.run_agent hostgroup="$hostgroup" zone="$zone" | jq -r '.return[] | to_entries | (.[] | if .value.retcode == 0 then "\nSUCCESS for " else "\nFAILURE for " end + .key + ":" , if .value.stderr != "" then .value.stdout + "\n******\n" + .value.stderr else .value.stdout end)'
}

result () {
    local nb_items=${1:-'1'}
    local rlimit="$(( nb_items - 1 ))"
    local cmd="${pgserver}/salt_result?user=eq.${salt_user}&order=jid.desc"

    if [ $rlimit -le -1 ] ; then echo "Input for the first argument (nb_items) should be above 0 but was ${nb_items}."; return; fi

    curl -f -s -H "Range: 0-${rlimit}" "${cmd}" | jq -C '.'
    if [ ! $? = 0 ]; then echo "Error: please check that ${pgserver} is up and running correctly" ;fi
}

result_for () {
    local jid=$1
    local cmd="${pgserver}/salt_result?select=ret&jid=eq.${jid}"
    curl -f -s "$cmd" | jq -r '(.[].ret[] | if .return.retcode == 0 then "SUCCESS for " else "FAILURE for " end + .id + ":", if .return.stderr != "" then .return.stdout + "\n******\n" + .return.stderr + "\n" else .return.stdout + "\n" end)'
}

refresh_pillar () {
    local role=$1
    local hostgroup=${2:-"$STACK"}
    if [ -z "$role" ]; then echo "expect a role"; return; fi
    pepper -C "G@role:${role} and G@hostgroup:${hostgroup}" saltutil.refresh_pillar
}

# all-facts () {
#     pep '*' grains.item os osrelease fqdn fqdn_ip4 subgroup role | jq '.return[] | .[] | { fqdn, ip: .fqdn_ip4[], os:  "\(.os) \(.osrelease)", subgroup, role }'
# }

# For efficiency sake, cache the completion result in a dot file
regenerate_completion () {
    pepper '*' test.ping | jq '.return[0]' | jq keys | jq -r 'join (" ")' > ".nodes-${zone}"
}

if ! [[ -f ".nodes-${zone}" ]]; then
    regenerate_completion
fi

set +o pipefail
set +e

_pep_completion () {
    local nodes=$(cat ".nodes-${zone}")
    COMPREPLY=( $(compgen -W "$nodes" -- ${COMP_WORDS[COMP_CWORD]}) )
    return 0
}

_hostgroup_completion () {
    COMPREPLY=( $(compgen -W "middleware bos nova gis brucat django fidus iam irisbox plone tms" -- ${COMP_WORDS[COMP_CWORD]}) )
    return 0
}

_help_topic () {
    COMPREPLY=( $(compgen -W "wheel runner execution" -- ${COMP_WORDS[COMP_CWORD]}) )
    return 0
}

_help_completion () {
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
        runner)
            local doc=$(cat ".doc-runner")
            COMPREPLY=( $(compgen -W "$doc" -- ${cur}) )
            return 0
            ;;
        wheel)
            local doc=$(cat ".doc-wheel")
            COMPREPLY=( $(compgen -W "$doc" -- ${cur}) )
            return 0
            ;;
        execution)
            local doc=$(cat ".doc-execution")
            COMPREPLY=( $(compgen -W "$doc" -- ${cur}) )
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "wheel runner execution" -- ${cur}) )
            return 0
            ;;
    esac
}
complete -F _pep_completion pepper data_on run_puppet_on du_on facts_on _facts_on
complete -F _hostgroup_completion ping facts
complete -F _help_topic commands_for
complete -F _help_completion help_for
