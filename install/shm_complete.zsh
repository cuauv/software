#
##compdef auv-shm-cli

local -a _shm_subcommands
python -c 'import shm; print(" ".join(shm.__all__[1:]))' | read -A _shm_subcommands
#_shm_subcommands=("${(@f)$(grep '^[a-zA-Z]' libshm/vars.conf | cut -d' ' -f1)}")
#echo ${subcommands[@]}
function _auv-shm-cli {
    local state
    #local integer NORMARG
    #_arguments -n -S -- \
    #    'group: :->group' \
    #    'variable: :->var' \
    #    'value: :->var_val' \

    #if [ $NORMARG == -1 ]; then NORMARG=1; done
    #echo $NORMARG
    _arguments \
        "1: :->group" \
        "2: :->var" \
        "3: :->var_val"

    case $state in
        group) _describe 'auv-shm-cli' _shm_subcommands ;;
        var)
            local -a group_vars
            #echo $words[0] $NORMARG
            #echo $words[$((NORMARG+1))]
            group_vars=("${(@f)$(grep _fields\.append $CUAUV_SOFTWARE/shm/${words[2]}.py | cut -d\" -f2)}")
            _describe 'auv-shm-cli' group_vars
            ;;
        var_val) _message -r "value=$(python -c "import shm; print(shm.$words[2].$words[3].get())")" ;;
    esac
}
compdef _auv-shm-cli auv-shm-cli
#        '-w' \
#        '-r' \
#        '--groups' \
#        '-a[show all]' \

