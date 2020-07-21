# Local Variables:
# mode: makefile
# indent-tabs-mode: nil
# End:
# vim: set ft=make :

alias show-secret := view-secret

provision tags='':
    #!/usr/bin/env bash
    username=$(yq '.all.vars.username' hosts.yml)
    username=${username//\"/}

    if [ -z {{tags}} ]; then
        ansible-playbook -i hosts.yml -u $username -K local.yml
    else
        ansible-playbook -i hosts.yml -u $username -K local.yml -t {{tags}}
    fi

list-tags:
    ansible-playbook --list-tags local.yml

# copy the SSH key, and create an ansible inventory (hosts.yml)
setup-box username ip port='22':
    #!/usr/bin/env bash
    ssh-copy-id -p {{port}} {{username}}@{{ip}}
    cat <<EOF > hosts.yml
    ---
    # Autogenerated by: \`just setup-box {{username}} {{ip}} {{port}}\`
    all:
      vars:
        username: {{username}}
      hosts:
        the_machine:
          ansible_host: {{ip}}
          ansible_port: {{port}}
          ansible_python_interpreter: auto_silent
    EOF

# for debugging
remove-user-from-wheel-group:
    #!/usr/bin/env bash
    username=$(yq '.all.vars.username' hosts.yml)
    username=${username//\"/}

    ansible -i hosts.yml -u $username the_machine -K \
            -a "gpasswd -d $username wheel" --become --become-method su

decrypt filename:
    ansible-vault decrypt {{filename}}

encrypt filename:
    ansible-vault encrypt {{filename}}

create-secret filename:
    ansible-vault create {{filename}}

view-secret filename:
    ansible-vault view {{filename}}

edit-secret filename:
    ansible-vault edit {{filename}}