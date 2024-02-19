#!/usr/bin/env bash

write_host_with_timestamp() {
    if [ -z "$1" ]
    then
        echo "No argument supplied"
        return
    fi

    timestamp=$(date +"%H:%M:%S")
    echo -e "\n── $timestamp - $1 ─────────────"
}
