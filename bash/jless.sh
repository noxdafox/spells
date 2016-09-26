# Format JSON files maintaining colored output.
# Function can be added as an alias

jless() {
    jq -C . $1 | less -R
}

jless $1
