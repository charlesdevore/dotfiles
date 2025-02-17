# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Set Emacs as default editor
export EDITOR="emacs -nw"

# Add MacTex to the path
export PATH="$PATH:/Library/TeX/texbin"

# Add usr local to the path
export PATH="$PATH:/usr/local/bin"

# Add a variable pointing to the Anaconda directory
export ANACONDA_HOME="/opt/miniconda3/"
