# Show Git Info and Auto-Completion in Bash

mv `git-completion.bash` and `git-prompt.sh` to somewhere, e.g. ~/git-shell/

open ~/.bashrc

Add the following text to your ~/.bashrc
```
# PS1
# specify(change) the path to your own.
# source ~/git-shell/git-completion.bash
# ...
source ~/git-2.22.0/contrib/completion/git-completion.bash
source ~/git-2.22.0/contrib/completion/git-prompt.sh

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="verbose git svn"

# Example
PS1='\[\e[32m\]\u \[\e[33m\]\w\[\e[36m\]$(__git_ps1 " <%s>")\[\e[31m\]\n\$ \[\e[0m\]'
```

than,
```bash
$ source ~/.bashrc
```
