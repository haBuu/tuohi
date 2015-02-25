#!/bin/bash

#
# bash script to run yesod-dev's auto reload inside cabal repl (faster template reloading)
#
# see also https://github.com/yesodweb/yesod/issues/754
#
# needs tmux and iwatch
#
# original version: https://gist.github.com/maerten/2c9152f68e2bbefa93ac
#

function showHelp() {
  echo "Usage: $0 -w"
  echo ""
  echo "(You need tmux and iwatch)"
}

function startReplYesodDev() {

  # start tmux with two windows:
  # - repl
  # - iwatch (to trigger recompile on file change)
  tmux start-server
  tmux new-session -d -s ghci_server -n ghci
  tmux split-window -t ghci

  # make the repl window bigger
  tmux resize-pane -t 0 -D 30

  # start yesod with repl
  tmux select-pane -t 0
  tmux send-keys -t ghci_server:ghci "cabal repl --ghc-options=\"-O0 -fobject-code\"" C-m
  tmux send-keys -t ghci_server:ghci ":set -DDEVELOPMENT" C-m
  tmux send-keys -t ghci_server:ghci ":l DevelMain" C-m
  tmux send-keys -t ghci_server:0 "DevelMain.update" C-m

  # watch files with iwatch
  tmux select-pane -t 1
  # watch every file for modify event recursively exluding folders dist/, yesod-devel/ and static/tmp/
  tmux send-keys -t ghci:0 "iwatch -X '\.git/*|dist/*|yesod-devel/*|static/tmp/*|weeklyapp.sqlite3' -r -e close_write -c \"$0 %e\" ." C-m

  # make the repl windows active
  tmux select-window -t ghci_server:ghci
  tmux attach-session -t ghci_server
}

function reloadTemplates() {
  # send reload and update commands to repl
  echo "reloading..."
  tmux select-window -t ghci_server:ghci
  tmux select-pane -t 0

  tmux send-keys -t ghci_server:0 "DevelMain.shutdown" C-m
  tmux send-keys -t ghci_server:ghci ":l DevelMain" C-m
  tmux send-keys -t ghci_server:0 "DevelMain.update" C-m
}

if [ "$1" == "" ]; then
  showHelp
elif [ "$1" = "-w" ]; then
  startReplYesodDev
else
  reloadTemplates
fi