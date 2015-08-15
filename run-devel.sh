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

# name of the yesod app
app="weeklyapp"
# files to ignore
ignore="\.git/*|dist/*|yesod-devel/*|.stack-work/*|static/tmp/*|$app.sqlite3|$app.cabal|stack.yaml|run-devel.sh|^.*\.(o|hi)"

function showHelp() {
  echo "Usage: $0 -w"
  echo ""
  echo "(You need tmux and iwatch)"
}

function startReplYesodDev() {

  # start tmux with three windows:
  # - repl
  # - iwatch (to trigger recompile on file change)
  # - iwatch (to trigger cabal clean and recompile on cabal file change)
  tmux start-server
  tmux new-session -d -s yesod -n repl

  # start yesod with repl
  tmux send-keys -t yesod:repl "stack ghci --ghc-options=\"-O0 -fobject-code\"" C-m
  tmux send-keys -t yesod:repl ":set -DDEVELOPMENT" C-m
  tmux send-keys -t yesod:repl ":l DevelMain" C-m
  tmux send-keys -t yesod:repl "DevelMain.update" C-m

  # main watcher
  tmux split-window
  tmux select-pane -t 1
  tmux send-keys "iwatch -X '$ignore' -r -e close_write -c \"$0 -r\" ." C-m

  # cabal/stack watcher
  tmux split-window
  tmux select-pane -t 2
  tmux send-keys "iwatch -t '$app.cabal|stack.yaml' -e close_write -c \"$0 -c\" ." C-m

  # hide watchers
  tmux select-pane -t 1
  tmux break-pane -d
  tmux select-pane -t 2
  tmux break-pane -d
  # rename watcher windows
  tmux select-window -t yesod:1
  tmux rename-window iwatch
  tmux select-window -t yesod:2
  tmux rename-window iwatch_cabal

  # make the repl window active and attach to session
  tmux select-window -t yesod:repl
  tmux attach-session -t yesod
}

function reloadTemplates() {
  # send reload and update commands to repl window
  echo "reloading..."
  tmux select-window -t yesod:repl
  tmux send-keys -t yesod:repl "DevelMain.shutdown" C-m
  tmux send-keys -t yesod:repl ":l DevelMain" C-m
  tmux send-keys -t yesod:repl "DevelMain.update" C-m
}

function cleanReload() {
  # send clean, build, reload and update commands to repl window
  echo "reloading..."
  tmux select-window -t yesod:repl
  tmux send-keys -t yesod:repl "DevelMain.shutdown" C-m
  tmux send-keys -t yesod:repl C-d
  tmux send-keys -t yesod:repl "stack clean" C-m
  tmux send-keys -t yesod:repl "stack build" C-m
  tmux send-keys -t yesod:repl "stack ghci --ghc-options=\"-O0 -fobject-code\"" C-m
  tmux send-keys -t yesod:repl ":set -DDEVELOPMENT" C-m
  tmux send-keys -t yesod:repl ":l DevelMain" C-m
  tmux send-keys -t yesod:repl "DevelMain.update" C-m
}

if [ "$1" = "-w" ]; then
  startReplYesodDev # startup
elif [ "$1" = "-c" ]; then
  cleanReload # reload with stack clean and build
elif [ "$1" = "-r" ]; then
  reloadTemplates # normal reload
else
  showHelp
fi