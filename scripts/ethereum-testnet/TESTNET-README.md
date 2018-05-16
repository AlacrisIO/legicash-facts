# Ethereum test net

The archive geth-data.tgz contains the initial state of the test net.
That file is used by the Gitlab CI to start up the test net.

The other scripts are:
  * run.sh : runs the test net as a background process
  * new-accounts.sh : creates new accounts (result already in initial state)
  * init.sh : initializes state from genesis data (result already in initial state)
  * list-accounts.sh : displays existing accounts
