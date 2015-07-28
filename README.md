WeeklyApp
==============================

Web application for disc golf competition live scoring. Currently used in Tampere's disc golf club's weekly and PDGA competitions. Includes player import from SFL Kisakone and score export to PDGA. 

### Requirements
You must have Haskell, Yesod and MySQL/MariaDB installed.

Instructions for installing Haskell and Yesod are available at http://www.yesodweb.com/page/quickstart

### Installation
1. Clone repo and install packages
  ```
  git clone https://github.com/haBuu/tuohi.git
  cabal sandbox init
  cabal install --only-dependencies
  ```
2. Set up MySQL. See config/settings.yml for database config.
3. Edit config/settings.yml as needed
4. Start application by running `yesod devel`

### ghci
run-devel.sh can be used to run application inside. It will watch for modifications and reload the application.
