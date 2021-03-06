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
  cd tuohi
  stack build
  ```

2. Set up database. See config/settings.yml for database config.

3. Edit config/settings.yml as needed

4. Start application by running `stack exec -- yesod devel`

### Running in ghci
run-devel.sh can be used to run application in ghci. It will watch for modifications and reload the application. It has some issues but works most of the time. You can also just run stack ghci and it will start the app and reload it automatically.
