# Qwde Backend
The project reads closing-prices and info from the project [pystock-data](https://github.com/eliangcs/pystock-data), which is inserted into a database (sqlite). From there, we do simple analysis and present it using a websocket implementation combined with the all-powerful [plotly](https://plot.ly/) and [tablesaw](https://jtablesaw.github.io/tablesaw/). 

* `analytics` folder contains code for ML, analysis, etc.
* `dataprovider` folder contains code for getting stock-data and making it available for other projects
* `webapi` folder contains code for presenting views and running web server

# Setup
`git clone --recursive https://github.com/andsild/qwde`  
if you forgot recursive:  
`git submodule update --init`

Install openjdk 13 or higher to run the code.
You can build the project with `./gradlew build`

# Debug software
We ship with a java dependency that takes care of all database and network. If you want to look at/debug the database, install sqlite version 3 or higher. After that, you can run something like `sqlite3 $XDG_CACHE_HOME/qwde/database.db`.

# Executables
`./gradlew :webapi:run`  

By default a sqlite database copy of pystock-data is written to   
* Unix: `$XDG_CACHE_HOME` (default ~/.cache/qwde)  
* Windows: `%APPDATA%/qwde/cache`  

Upon running the application, it is checked whether or not the database exists and has data. If not, it is generated. To re-generate the data, you can point `$XDG_CACHE_HOME` elsewhere, or delete the database.db file.

