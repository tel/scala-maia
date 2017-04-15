
# Maia Example App

Example server/client application speaking over a Maia API.

## To run

Kick things off with

```
> sbt
> ~ ;exampleAppUi/fastOptJS ;exampleAppServer/re-start
```

The "development" page is available at

```
http://localhost:12345/ui/target/scala-2.12/classes/index-dev.html
```

This is served by the Workbench server instead of the primary web
server. To access the "production" web server directly go to

```
http://localhost:8080/
```