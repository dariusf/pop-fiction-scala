

all: js
	cp target/scala-2.11/lolli-fastopt.js site/

target/scala-2.11/lolli-fastopt.js: src/main/scala/*.scala
	sbt fastOptJS

js: target/scala-2.11/lolli-fastopt.js
