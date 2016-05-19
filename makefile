

all: js
	cp target/scala-2.11/lolli-fastopt.js site/

js: target/scala-2.11/lolli-fastopt.js
	sbt fastOptJS
