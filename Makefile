run:
	@scala-cli run . --main-class lox.run -- $(filter-out $@,$(MAKECMDGOALS))

compile:
	@scala-cli compile .

test:
	@scala-cli test .

clean:
	@scala-cli clean .
	
binary:
	@scala-cli --power package . --main-class lox.run -o lox --native-image --graalvm-jvm-id=graalvm-java23:23.0.2 -f
	@chmod +x lox

