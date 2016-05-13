elm-sh:
	docker-compose run elm /bin/bash

test:
	cd test && \
	elm package install -y && \
	elm make TestRunner.elm --output tests.js && \
	node tests.js

.PHONY: test
