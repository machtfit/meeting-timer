.PHONY: setup-dev dev release

setup-dev:
	npm install

dev:
	npx shadow-cljs watch main

release:
	rm -rf release 2> /dev/null || true
	cp -r main/ release
	rm -rf release/js/generated 2> /dev/null || true
	npx shadow-cljs release main --config-merge '{:output-dir "release/js/generated"}'
