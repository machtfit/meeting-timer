.PHONY: setup-dev dev release

setup-dev:
	yarn install

dev:
	npx shadow-cljs watch main

release:
	if [[ ! -d release ]]; then git clone git@github.com:machtfit/machtfit.github.io.git release; fi
	cd release && git pull
	rm -rf release/meeting-timer 2> /dev/null || true
	cp -r main/ release/meeting-timer
	rm -rf release/meeting-timer/js/generated 2> /dev/null || true
	npx shadow-cljs release main --config-merge '{:output-dir "release/meeting-timer/js/generated"}'

deploy:
	cd release && git add meeting-timer && git commit -a -m 'Deploy'
	cd release && git push
