CURRENT_REV := $(shell git rev-parse hakyll)

all: watch

site: clean
	stack build

build: site
	stack exec site -- build

watch: site build
	stack exec site -- watch -h 0.0.0.0

clean:
	rm -rf _site/* _store _cache
	stack clean

clone:
	rm -rf _site
	git clone git@github.com:carymrobbins/carymrobbins.github.io.git _site

check_branch:
	@echo "* Validating on hakyll branch"
	test "$(shell git rev-parse --abbrev-ref HEAD)" = "hakyll"

check_tree:
	@echo "* Validating tree is committed"
	git diff-index --quiet HEAD

check_origin:
	@echo "* Validating local matches remote"
	git fetch origin
	test "$(CURRENT_REV)" = "$(shell git rev-parse origin/hakyll)"

check_all: check_branch check_tree check_origin

deploy: check_all clean clone build
	cd _site; git add --all; git commit -m "Deploy $(CURRENT_REV)"; git push
