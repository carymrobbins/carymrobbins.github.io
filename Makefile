CURRENT_REV := $(shell git rev-parse hakyll)
REMOTE := $(shell git remote get-url origin)
GIT_NAME := $(shell git config user.name)
GIT_EMAIL := $(shell git config user.email)
GIT_SSHCOMMAND := $(shell git config core.sshcommand)

all: clean watch

site:
	bower install
	stack build

build: site
	stack exec site -- build

watch: site build
	stack exec site -- watch --host 0.0.0.0

clean:
	rm -rf _site/* _store _cache
	stack clean

clone:
	rm -rf _site
	git clone git@github.com:carymrobbins/carymrobbins.github.io.git _site
	cd _site; git config user.name "$(GIT_NAME)"; git config user.email "$(GIT_EMAIL)"
ifdef GIT_SSHCOMMAND
	cd _site; git config core.sshcommand "$(GIT_SSHCOMMAND)"
endif

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

check_git_user:
	@echo "* Validating git config user.name is set"
	test "$(GIT_NAME)" != ""

check_git_email:
	@echo "* Validating git config user.email is set"
	test "$(GIT_EMAIL)" != ""

check_all: check_branch check_tree check_origin check_git_user check_git_email

deploy: check_all clean clone build
	cd _site; git add --all; git commit -m "Deploy $(CURRENT_REV)"; git push
