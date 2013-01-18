REBAR=./rebar
CODE_DIR=apps/popcorn
CT_LOG_DIR=apps/popcorn/ctest/*/logs
NODE?=popcorn
CONFIG?=rel/files/sys.config
SERVER := erl -pa apps/popcorn/ebin -pa deps/*/ebin -smp enable -s lager -setcookie POPCORN -config ${CONFIG} ${ERL_ARGS}

all:
	${REBAR} compile

gc:
	@echo 'Removing all emacs backup files'
	@find . -name "*~" -exec rm -f {} \;
	@find . -name "erl_crash.dump" -exec rm -f {} \;
	@rm -f ${CODE_DIR}/src/*.P
	@rm -f ${CODE_DIR}/src/*/*.P
	@rm -f ${CODE_DIR}/src/*.beam
	@rm -f ${CODE_DIR}/src/*/*.beam
	@echo 'Removing all common_test logs'
	@rm -rf ${CT_LOG_DIR}/*.*
	@rm -f ${CT_LOG_DIR}/variables-ct*

test:
	@make eunit && make ct

eunit: all
	ERL_LIBS=deps ${REBAR} eunit skip_deps=true

## To get the ptl file use
##    dialyzer --build_plt --apps erts kernel stdlib
dialyze: all
	dialyzer --verbose --plt .dialyzer-R15B01.plt \
		-Wunmatched_returns -Werror_handling \
		apps/*/ebin

dialyze_plt: all
	dialyzer --verbose --build_plt \
	  --output_plt .dialyzer-R15B01.plt \
		--apps kernel stdlib sasl erts ssl \
		  tools os_mon runtime_tools crypto \
			inets xmerl webtool eunit syntax_tools \
			compiler edoc hipe mnesia otp_mibs public_key \
			snmp -pa apps/*/ebin

# Please install lessc first using
#   npm install -g less
css: popcorn.css
popcorn.css: apps/popcorn/priv/css/popcorn.less
	/usr/local/bin/lessc apps/popcorn/priv/css/popcorn.less > apps/popcorn/priv/css/popcorn.css

templates:
	cp apps/popcorn/priv/templates/*.mustache apps/popcorn/ebin
	
types:
	typer --plt .dialyzer-R15B01.plt -r apps/popcorn/src -I apps/popcorn/include -I apps/popcorn -pa deps/*/ebin --annotate

xref:
	${REBAR} skip_deps=true xref

shell:
		FOLSOM_PORT=5566 ${SERVER} -name ${NODE}@`hostname` -boot start_sasl -s crypto -s popcorn
