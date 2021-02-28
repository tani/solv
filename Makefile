all: solv

data: src/cpc.pl src/ipc.pl src/solv.pl
	tar -zcf data $^

solv: src/cli.sh data
	cat $^ > $@
	chmod +x $@

.PHONY: clean

clean:
	rm data solv
